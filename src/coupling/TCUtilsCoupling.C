//////////////////////////////////////////////////////////////////////////////////////
//       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
//      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
//     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
//      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
//  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
//  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
//                                                                                  //
//  Originally developed by D. Pizzocri & T. Barani                                 //
//                                                                                  //
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Constants.h"
#include "TCUtilsCoupling.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstddef>
#include <exception>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

// Thermochimica C interface: bind(C) TCAPI_* routines of thermochimica-for-sciantix/src/api/CouplingUtilitiesISO_C.f90
// (prototypes as in thermochimica-for-sciantix/src/Thermochimica.h). Strings are passed as (pointer, length by value).
extern "C"
{
    void  TCAPI_setThermoFilename(const char*, std::size_t);
    void  TCAPI_setUnitTemperature(const char*, std::size_t);
    void  TCAPI_setUnitPressure(const char*, std::size_t);
    void  TCAPI_setUnitMass(const char*, std::size_t);
    void  TCAPI_setTemperaturePressure(double*, double*);
    void  TCAPI_setElementMass(int*, double*);
    void  TCAPI_setPrintResultsMode(int*);
    void  TCAPI_checkInfoThermo(int*);
    void  TCAPI_parseCSDataFile();
    void  TCAPI_thermochimica();
    void  TCAPI_resetThermo();
    void  TCAPI_resetThermoAll();
    void  TCAPI_saveReinitData();
    void  TCAPI_setReinitRequested(int*);
    void  TCAPI_getNumberElementsDatabase(int*);
    char* TCAPI_getElementAtIndex(int*, int*);
    void  TCAPI_getNumberPhasesSystem(int*, int*);
    char* TCAPI_getPhaseNameAtIndex(int*, int*);
    void  TCAPI_getNumberSpeciesSystem(int*);
    char* TCAPI_getSpeciesAtIndex(int*, int*);
    void  TCAPI_isPhaseGas(const int*, bool*);
    void  TCAPI_getSolnPhaseMol(const char*, std::size_t, double*, int*);
    void  TCAPI_getPureConPhaseMol(const char*, std::size_t, double*, int*);
    void  TCAPI_getElementMolesInPhase(const char*, std::size_t, const char*, std::size_t, double*, int*);
    void  TCAPI_getOutputSolnSpecies(const char*, std::size_t, const char*, std::size_t, double*, double*, int*);
    void  TCAPI_getOutputChemPot(const char*, std::size_t, double*, int*);
}

namespace TCUtilsCouplingDetail
{
    std::string trimCopy(const std::string& text)
    {
        const auto begin = text.find_first_not_of(" \t\r\n");
        if (begin == std::string::npos)
            return "";
        const auto end = text.find_last_not_of(" \t\r\n");
        return text.substr(begin, end - begin + 1);
    }

    std::string upperCopy(std::string text)
    {
        std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) { return std::toupper(c); });
        return text;
    }

    std::string lowerCopy(std::string text)
    {
        std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) { return std::tolower(c); });
        return text;
    }

    // Fortran strings returned by pointer + length (not NUL-terminated)
    std::string fortranString(const char* buffer, int length)
    {
        if (buffer == nullptr || length <= 0)
            return "";
        return trimCopy(std::string(buffer, buffer + length));
    }

    bool isLiquidPhase(const std::string& phase_name)
    {
        return phase_name == "liquid" || phase_name == "ionic_liquid" || phase_name == "liquid_ionic";
    }

    // Labels follow the OpenCalphad conventions used in input_thermochemistry.txt:
    // upper case, without the Thermochimica state suffix of pure condensed phases, e.g. "Cs2Te(s)" -> "CS2TE".
    std::string compoundLabel(const std::string& thermochimica_name)
    {
        std::string name = trimCopy(thermochimica_name);
        for (const std::string suffix : {"(s)", "(l)", "(g)", "(S)", "(L)", "(G)"})
        {
            if (name.size() > suffix.size() && name.compare(name.size() - suffix.size(), suffix.size(), suffix) == 0)
            {
                name.erase(name.size() - suffix.size());
                break;
            }
        }
        return upperCopy(trimCopy(name));
    }

    double atomicMass(const std::string& element_name)
    {
        const auto atomic_mass = thermochemistry_atomic_masses.find(element_name);
        if (atomic_mass == thermochemistry_atomic_masses.end())
            throw std::runtime_error("Atomic mass not available for element " + element_name);

        return atomic_mass->second;
    }

    // Element stoichiometry from a species name, e.g. "Cs2Te3" -> {Cs: 2, Te: 3}, "Cs+:Va" -> {Cs: 1, Va: 1}.
    // Same parsing rules as the OpenCalphad coupling (two-letter element symbols first, charges skipped).
    std::map<std::string, double> speciesStoichiometry(const std::string&              species_name,
                                                       const std::vector<std::string>& valid_elements)
    {
        std::map<std::string, std::string> valid_set;
        for (const auto& element : valid_elements)
            valid_set[upperCopy(element)] = element;

        std::map<std::string, double> stoichiometry;
        size_t                        i = 0;
        while (i < species_name.size())
        {
            const unsigned char character = static_cast<unsigned char>(species_name[i]);
            if (species_name[i] == '+' || species_name[i] == '-')
            {
                ++i;
                while (i < species_name.size() && std::isdigit(static_cast<unsigned char>(species_name[i])))
                    ++i;
                continue;
            }

            if (species_name[i] == ':' || species_name[i] == '_' || !std::isalpha(character))
            {
                ++i;
                continue;
            }

            std::string element;
            if (i + 2 <= species_name.size())
            {
                const std::string candidate = upperCopy(species_name.substr(i, 2));
                if (candidate == "VA")
                {
                    element = "Va";
                    i += 2;
                }
                else
                {
                    const auto it = valid_set.find(candidate);
                    if (it != valid_set.end())
                    {
                        element = it->second;
                        i += 2;
                    }
                }
            }

            if (element.empty())
            {
                const std::string candidate = upperCopy(species_name.substr(i, 1));
                const auto        it        = valid_set.find(candidate);
                if (it != valid_set.end())
                {
                    element = it->second;
                    ++i;
                }
                else
                {
                    ++i;
                    continue;
                }
            }

            double       count       = 1.0;
            const size_t count_begin = i;
            while (i < species_name.size() && std::isdigit(static_cast<unsigned char>(species_name[i])))
                ++i;
            if (i > count_begin)
                count = std::stod(species_name.substr(count_begin, i - count_begin));

            stoichiometry[element] += count;
        }

        return stoichiometry;
    }

    void addElementInventory(std::map<std::string, double>& target, const std::map<std::string, double>& source)
    {
        for (const auto& element_entry : source)
            target[element_entry.first] += element_entry.second;
    }

    std::string resolveDatabasePath(const std::string& path)
    {
        for (const std::string& candidate : {path, path + ".dat", path + ".DAT"})
        {
            if (std::ifstream(candidate).good())
                return candidate;
        }
        return path;
    }

    bool fileExists(const std::string& file_path)
    {
        std::ifstream file(file_path);
        return static_cast<bool>(file);
    }
}  // namespace TCUtilsCouplingDetail

using namespace TCUtilsCouplingDetail;

namespace TCAdapter
{
    // Thermochimica keeps its whole state in Fortran module variables: a single database can
    // be loaded at a time, which is enough for the fission-product (grain-boundary) chemistry.
    class ThermochimicaInterface
    {
      public:
        bool solve(const std::string&                      database_path,
                   double                                  temperature,
                   double                                  pressure,
                   const std::vector<InputComponent>&      components,
                   TCUtilsCoupling::ThermochimicaSolveMode solve_mode,
                   OCOutputData&                           output_data);

      private:
        bool ensureDatabaseLoaded(const std::string& database_path);
        void discardState();
        bool
        extractResults(const std::vector<InputComponent>& components, double temperature, OCOutputData& output_data);

        bool                     database_loaded_ = false;
        bool                     reinit_saved_    = false;
        std::string              loaded_database_path_;
        std::vector<std::string> database_elements_;
    };

    ThermochimicaInterface& getThermochimicaInterface()
    {
        static ThermochimicaInterface thermochimica_interface;
        return thermochimica_interface;
    }

    void ThermochimicaInterface::discardState()
    {
        TCAPI_resetThermoAll();  // also resets the reinit data
        database_loaded_ = false;
        reinit_saved_    = false;
        loaded_database_path_.clear();
        database_elements_.clear();
    }

    bool ThermochimicaInterface::ensureDatabaseLoaded(const std::string& database_path)
    {
        const std::string resolved_path = resolveDatabasePath(database_path);
        if (database_loaded_ && resolved_path == loaded_database_path_)
            return true;

        discardState();

        if (!fileExists(resolved_path))
        {
            std::cerr << "Error: Thermochimica database not found: " << database_path << std::endl;
            return false;
        }

        TCAPI_setThermoFilename(resolved_path.c_str(), resolved_path.length());
        TCAPI_parseCSDataFile();

        int info = 0;
        TCAPI_checkInfoThermo(&info);
        if (info != 0)
        {
            std::cerr << "Error: Thermochimica failed to parse database " << resolved_path << " (INFOThermo = " << info
                      << ")" << std::endl;
            discardState();
            return false;
        }

        int n_elements = 0;
        TCAPI_getNumberElementsDatabase(&n_elements);
        for (int index = 1; index <= n_elements; ++index)
        {
            int         length = 0;
            const char* name   = TCAPI_getElementAtIndex(&index, &length);
            database_elements_.push_back(fortranString(name, length));
        }

        loaded_database_path_ = resolved_path;
        database_loaded_      = true;
        return true;
    }

    bool ThermochimicaInterface::solve(const std::string&                      database_path,
                                       double                                  temperature,
                                       double                                  pressure,
                                       const std::vector<InputComponent>&      components,
                                       TCUtilsCoupling::ThermochimicaSolveMode solve_mode,
                                       OCOutputData&                           output_data)
    {
        output_data.solution_phases.clear();
        output_data.components.clear();

        if (solve_mode == TCUtilsCoupling::ThermochimicaSolveMode::FreshParseRecovery)
            discardState();

        if (!ensureDatabaseLoaded(database_path))
            return false;

        std::map<std::string, double> amounts;
        for (const auto& component : components)
        {
            if (std::find(database_elements_.begin(), database_elements_.end(), component.name) ==
                database_elements_.end())
            {
                std::cerr << "Error: element " << component.name << " is not in the Thermochimica database "
                          << loaded_database_path_ << std::endl;
                return false;
            }
            amounts[component.name] = component.fraction;
        }

        // CheckThermoInput converts T and P in place and overwrites the unit settings (to K and atm):
        // units, temperature and pressure must be set again before every calculation.
        TCAPI_setUnitTemperature("K", 1);
        TCAPI_setUnitPressure("Pa", 2);
        TCAPI_setUnitMass("moles", 5);
        int print_mode = 0;
        TCAPI_setPrintResultsMode(&print_mode);
        TCAPI_setTemperaturePressure(&temperature, &pressure);

        // Element amounts are not reset between calculations: every database element is set.
        for (const auto& element : database_elements_)
        {
            const auto atomic_number = thermochemistry_atomic_numbers.find(element);
            if (atomic_number == thermochemistry_atomic_numbers.end())
            {
                std::cerr << "Error: atomic number not available for Thermochimica element " << element << std::endl;
                return false;
            }
            int    z      = atomic_number->second;
            double amount = amounts.count(element) ? amounts.at(element) : 0.0;
            TCAPI_setElementMass(&z, &amount);
        }

        int reinit_requested =
            (solve_mode == TCUtilsCoupling::ThermochimicaSolveMode::ReinitWarmStart && reinit_saved_) ? 1 : 0;
        TCAPI_setReinitRequested(&reinit_requested);

        TCAPI_thermochimica();

        int info = 0;
        TCAPI_checkInfoThermo(&info);
        if (info != 0)
        {
            std::cerr << "Warning: Thermochimica calculation failed at T = " << temperature << " K, P = " << pressure
                      << " Pa (INFOThermo = " << info << ")" << std::endl;
            // Same recovery as Thermochimica's own RunCalculationList: reset all and re-parse next time.
            discardState();
            return false;
        }

        bool extracted = false;
        try
        {
            extracted = extractResults(components, temperature, output_data);
        }
        catch (const std::exception& e)
        {
            std::cerr << "Exception while reading Thermochimica results: " << e.what() << std::endl;
            extracted = false;
        }

#if !defined(COUPLING_TU)
        // In the TRANSURANUS coupling consecutive calls belong to different nodes:
        // a warm start from the previous node would bias the solution.
        if (extracted)
        {
            TCAPI_saveReinitData();
            reinit_saved_ = true;
        }
#endif

        // Results must be read before this call, which deallocates them.
        TCAPI_resetThermo();

        if (!extracted)
        {
            output_data.solution_phases.clear();
            output_data.components.clear();
        }
        return extracted;
    }

    bool ThermochimicaInterface::extractResults(const std::vector<InputComponent>& components,
                                                double                             temperature,
                                                OCOutputData&                      output_data)
    {
        output_data.solution_phases.clear();
        output_data.components.clear();

        int n_solution_phases = 0, n_condensed_phases = 0;
        TCAPI_getNumberPhasesSystem(&n_solution_phases, &n_condensed_phases);

        // Cumulative number of species of the solution phases in the system
        // (species of phase i: species_end[i-2]+1 ... species_end[i-1], 1-based).
        std::vector<int> species_end(static_cast<size_t>(std::max(n_solution_phases, 0)) + 64, 0);
        if (n_solution_phases > 0)
            TCAPI_getNumberSpeciesSystem(species_end.data());

        auto elementMolesInPhase = [&](const std::string& phase_name)
        {
            std::map<std::string, double> elements;
            for (const auto& element : database_elements_)
            {
                double moles = 0.0;
                int    info  = 0;
                TCAPI_getElementMolesInPhase(
                    element.c_str(), element.length(), phase_name.c_str(), phase_name.length(), &moles, &info);
                // INFO > 0 is not reliable in GetElementMolesInPhase; only INFO < 0 means no valid result.
                if (info >= 0 && std::isfinite(moles) && moles > 0.0)
                    elements[element] = moles;
            }
            return elements;
        };

        auto elementMasses = [](const std::map<std::string, double>& elements)
        {
            std::map<std::string, double> masses;
            for (const auto& element_entry : elements)
                masses[element_entry.first] = element_entry.second * atomicMass(element_entry.first);
            return masses;
        };

        auto totalMass = [](const std::map<std::string, double>& masses)
        {
            double mass = 0.0;
            for (const auto& mass_entry : masses)
                mass += mass_entry.second;
            return mass;
        };

        // Solution phases
        for (int phase_index = 1; phase_index <= n_solution_phases; ++phase_index)
        {
            int               length     = 0;
            const char*       name       = TCAPI_getPhaseNameAtIndex(&phase_index, &length);
            const std::string phase_name = fortranString(name, length);

            double phase_moles = 0.0;
            int    info        = 0;
            TCAPI_getSolnPhaseMol(phase_name.c_str(), phase_name.length(), &phase_moles, &info);
            if (info != 0 || !(phase_moles > 0.0))
                continue;  // not in the stable assemblage

            bool is_gas = false;
            TCAPI_isPhaseGas(&phase_index, &is_gas);

            std::string phase_bucket = lowerCopy(phase_name);
            if (is_gas)
                phase_bucket = "gas";
            else if (!isLiquidPhase(phase_bucket))
                phase_bucket = "condensed";

            OCPhaseData phase_data;
            phase_data.moles          = phase_moles;
            phase_data.form_units     = phase_moles;
            phase_data.elements       = elementMolesInPhase(phase_name);
            phase_data.element_masses = elementMasses(phase_data.elements);
            phase_data.mass           = totalMass(phase_data.element_masses);

            // Thermochimica exposes no sublattice constituent names through its API: each solution
            // phase is reported as one pseudo-sublattice whose constituents are the phase species
            // with their mole fractions.
            OCSublatticeData sublattice;
            sublattice.index            = 1;
            sublattice.sites            = 1.0;
            sublattice.phase_moles      = phase_moles;
            sublattice.phase_form_units = phase_moles;
            sublattice.phase_instance   = upperCopy(phase_name);

            std::map<std::string, OCSpeciesData> phase_species;
            const int first_species = (phase_index == 1) ? 1 : species_end[phase_index - 2] + 1;
            const int last_species  = species_end[phase_index - 1];
            for (int species_index = first_species; species_index <= last_species; ++species_index)
            {
                int               species_length = 0;
                const char*       species_buffer = TCAPI_getSpeciesAtIndex(&species_index, &species_length);
                const std::string species_name   = fortranString(species_buffer, species_length);
                if (species_name.empty())
                    continue;

                double mole_fraction = 0.0, chemical_potential = 0.0;
                int    species_info = 0;
                TCAPI_getOutputSolnSpecies(phase_name.c_str(),
                                           phase_name.length(),
                                           species_name.c_str(),
                                           species_name.length(),
                                           &mole_fraction,
                                           &chemical_potential,
                                           &species_info);
                if (species_info != 0 || !std::isfinite(mole_fraction) || mole_fraction <= 0.0)
                    continue;

                const std::string label = compoundLabel(species_name);
                sublattice.composition[label] += mole_fraction;

                OCSpeciesData& species = phase_species[label];
                const double   moles   = mole_fraction * phase_moles;
                species.moles += moles;
                for (const auto& element_entry : speciesStoichiometry(species_name, database_elements_))
                {
                    if (element_entry.first == "Va")
                        continue;
                    const double element_moles = element_entry.second * moles;
                    const double element_mass  = element_moles * atomicMass(element_entry.first);
                    species.elements[element_entry.first] += element_moles;
                    species.element_masses[element_entry.first] += element_mass;
                    species.mass += element_mass;
                }
            }
            sublattice.constituents_count = static_cast<int>(sublattice.composition.size());

            OCPhaseData& output_phase = output_data.solution_phases[phase_bucket];
            output_phase.moles += phase_data.moles;
            output_phase.form_units += phase_data.form_units;
            output_phase.mass += phase_data.mass;
            addElementInventory(output_phase.elements, phase_data.elements);
            addElementInventory(output_phase.element_masses, phase_data.element_masses);

            if (phase_bucket == "condensed")
            {
                // Same as OpenCalphad: a condensed solution phase is one "species" named after the phase.
                OCSpeciesData& species = output_phase.species[upperCopy(phase_name)];
                species.moles += phase_data.form_units;
                species.mass += phase_data.mass;
                species.sublattices.push_back(sublattice);
                addElementInventory(species.elements, phase_data.elements);
                addElementInventory(species.element_masses, phase_data.element_masses);
            }
            else
            {
                output_phase.sublattices.push_back(sublattice);
                for (const auto& species_entry : phase_species)
                {
                    OCSpeciesData& species = output_phase.species[species_entry.first];
                    species.moles += species_entry.second.moles;
                    species.mass += species_entry.second.mass;
                    addElementInventory(species.elements, species_entry.second.elements);
                    addElementInventory(species.element_masses, species_entry.second.element_masses);
                }
            }
        }

        // Pure condensed phases (stoichiometric compounds)
        for (int phase_index = n_solution_phases + 1; phase_index <= n_solution_phases + n_condensed_phases;
             ++phase_index)
        {
            int               length     = 0;
            const char*       name       = TCAPI_getPhaseNameAtIndex(&phase_index, &length);
            const std::string phase_name = fortranString(name, length);

            double phase_moles = 0.0;
            int    info        = 0;
            TCAPI_getPureConPhaseMol(phase_name.c_str(), phase_name.length(), &phase_moles, &info);
            if (info != 0 || !(phase_moles > 0.0))
                continue;  // not in the stable assemblage

            const std::string label = compoundLabel(phase_name);

            OCSublatticeData sublattice;
            sublattice.index              = 1;
            sublattice.constituents_count = 1;
            sublattice.sites              = 1.0;
            sublattice.phase_moles        = phase_moles;
            sublattice.phase_form_units   = phase_moles;
            sublattice.phase_instance     = label;
            sublattice.composition[label] = 1.0;

            std::map<std::string, double> elements = elementMolesInPhase(phase_name);
            if (elements.empty())
            {
                // GetElementMolesInPhase skips the pure condensed phases when no solution phase is stable:
                // the element amounts of the compound come from its formula (name before "_" or "(").
                const std::string formula = phase_name.substr(0, phase_name.find_first_of("_("));
                for (const auto& element_entry : speciesStoichiometry(formula, database_elements_))
                {
                    if (element_entry.first != "Va")
                        elements[element_entry.first] = element_entry.second * phase_moles;
                }
            }
            const std::map<std::string, double> masses = elementMasses(elements);
            const double                        mass   = totalMass(masses);

            OCPhaseData& output_phase = output_data.solution_phases["condensed"];
            output_phase.moles += phase_moles;
            output_phase.form_units += phase_moles;
            output_phase.mass += mass;
            addElementInventory(output_phase.elements, elements);
            addElementInventory(output_phase.element_masses, masses);

            OCSpeciesData& species = output_phase.species[label];
            species.moles += phase_moles;
            species.mass += mass;
            species.sublattices.push_back(sublattice);
            addElementInventory(species.elements, elements);
            addElementInventory(species.element_masses, masses);
        }

        // Components: input amounts and element chemical potentials (J/mol)
        constexpr double gas_constant_si = 8.31446261815324;
        for (const auto& component : components)
        {
            OCComponentData component_data;
            component_data.moles         = component.fraction;
            component_data.mass          = component.fraction * atomicMass(component.name);
            component_data.mole_fraction = component.fraction;

            double chemical_potential = 0.0;
            int    info               = 0;
            TCAPI_getOutputChemPot(component.name.c_str(), component.name.length(), &chemical_potential, &info);
            if (info == 0 && temperature > 0.0)
            {
                component_data.chemical_potential_over_rt = chemical_potential / (gas_constant_si * temperature);
                component_data.activity                   = std::exp(component_data.chemical_potential_over_rt);
            }
            output_data.components[component.name] = component_data;
        }

        return !output_data.solution_phases.empty();
    }
}  // namespace TCAdapter

namespace TCUtilsCoupling
{
    bool writePhaseSublatticeCompositionOutput(const std::string&  file_path,
                                               double              time_hours,
                                               const std::string&  location,
                                               const OCOutputData& output_data,
                                               double              content_scaling_factor)
    {
        const bool    write_header = !fileExists(file_path);
        std::ofstream output_file(file_path, std::ios::app);
        if (!output_file)
            return false;

        if (write_header)
        {
            output_file << "Time (h)\tLocation\tPhase\tPhase instance\tMoles (mol/m3)\t"
                        << "Form units (mol/m3)\tSublattice\tSites\tConstituent\tSite fraction\n";
        }

        output_file << std::setprecision(10);
        for (const auto& phase_entry : output_data.solution_phases)
        {
            const std::string& phase_name = phase_entry.first;
            const OCPhaseData& phase_data = phase_entry.second;

            if (phase_name == "condensed")
            {
                for (const auto& species_entry : phase_data.species)
                {
                    const std::string&   species_name = species_entry.first;
                    const OCSpeciesData& species_data = species_entry.second;

                    for (const auto& sublattice : species_data.sublattices)
                    {
                        for (const auto& constituent_entry : sublattice.composition)
                        {
                            output_file << time_hours << "\t" << location << "\t" << species_name << "\t"
                                        << sublattice.phase_instance << "\t"
                                        << sublattice.phase_moles * content_scaling_factor << "\t"
                                        << sublattice.phase_form_units * content_scaling_factor << "\t"
                                        << sublattice.index << "\t" << sublattice.sites << "\t"
                                        << constituent_entry.first << "\t" << constituent_entry.second << "\n";
                        }
                    }
                }
                continue;
            }

            for (const auto& sublattice : phase_data.sublattices)
            {
                for (const auto& constituent_entry : sublattice.composition)
                {
                    output_file << time_hours << "\t" << location << "\t" << phase_name << "\t"
                                << sublattice.phase_instance << "\t" << sublattice.phase_moles * content_scaling_factor
                                << "\t" << sublattice.phase_form_units * content_scaling_factor << "\t"
                                << sublattice.index << "\t" << sublattice.sites << "\t" << constituent_entry.first
                                << "\t" << constituent_entry.second << "\n";
                }
            }
        }

        return output_file.good();
    }

    std::vector<InputComponent> buildInputComponents(const std::set<std::string>&     selected_elements,
                                                     SciantixArray<SciantixVariable>& sciantix_variable,
                                                     SciantixArray<System>&           sciantix_system,
                                                     double&                          total_content,
                                                     const std::string&               location)
    {
        std::vector<InputComponent> components;
        total_content = 0.0;

        if (location != "at grain boundary")
        {
            std::cerr << "Error: the Thermochimica coupling supports only fission products at grain boundary, not "
                      << location << std::endl;
            return components;
        }

        if (selected_elements.count("O") > 0)
        {
            InputComponent component;
            component.name    = "O";
            component.content = std::max(0.0, sciantix_variable["O available content"].getFinalValue());

            if (component.content > 0.0)
            {
                total_content += component.content;
                components.push_back(component);
            }
        }

        // FP component
        for (auto& system : sciantix_system)
        {
            const std::string element_name = system.getFissionProductName();
            if (selected_elements.count(element_name) == 0)
                continue;

            InputComponent component;
            component.name = element_name;

            if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
            {
                double atoms_available = sciantix_variable[element_name + " produced"].getFinalValue() -
                                         sciantix_variable[element_name + " released"].getInitialValue();

                component.content = std::max(0.0, atoms_available / avogadro_number);
            }
            else if (system.getRestructuredMatrix() == 0 && (system.isMetallicFP() || system.isCeramicFP()))
            {
                double atoms_available = sciantix_variable[element_name + " produced"].getFinalValue();

                component.content = std::max(0.0, atoms_available / avogadro_number);
            }

            if (component.content > 0.0)
            {
                total_content += component.content;
                components.push_back(component);
            }
        }

        if (total_content <= 0.0 || components.empty())
            return components;

        for (auto& component : components)
            component.fraction = component.content / total_content;

        components.erase(std::remove_if(components.begin(),
                                        components.end(),
                                        [](const InputComponent& component)
                                        {
                                            return component.fraction < 1.0e-8;  // cut-off
                                        }),
                         components.end());

        total_content = 0.0;
        for (const auto& component : components)
            total_content += component.content;

        if (total_content <= 0.0 || components.empty())
            return components;

        for (auto& component : components)
            component.fraction = component.content / total_content;

        return components;
    }

    bool runThermochimicaCase(const std::string&                 database_path,
                              double                             temperature,
                              double                             pressure,
                              const std::vector<InputComponent>& components,
                              ThermochimicaSolveMode             solve_mode,
                              OCOutputData&                      output_data)
    {
        try
        {
            return TCAdapter::getThermochimicaInterface().solve(
                database_path, temperature, pressure, components, solve_mode, output_data);
        }
        catch (const std::exception& e)
        {
            std::cerr << "Exception in runThermochimicaCase: " << e.what() << std::endl;
            output_data.solution_phases.clear();
            output_data.components.clear();
            return false;
        }
    }

    bool validateThermochimicaOutput(const OCOutputData&                output_data,
                                     const std::vector<InputComponent>& input_components,
                                     const std::string&                 location)
    {
        constexpr double significant_input_fraction   = 1.0e-8;
        constexpr double inventory_relative_tolerance = 5.0e-2;
        constexpr double minimum_recovered_fraction   = 1.0e-3;
        constexpr double absolute_tolerance           = 1.0e-12;

        if (output_data.solution_phases.empty())
        {
            std::cerr << "Error: Thermochimica returned no stable phases for " << location << std::endl;
            return false;
        }

        // The Thermochimica system is closed (no imposed potentials): every input element is checked.
        std::map<std::string, double> input_inventory;
        for (const auto& component : input_components)
        {
            if (!std::isfinite(component.fraction) || component.fraction < 0.0)
            {
                std::cerr << "Error: invalid Thermochimica input fraction for " << component.name << " at " << location
                          << std::endl;
                return false;
            }

            if (component.fraction >= significant_input_fraction)
                input_inventory[component.name] += component.fraction;
        }

        std::map<std::string, double> phase_inventory;
        for (const auto& phase_entry : output_data.solution_phases)
        {
            const OCPhaseData& phase_data = phase_entry.second;
            if (!std::isfinite(phase_data.moles) || phase_data.moles < -absolute_tolerance)
            {
                std::cerr << "Error: invalid Thermochimica phase amount for " << phase_entry.first << " at " << location
                          << std::endl;
                return false;
            }

            for (const auto& element_entry : phase_data.elements)
            {
                if (!std::isfinite(element_entry.second) || element_entry.second < -absolute_tolerance)
                {
                    std::cerr << "Error: invalid Thermochimica element inventory for " << element_entry.first
                              << " in phase " << phase_entry.first << " at " << location << std::endl;
                    return false;
                }

                phase_inventory[element_entry.first] += std::max(0.0, element_entry.second);
            }
        }

        for (const auto& component_entry : output_data.components)
        {
            const OCComponentData& component_data = component_entry.second;
            if (!std::isfinite(component_data.moles) || !std::isfinite(component_data.mole_fraction) ||
                !std::isfinite(component_data.chemical_potential_over_rt) || !std::isfinite(component_data.activity) ||
                component_data.moles < -absolute_tolerance || component_data.mole_fraction < -absolute_tolerance ||
                component_data.activity < -absolute_tolerance)
            {
                std::cerr << "Error: invalid Thermochimica component data for " << component_entry.first << " at "
                          << location << std::endl;
                return false;
            }
        }

        double input_total  = 0.0;
        double output_total = 0.0;
        for (const auto& element_entry : input_inventory)
            input_total += element_entry.second;
        for (const auto& element_entry : phase_inventory)
            output_total += element_entry.second;

        if (input_total <= 0.0 || output_total <= 0.0)
        {
            std::cerr << "Error: Thermochimica inventory check has zero input or output for " << location << std::endl;
            return false;
        }

        bool balanced = true;
        for (const auto& input_entry : input_inventory)
        {
            const double expected_fraction = input_entry.second / input_total;
            const double output_moles      = phase_inventory[input_entry.first];
            const double output_fraction   = output_moles / output_total;
            const double difference        = std::abs(output_fraction - expected_fraction);
            const double tolerance = std::max(inventory_relative_tolerance * expected_fraction, absolute_tolerance);

            if (output_moles <=
                    std::max(absolute_tolerance, minimum_recovered_fraction * expected_fraction * output_total) ||
                difference > tolerance)
            {
                std::cerr << "Error: Thermochimica inventory mismatch for " << input_entry.first << " at " << location
                          << " input fraction=" << expected_fraction << " output fraction=" << output_fraction
                          << std::endl;
                balanced = false;
            }
        }

        if (!balanced)
            std::cerr << "Error: rejecting Thermochimica results for " << location
                      << " to avoid propagating inconsistent data" << std::endl;

        return balanced;
    }

    void updateThermochemistryVariablesFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                                  const std::string&                        location,
                                                  double                                    content_scaling_factor,
                                                  SciantixArray<ThermochemistryVariable>&   thermochemistry_variable)
    {
        auto setThermochemistryMass = [](ThermochemistryVariable&                            variable,
                                         double                                              mass,
                                         const std::map<std::string, double>&                composition,
                                         const std::map<int, std::map<std::string, double>>& sublattice_composition)
        {
            variable.setComposition(composition);
            variable.setSublatticeComposition(sublattice_composition);
            variable.setFinalValue(mass);
        };

        auto computePhaseComposition = [](const OCPhaseData& phase_data)
        {
            std::map<std::string, double> composition;
            if (phase_data.moles <= 0.0)
                return composition;

            for (const auto& element_entry : phase_data.elements)
                composition[element_entry.first] = std::max(0.0, element_entry.second) / phase_data.moles;

            return composition;
        };

        auto computeSublatticeComposition = [](const std::vector<OCSublatticeData>& sublattices)
        {
            std::map<int, std::map<std::string, double>> composition;
            for (const auto& sublattice : sublattices)
            {
                for (const auto& constituent_entry : sublattice.composition)
                    composition[sublattice.index][constituent_entry.first] += constituent_entry.second;
            }

            return composition;
        };

        auto setElementVariable =
            [&](const std::string& element_name, const std::string& phase_name, const OCPhaseData& phase_data)
        {
            const std::string variable_name = element_name + " (" + phase_name + ", " + location + ")";
            const std::string uppercase_variable_name =
                upperCopy(element_name) + " (" + phase_name + ", " + location + ")";
            const std::string* target = nullptr;
            if (thermochemistry_variable.isElementPresent(variable_name))
                target = &variable_name;
            else if (thermochemistry_variable.isElementPresent(uppercase_variable_name))
                target = &uppercase_variable_name;
            if (target == nullptr)
                return;

            const auto element_mass = phase_data.element_masses.find(element_name);
            setThermochemistryMass(thermochemistry_variable[*target],
                                   (element_mass != phase_data.element_masses.end() ? element_mass->second : 0.0) *
                                       content_scaling_factor,
                                   {{element_name, 1.0}},
                                   {});
        };

        for (const auto& phase_entry : solution_phases)
        {
            const std::string& phase_name   = phase_entry.first;
            const OCPhaseData& phase_data   = phase_entry.second;
            const bool         liquid_phase = isLiquidPhase(phase_name);

            if (liquid_phase)
            {
                const std::string liquid_variable_name = "LIQUID (" + phase_name + ", " + location + ")";
                if (thermochemistry_variable.isElementPresent(liquid_variable_name))
                {
                    const std::map<std::string, double> composition = computePhaseComposition(phase_data);
                    if (!composition.empty())
                        setThermochemistryMass(thermochemistry_variable[liquid_variable_name],
                                               phase_data.mass * content_scaling_factor,
                                               composition,
                                               computeSublatticeComposition(phase_data.sublattices));
                }
            }

            for (const auto& species_entry : phase_data.species)
            {
                const std::string variable_name = species_entry.first + " (" + phase_name + ", " + location + ")";
                if (!thermochemistry_variable.isElementPresent(variable_name))
                    continue;

                std::map<std::string, double> composition;
                if (species_entry.second.moles > 0.0)
                {
                    for (const auto& element_entry : species_entry.second.elements)
                        composition[element_entry.first] = element_entry.second / species_entry.second.moles;
                }
                if (!composition.empty())
                {
                    setThermochemistryMass(thermochemistry_variable[variable_name],
                                           species_entry.second.mass * content_scaling_factor,
                                           composition,
                                           {});
                }
            }

            if (liquid_phase)
                continue;

            for (const auto& element_entry : phase_data.elements)
                setElementVariable(element_entry.first, phase_name, phase_data);
        }
    }

    void updateGrainBoundaryFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                       const std::set<std::string>&              selected_elements,
                                       double                                    content_scaling_factor,
                                       SciantixArray<SciantixVariable>&          sciantix_variable,
                                       SciantixArray<System>&                    sciantix_system)
    {
        const auto gas_phase = solution_phases.find("gas");

        for (auto& system : sciantix_system)
        {
            const std::string element = system.getFissionProductName();
            if (selected_elements.count(element) == 0)
                continue;

            double gas_moles = 0.0;
            if (gas_phase != solution_phases.end() && gas_phase->second.elements.count(element) > 0)
                gas_moles = gas_phase->second.elements.at(element) * content_scaling_factor;

            if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
            {
                const double fuel_available = std::max(0.0,
                                                       sciantix_variable[element + " produced"].getFinalValue() -
                                                           sciantix_variable[element + " released"].getInitialValue());

                const double gas_atoms = std::min(fuel_available, std::max(0.0, gas_moles * avogadro_number));

                sciantix_variable[element + " at grain boundary"].setFinalValue(gas_atoms);
                sciantix_variable[element + " reacted"].setFinalValue(fuel_available - gas_atoms);
            }
            else if (system.getRestructuredMatrix() == 0 && (system.isMetallicFP() || system.isCeramicFP()))
            {
                const double available = sciantix_variable[element + " produced"].getFinalValue();

                const double updated_atoms = std::min(available, gas_moles * avogadro_number);
                sciantix_variable[element + " in solution"].setFinalValue(updated_atoms);
                sciantix_variable[element + " reacted"].setFinalValue(available - updated_atoms);
            }
        }
    }
}  // namespace TCUtilsCoupling
