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
//  Version: under development                                                      //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E.Cappellari                                    //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "OCASIAdapter.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <stdexcept>

// Fortran C interface to OpenCalphad

extern "C" int c_nel;         // Number of elements
extern "C" int c_ntup;        // Number of components
extern "C" char *c_cnam[41];  // Component names array

extern "C"
{

    // Initiate OCTQ, to be called before any of the other subroutines.
    // It returns a pointer, “ceq” to the equilibrium data structure
    // which is needed in many of the other subroutines.
    // (n = unused integer, pointer in output = ceq)
    void c_tqini(int, void *);

    // Read database file, in particular it reads all phases for a
    // selected set of elements.
    // (database filename, number of selected elements,
    //  elements to be read by the database, current equilibrium = ceq)
    void c_tqrpfil(char *, int, char **, void *);

    // Get number of phases and composition sets
    // After reading the database each phase has one composition set.
    // But as several phases may have miscibility gaps
    //  new phase tuples may be created after a calculation.
    // This routine should thus be called after each calculation
    //  when the grid minimizer is used.
    // (number of phases in output, current equilibrium = ceq)
    void c_tqgnp(int *, void *);

    // Get phase name by index
    // For phases with several composition sets it will have a suffix #digit
    // (index in phase tuple array, phase name in output, current equilibrium = ceq)
    void c_tqgpn(int, char *, void *);

    // Get index of phase using name
    // If the phase name has a suffix #digit, the index of that composition set
    // will be returned
    // (index in phase tuple array in output, phase name, current equilibrium = ceq)
    void c_tqgpi(int *, char *, void *);

    // Get the base phase index and composition-set index for a phase tuple name.
    // A name with a suffix such as "#2" resolves to the corresponding
    // composition set; otherwise the first composition set is returned.
    // (phase index in output, composition set in output, phase name, ceq)
    void c_tqgpi2(int *, int *, char *, void *);

    // Set condition
    // ( stavar is state variable as text,
    //   n1 =0 or extended phase index: 10*phase_number+comp.setand
    //   n2 are auxilliary indices,
    //   value is the value of the condition,
    //   cnum is returned as an index of the condition,
    //   ceq )
    // A condition is removed by setting its value to RNONE (-1.0e-36).
    void c_tqsetc(char *, int, int, double, int *, void *);

    // Calculate equilibrium
    // (target unused, n1 = -1 will not call the grid minimizer,
    //  n2, value as output, ceq)
    void c_tqce(char *, int, int, double *, void *);

    // Calculate equilibrium using OpenCalphad's higher-level modes.
    // mode=0 no grid minimizer, mode=1 with global grid minimizer,
    // mode=2 carefully/default, corresponding to the checked solve path.
    void c_tqcalc(void *, int);

    // Exact wrapper for the interactive OpenCalphad "calculate with_check_after"
    void c_tqce_with_check_after(void *);

    // Get state variable value
    // (state variable in caputal letters,
    //  n1 can be a phase tuple index but if <0 means all,
    //  n2 can be a component index but if <0 means all,
    //  n3 dimension of the array values,
    //  array with calculated values,
    //  ceq)
    void c_tqgetv(char *, int, int, int *, double *, void *);

    // Get index of constituent using name (alphabetically)
    // (phase number, extended constituent index in output
    //  10*species_number+sublattice, constituent name, current equilibrium = ceq)
    void c_tqgpci(int, int *, char *, void *);

    // Get constituent name by the sequential constituent index returned by
    // c_tqgphc1. The constituents are numbered over all sublattices from
    // first to last.
    // (base phase index, constituent index, constituent name in output, ceq)
    void c_tqgpcn2(int, int, char *, void *);

    // Get phase composition detailed
    // (phase tuple index,
    //  nsub is the number of sublattices (1 if no sublattices)
    //  cinsub is an array with the number of constituents in each sublattice
    //  spix is an array with the species index of the constituents in all sublattices
    //  yfrac is the constituent fractions in same order as in spix
    //  sites is an array of the site ratios for all sublattices.
    //  extra is an array with some extra values:
    //    extra(1) is the number of moles of components per formula unit
    //    extra(2) is the net charge of the phase
    //  ceq)
    void c_tqgphc1(int, int *, int *, int *, double *, double *, double *, void *);

    // Change phase status
    // Fixed = 2. Entered = 0.
    // Suspended = -3. Dormant = -2.
    // (phase name, status, value if entered or fixed as estimate, ceq )
    void c_Change_Status_Phase(char *, int, double, void *);

    // Set reference state
    // (element, phase, temperature and pressure of reference, ceq )
    void c_Set_Reference_State(int, char *, double *, void *);

    // Reset conditions
    // (condition line, ceq)
    void c_reset_conditions(char *, void *);
}

namespace OCASIAdapter
{

    namespace
    {
        std::string trimOcName(const std::string &input)
        {
            const auto end = input.find_last_not_of(" .\0", std::string::npos);
            if (end == std::string::npos)
                return "";
            const auto begin = input.find_first_not_of(" .\0");
            return input.substr(begin, end - begin + 1);
        }

        std::string upperCopy(std::string text)
        {
            std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c)
                           { return std::toupper(c); });
            return text;
        }

        std::string lowerCopy(std::string text)
        {
            std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c)
                           { return std::tolower(c); });
            return text;
        }

        std::string ElementName(std::string text)
        {
            text = trimOcName(text);
            if (text.empty())
                return text;

            text = lowerCopy(text);
            text[0] = static_cast<char>(std::toupper(static_cast<unsigned char>(text[0])));
            return text;
        }

        std::string normalizeSpeciesName(std::string name)
        {
            while (!name.empty() && name.back() == '.')
                name.pop_back();

            const size_t hash_pos = name.find('#');
            if (hash_pos != std::string::npos)
                name = name.substr(0, hash_pos);

            const size_t auto_pos = name.find("_AUTO");
            if (auto_pos != std::string::npos)
                name = name.substr(0, auto_pos);

            const size_t chkd_pos = name.find("_CHKD");
            if (chkd_pos != std::string::npos)
                name = name.substr(0, chkd_pos);

            return trimOcName(name);
        }

        std::string normalizePhaseInstanceName(std::string name)
        {
            while (!name.empty() && name.back() == '.')
                name.pop_back();

            const size_t auto_pos = name.find("_AUTO");
            if (auto_pos != std::string::npos)
                name.erase(auto_pos, 5);

            const size_t chkd_pos = name.find("_CHKD");
            if (chkd_pos != std::string::npos)
                name.erase(chkd_pos, 5);

            return trimOcName(name);
        }

        std::string normalizePhaseName(const std::string &phase_name)
        {
            const std::string base = upperCopy(normalizeSpeciesName(phase_name));

            if (base == "GAS" || base == "LIQUID" || base == "LIQUID_IONIC" ||
                base == "IONIC_LIQUID" || base == "PURE_CONDENSED" || base == "SOLID")
                return lowerCopy(base);

            return "condensed";
        }

        // Merge per-element inventories while preserving elements that are
        // present in only one source phase or species.
        void addElementInventory(std::map<std::string, double> &target,
                                 const std::map<std::string, double> &source)
        {
            for (const auto &element_entry : source)
                target[element_entry.first] += element_entry.second;
        }

        std::string resolveTdbPath(const std::string &path)
        {
            if (std::ifstream(path).good())
                return path;

            const std::string upper_tdb = path + ".TDB";
            if (std::ifstream(upper_tdb).good())
                return upper_tdb;

            const std::string lower_tdb = path + ".tdb";
            if (std::ifstream(lower_tdb).good())
                return lower_tdb;

            return path;
        }
    } // namespace

    // OpenCalphadInterface Implementation

    static std::unique_ptr<OpenCalphadInterface> g_ocasi_interface;

    OpenCalphadInterface &getOpenCalphadInterface(OpenCalphadContext context)
    {
        (void)context;

        if (!g_ocasi_interface)
            g_ocasi_interface = std::make_unique<OpenCalphadInterface>();
        return *g_ocasi_interface;
    }

    OpenCalphadInterface::OpenCalphadInterface()
    {
        c_tqini(0, &ceq_);
        if (ceq_ == nullptr)
            throw std::runtime_error("Failed to initialize OpenCalphad via OCASI");
    }

    OpenCalphadInterface::~OpenCalphadInterface()
    {
        // The Fortran side owns the equilibrium data structure. At process
        // shutdown the OpenCalphad globals may already be partly finalized, so
        // avoid calling back into OC from the C++ singleton destructor.
    }

    // Return the one-based OpenCalphad component index for a loaded element.
    // OpenCalphad returns zero/negative indices for missing components, so this
    // wrapper returns 0 when the element is not part of the selected system.
    int OpenCalphadInterface::getComponentIndex(const std::string &component_name) const
    {
        const std::string target = upperCopy(component_name);
        const auto element_it = std::find_if(element_names_.begin(),
                                             element_names_.end(),
                                             [&target](const std::string &name)
                                             {
                                                 return upperCopy(name) == target;
                                             });

        if (element_it == element_names_.end())
            return 0;

        return static_cast<int>(std::distance(element_names_.begin(), element_it)) + 1;
    }

    bool OpenCalphadInterface::loadDatabase(const std::string &tdb_file_path,
                                            const std::vector<std::string> &selected_elements)
    {
        // Reinitialize the TQ interface so each database load starts from a
        // clean OpenCalphad equilibrium object.
        c_tqini(0, &ceq_);

        if (!ceq_)
            return false;

        database_loaded_ = false;
        const std::string resolved_tdb_file_path = resolveTdbPath(tdb_file_path);

        char tdb_path[256];
        std::strncpy(tdb_path, resolved_tdb_file_path.c_str(), sizeof(tdb_path) - 1);
        tdb_path[sizeof(tdb_path) - 1] = '\0';

        // Load selected elements only
        std::vector<char *> el_array;
        std::vector<std::string> temp_strings;

        for (const auto &el : selected_elements)
        {
            temp_strings.push_back(upperCopy(el));
        }

        for (auto &el : temp_strings)
        {
            el_array.push_back(const_cast<char *>(el.c_str()));
        }

        // Read and select elements from TDB 
        c_tqrpfil(tdb_path, static_cast<int>(el_array.size()), el_array.data(), &ceq_);

        // Cache OpenCalphad element names in the same one-based order used by
        // c_tqsetc/c_tqgetv component indices.
        nel_ = c_nel;
        element_names_.clear();
        element_names_.resize(nel_);

        for (int i = 0; i < nel_; ++i)
        {
            if (c_cnam[i])
            {
                element_names_[i] = ElementName(c_cnam[i]);
            }
        }

        database_loaded_ = true;
        loaded_database_path_ = resolved_tdb_file_path;
        loaded_selected_elements_ = selected_elements;
        return true;
    }

    bool OpenCalphadInterface::ensureDatabaseLoaded(const std::string &tdb_file_path,
                                                    const std::vector<std::string> &selected_elements)
    {
        const std::string resolved_tdb_file_path = resolveTdbPath(tdb_file_path);
        if (!database_loaded_ ||
            loaded_database_path_ != resolved_tdb_file_path ||
            loaded_selected_elements_ != selected_elements)
        {
            return loadDatabase(resolved_tdb_file_path, selected_elements);
        }

        //reset(false);
        return true;
    }

    bool OpenCalphadInterface::setConditions(double temperature,
                                             double pressure,
                                             const std::map<std::string, double> &components)
    {
        int condition_number = 0;

        // Set temperature (in Kelvin)
        char temperature_condition[] = "T";
        c_tqsetc(temperature_condition, 0, 0, temperature, &condition_number, &ceq_);

        // Set pressure (in Pa)
        char pressure_condition[] = "P";
        c_tqsetc(pressure_condition, 0, 0, pressure, &condition_number, &ceq_);

        // Set component contents (in moles)
        for (const auto &comp : components)
        {
            const int component_index = getComponentIndex(comp.first);
            if (component_index <= 0)
            {
                std::cerr << "Warning: component " << comp.first << " is not present in loaded OpenCalphad system" << std::endl;
                continue;
            }
            char component_condition[] = "N";
            c_tqsetc(component_condition, component_index, 0, comp.second, &condition_number, &ceq_);
        }

        return true;
    }

    bool OpenCalphadInterface::setPressure(double pressure)
    {
        if (!ceq_ || !database_loaded_)
            return false;

        int condition_number = 0;
        char pressure_condition[] = "P";
        c_tqsetc(pressure_condition, 0, 0, pressure, &condition_number, &ceq_);
        return true;
    }

    bool OpenCalphadInterface::setReferenceState(const std::string &component_name,
                                                 const std::string &phase_name,
                                                 double temperature,
                                                 double pressure)
    {
        const int component_index = getComponentIndex(component_name);
        if (component_index <= 0)
            return false;

        char phase[24] = {0};
        std::strncpy(phase, upperCopy(phase_name).c_str(), sizeof(phase) - 1);

        double reference_temperature_pressure[2] = {temperature, pressure};
        c_Set_Reference_State(component_index, phase, reference_temperature_pressure, &ceq_);
        return true;
    }

    bool OpenCalphadInterface::removeComponentCondition(const std::string &component_name)
    {
        if (!ceq_ || !database_loaded_)
            return false;

        const int component_index = getComponentIndex(component_name);
        if (component_index <= 0)
            return false;

        int condition_number = 0;
        char condition_name[] = "N";
        constexpr double rnone = -1.0e-36;
        c_tqsetc(condition_name, component_index, -1, rnone, &condition_number, &ceq_);
        
        return true;
    }

    bool OpenCalphadInterface::setComponentPotential(const std::string &component_name, double chemical_potential)
    {
        if (!ceq_ || !database_loaded_)
            return false;

        const int component_index = getComponentIndex(component_name);
        if (component_index <= 0)
            return false;

        int condition_number = 0;
        char condition_name[] = "MU";
        c_tqsetc(condition_name, component_index, 0, chemical_potential, &condition_number, &ceq_);
        
        return true;
    }

    bool OpenCalphadInterface::setPhaseStatus(const std::string &phase_name,
                                              int status,
                                              double value)
    {
        if (!ceq_ || !database_loaded_)
            return false;

        char ph_name[64];
        std::strncpy(ph_name, phase_name.c_str(), sizeof(ph_name) - 1);
        ph_name[sizeof(ph_name) - 1] = '\0';

        c_Change_Status_Phase(ph_name, status, value, &ceq_);
        return true;
    }

    bool OpenCalphadInterface::calculateEquilibrium(int grid_minimizer)
    {
        if (!ceq_ || !database_loaded_)
            return false;
            
        char target[] = "";
        double g_val = 0.0;
        c_tqce(target, grid_minimizer, 0, &g_val, &ceq_);

        return true;
    }

    bool OpenCalphadInterface::calculateEquilibriumChecked()
    {
        if (!ceq_ || !database_loaded_)
            return false;
        c_tqce_with_check_after(&ceq_);
        return true;
    }

    bool OpenCalphadInterface::extractResults(OCOutputData &output_data)
    {
        if (!ceq_ || !database_loaded_)
            return false;

        output_data.solution_phases.clear();
        output_data.components.clear();

        // Get number of phases
        int nphases = 0;
        c_tqgnp(&nphases, &ceq_);

        // Process each phase
        for (int ph = 0; ph < nphases; ++ph)
        {
            char phase_name[256] = {0};
            const int phase_index = ph + 1;
            // Get phase name by index
            c_tqgpn(phase_index, phase_name, &ceq_);
            phase_name[sizeof(phase_name) - 1] = '\0';
            const std::string oc_phase_name = trimOcName(phase_name);

            OCPhaseData phase_data;

            // Get phase moles
            int n_values = 1;
            double phase_moles = 0.0;
            char phase_moles_variable[] = "NP";
            c_tqgetv(phase_moles_variable, phase_index, 0, &n_values, &phase_moles, &ceq_);
            phase_data.moles = (n_values == 1) ? phase_moles : 0.0;

            constexpr int max_sublattices = 32;
            constexpr int max_constituents = 512;
            int n_sublattices = 0;
            int constituents_per_sublattice[max_sublattices] = {0};
            int constituent_indices[max_constituents] = {0};
            double constituent_fractions[max_constituents] = {0.0};
            double sublattice_sites[max_sublattices] = {0.0};
            double phase_extra[8] = {0.0};

            // Get phase constitution: constituent fractions are returned in
            // sequential order over all sublattices, with site ratios reported
            // separately per sublattice.
            c_tqgphc1(phase_index,
                      &n_sublattices,
                      constituents_per_sublattice,
                      constituent_indices,
                      constituent_fractions,
                      sublattice_sites,
                      phase_extra,
                      &ceq_);

            std::vector<OCSublatticeData> phase_sublattices;
            if (n_sublattices > 0 && n_sublattices <= max_sublattices)
            {
                const double components_per_formula_unit = phase_extra[0];
                const double phase_form_units =
                    components_per_formula_unit > 0.0 ? phase_data.moles / components_per_formula_unit : phase_data.moles;

                int base_phase_index = phase_index;
                int composition_set_index = 0;
                char phase_lookup_name[256] = {0};
                std::strncpy(phase_lookup_name, oc_phase_name.c_str(), sizeof(phase_lookup_name) - 1);
                phase_lookup_name[sizeof(phase_lookup_name) - 1] = '\0';
                
                // Get phase and composition set indices by name
                c_tqgpi2(&base_phase_index, &composition_set_index, phase_lookup_name, &ceq_);

                int extended_constituent_index = 0;
                for (int sublattice_index = 0; sublattice_index < n_sublattices; ++sublattice_index)
                {
                    OCSublatticeData sublattice;
                    sublattice.index = sublattice_index + 1;
                    sublattice.constituents_count = constituents_per_sublattice[sublattice_index];
                    sublattice.sites = sublattice_sites[sublattice_index];
                    sublattice.phase_moles = phase_data.moles;
                    sublattice.phase_form_units = phase_form_units;
                    sublattice.phase_instance = normalizePhaseInstanceName(oc_phase_name);

                    for (int constituent = 0;
                         constituent < constituents_per_sublattice[sublattice_index] &&
                         extended_constituent_index < max_constituents;
                         ++constituent)
                    {
                        char constituent_name[256] = {0};
                        // Get constituent name by extended index
                        c_tqgpcn2(base_phase_index, extended_constituent_index + 1, constituent_name, &ceq_);
                        constituent_name[sizeof(constituent_name) - 1] = '\0';
                        const std::string name = trimOcName(constituent_name);
                        if (!name.empty())
                            sublattice.composition[name] += constituent_fractions[extended_constituent_index];

                        ++extended_constituent_index;
                    }

                    phase_sublattices.push_back(sublattice);
                }

                phase_data.form_units = phase_form_units;
            }

            // Get element composition in phase
            for (size_t element_index = 0; element_index < element_names_.size(); ++element_index)
            {
                const auto &el = element_names_[element_index];
                const int component_index = static_cast<int>(element_index) + 1;
                int n_values = 1;
                double el_moles = 0.0;
                // get state variable value
                char element_moles_variable[] = "N";
                c_tqgetv(element_moles_variable, phase_index, component_index, &n_values, &el_moles, &ceq_);

                if (n_values == 1 && el_moles > 0.0)
                {
                    phase_data.elements[el] = el_moles;
                }
            }

            if (phase_data.moles <= 0.0 && phase_data.elements.empty())
                continue;

            const std::string phase_bucket = normalizePhaseName(oc_phase_name);
            OCPhaseData &output_phase = output_data.solution_phases[phase_bucket];
            output_phase.moles += phase_data.moles;
            output_phase.form_units += phase_data.form_units;
            output_phase.volume += phase_data.volume;
            addElementInventory(output_phase.elements, phase_data.elements);

            if (phase_bucket == "condensed")
            {
                const std::string species_name = normalizeSpeciesName(oc_phase_name);
                OCSpeciesData &species = output_phase.species[species_name];
                species.moles += phase_data.moles;
                species.atom_equivalent_moles += phase_data.moles;
                species.volume += phase_data.volume;
                species.sublattices.insert(species.sublattices.end(),
                                           phase_sublattices.begin(),
                                           phase_sublattices.end());
                addElementInventory(species.elements, phase_data.elements);
            }
            else
            {
                output_phase.sublattices.insert(output_phase.sublattices.end(),
                                                phase_sublattices.begin(),
                                                phase_sublattices.end());
            }
        }

        // Extract component data (for chemical potentials, activities if available)
        for (int comp = 0; comp < c_ntup; ++comp)
        {
            if (c_cnam[comp])
            {
                std::string comp_name(c_cnam[comp]);
                comp_name = ElementName(comp_name);

                if (!comp_name.empty())
                {
                    OCComponentData comp_data;
                    const int component_index = getComponentIndex(comp_name);

                    int n_values = 1;
                    double component_moles = 0.0;
                    char component_moles_variable[] = "N";
                    c_tqgetv(component_moles_variable, component_index, 0, &n_values, &component_moles, &ceq_);
                    if (n_values == 1)
                        comp_data.moles = component_moles;

                    n_values = 1;
                    double mole_fraction = 0.0;
                    char mole_fraction_variable[] = "X";
                    c_tqgetv(mole_fraction_variable, component_index, 0, &n_values, &mole_fraction, &ceq_);
                    if (n_values == 1)
                        comp_data.mole_fraction = mole_fraction;

                    n_values = 1;
                    double temperature = 0.0;
                    char temperature_variable[] = "T";
                    c_tqgetv(temperature_variable, 0, 0, &n_values, &temperature, &ceq_);

                    n_values = 1;
                    double chemical_potential = 0.0;
                    char chemical_potential_variable[] = "MU";
                    c_tqgetv(chemical_potential_variable, component_index, 0, &n_values, &chemical_potential, &ceq_);
                    if (n_values == 1 && temperature > 0.0)
                    {
                        constexpr double gas_constant = 8.31446261815324;
                        comp_data.chemical_potential_over_rt = chemical_potential / (gas_constant * temperature);
                        comp_data.activity = std::exp(comp_data.chemical_potential_over_rt);
                    }
                    output_data.components[comp_name] = comp_data;
                }
            }
        }

        return true;
    }

    void OpenCalphadInterface::reset(bool clear_database)
    {
        if (!ceq_)
            return;

        char empty_str[] = "";
        c_reset_conditions(empty_str, &ceq_);

        database_loaded_ = !clear_database;
        if (clear_database)
        {
            loaded_database_path_.clear();
            loaded_selected_elements_.clear();
            element_names_.clear();
            nel_ = 0;
        }
    }

    std::string OpenCalphadInterface::getPhaseNameAtIndex(int phase_index) const
    {
        if (!ceq_)
            return "";

        char phase_name_buf[64] = {0};
        void *ceq = const_cast<void *>(ceq_);
        c_tqgpn(phase_index, phase_name_buf, &ceq);
        return std::string(phase_name_buf);
    }

    int OpenCalphadInterface::getPhaseIndex(const std::string &phase_name) const
    {
        if (!ceq_)
            return -1;

        int ph_idx = -1;
        char ph_name[64];
        std::strncpy(ph_name, phase_name.c_str(), sizeof(ph_name) - 1);
        ph_name[sizeof(ph_name) - 1] = '\0';

        int idx = 0;
        void *ceq = const_cast<void *>(ceq_);
        c_tqgpi(&idx, ph_name, &ceq);
        return idx >= 0 ? idx : -1;
    }

} // namespace OCASIAdapter
