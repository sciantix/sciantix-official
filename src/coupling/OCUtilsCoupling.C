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

#include "OCUtilsCoupling.h"
#include "Constants.h"
#include "MainVariables.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstring>
#include <exception>
#include <fstream>
#include <iomanip>
#include <memory>
#include <iostream>
#include <set>
#include <stdexcept>
#include <vector>

namespace
{
std::string toUpperCopy(std::string text)
{
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) { return std::toupper(c); });
    return text;
}

bool isLiquidPhase(const std::string& phase_name)
{
    return phase_name == "liquid" || phase_name == "ionic_liquid" || phase_name == "liquid_ionic";
}

std::string equilibriumRecordName(const std::string& location,
                                  OCUtilsCoupling::OpenCalphadSolveMode solver)
{
    const std::string location_prefix = (location == "matrix") ? "M" : "GB";

    using OCSolver = OCUtilsCoupling::OpenCalphadSolveMode;
    switch (solver)
    {
        case OCSolver::SaveReadWarmStart:
            return location_prefix + "_WARM";
        case OCSolver::GlobalEquilibrium:
            return location_prefix + "_GLOBAL";
        case OCSolver::OnlyC1MO2:
            return location_prefix + "_C1MO2";
    }

    return location_prefix + "_UNKNOWN";
}

}  // namespace

namespace OCASIAdapter
{
    enum class OpenCalphadContext
    {
        Matrix,
        FissionProducts
    };

    class OpenCalphadInterface
    {
    public:
        explicit OpenCalphadInterface(bool use_prefixed_symbols);
        ~OpenCalphadInterface();

        bool loadDatabase(const std::string &tdb_file_path,
                          const std::vector<std::string> &selected_elements);
        bool ensureDatabaseLoaded(const std::string &tdb_file_path,
                                  const std::vector<std::string> &selected_elements);
        bool prepareCalculationRecord(const std::string &record_name,
                                      bool reuse_existing_record);
        bool setConditions(double temperature,
                           double pressure,
                           const std::map<std::string, double> &components);
        bool setReferenceState(const std::string &component_name,
                               const std::string &phase_name,
                               double temperature,
                               double pressure);
        bool setComponentPotential(const std::string &component_name,
                                   double chemical_potential);
        bool setPhaseStatus(const std::string &phase_name,
                            int status,
                            double value);
        bool calculateEquilibrium(int grid_minimizer);
        bool calculateEquilibriumChecked();
        bool listResults(int output_mode);
        bool extractResults(OCOutputData &output_data);
        void reset(bool clear_database);

    private:
        int getComponentIndex(const std::string &component_name) const;
        int currentElementCount() const;
        char* currentComponentName(int index) const;
        bool isPhaseTupleStable(int phase_tuple_index);

        bool use_prefixed_symbols_ = false;
        void *base_ceq_ = nullptr;
        void *ceq_ = nullptr;
        bool database_loaded_ = false;
        std::string loaded_database_path_;
        std::vector<std::string> loaded_selected_elements_;
        std::set<std::string> known_equilibrium_records_;
        int nel_ = 0;
        std::vector<std::string> element_names_;
    };

    OpenCalphadInterface &getOpenCalphadInterface(OpenCalphadContext context);

} // namespace OCASIAdapter

// Fortran C interface to OpenCalphad

extern "C" int c_nel;         // Number of elements
extern "C" int c_ntup;        // Number of components
extern "C" char *c_cnam[41];  // Component names array

extern "C" int gb_c_nel;
extern "C" int gb_c_ntup;
extern "C" char *gb_c_cnam[41];

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

    // Exact wrapper for the interactive OpenCalphad "calculate with_check_after"
    void c_tqce_with_check_after(void *);

    // List results. This prints the same information as the interactive
    // OpenCalphad `l r` command, including the current conditions.
    void c_tqlr(int, void *);
    void c_tqcheckphstab(bool *, int, void *);

    // Get state variable value
    // (state variable in caputal letters,
    //  n1 can be a phase tuple index but if <0 means all,
    //  n2 can be a component index but if <0 means all,
    //  n3 dimension of the array values,
    //  array with calculated values,
    //  ceq)
    void c_tqgetv(char *, int, int, int *, double *, void *);

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

    // Copy/select/delete equilibrium records. A copied equilibrium shares the
    // static database data but owns independent conditions and results.
    void c_tqcceq(char *, int *, void **, void **);
    void c_tqselceq(char *, void **);

    void gb_c_tqini(int, void *);
    void gb_c_tqrpfil(char *, int, char **, void *);
    void gb_c_tqgnp(int *, void *);
    void gb_c_tqgpn(int, char *, void *);
    void gb_c_tqgpi(int *, char *, void *);
    void gb_c_tqgpi2(int *, int *, char *, void *);
    void gb_c_tqsetc(char *, int, int, double, int *, void *);
    void gb_c_tqce(char *, int, int, double *, void *);
    void gb_c_tqce_with_check_after(void *);
    void gb_c_tqlr(int, void *);
    void gb_c_tqcheckphstab(bool *, int, void *);
    void gb_c_tqgetv(char *, int, int, int *, double *, void *);
    void gb_c_tqgpcn2(int, int, char *, void *);
    void gb_c_tqgphc1(int, int *, int *, int *, double *, double *, double *, void *);
    void gb_c_Change_Status_Phase(char *, int, double, void *);
    void gb_c_Set_Reference_State(int, char *, double *, void *);
    void gb_c_reset_conditions(char *, void *);
    void gb_c_tqcceq(char *, int *, void **, void **);
    void gb_c_tqselceq(char *, void **);
}

#define OCASI_CALL(symbol, ...)                         \
    do                                                  \
    {                                                   \
        if (use_prefixed_symbols_)                      \
            gb_##symbol(__VA_ARGS__);                   \
        else                                            \
            symbol(__VA_ARGS__);                        \
    } while (false)

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

        double atomicMass(const std::string& element_name)
        {
            static const std::map<std::string, double> atomic_masses = {
                {"Cs", 132.90545196},
                {"I", 126.90447},
                {"Mo", 95.95},
                {"O", 15.999},
                {"Te", 127.60},
                {"U", 238.02891},
                {"Pu", 239.052},
                {"Va", 0.0},
                {"Pd", 106.42},
                {"Rh", 102.91},
                {"Ru", 101.07},
                {"Tc", 98.906}
            };

            const auto atomic_mass = atomic_masses.find(ElementName(element_name));
            if (atomic_mass == atomic_masses.end())
                throw std::runtime_error("Atomic mass not available for element " + element_name);

            return atomic_mass->second;
        }

        std::map<std::string, double> speciesStoichiometry(
            const std::string& species_name,
            const std::vector<std::string>& valid_elements)
        {
            std::map<std::string, std::string> valid_set;
            for (const auto& element : valid_elements)
                valid_set[upperCopy(element)] = element;

            std::map<std::string, double> stoichiometry;
            size_t i = 0;
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
                    const auto it = valid_set.find(candidate);
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

                double count = 1.0;
                const size_t count_begin = i;
                while (i < species_name.size() && std::isdigit(static_cast<unsigned char>(species_name[i])))
                    ++i;
                if (i > count_begin)
                    count = std::stod(species_name.substr(count_begin, i - count_begin));

                stoichiometry[element] += count;
            }

            return stoichiometry;
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

    OpenCalphadInterface &getOpenCalphadInterface(OpenCalphadContext context)
    {
        static std::unique_ptr<OpenCalphadInterface> matrix_interface;
        static std::unique_ptr<OpenCalphadInterface> fission_products_interface;

        if (context == OpenCalphadContext::Matrix)
        {
            if (!matrix_interface)
                matrix_interface = std::make_unique<OpenCalphadInterface>(false);
            return *matrix_interface;
        }

        if (!fission_products_interface)
            fission_products_interface = std::make_unique<OpenCalphadInterface>(true);
        return *fission_products_interface;
    }

    OpenCalphadInterface::OpenCalphadInterface(bool use_prefixed_symbols)
        : use_prefixed_symbols_(use_prefixed_symbols)
    {
        OCASI_CALL(c_tqini, 0, &base_ceq_);
        ceq_ = base_ceq_;
        if (base_ceq_ == nullptr)
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
        OCASI_CALL(c_tqini, 0, &base_ceq_);
        ceq_ = base_ceq_;

        if (!base_ceq_)
            return false;

        database_loaded_ = false;
        known_equilibrium_records_.clear();
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
        OCASI_CALL(c_tqrpfil, tdb_path, static_cast<int>(el_array.size()), el_array.data(), &base_ceq_);

        // Cache OpenCalphad element names in the same one-based order used by
        // c_tqsetc/c_tqgetv component indices.
        nel_ = currentElementCount();
        element_names_.clear();
        element_names_.resize(nel_);

        for (int i = 0; i < nel_; ++i)
        {
            if (currentComponentName(i))
            {
                element_names_[i] = ElementName(currentComponentName(i));
            }
        }

        database_loaded_ = true;
        loaded_database_path_ = resolved_tdb_file_path;
        loaded_selected_elements_ = selected_elements;
        ceq_ = base_ceq_;

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

        return true;
    }

    bool OpenCalphadInterface::prepareCalculationRecord(const std::string &record_name,
                                                        bool reuse_existing_record)
    {
        if (!base_ceq_ || !database_loaded_)
            return false;

        std::string bounded_name = record_name.substr(0, 23);
        if (bounded_name.empty())
            bounded_name = "SCIANTIX_EQ";

        std::vector<char> ceq_name(25, ' ');
        std::copy(bounded_name.begin(), bounded_name.end(), ceq_name.begin());
        ceq_name.back() = '\0';

        const bool known_record = known_equilibrium_records_.count(bounded_name) > 0;

        if (known_record)
        {
            OCASI_CALL(c_tqselceq, ceq_name.data(), &ceq_);
            return ceq_ != nullptr;
        }

        void *new_ceq = nullptr;
        int equilibrium_index = 0;
        OCASI_CALL(c_tqcceq, ceq_name.data(), &equilibrium_index, &new_ceq, &base_ceq_);
        if (!new_ceq)
            return false;

        ceq_ = new_ceq;
        known_equilibrium_records_.insert(bounded_name);
        return true;
    }

    bool OpenCalphadInterface::setConditions(double temperature,
                                             double pressure,
                                             const std::map<std::string, double> &components)
    {
        int condition_number = 0;

        // Set temperature (in Kelvin)
        char temperature_condition[] = "T";
        OCASI_CALL(c_tqsetc, temperature_condition, 0, 0, temperature, &condition_number, &ceq_);

        // Set pressure (in Pa)
        char pressure_condition[] = "P";
        OCASI_CALL(c_tqsetc, pressure_condition, 0, 0, pressure, &condition_number, &ceq_);

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
            OCASI_CALL(c_tqsetc, component_condition, component_index, 0, comp.second, &condition_number, &ceq_);
        }

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
        OCASI_CALL(c_Set_Reference_State, component_index, phase, reference_temperature_pressure, &ceq_);
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
        OCASI_CALL(c_tqsetc, condition_name, component_index, 0, chemical_potential, &condition_number, &ceq_);

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

        OCASI_CALL(c_Change_Status_Phase, ph_name, status, value, &ceq_);
        return true;
    }

    bool OpenCalphadInterface::calculateEquilibrium(int grid_minimizer)
    {
        if (!ceq_ || !database_loaded_)
            return false;
            
        char target[] = "";
        double g_val = 0.0;
        OCASI_CALL(c_tqce, target, grid_minimizer, 0, &g_val, &ceq_);

        return true;
    }

    bool OpenCalphadInterface::calculateEquilibriumChecked()
    {
        if (!ceq_ || !database_loaded_)
            return false;
        OCASI_CALL(c_tqce_with_check_after, &ceq_);

        return true;
    }

    bool OpenCalphadInterface::listResults(int output_mode)
    {
        if (!ceq_ || !database_loaded_)
            return false;

        OCASI_CALL(c_tqlr, output_mode, &ceq_);

        return true;
    }

    bool OpenCalphadInterface::isPhaseTupleStable(int phase_tuple_index)
    {
        if (!ceq_ || !database_loaded_)
            return false;

        bool is_stable = false;
        OCASI_CALL(c_tqcheckphstab, &is_stable, phase_tuple_index, &ceq_);
        return is_stable;
    }

    bool OpenCalphadInterface::extractResults(OCOutputData &output_data)
    {
        output_data.solution_phases.clear();
        output_data.components.clear();

        if (!ceq_ || !database_loaded_)
            return false;

        output_data.solution_phases.clear();
        output_data.components.clear();

        // Get number of phases
        int nphases = 0;
        OCASI_CALL(c_tqgnp, &nphases, &ceq_);

        // Process each phase
        for (int ph = 0; ph < nphases; ++ph)
        {
            char phase_name[256] = {0};
            const int phase_index = ph + 1;
            if (!isPhaseTupleStable(phase_index))
                continue;

            // Get phase name by index
            OCASI_CALL(c_tqgpn, phase_index, phase_name, &ceq_);
            phase_name[sizeof(phase_name) - 1] = '\0';
            const std::string oc_phase_name = trimOcName(phase_name);

            OCPhaseData phase_data;

            // Get phase moles
            int n_values = 1;
            double phase_moles = 0.0;
            char phase_moles_variable[] = "NP";
            OCASI_CALL(c_tqgetv, phase_moles_variable, phase_index, 0, &n_values, &phase_moles, &ceq_);
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
            OCASI_CALL(c_tqgphc1,
                      phase_index,
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
                OCASI_CALL(c_tqgpi2, &base_phase_index, &composition_set_index, phase_lookup_name, &ceq_);

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
                        OCASI_CALL(c_tqgpcn2, base_phase_index, extended_constituent_index + 1, constituent_name, &ceq_);
                        constituent_name[sizeof(constituent_name) - 1] = '\0';
                        const std::string name = normalizeSpeciesName(trimOcName(constituent_name));
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
                OCASI_CALL(c_tqgetv, element_moles_variable, phase_index, component_index, &n_values, &el_moles, &ceq_);

                if (n_values == 1 && el_moles > 0.0)
                {
                    phase_data.elements[el] = el_moles;
                }

                n_values = 1;
                double el_mass = 0.0;
                // get state variable value
                char element_mass_variable[] = "B";
                OCASI_CALL(c_tqgetv, element_mass_variable, phase_index, component_index, &n_values, &el_mass, &ceq_);

                if (n_values == 1 && el_mass > 0.0)
                    phase_data.element_masses[el] = el_mass;
                else if (el_moles > 0.0)
                    phase_data.element_masses[el] = el_moles * atomicMass(el);
            }

            phase_data.mass = 0.0;
            for (const auto& element_mass_entry : phase_data.element_masses)
                phase_data.mass += element_mass_entry.second;

            if (phase_data.moles <= 0.0 && phase_data.elements.empty())
                continue;

            const std::string phase_bucket = normalizePhaseName(oc_phase_name);
            OCPhaseData &output_phase = output_data.solution_phases[phase_bucket];
            output_phase.moles += phase_data.moles;
            output_phase.form_units += phase_data.form_units;
            output_phase.mass += phase_data.mass;
            addElementInventory(output_phase.elements, phase_data.elements);
            addElementInventory(output_phase.element_masses, phase_data.element_masses);

            if (phase_bucket == "condensed")
            {
                const std::string species_name = normalizeSpeciesName(oc_phase_name);
                OCSpeciesData &species = output_phase.species[species_name];
                const double species_moles =
                    phase_data.form_units > 0.0 ? phase_data.form_units : phase_data.moles;
                species.moles += species_moles;
                species.mass += phase_data.mass;
                species.sublattices.insert(species.sublattices.end(),
                                           phase_sublattices.begin(),
                                           phase_sublattices.end());
                addElementInventory(species.elements, phase_data.elements);
                addElementInventory(species.element_masses, phase_data.element_masses);
            }
            else
            {
                output_phase.sublattices.insert(output_phase.sublattices.end(),
                                                phase_sublattices.begin(),
                                                phase_sublattices.end());

                std::map<std::string, double> species_moles_by_name;
                for (const auto& sublattice : phase_sublattices)
                {
                    const double species_moles_base =
                        sublattice.phase_form_units > 0.0 ? sublattice.phase_form_units : phase_data.moles;
                    for (const auto& constituent_entry : sublattice.composition)
                    {
                        species_moles_by_name[constituent_entry.first] +=
                            species_moles_base * sublattice.sites * constituent_entry.second;
                    }
                }

                if (species_moles_by_name.empty() && phase_data.moles > 0.0)
                    species_moles_by_name[normalizeSpeciesName(oc_phase_name)] = phase_data.moles;

                for (const auto& species_entry : species_moles_by_name)
                {
                    const std::string& species_name = species_entry.first;
                    const double species_moles = species_entry.second;
                    OCSpeciesData& species = output_phase.species[species_name];
                    species.moles += species_moles;

                    const std::map<std::string, double> stoichiometry =
                        speciesStoichiometry(species_name, element_names_);
                    for (const auto& element_entry : stoichiometry)
                    {
                        species.elements[element_entry.first] += element_entry.second * species_moles;
                        const double element_mass =
                            element_entry.second * species_moles * atomicMass(element_entry.first);
                        species.element_masses[element_entry.first] += element_mass;
                        species.mass += element_mass;
                    }
                }
            }
        }


        // Extract component data for the currently loaded system only. The
        // OpenCalphad component globals can retain names from a previously
        // loaded database when matrix and grain-boundary calculations are
        // interleaved.
        for (const auto &comp_name : element_names_)
        {
            if (!comp_name.empty())
            {
                OCComponentData comp_data;
                const int component_index = getComponentIndex(comp_name);
                if (component_index <= 0)
                    continue;

                int n_values = 1;
                double component_moles = 0.0;
                char component_moles_variable[] = "N";
                OCASI_CALL(c_tqgetv, component_moles_variable, component_index, 0, &n_values, &component_moles, &ceq_);
                if (n_values == 1)
                    comp_data.moles = component_moles;

                n_values = 1;
                double component_mass = 0.0;
                char component_mass_variable[] = "B";
                OCASI_CALL(c_tqgetv, component_mass_variable, component_index, 0, &n_values, &component_mass, &ceq_);
                if (n_values == 1 && component_mass > 0.0)
                    comp_data.mass = component_mass;
                else if (component_moles > 0.0)
                    comp_data.mass = component_moles * atomicMass(comp_name);

                n_values = 1;
                double mole_fraction = 0.0;
                char mole_fraction_variable[] = "X";
                OCASI_CALL(c_tqgetv, mole_fraction_variable, component_index, 0, &n_values, &mole_fraction, &ceq_);
                if (n_values == 1)
                    comp_data.mole_fraction = mole_fraction;

                n_values = 1;
                double temperature = 0.0;
                char temperature_variable[] = "T";
                OCASI_CALL(c_tqgetv, temperature_variable, 0, 0, &n_values, &temperature, &ceq_);

                n_values = 1;
                double chemical_potential = 0.0;
                char chemical_potential_variable[] = "MU";
                OCASI_CALL(c_tqgetv, chemical_potential_variable, component_index, 0, &n_values, &chemical_potential, &ceq_);
                if (n_values == 1 && temperature > 0.0)
                {
                    constexpr double gas_constant = 8.31446261815324;
                    comp_data.chemical_potential_over_rt = chemical_potential / (gas_constant * temperature);
                    comp_data.activity = std::exp(comp_data.chemical_potential_over_rt);
                }
                output_data.components[comp_name] = comp_data;
            }
        }

        return true;
    }

    void OpenCalphadInterface::reset(bool clear_database)
    {
        if (!ceq_)
            return;

        char empty_str[] = "";
        OCASI_CALL(c_reset_conditions, empty_str, &ceq_);

        database_loaded_ = !clear_database;
        if (clear_database)
        {
            loaded_database_path_.clear();
            loaded_selected_elements_.clear();
            element_names_.clear();
            nel_ = 0;
        }
    }
    
    int OpenCalphadInterface::currentElementCount() const
    {
        return use_prefixed_symbols_ ? gb_c_nel : c_nel;
    }

    char* OpenCalphadInterface::currentComponentName(int index) const
    {
        return use_prefixed_symbols_ ? gb_c_cnam[index] : c_cnam[index];
    }

} // namespace OCASIAdapter

#undef OCASI_CALL

namespace OCUtilsCoupling
{
    
bool fileExists(const std::string& file_path)
{
    std::ifstream file(file_path);
    return static_cast<bool>(file);
}

bool writePhaseSublatticeCompositionOutput(const std::string& file_path,
                                           double             time_hours,
                                           const std::string& location,
                                           const OCOutputData& output_data,
                                           double             content_scaling_factor)
{
    const bool write_header = !fileExists(file_path);
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
                const std::string& species_name = species_entry.first;
                const OCSpeciesData& species_data = species_entry.second;

                for (const auto& sublattice : species_data.sublattices)
                {
                    for (const auto& constituent_entry : sublattice.composition)
                    {
                        output_file << time_hours << "\t"
                                    << location << "\t"
                                    << species_name << "\t"
                                    << sublattice.phase_instance << "\t"
                                    << sublattice.phase_moles * content_scaling_factor << "\t"
                                    << sublattice.phase_form_units * content_scaling_factor << "\t"
                                    << sublattice.index << "\t"
                                    << sublattice.sites << "\t"
                                    << constituent_entry.first << "\t"
                                    << constituent_entry.second << "\n";
                    }
                }
            }
            continue;
        }

        for (const auto& sublattice : phase_data.sublattices)
        {
            for (const auto& constituent_entry : sublattice.composition)
            {
                output_file << time_hours << "\t"
                            << location << "\t"
                            << phase_name << "\t"
                            << sublattice.phase_instance << "\t"
                            << sublattice.phase_moles * content_scaling_factor << "\t"
                            << sublattice.phase_form_units * content_scaling_factor << "\t"
                            << sublattice.index << "\t"
                            << sublattice.sites << "\t"
                            << constituent_entry.first << "\t"
                            << constituent_entry.second << "\n";
            }
        }
    }

    return output_file.good();
}

std::vector<InputComponent> buildInputComponents(
     const std::set<std::string>&     selected_elements,
     SciantixArray<SciantixVariable>& sciantix_variable,
     SciantixArray<System>&           sciantix_system,
     double&                          total_content,
     const std::string& location)
{
    std::vector<InputComponent> components;
    total_content = 0.0;

    if (location == "matrix")
    {
        // Matrix component
        for (const auto& element_name : selected_elements)
        {
            InputComponent component;
            component.name = element_name;
            component.content = std::max(0.0, sciantix_variable[element_name + " content"].getFinalValue());

            if (component.content > 0.0)
            {
                total_content += component.content;
                components.push_back(component);
            }
        }
    }
    else if (location == "at grain boundary")
    {
        if (selected_elements.count("O") > 0)
        {
            InputComponent component;
            component.name = "O";
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
                const double atoms_available =
                    sciantix_variable[element_name + " produced"].getFinalValue() -
                    sciantix_variable[element_name + " decayed"].getFinalValue() -
                    sciantix_variable[element_name + " in grain"].getFinalValue() -
                    sciantix_variable[element_name + " released"].getInitialValue();

                component.content = std::max(0.0, atoms_available / avogadro_number);
            }
            else if (system.getRestructuredMatrix() == 0 && system.isMetallicFP())
            {
                const double atoms_available =
                    sciantix_variable[element_name + " produced"].getFinalValue();

                component.content = std::max(0.0, atoms_available / avogadro_number);
            }

            if (component.content > 0.0)
            {
                total_content += component.content;
                components.push_back(component);
            }
        }
    }

    if (total_content <= 0.0 || components.empty())
        return components;

    for (auto& component : components)
        component.fraction = component.content / total_content;

    components.erase(
        std::remove_if(
            components.begin(),
            components.end(),
            [](const InputComponent& component)
            {
                return component.fraction < 1.0e-8; // cut-off
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

bool runOpenCalphadCaseOCASI(const std::string& database_path,
                             double temperature,
                             double pressure,
                             const std::vector<InputComponent>& components,
                             const std::vector<std::string>& valid_elements,
                             OpenCalphadSolveMode solve_mode,
                             const std::string& location,
                             double oxygen_potential_kj_per_mol_o2,
                             OCOutputData& output_data)
{
    try
    {
        if (location == "matrix")
        {
            auto& oc = OCASIAdapter::getOpenCalphadInterface(
                        OCASIAdapter::OpenCalphadContext::Matrix
                    );

            const bool database_ready = oc.ensureDatabaseLoaded(database_path, valid_elements);
            if (!database_ready)
            {
                std::cerr << "Error: Failed to load OpenCalphad database: " << database_path << std::endl;
                return false;
            }

            const bool reuse_existing_record = solve_mode == OpenCalphadSolveMode::SaveReadWarmStart;
            const bool record_ready =
                oc.prepareCalculationRecord(equilibriumRecordName(location, solve_mode), reuse_existing_record);
            if (!record_ready)
            {
                std::cerr << "Error: Failed to prepare OpenCalphad equilibrium record" << std::endl;
                return false;
            }
            oc.reset(false);
            const bool reference_state_ready =
                oc.setReferenceState("O", "GAS", -1.0, reference_oxygen_pressure_bar * 1.0e6);
            if (!reference_state_ready)
                std::cerr << "Warning: Failed to set OpenCalphad oxygen gas reference state" << std::endl;

            std::map<std::string, double> components_map;
            for (const auto& comp : components)
                components_map[comp.name] = comp.fraction;

            const bool conditions_ready = oc.setConditions(temperature, pressure, components_map);
            if (!conditions_ready)
            {
                std::cerr << "Error: Failed to set OpenCalphad conditions" << std::endl;
                return false;
            }

            // Same first solve as the previous macro `c e`: no grid minimizer.
            bool clear_equilibrium = true;

            const bool initial_equilibrium_ready = oc.calculateEquilibrium(-1);
            if (!initial_equilibrium_ready)
            {
                std::cerr << "Warning: Initial OpenCalphad equilibrium calculation failed" << std::endl;
                clear_equilibrium = false;
            }

            if (solve_mode == OpenCalphadSolveMode::SaveReadWarmStart ||
                solve_mode == OpenCalphadSolveMode::GlobalEquilibrium)
            {
                const bool checked_equilibrium_ready = oc.calculateEquilibriumChecked();
                if (!checked_equilibrium_ready)
                {
                    std::cerr << "Warning: OpenCalphad checked equilibrium calculation failed" << std::endl;
                    clear_equilibrium = false;
                }
            }
            else if (solve_mode == OpenCalphadSolveMode::OnlyC1MO2)
            {
                oc.setPhaseStatus("*", -2, 0.0);
                oc.setPhaseStatus("GAS", 0, 1.0);
                const bool gas_only_ready = oc.calculateEquilibrium(-1);
                if (!gas_only_ready)
                {
                    std::cerr << "Warning: OpenCalphad gas-only equilibrium calculation failed" << std::endl;
                    clear_equilibrium = false;
                }
                oc.setPhaseStatus("C1_MO2", 0, 1.0);
                const bool c1_mo2_ready = oc.calculateEquilibrium(-1) && oc.calculateEquilibriumChecked();
                if (!c1_mo2_ready)
                {
                    std::cerr << "Warning: OpenCalphad fixed C1_MO2 equilibrium failed" << std::endl;
                    clear_equilibrium = false;
                }
            }

            oc.extractResults(output_data);

            return clear_equilibrium;
        }
        else if (location == "at grain boundary")
        {
            auto& oc = OCASIAdapter::getOpenCalphadInterface(OCASIAdapter::OpenCalphadContext::FissionProducts);

            const bool database_ready = oc.ensureDatabaseLoaded(database_path, valid_elements);
            if (!database_ready)
            {
                std::cerr << "Error: Failed to load OpenCalphad database: " << database_path << std::endl;
                return false;
            }

            const bool reuse_existing_record = solve_mode == OpenCalphadSolveMode::SaveReadWarmStart;
            const bool record_ready =
                oc.prepareCalculationRecord(equilibriumRecordName(location, solve_mode), reuse_existing_record);
            if (!record_ready)
            {
                std::cerr << "Error: Failed to prepare OpenCalphad equilibrium record" << std::endl;
                return false;
            }
            oc.reset(false);
            const bool reference_state_ready =
                oc.setReferenceState("O", "GAS", -1.0, reference_oxygen_pressure_bar * 1.0e6);
            if (!reference_state_ready)
                std::cerr << "Warning: Failed to set OpenCalphad oxygen gas reference state" << std::endl;

            std::map<std::string, double> components_map;
            for (const auto& comp : components)
            {
                if (toUpperCopy(comp.name) == "O")
                    continue;
                components_map[comp.name] = comp.fraction;
            }

            const bool conditions_ready = oc.setConditions(
                temperature,
                pressure,
                components_map);
            if (!conditions_ready)
            {
                std::cerr << "Error: Failed to set OpenCalphad conditions" << std::endl;
                return false;
            }

            const double oxygen_potential_j_per_mol_o = oxygen_potential_kj_per_mol_o2 * 1.0e3 / 2.0;
            const bool oxygen_potential_ready = oc.setComponentPotential("O", oxygen_potential_j_per_mol_o);
            if (!oxygen_potential_ready)
            {
                std::cerr << "Error: Failed to set OpenCalphad oxygen potential condition" << std::endl;
                return false;
            }

            bool clear_equilibrium = true;

            const bool initial_equilibrium_ready =
#if defined(COUPLING_TU)
                oc.calculateEquilibrium(-1);
#else
                oc.calculateEquilibrium(0);
#endif
            if (!initial_equilibrium_ready)
            {
                std::cerr << "Warning: Initial OpenCalphad equilibrium calculation failed" << std::endl;
                clear_equilibrium = false;
            }

            #if !defined(COUPLING_TU)
            const bool checked_equilibrium_ready = oc.calculateEquilibriumChecked();
            if (!checked_equilibrium_ready)
            {
                std::cerr << "Warning: OpenCalphad checked equilibrium calculation failed" << std::endl;
                clear_equilibrium = false;
            }
            #endif

            oc.listResults(2);

            const bool extracted = oc.extractResults(output_data);

            const bool output_valid =
                extracted && validateOpenCalphadOutput(output_data, components, location);

            return clear_equilibrium && output_valid;
        }
        else
        {
            std::cerr << "Error: Invalid location for OpenCalphad case: " << location << std::endl;
            return false;
        }
    }
    catch (const std::exception& e)
    {
        std::cerr << "Exception in runOpenCalphadCaseOCASI: " << e.what() << std::endl;
        return false;
    }
}

bool validateOpenCalphadOutput(const OCOutputData& output_data,
                               const std::vector<InputComponent>& input_components,
                               const std::string& location)
{
    if (location == "matrix")
        return true; // skip detailed checks for matrix cases due to potential minor inconsistencies with the simplified input
    constexpr double significant_input_fraction = 1.0e-8;
    constexpr double inventory_relative_tolerance = 5.0e-2;
    constexpr double minimum_recovered_fraction = 1.0e-3;
    constexpr double absolute_tolerance = 1.0e-12;

    if (output_data.solution_phases.empty())
    {
        std::cerr << "Error: OpenCalphad returned no stable phases for "
                  << location << std::endl;
        return false;
    }

    std::map<std::string, double> input_inventory;
    for (const auto& component : input_components)
    {
        if (!std::isfinite(component.fraction) || component.fraction < 0.0)
        {
            std::cerr << "Error: invalid OpenCalphad input fraction for "
                      << component.name << " at " << location << std::endl;
            return false;
        }

        if (location == "at grain boundary" && toUpperCopy(component.name) == "O")
            continue;

        if (component.fraction >= significant_input_fraction)
            input_inventory[component.name] += component.fraction;
    }

    std::map<std::string, double> phase_inventory;
    for (const auto& phase_entry : output_data.solution_phases)
    {
        const OCPhaseData& phase_data = phase_entry.second;
        if (!std::isfinite(phase_data.moles) || phase_data.moles < -absolute_tolerance)
        {
            std::cerr << "Error: invalid OpenCalphad phase amount for "
                      << phase_entry.first << " at " << location << std::endl;
            return false;
        }

        for (const auto& element_entry : phase_data.elements)
        {
            if (!std::isfinite(element_entry.second) || element_entry.second < -absolute_tolerance)
            {
                std::cerr << "Error: invalid OpenCalphad element inventory for "
                          << element_entry.first << " in phase " << phase_entry.first
                          << " at " << location << std::endl;
                return false;
            }

            if (location == "at grain boundary" && toUpperCopy(element_entry.first) == "O")
                continue;

            phase_inventory[element_entry.first] += std::max(0.0, element_entry.second);
        }
    }

    for (const auto& component_entry : output_data.components)
    {
        const OCComponentData& component_data = component_entry.second;
        if (!std::isfinite(component_data.moles) ||
            !std::isfinite(component_data.mole_fraction) ||
            !std::isfinite(component_data.chemical_potential_over_rt) ||
            !std::isfinite(component_data.activity) ||
            component_data.moles < -absolute_tolerance ||
            component_data.mole_fraction < -absolute_tolerance ||
            component_data.activity < -absolute_tolerance)
        {
            std::cerr << "Error: invalid OpenCalphad component data for "
                      << component_entry.first << " at " << location << std::endl;
            return false;
        }
    }

    double input_total = 0.0;
    double output_total = 0.0;
    for (const auto& element_entry : input_inventory)
        input_total += element_entry.second;
    for (const auto& element_entry : phase_inventory)
        output_total += element_entry.second;

    if (input_total <= 0.0 || output_total <= 0.0)
    {
        std::cerr << "Error: OpenCalphad inventory check has zero input or output for "
                  << location << std::endl;
        return false;
    }

    bool balanced = true;
    for (const auto& input_entry : input_inventory)
    {
        const double expected_fraction = input_entry.second / input_total;
        const double output_moles = phase_inventory[input_entry.first];
        const double output_fraction = output_moles / output_total;
        const double difference = std::abs(output_fraction - expected_fraction);
        const double tolerance =
            std::max(inventory_relative_tolerance * expected_fraction, absolute_tolerance);

        if (output_moles <= std::max(absolute_tolerance,
                                     minimum_recovered_fraction * expected_fraction * output_total) ||
            difference > tolerance)
        {
            std::cerr << "Error: OpenCalphad inventory mismatch for "
                      << input_entry.first << " at " << location
                      << " input fraction=" << expected_fraction
                      << " output fraction=" << output_fraction
                      << std::endl;
            balanced = false;
        }
    }

    if (!balanced)
        std::cerr << "Error: rejecting OpenCalphad results for "
                  << location << " to avoid propagating inconsistent data"
                  << std::endl;

    return balanced;
}

void updateThermochemistryVariablesFromOutput(const std::map<std::string, OCPhaseData>&  solution_phases,
                                              const std::string&                         location,
                                              double                                     content_scaling_factor,
                                              SciantixArray<ThermochemistryVariable>&    thermochemistry_variable,
                                              SciantixArray<SciantixVariable>&           sciantix_variable)
{
    auto setThermochemistryMass = [](ThermochemistryVariable& variable,
                                     double                   mass,
                                     const std::map<std::string, double>& composition)
    {
        variable.setComposition(composition);
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

    double oxygen_with_fps = 0.0;
    for (const auto& phase_entry : solution_phases)
    {
        const std::string& phase_name = phase_entry.first;
        const OCPhaseData& phase_data = phase_entry.second;
        const bool liquid_phase = isLiquidPhase(phase_name);

        const auto oxygen = phase_data.elements.find("O");
        if (oxygen != phase_data.elements.end())
            oxygen_with_fps += oxygen->second * content_scaling_factor;

        if (liquid_phase)
        {
            const std::string liquid_variable_name = "LIQUID (" + phase_name + ", " + location + ")";
            if (thermochemistry_variable.isElementPresent(liquid_variable_name))
            {
                const std::map<std::string, double> composition = computePhaseComposition(phase_data);
                if (!composition.empty())
                    setThermochemistryMass(
                        thermochemistry_variable[liquid_variable_name],
                        phase_data.mass * content_scaling_factor,
                        composition);
            }
        }

        if (!phase_data.species.empty())
        {
            for (const auto& species_entry : phase_data.species)
            {
                const std::string variable_name =
                    species_entry.first + " (" + phase_name + ", " + location + ")";

                if (thermochemistry_variable.isElementPresent(variable_name))
                {
                    std::map<std::string, double> composition;
                    if (species_entry.second.moles > 0.0)
                    {
                        for (const auto& element_entry : species_entry.second.elements)
                            composition[element_entry.first] = element_entry.second / species_entry.second.moles;
                    }
                    if (!composition.empty())
                    {
                        setThermochemistryMass(
                            thermochemistry_variable[variable_name],
                            species_entry.second.mass * content_scaling_factor,
                            composition);
                    }
                }
            }

            if (liquid_phase)
                continue;

            for (const auto& element_entry : phase_data.elements)
            {
                const std::string variable_name = element_entry.first + " (" + phase_name + ", " + location + ")";
                const std::string uppercase_variable_name =
                    toUpperCopy(element_entry.first) + " (" + phase_name + ", " + location + ")";
                const bool has_variable = thermochemistry_variable.isElementPresent(variable_name);
                const bool has_uppercase_variable =
                    thermochemistry_variable.isElementPresent(uppercase_variable_name);

                if (has_variable)
                {
                    const auto element_mass = phase_data.element_masses.find(element_entry.first);
                    setThermochemistryMass(
                        thermochemistry_variable[variable_name],
                        (element_mass != phase_data.element_masses.end() ? element_mass->second : 0.0) *
                            content_scaling_factor,
                        {{element_entry.first, 1.0}});
                }
                else if (has_uppercase_variable)
                {
                    const auto element_mass = phase_data.element_masses.find(element_entry.first);
                    setThermochemistryMass(
                        thermochemistry_variable[uppercase_variable_name],
                        (element_mass != phase_data.element_masses.end() ? element_mass->second : 0.0) *
                            content_scaling_factor,
                        {{element_entry.first, 1.0}});
                }
            }
            continue;
        }

        if (liquid_phase)
            continue;

        for (const auto& element_entry : phase_data.elements)
        {
            const std::string variable_name = element_entry.first + " (" + phase_name + ", " + location + ")";
            const std::string uppercase_variable_name =
                toUpperCopy(element_entry.first) + " (" + phase_name + ", " + location + ")";

            if (thermochemistry_variable.isElementPresent(variable_name))
            {
                const auto element_mass = phase_data.element_masses.find(element_entry.first);
                setThermochemistryMass(
                    thermochemistry_variable[variable_name],
                    (element_mass != phase_data.element_masses.end() ? element_mass->second : 0.0) *
                        content_scaling_factor,
                    {{element_entry.first, 1.0}});
            }
            else if (thermochemistry_variable.isElementPresent(uppercase_variable_name))
            {
                const auto element_mass = phase_data.element_masses.find(element_entry.first);
                setThermochemistryMass(
                    thermochemistry_variable[uppercase_variable_name],
                    (element_mass != phase_data.element_masses.end() ? element_mass->second : 0.0) *
                        content_scaling_factor,
                    {{element_entry.first, 1.0}});
            }
        }
    }

    if (location == "at grain boundary")
        sciantix_variable["O available content"].setFinalValue(oxygen_with_fps);
}

void updateMatrixFromOutput(const OCOutputData&              output_data,
                            double                           temperature,
                            SciantixArray<SciantixVariable>& sciantix_variable)
{
    const auto oxygen_component = output_data.components.find("O");
    double calphad_oxygen_potential(0.0), calphad_oxygen_partial_pressure(0.0);
    const bool has_usable_oxygen_component =
        oxygen_component != output_data.components.end() &&
        oxygen_component->second.activity > 0.0;
    if (has_usable_oxygen_component)
    {
        calphad_oxygen_potential =
            2.0 * oxygen_component->second.chemical_potential_over_rt * gas_constant * temperature * 1.0e-3;
        calphad_oxygen_partial_pressure =
            reference_oxygen_pressure_bar * oxygen_component->second.activity * oxygen_component->second.activity;

        sciantix_variable["Fuel oxygen partial pressure - CALPHAD"].setFinalValue(calphad_oxygen_partial_pressure);
        sciantix_variable["Fuel oxygen potential - CALPHAD"].setFinalValue(calphad_oxygen_potential);
    }

    if (calphad_oxygen_partial_pressure > 0.0)
    {
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(calphad_oxygen_partial_pressure);
        sciantix_variable["Fuel oxygen potential"].setFinalValue(calphad_oxygen_potential);
    }
}

void updateGrainBoundaryFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                   const std::set<std::string>&               selected_elements,
                                   double                                     content_scaling_factor,
                                   SciantixArray<SciantixVariable>&           sciantix_variable,
                                   SciantixArray<System>&                     sciantix_system)
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
            const double available = (
                sciantix_variable[element + " produced"].getFinalValue() -
                sciantix_variable[element + " decayed"].getFinalValue() -
                sciantix_variable[element + " in grain"].getFinalValue() -
                sciantix_variable[element + " released"].getInitialValue()
            );

            const double updated_atoms = std::min(available, gas_moles * avogadro_number);
            sciantix_variable[element + " at grain boundary"].setFinalValue(updated_atoms);
            sciantix_variable[element + " reacted"].setFinalValue(available - updated_atoms);
        }
        else if (system.getRestructuredMatrix() == 0 && system.isMetallicFP())
        {
            const double available =
                sciantix_variable[element + " produced"].getFinalValue();

            const double updated_atoms = std::min(available, gas_moles * avogadro_number);
            sciantix_variable[element + " in solution"].setFinalValue(updated_atoms);
            sciantix_variable[element + " reacted"].setFinalValue(available - updated_atoms);
        }
    }
}
}  // namespace OCUtilsCoupling
