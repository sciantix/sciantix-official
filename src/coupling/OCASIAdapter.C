//////////////////////////////////////////////////////////////////////////////////////
//       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
//      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
//     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
//      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
//  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
//  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
//                                                                                  //
//  OCASI Adapter Implementation                                                   //
//  Direct C++ interface to OpenCalphad Fortran core via OCASI bindings             //
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

// ============================================================================
// Fortran C Bindings - Direct interface to OpenCalphad Fortran core
// ============================================================================

extern "C" int c_nel;              // Number of elements
extern "C" int c_maxp;             // Max phases
extern "C" int c_ntup;             // Number of components
extern "C" char *c_cnam[41];       // Component names array
extern "C" int c_noofcs(int);      // Number of composition sets for phase

extern "C" {
    void c_tqini(int, void *);                                               // Initialize
    void c_tqrfil(char *, void *);                                          // Read all elements from TDB
    void c_tqrpfil(char *, int, char **, void *);                           // Read selected elements from TDB
    void c_tqgcom(int *, char *, void *);                                   // Get component names
    void c_tqgnp(int *, void *);                                            // Get number of phases
    void c_tqgpn(int, char *, void *);                                      // Get phase name by index
    void c_tqgpi(int *, char *, void *);                                    // Get phase index by name
    void c_tqgpi2(int *, int *, char *, void *);                            // Get phase and composition set indices by name
    void c_tqsetc(char *, int, int, double, int *, void *);                 // Set condition
    void c_tqce(char *, int, int, double *, void *);                        // Calculate equilibrium
    void c_tqgetv(char *, int, int, int *, double *, void *);               // Get equilibrium result
    void c_tqgpcs(int, int, double *, double *, void *);                    // Get phase composition
    void c_tqgpci(int, int *, char *, void *);                              // Get constituent name
    void c_tqgpcn2(int, int, char *);                                       // Get constituent name by extended index
    void c_tqgnpc(int, int *, void *);                                      // Get number of constituents in phase
    void c_tqgphc1(int, int *, int *, int *, double *, double *, double *, void *); // Get phase composition detailed
    void c_Change_Status_Phase(char *, int, double, void *);                // Change phase status
    void c_Set_Reference_State(int, char *, double *, void *);              // Set component reference state
    void c_reset_conditions(char *, void *);                                // Reset conditions
    int c_errors_number();                                                  // Error counter
    void c_reset_errors_number();                                           // Reset error counter
}

namespace OCASIAdapter
{

namespace
{
std::string trimOcName(const std::string& input)
{
    const auto end = input.find_last_not_of(" .\0", std::string::npos);
    if (end == std::string::npos)
        return "";
    const auto begin = input.find_first_not_of(" .\0");
    return input.substr(begin, end - begin + 1);
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

std::string canonicalElementName(std::string text)
{
    text = trimOcName(text);
    if (text.empty())
        return text;

    text = lowerCopy(text);
    text[0] = static_cast<char>(std::toupper(static_cast<unsigned char>(text[0])));
    return text;
}

std::string stripPhaseDecorations(std::string name)
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

std::string phaseInstanceName(std::string name)
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

std::string normalizePhaseBucket(const std::string& phase_name)
{
    const std::string base = upperCopy(stripPhaseDecorations(phase_name));

    if (base == "GAS" || base == "LIQUID" || base == "LIQUID_IONIC" ||
        base == "IONIC_LIQUID" || base == "PURE_CONDENSED" || base == "SOLID")
        return lowerCopy(base);

    return "condensed";
}

void addElementInventory(std::map<std::string, double>& target,
                         const std::map<std::string, double>& source)
{
    for (const auto& element_entry : source)
        target[element_entry.first] += element_entry.second;
}

bool fileExists(const std::string& path)
{
    return std::ifstream(path).good();
}

std::string resolveTdbPath(const std::string& path)
{
    if (fileExists(path))
        return path;

    const std::string upper_tdb = path + ".TDB";
    if (fileExists(upper_tdb))
        return upper_tdb;

    const std::string lower_tdb = path + ".tdb";
    if (fileExists(lower_tdb))
        return lower_tdb;

    return path;
}
}  // namespace

// Global singleton instance
static std::unique_ptr<OpenCalphadInterface> g_ocasi_interface;

OpenCalphadInterface& getOpenCalphadInterface()
{
    if (!g_ocasi_interface)
        g_ocasi_interface = std::make_unique<OpenCalphadInterface>();
    return *g_ocasi_interface;
}

// ============================================================================
// OpenCalphadInterface Implementation
// ============================================================================

OpenCalphadInterface::OpenCalphadInterface()
{
    c_tqini(0, &ceq_);
    if (ceq_ == nullptr)
        throw std::runtime_error("Failed to initialize OpenCalphad via OCASI");
}

OpenCalphadInterface::~OpenCalphadInterface()
{
    reset(true);
}

int OpenCalphadInterface::getComponentIndex(const std::string& component_name) const
{
    const std::string target = upperCopy(component_name);
    const auto element_it = std::find_if(element_names_.begin(),
                                         element_names_.end(),
                                         [&target](const std::string& name) {
                                             return upperCopy(name) == target;
                                         });

    if (element_it == element_names_.end())
        return 0;

    return static_cast<int>(std::distance(element_names_.begin(), element_it)) + 1;
}

bool OpenCalphadInterface::loadDatabase(const std::string& tdb_file_path,
                                       const std::vector<std::string>& selected_elements)
{
    // Initiate the TQ interface - this will also reset any previously loaded database and conditions
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

    for (const auto& el : selected_elements)
    {
        temp_strings.push_back(upperCopy(el));
    }

    for (auto& el : temp_strings)
    {
        el_array.push_back(const_cast<char *>(el.c_str()));
    }

    c_reset_errors_number();

    // Read and select elements from TDB - this is necessary to properly initialize the system with correct element list and order
    c_tqrpfil(tdb_path, static_cast<int>(el_array.size()), el_array.data(), &ceq_);


    if (c_errors_number() != 0)
        return false;

    // Cache element information
    nel_ = c_nel;
    element_names_.clear();
    element_names_.resize(nel_);

    for (int i = 0; i < nel_; ++i)
    {
        if (c_cnam[i])
        {
            element_names_[i] = canonicalElementName(c_cnam[i]);
        }
    }

    database_loaded_ = true;
    return true;
}

bool OpenCalphadInterface::setConditions(double temperature,
                                        double pressure,
                                        const std::map<std::string, double>& components)
{
    if (!ceq_ || !database_loaded_)
        return false;

    int condition_number = 0;

    // Set temperature (in Kelvin)
    char t_var[] = "T";
    c_reset_errors_number();
    c_tqsetc(t_var, 0, 0, temperature, &condition_number, &ceq_);
    if (c_errors_number() != 0)
    {
        std::cerr << "Error: Could not set OpenCalphad temperature condition" << std::endl;
        return false;
    }   

    // Set pressure (in Pa)
    char p_var[] = "P";
    c_reset_errors_number();
    c_tqsetc(p_var, 0, 0, pressure, &condition_number, &ceq_);
    if (c_errors_number() != 0)
    {
        std::cerr << "Error: Could not set OpenCalphad pressure condition" << std::endl;
        return false;
    }

    // Set component contents
    for (const auto& comp : components)
    {
        const int component_index = getComponentIndex(comp.first);
        if (component_index <= 0)
        {
            std::cerr << "Warning: component " << comp.first << " is not present in loaded OpenCalphad system" << std::endl;
            continue;
        }

        // Content condition
        char n_var[] = "N";
        c_reset_errors_number();
        c_tqsetc(n_var, component_index, 0, comp.second, &condition_number, &ceq_);
        if (c_errors_number() != 0)
        {
            std::cerr << "Warning: Could not set condition for " << comp.first << std::endl;
            // Continue - component might not be in system
        }
        std::cout << "Set component " << comp.first << ": " << comp.second << std::endl;
    }

    return true;
}

bool OpenCalphadInterface::setReferenceState(const std::string& component_name,
                                             const std::string& phase_name,
                                             double temperature,
                                             double pressure)
{
    if (!ceq_ || !database_loaded_)
        return false;

    const int component_index = getComponentIndex(component_name);
    if (component_index <= 0)
        return false;

    char phase[24] = {0};
    std::strncpy(phase, upperCopy(phase_name).c_str(), sizeof(phase) - 1);

    double tpref[2] = {temperature, pressure};
    c_reset_errors_number();
    c_Set_Reference_State(component_index, phase, tpref, &ceq_);
    return c_errors_number() == 0;
}

bool OpenCalphadInterface::removeComponentCondition(const std::string& component_name)
{
    if (!ceq_ || !database_loaded_)
        return false;

    const int component_index = getComponentIndex(component_name);
    if (component_index <= 0)
        return false;

    int condition_number = 0;
    char n_var[] = "N";
    c_reset_errors_number();
    // to remove a condition the value should be equal to RNONE ????
    c_tqsetc(n_var, component_index, -1, -1.0e-36, &condition_number, &ceq_);
    return c_errors_number() == 0;
}

bool OpenCalphadInterface::setComponentPotential(const std::string& component_name, double chemical_potential)
{
    if (!ceq_ || !database_loaded_)
        return false;

    const int component_index = getComponentIndex(component_name);
    if (component_index <= 0)
        return false;

    int condition_number = 0;
    char mu_var[] = "MU";
    c_reset_errors_number();
    c_tqsetc(mu_var, component_index, 0, chemical_potential, &condition_number, &ceq_);
    return c_errors_number() == 0;
}

bool OpenCalphadInterface::setPhaseStatus(const std::string& phase_name,
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

bool OpenCalphadInterface::calculateEquilibrium(bool suspend_gas)
{
    if (!ceq_ || !database_loaded_)
        return false;

    // i do not think this is needed, as the conditions should be properly set before calling this function, but just in case reset any conditions with invalid values
    // if (suspend_gas)
    // {
    //     // Suspend gas phase before calculation
    //     setPhaseStatus("GAS", -3, 0.0);
    // }

    char dummy[] = "";
    double g_val = 0.0;
    c_reset_errors_number();
    c_tqce(dummy, 0, 0, &g_val, &ceq_);

    return c_errors_number() == 0;
}

bool OpenCalphadInterface::extractResults(OCOutputData& output_data)
{
    if (!ceq_ || !database_loaded_)
        return false;

    output_data.solution_phases.clear();
    output_data.components.clear();

    // Get number of phases
    int nphases = 0;
    // Get number of phases and composition sets
    c_tqgnp(&nphases, &ceq_);

    // Process each phase
    for (int ph = 0; ph < nphases; ++ph)
    {
        // Get phase name
        char phase_name_buf[64] = {0};
        const int phase_index = ph + 1;
        // Get phase name by index
        c_tqgpn(phase_index, phase_name_buf, &ceq_);
        const std::string oc_phase_name = trimOcName(phase_name_buf);

        OCPhaseData phase_data;

        // Get phase moles
        int n_values = 1;
        double phase_moles = 0.0;
        char moles_var[] = "NP";
        c_reset_errors_number();
        // Get state variable value
        c_tqgetv(moles_var, phase_index, 0, &n_values, &phase_moles, &ceq_);
        phase_data.moles = (n_values == 1 && c_errors_number() == 0) ? phase_moles : 0.0;

        constexpr int max_sublattices = 32;
        constexpr int max_constituents = 512;
        int n_sublattices = 0;
        int constituents_per_sublattice[max_sublattices] = {0};
        int constituent_indices[max_constituents] = {0};
        double constituent_fractions[max_constituents] = {0.0};
        double sublattice_sites[max_sublattices] = {0.0};
        double phase_extra[8] = {0.0};

        c_reset_errors_number();
        // get phase constitution
        c_tqgphc1(phase_index,
                  &n_sublattices,
                  constituents_per_sublattice,
                  constituent_indices,
                  constituent_fractions,
                  sublattice_sites,
                  phase_extra,
                  &ceq_);

        std::vector<OCSublatticeData> phase_sublattices;
        if (c_errors_number() == 0 && n_sublattices > 0 && n_sublattices <= max_sublattices)
        {
            const double components_per_formula_unit = phase_extra[0];
            const double phase_form_units =
                components_per_formula_unit > 0.0 ? phase_data.moles / components_per_formula_unit : phase_data.moles;

            int base_phase_index = phase_index;
            int composition_set_index = 0;
            char phase_lookup_name[64] = {0};
            std::strncpy(phase_lookup_name, oc_phase_name.c_str(), sizeof(phase_lookup_name) - 1);
            // get phase and composition indices of phase using its name
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
                sublattice.phase_instance = phaseInstanceName(oc_phase_name);

                for (int constituent = 0;
                     constituent < constituents_per_sublattice[sublattice_index] &&
                     extended_constituent_index < max_constituents;
                     ++constituent)
                {
                    char constituent_name[24] = {0};
                    // Get phase constituent index using name
                    c_tqgpcn2(base_phase_index, extended_constituent_index + 1, constituent_name);
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
            const auto& el = element_names_[element_index];
            const int component_index = static_cast<int>(element_index) + 1;
            int n_values = 1;
            double el_moles = 0.0;
            char n_var[] = "N";
            c_reset_errors_number();
            // get state variable value
            c_tqgetv(n_var, phase_index, component_index, &n_values, &el_moles, &ceq_);

            if (n_values == 1 && c_errors_number() == 0 && el_moles > 0.0)
            {
                phase_data.elements[el] = el_moles;
            }
        }

        if (phase_data.moles <= 0.0 && phase_data.elements.empty())
            continue;

        const std::string phase_bucket = normalizePhaseBucket(oc_phase_name);
        OCPhaseData& output_phase = output_data.solution_phases[phase_bucket];
        output_phase.moles += phase_data.moles;
        output_phase.form_units += phase_data.form_units;
        output_phase.volume += phase_data.volume;
        addElementInventory(output_phase.elements, phase_data.elements);

        if (phase_bucket == "condensed")
        {
            const std::string species_name = stripPhaseDecorations(oc_phase_name);
            OCSpeciesData& species = output_phase.species[species_name];
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
            comp_name = canonicalElementName(comp_name);

            if (!comp_name.empty())
            {
                OCComponentData comp_data;
                const int component_index = getComponentIndex(comp_name);

                int n_values = 1;
                double component_moles = 0.0;
                char n_var[] = "N";
                c_reset_errors_number();
                // get state variable value
                c_tqgetv(n_var, component_index, 0, &n_values, &component_moles, &ceq_);
                if (n_values == 1 && c_errors_number() == 0)
                    comp_data.moles = component_moles;

                n_values = 1;
                double mole_fraction = 0.0;
                char x_var[] = "X";
                c_reset_errors_number();
                // get state variable value
                c_tqgetv(x_var, component_index, 0, &n_values, &mole_fraction, &ceq_);
                if (n_values == 1 && c_errors_number() == 0)
                    comp_data.mole_fraction = mole_fraction;

                n_values = 1;
                double temperature = 0.0;
                char t_var[] = "T";
                c_reset_errors_number();
                // get state variable value
                c_tqgetv(t_var, 0, 0, &n_values, &temperature, &ceq_);

                n_values = 1;
                double chemical_potential = 0.0;
                char mu_var[] = "MU";
                c_reset_errors_number();
                // get state variable value
                c_tqgetv(mu_var, component_index, 0, &n_values, &chemical_potential, &ceq_);
                if (n_values == 1 && c_errors_number() == 0 && temperature > 0.0)
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
}

std::string OpenCalphadInterface::getPhaseNameAtIndex(int phase_index) const
{
    if (!ceq_)
        return "";

    char phase_name_buf[64] = {0};
    void* ceq = const_cast<void *>(ceq_);
    c_tqgpn(phase_index, phase_name_buf, &ceq);
    return std::string(phase_name_buf);
}

int OpenCalphadInterface::getPhaseIndex(const std::string& phase_name) const
{
    if (!ceq_)
        return -1;

    int ph_idx = -1;
    char ph_name[64];
    std::strncpy(ph_name, phase_name.c_str(), sizeof(ph_name) - 1);
    ph_name[sizeof(ph_name) - 1] = '\0';

    int idx = 0;
    void* ceq = const_cast<void *>(ceq_);
    c_tqgpi(&idx, ph_name, &ceq);
    return idx >= 0 ? idx : -1;
}

}  // namespace OCASIAdapter
