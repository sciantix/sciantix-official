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

#include "InputReading.h"
#include "MainVariables.h"
#include "ThermochemistrySettings.h"

#include <algorithm>
#include <iostream>
#include <map>
#include <set>

/**
 * @brief Parses an input file made of keyed entries.
 * @param path The file to parse. A file that cannot be opened yields no entries.
 * @return The entries of the file, by key.
 *
 * input_settings.txt, input_initial_conditions.txt and input_scaling_factors.txt share one
 * format: every entry is a single line
 *
 *     <value(s)>  #  <Key> (<description>)
 *
 * where <Key> is the first token after the '#'.
 */
NamedInput ParseNamedEntries(const std::string& path)
{
    NamedInput        parsed;
    std::ifstream     file(path, std::ios::in);
    const std::string file_name = path.substr(path.find_last_of('/') + 1);  // for the messages
    std::string       line;
    int               line_number(0);

    while (std::getline(file, line))
    {
        ++line_number;
        const std::size_t hash   = line.find('#');
        const std::string before = line.substr(0, hash);

        if (before.find_first_not_of(" \t\r") == std::string::npos)
            continue;  // blank line, or a comment line

        std::string key;
        if (hash != std::string::npos)
            std::istringstream(line.substr(hash + 1)) >> key;

        if (key.empty())
            ErrorMessages::Fatal(file_name.c_str(),
                                 "line " + std::to_string(line_number) +
                                     " has a value but no key; entries are written as \"<value(s)> # <key>\" "
                                     "(convert older inputs with utilities/inputExample/convert_to_named_inputs.py)");

        // Silently keeping one of two values would run the case with an input the file contradicts
        if (parsed.value.count(key))
            ErrorMessages::Fatal(file_name.c_str(), "the input entry \"" + key + "\" is given more than once");

        parsed.value[key] = before;
    }

    return parsed;
}

/**
 * @brief Reads the values of an entry, or reports that the entry is absent.
 * @param key The entry to look up.
 * @param parsed The parsed input file.
 * @param count The number of values the entry must hold.
 * @param values Receives the values; left untouched when the entry is absent.
 * @return true when the entry is present.
 */
static bool lookUpEntry(const std::string& key, const NamedInput& parsed, std::size_t count, std::vector<double>& values)
{
    const auto entry = parsed.value.find(key);
    if (entry == parsed.value.end())
        return false;

    parsed.consumed.insert(key);

    std::vector<double> read;
    std::istringstream  stream(entry->second);
    double              value(0.0);
    while (stream >> value)
        read.push_back(value);

    if (!stream.eof() || read.size() != count)
        ErrorMessages::Fatal("InputReading.C",
                             "the input entry \"" + key + "\" must hold exactly " + std::to_string(count) +
                                 " numeric value(s)");

    values = read;
    return true;
}

/**
 * @brief Reads one setting from a parsed input_settings.txt.
 * @param variable_name The key of the setting.
 * @param settings The parsed settings file.
 * @param output_file Stream where the setting is logged (input_check.txt).
 * @return The setting; 0 when the file leaves it out.
 */
unsigned short int
ReadOneSetting(const std::string& variable_name, const NamedInput& settings, std::ofstream& output_file)
{
    std::vector<double> values{0.0};
    lookUpEntry(variable_name, settings, 1, values);

    if (values[0] < 0.0 || values[0] != static_cast<unsigned short int>(values[0]))
        ErrorMessages::Fatal("InputReading.C", "the setting \"" + variable_name + "\" must be a non-negative integer");

    const unsigned short int variable = static_cast<unsigned short int>(values[0]);
    output_file << variable_name << " = " << variable << std::endl;
    return variable;
}

/**
 * @brief Reads one parameter from a parsed input file.
 * @param variable_name The key of the parameter.
 * @param parsed The parsed input file.
 * @param output_file Stream where the parameter is logged (input_check.txt).
 * @param fallback The value used when the file leaves the entry out.
 * @return The parameter.
 */
double ReadOneParameter(const std::string& variable_name,
                        const NamedInput&  parsed,
                        std::ofstream&     output_file,
                        double             fallback)
{
    std::vector<double> values{fallback};
    lookUpEntry(variable_name, parsed, 1, values);
    output_file << variable_name << " = " << values[0] << std::endl;
    return values[0];
}

/**
 * @brief Reads an entry holding several values from a parsed input file.
 * @param variable_name The key of the entry.
 * @param parsed The parsed input file.
 * @param count The number of values the entry holds.
 * @param output_file Stream where the values are logged (input_check.txt).
 * @param fallback The value used for every component when the file leaves the entry out.
 * @return The `count` values of the entry.
 */
std::vector<double> ReadSeveralParameters(const std::string& variable_name,
                                          const NamedInput&  parsed,
                                          std::size_t        count,
                                          std::ofstream&     output_file,
                                          double             fallback)
{
    std::vector<double> values(count, fallback);
    lookUpEntry(variable_name, parsed, count, values);
    for (std::size_t k = 0; k < count; ++k)
        output_file << variable_name << k << " = " << values[k] << std::endl;
    return values;
}

/**
 * @brief Stops the run on entries of a parsed file that no reader asked for.
 * @param file_name The file, for the message.
 * @param parsed The parsed file.
 */
void ReportUnrecognisedEntries(const std::string& file_name, const NamedInput& parsed)
{
    for (const auto& entry : parsed.value)
        if (!parsed.consumed.count(entry.first))
            ErrorMessages::Fatal(file_name.c_str(),
                                 "the entry \"" + entry.first +
                                     "\" is not one this file accepts; see utilities/InputExplanation.md");
}

/// Keys of input_scaling_factors.txt, by index in Sciantix_scaling_factors[].
static const std::vector<std::string> scaling_factor_keys = {
    "sf_resolution_rate",
    "sf_trapping_rate",
    "sf_nucleation_rate",
    "sf_diffusivity",
    "sf_temperature",
    "sf_fission_rate",
    "sf_diffusion_based_release",
    "sf_helium_production_rate",
    "sf_grain_boundary_energy",
    "sf_fabricated_porosity",
    "sf_cs_production",
};

void ReadScalingFactors(const std::string& path, double Sciantix_scaling_factors[], std::ofstream& input_check)
{
    // The file is optional; a missing file behaves as an empty one, with every factor at 1.0
    const NamedInput parsed = ParseNamedEntries(path);
    for (std::size_t i = 0; i < scaling_factor_keys.size(); ++i)
        Sciantix_scaling_factors[i] = ReadOneParameter(scaling_factor_keys[i], parsed, input_check, 1.0);

    ReportUnrecognisedEntries("input_scaling_factors.txt", parsed);
}

void InputReading(int    Sciantix_options[],
                  double Sciantix_variables[],
                  double Sciantix_scaling_factors[],
                  double /* Sciantix_thermochemistry */[],
                  ThermochemistrySettings*& Sciantix_thermochemistry_settings,
                  int&                      Input_history_points,
                  std::vector<double>&      Time_input,
                  std::vector<double>&      Temperature_input,
                  std::vector<double>&      Fissionrate_input,
                  std::vector<double>&      Hydrostaticstress_input,
                  std::vector<double>&      Steampressure_input,
                  std::vector<double>&      Systempressure_input,
                  std::vector<double>&      OMratio_input,
                  double&                   Time_end_h,
                  double&                   Time_end_s)
{
    /**
     * Besides reading the input files, this routine writes input_check.txt, which reports every
     * input value as it was read (defaults included). Malformed or unknown entries stop the run
     * with a message instead.
     */

    std::ofstream input_check(TestPath + "input_check.txt", std::ios::out);

    // Abort execution if any of the input files does not exist
    if (!std::ifstream(TestPath + "input_settings.txt", std::ios::in))
        ErrorMessages::MissingInputFile("input_settings.txt");

    if (!std::ifstream(TestPath + "input_initial_conditions.txt", std::ios::in))
        ErrorMessages::MissingInputFile("input_initial_conditions.txt");

    std::ifstream input_history(TestPath + "input_history.txt", std::ios::in);
    if (!input_history)
        ErrorMessages::MissingInputFile("input_history.txt");

    // Keyed entries, see ParseNamedEntries; input_scaling_factors.txt is read below by ReadScalingFactors
    const NamedInput settings           = ParseNamedEntries(TestPath + "input_settings.txt");
    const NamedInput initial_conditions = ParseNamedEntries(TestPath + "input_initial_conditions.txt");

    Sciantix_options[0]  = ReadOneSetting("iGrainGrowth", settings, input_check);
    Sciantix_options[1]  = ReadOneSetting("iFissionProductDiffusivity", settings, input_check);
    Sciantix_options[2]  = ReadOneSetting("iDiffusionSolver", settings, input_check);
    Sciantix_options[3]  = ReadOneSetting("iIntraGranularBubbleBehavior", settings, input_check);
    Sciantix_options[4]  = ReadOneSetting("iResolutionRate", settings, input_check);
    Sciantix_options[5]  = ReadOneSetting("iTrappingRate", settings, input_check);
    Sciantix_options[6]  = ReadOneSetting("iNucleationRate", settings, input_check);
    Sciantix_options[7]  = ReadOneSetting("iOutput", settings, input_check);
    Sciantix_options[8]  = ReadOneSetting("iGrainBoundaryVacancyDiffusivity", settings, input_check);
    Sciantix_options[9]  = ReadOneSetting("iGrainBoundaryBehaviour", settings, input_check);
    Sciantix_options[10] = ReadOneSetting("iGrainBoundaryMicroCracking", settings, input_check);
    Sciantix_options[11] = ReadOneSetting("iFuelMatrix", settings, input_check);
    Sciantix_options[12] = ReadOneSetting("iGrainBoundaryVenting", settings, input_check);
    Sciantix_options[13] = ReadOneSetting("iRadioactiveFissionGas", settings, input_check);
    Sciantix_options[14] = ReadOneSetting("iHelium", settings, input_check);
    Sciantix_options[15] = ReadOneSetting("iHeDiffusivity", settings, input_check);
    Sciantix_options[16] = ReadOneSetting("iGrainBoundarySweeping", settings, input_check);
    Sciantix_options[17] = ReadOneSetting("iHighBurnupStructureFormation", settings, input_check);
    Sciantix_options[18] = ReadOneSetting("iHighBurnupStructurePorosity", settings, input_check);
    Sciantix_options[19] = ReadOneSetting("iHeliumProductionRate", settings, input_check);
    Sciantix_options[20] = ReadOneSetting("iStoichiometryDeviation", settings, input_check);
    Sciantix_options[21] = ReadOneSetting("iBubbleDiffusivity", settings, input_check);
    Sciantix_options[22] = ReadOneSetting("iChromiumSolubility", settings, input_check);
    Sciantix_options[23] = ReadOneSetting("iDensification", settings, input_check);
    Sciantix_options[24] = ReadOneSetting("iReleaseMode", settings, input_check);
    Sciantix_options[25] = ReadOneSetting("iThermochimica", settings, input_check);

    {
        const NamedInput& ic = initial_conditions;

        Sciantix_variables[0] = ReadOneParameter("Grain_radius[0]", ic, input_check, 0.0);

        const std::vector<double> xe = ReadSeveralParameters("Initial_composition_Xe", ic, 6, input_check, 0.0);
        std::copy(xe.begin(), xe.end(), Sciantix_variables + 1);  // [1..6]

        const std::vector<double> kr = ReadSeveralParameters("Initial_composition_Kr", ic, 6, input_check, 0.0);
        std::copy(kr.begin(), kr.end(), Sciantix_variables + 7);  // [7..12]

        const std::vector<double> he = ReadSeveralParameters("Initial_composition_He", ic, 6, input_check, 0.0);
        std::copy(he.begin(), he.end(), Sciantix_variables + 13);  // [13..18]

        const std::vector<double> bubbles =
            ReadSeveralParameters("Initial_intragranular_bubbles", ic, 2, input_check, 0.0);
        std::copy(bubbles.begin(), bubbles.end(), Sciantix_variables + 19);  // [19..20]

        Sciantix_variables[38] = ReadOneParameter("Burn_up[0]", ic, input_check, 0.0);
        Sciantix_variables[39] = ReadOneParameter("Effective_burn_up[0]", ic, input_check, 0.0);
        Sciantix_variables[65] = ReadOneParameter("Irradiation_time[0]", ic, input_check, 0.0);
        Sciantix_variables[40] = ReadOneParameter("Fuel_density[0]", ic, input_check, 0.0);

        const std::vector<double> u = ReadSeveralParameters("Initial_composition_U", ic, 5, input_check, 0.0);
        std::copy(u.begin(), u.end(), Sciantix_variables + 41);  // U-234 .. U-238, [41..45]

        const std::vector<double> xe133 = ReadSeveralParameters("Initial_composition_Xe133", ic, 7, input_check, 0.0);
        std::copy(xe133.begin(), xe133.end(), Sciantix_variables + 48);  // [48..54]

        const std::vector<double> kr85m = ReadSeveralParameters("Initial_composition_Kr85m", ic, 7, input_check, 0.0);
        std::copy(kr85m.begin(), kr85m.end(), Sciantix_variables + 57);  // [57..63]

        Sciantix_variables[66]  = ReadOneParameter("Initial_stoichiometry_deviation[0]", ic, input_check, 0.0);
        Sciantix_variables[150] = ReadOneParameter("Chromium_content", ic, input_check, 0.0);

        // MOX
        if (Sciantix_options[11] == 2)
        {
            const std::vector<double> pu = ReadSeveralParameters("Initial_composition_Pu", ic, 5, input_check, 0.0);
            std::copy(pu.begin(), pu.end(), Sciantix_variables + 171);  // [171..175]

            Sciantix_variables[177] = ReadOneParameter("q", ic, input_check, 0.0);
        }
        else
        {
            // Recognised keys, unused outside MOX: do not report them as unknown
            ic.consumed.insert("Initial_composition_Pu");
            ic.consumed.insert("q");
        }
    }

    // Optional, 0.0 means "not provided", and Initialization.C and SetMatrix.C then use the built-in values.
    Sciantix_variables[25] =
        ReadOneParameter("Intergranular_bubble_concentration[0]", initial_conditions, input_check, 0.0);
    Sciantix_variables[203] = ReadOneParameter("Surface_tension", initial_conditions, input_check, 0.0);

    ReportUnrecognisedEntries("input_initial_conditions.txt", initial_conditions);

    // Optional, per-case time discretisation: the number of steps the solver takes between
    // two consecutive rows of input_history.txt.
    Number_of_time_steps_per_interval =
        ReadOneParameter("Number_of_time_steps_per_interval", settings, input_check, Number_of_time_steps_per_interval);
    if (Number_of_time_steps_per_interval <= 0.0)
        ErrorMessages::Fatal("InputReading.C",
                             "\"Number_of_time_steps_per_interval\" must be a positive number of steps");

    ReportUnrecognisedEntries("input_settings.txt", settings);

    const bool needs_steam_pressure  = Sciantix_options[20] > 0 && Sciantix_options[20] < 7;
    const bool needs_system_pressure = Sciantix_options[25] != 0;
    const bool needs_OM_ratio        = Sciantix_options[20] == 9;

    Time_input.clear();
    Temperature_input.clear();
    Fissionrate_input.clear();
    Hydrostaticstress_input.clear();
    Steampressure_input.clear();
    Systempressure_input.clear();
    OMratio_input.clear();

    double time_in(0.0), temperature_in(0.0), fissionrate_in(0.0), hydrostaticstress_in(0.0), steampressure_in(0.0),
        systempressure_in(0.0), omratio_in(0.0);

    while (input_history >> time_in >> temperature_in >> fissionrate_in >> hydrostaticstress_in)
    {
        if (needs_steam_pressure)
        {
            if (!(input_history >> steampressure_in))
            {
                std::cerr
                    << "ERROR: Missing steam pressure in input_history.txt while iStoichiometryDeviation requires it."
                    << std::endl;
                exit(1);
            }
        }
        else
            steampressure_in = 0.0;

        if (needs_system_pressure)
        {
            if (!(input_history >> systempressure_in))
            {
                std::cerr << "ERROR: Missing systempressure in input_history.txt while iThermochimica is enabled."
                          << std::endl;
                exit(1);
            }
        }
        else
            systempressure_in = 0.0;

        if (needs_OM_ratio)
        {
            if (!(input_history >> omratio_in))
            {
                std::cerr << "ERROR: Missing O/M ratio in input_history.txt while iStoichiometryDeviation = 9."
                          << std::endl;
                exit(1);
            }
        }
        else
            omratio_in = 2.0 + Sciantix_variables[66];

        Time_input.push_back(time_in);
        Temperature_input.push_back(temperature_in);
        Fissionrate_input.push_back(fissionrate_in);
        Hydrostaticstress_input.push_back(hydrostaticstress_in);
        if (needs_steam_pressure)
            Steampressure_input.push_back(steampressure_in);
        if (needs_system_pressure)
            Systempressure_input.push_back(systempressure_in);
        if (needs_OM_ratio)
            OMratio_input.push_back(omratio_in);

        input_check << time_in << "\t";
        input_check << temperature_in << "\t";
        input_check << fissionrate_in << "\t";
        input_check << hydrostaticstress_in << "\t";

        if (needs_steam_pressure)
            input_check << steampressure_in << "\t";

        if (needs_system_pressure)
            input_check << systempressure_in << "\t";

        if (needs_OM_ratio)
            input_check << omratio_in << "\t";

        input_check << std::endl;

        Input_history_points = static_cast<int>(Time_input.size());
    }

    if (Input_history_points == 0)
        ErrorMessages::Fatal("InputReading.C", "input_history.txt is empty or its first row is malformed");

    // Without the steam-pressure column the history is interpolated as identically zero
    if (needs_steam_pressure == false)
        Steampressure_input.assign(Input_history_points, 0.0);

    // Without the system-pressure column the history is interpolated as identically zero
    if (needs_system_pressure == false)
        Systempressure_input.assign(Input_history_points, 0.0);

    // Without the oxygen column the history is interpolated as identically 2.0 + stoichiometry deviation
    if (needs_OM_ratio == false)
        OMratio_input.assign(Input_history_points, 2.0 + Sciantix_variables[66]);

    Time_end_h = Time_input[Input_history_points - 1];
    Time_end_s = Time_end_h * 3600.0;

    ReadScalingFactors(TestPath + "input_scaling_factors.txt", Sciantix_scaling_factors, input_check);

    input_check.close();
    input_history.close();

    delete Sciantix_thermochemistry_settings;
    Sciantix_thermochemistry_settings = nullptr;
    if (Sciantix_options[25] > 0)
    {
        Sciantix_thermochemistry_settings =
            new ThermochemistrySettings(LoadThermochemistrySettings(TestPath + "input_thermochemistry_settings.txt"));
    }
}
