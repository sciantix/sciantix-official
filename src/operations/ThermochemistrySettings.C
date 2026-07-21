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

#include "ThermochemistrySettings.h"
#include "ThermochemistryParsingUtils.h"

#include <fstream>
#include <iostream>
#include <sstream>

using namespace ThermochemistryParsingUtils;

namespace ThermochemistrySettingsDetail
{
// Comma-separated list fields (elements, locations) drop empty tokens, unlike the
// pipe-delimited manifest fields, which keep them to preserve column positions.
std::vector<std::string> splitList(const std::string& input, const char delimiter)
{
    return split(input, delimiter, /* skip_empty = */ true);
}

bool parseBool(const std::string& input)
{
    const std::string value = trim(input);

    if (value == "true" || value == "1" || value == "TRUE" || value == "True")
        return true;

    if (value == "false" || value == "0" || value == "FALSE" || value == "False")
        return false;

    std::cerr << "Error: Invalid thermochemistry settings boolean value: " << input << std::endl;
    exit(1);
}
}  // namespace ThermochemistrySettingsDetail

using namespace ThermochemistrySettingsDetail;

ThermochemistrySettings LoadThermochemistrySettings(const std::string& path)
{
    std::ifstream input(path);
    if (!input)
    {
        std::cerr << "Error: Cannot open thermochemistry settings file: " << path << std::endl;
        exit(1);
    }

    ThermochemistrySettings settings;
    std::string             line;
    int                     line_number = 0;

    while (std::getline(input, line))
    {
        ++line_number;

        const size_t comment_pos = line.find('#');
        if (comment_pos != std::string::npos)
            line = line.substr(0, comment_pos);

        line = trim(line);
        if (line.empty())
            continue;

        const size_t separator = line.find('=');
        if (separator == std::string::npos)
        {
            std::cerr << "Error: Invalid thermochemistry settings line " << line_number << ": " << line << std::endl;
            exit(1);
        }

        const std::string key   = trim(line.substr(0, separator));
        const std::string value = trim(line.substr(separator + 1));

        if (key == "opencalphad.path")
            settings.opencalphad_path = value;
        else if (key == "kc")
            settings.kc = parseBool(value);
        else if (key == "kc_time")
            settings.kc_time = std::stod(value);
        else if (key == "langmuir")
            settings.langmuir = parseBool(value);
        else if (key == "langmuir_coefficient")
            settings.langmuir_coefficient = std::stod(value);
        else if (key == "output.phase_sublattice_composition")
            settings.output_phase_sublattice_composition = parseBool(value);
        else if (key == "coupling.temperature_tolerance")
            settings.coupling_temperature_tolerance = std::stod(value);
        else if (key == "coupling.composition_tolerance")
            settings.coupling_composition_tolerance = std::stod(value);
        else if (key == "coupling.max_stale_steps")
            settings.coupling_max_stale_steps = std::stoi(value);
        else if (key == "fission_products.module")
            settings.fission_products.module = value;
        else if (key == "fission_products.database")
            settings.fission_products.database = value;
        else if (key == "fission_products.elements")
            settings.fission_products.elements = splitList(value, ',');
        else if (key == "fission_products.locations")
            settings.fission_products.locations = splitList(value, ',');
        else if (key == "fission_products.gap_settings")
            settings.fission_products.gap_settings = parseBool(value);
        else if (key == "fission_products.gap_temperature")
            settings.fission_products.gap_temperature = std::stod(value);
        else if (key == "fission_products.gap_pressure")
            settings.fission_products.gap_pressure = std::stod(value);
        else if (key == "matrix.module")
            settings.matrix.module = value;
        else if (key == "matrix.database")
            settings.matrix.database = value;
        else if (key == "matrix.elements")
            settings.matrix.elements = splitList(value, ',');
        else if (key == "matrix.locations")
            settings.matrix.locations = splitList(value, ',');
        else if (key == "matrix.gap_settings")
            settings.matrix.gap_settings = parseBool(value);
        else if (key == "matrix.gap_temperature")
            settings.matrix.gap_temperature = std::stod(value);
        else if (key == "matrix.gap_pressure")
            settings.matrix.gap_pressure = std::stod(value);
        else
        {
            std::cerr << "Error: Unknown thermochemistry settings key: " << key << std::endl;
            exit(1);
        }
    }

    return settings;
}
