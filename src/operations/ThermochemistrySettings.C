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

#include "ThermochemistrySettings.h"

#include <fstream>
#include <iostream>
#include <sstream>

namespace
{
std::string trim(const std::string& input)
{
    const std::string whitespace = " \t\r\n";
    const size_t      begin      = input.find_first_not_of(whitespace);
    if (begin == std::string::npos)
        return "";

    const size_t end = input.find_last_not_of(whitespace);
    return input.substr(begin, end - begin + 1);
}

std::vector<std::string> split(const std::string& input, const char delimiter)
{
    std::vector<std::string> parts;
    std::stringstream        stream(input);
    std::string              item;

    while (std::getline(stream, item, delimiter))
    {
        item = trim(item);
        if (!item.empty())
            parts.push_back(item);
    }

    return parts;
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
}  // namespace

ThermochemistrySettings loadThermochemistrySettings(const std::string& path)
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
        else if (key == "fission_products.module")
            settings.fission_products.module = value;
        else if (key == "fission_products.database")
            settings.fission_products.database = value;
        else if (key == "fission_products.elements")
            settings.fission_products.elements = split(value, ',');
        else if (key == "fission_products.locations")
            settings.fission_products.locations = split(value, ',');
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
            settings.matrix.elements = split(value, ',');
        else if (key == "matrix.locations")
            settings.matrix.locations = split(value, ',');
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
