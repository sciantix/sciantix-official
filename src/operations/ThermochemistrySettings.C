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

#include <unistd.h>
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

    std::string parentPath(const std::string& path)
    {
        if (path.empty() || path == "/")
            return "";

        const size_t end = path.find_last_not_of('/');
        if (end == std::string::npos)
            return "";

        const size_t separator = path.find_last_of('/', end);
        if (separator == std::string::npos)
            return "";
        if (separator == 0)
            return "/";

        return path.substr(0, separator);
    }

    std::string baseName(const std::string& path)
    {
        const size_t end = path.find_last_not_of('/');
        if (end == std::string::npos)
            return "";

        const size_t separator = path.find_last_of('/', end);
        if (separator == std::string::npos)
            return path.substr(0, end + 1);

        return path.substr(separator + 1, end - separator);
    }

    std::string directoryName(const std::string& path)
    {
        const size_t separator = path.find_last_of('/');
        if (separator == std::string::npos)
            return ".";
        if (separator == 0)
            return "/";

        return path.substr(0, separator);
    }

    std::string withTrailingSlash(const std::string& path)
    {
        if (path.empty() || path.back() == '/')
            return path;

        return path + "/";
    }

    std::string currentWorkingDirectory()
    {
        char buffer[4096];
        if (getcwd(buffer, sizeof(buffer)) == nullptr)
        {
            std::cerr << "Error: Cannot determine current working directory while locating OpenCalphad." << std::endl;
            exit(1);
        }

        return std::string(buffer);
    }

    std::string absoluteDirectoryFromSettingsPath(const std::string& settings_path)
    {
        std::string settings_directory = directoryName(settings_path);
        if (!settings_directory.empty() && settings_directory.front() == '/')
            return settings_directory;

        if (settings_directory == ".")
            return currentWorkingDirectory();

        return withTrailingSlash(currentWorkingDirectory()) + settings_directory;
    }

    std::string defaultOpenCalphadPath(const std::string& settings_path)
    {
        std::string current_path = absoluteDirectoryFromSettingsPath(settings_path);

        while (!current_path.empty())
        {
            if (baseName(current_path) == "regression")
                return withTrailingSlash(parentPath(current_path)) + "OC/";

            const std::string parent = parentPath(current_path);
            if (parent == current_path)
                break;

            current_path = parent;
        }

        std::cerr << "Error: Cannot infer the default OpenCalphad path. Expected the settings file "
                  << "to be under the SCIANTIX regression directory: " << settings_path << std::endl;
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

    if (settings.opencalphad_path.empty())
        settings.opencalphad_path = defaultOpenCalphadPath(path);
    else
        settings.opencalphad_path = withTrailingSlash(settings.opencalphad_path);

    return settings;
}
