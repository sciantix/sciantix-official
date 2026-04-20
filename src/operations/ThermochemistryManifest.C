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

#include "ThermochemistryManifest.h"

#include <algorithm>
#include <cctype>
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
        parts.push_back(trim(item));

    return parts;
}

std::map<std::string, int> parseStoichiometry(const std::string& input)
{
    std::map<std::string, int> stoichiometry;

    if (trim(input).empty())
        return stoichiometry;

    for (const auto& token : split(input, ','))
    {
        const size_t separator = token.find(':');
        if (separator == std::string::npos)
        {
            std::cerr << "Error: Invalid thermochemistry stoichiometry token: " << token << std::endl;
            exit(1);
        }

        const std::string element = trim(token.substr(0, separator));
        const std::string value   = trim(token.substr(separator + 1));
        stoichiometry[element]    = std::stoi(value);
    }

    return stoichiometry;
}
}  // namespace

std::string ThermochemistryManifestEntry::getLabel() const
{
    return compound + " (" + phase + ", " + location + ")";
}

std::vector<ThermochemistryManifestEntry> loadThermochemistryManifest(const std::string& path)
{
    std::ifstream manifest_file(path);
    if (!manifest_file)
    {
        std::cerr << "Error: Cannot open thermochemistry manifest: " << path << std::endl;
        exit(1);
    }

    std::vector<ThermochemistryManifestEntry> manifest;
    std::string                               line;
    int                                       line_number = 0;

    while (std::getline(manifest_file, line))
    {
        ++line_number;
        line = trim(line);

        if (line.empty() || line[0] == '#')
            continue;

        const std::vector<std::string> fields = split(line, '|');
        if (fields.size() != 7)
        {
            std::cerr << "Error: Invalid thermochemistry manifest line " << line_number << ": " << line << std::endl;
            exit(1);
        }

        ThermochemistryManifestEntry entry;
        entry.index         = std::stoi(fields[0]);
        entry.category      = fields[1];
        entry.phase         = fields[2];
        entry.compound      = fields[3];
        entry.location      = fields[4];
        entry.uom           = fields[5];
        entry.output        = std::stoi(fields[6]) != 0;

        manifest.push_back(entry);
    }

    std::sort(manifest.begin(),
              manifest.end(),
              [](const ThermochemistryManifestEntry& lhs, const ThermochemistryManifestEntry& rhs) {
                  return lhs.index < rhs.index;
              });

    for (size_t i = 0; i < manifest.size(); ++i)
    {
        if (manifest[i].index != static_cast<int>(i))
        {
            std::cerr << "Error: Thermochemistry manifest indices must be contiguous starting from zero." << std::endl;
            exit(1);
        }
    }

    return manifest;
}