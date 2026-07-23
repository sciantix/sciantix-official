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

#include "ThermochemistryManifest.h"
#include "ThermochemistryParsingUtils.h"

#include <algorithm>
#include <cctype>
#include <fstream>
#include <iostream>
#include <sstream>

using namespace ThermochemistryParsingUtils;

std::string ThermochemistryManifestEntry::getLabel() const
{
    return compound + " (" + phase + ", " + location + ")";
}

std::vector<ThermochemistryManifestEntry> LoadThermochemistryManifest(const std::string& path)
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
        if (fields.size() != 7 && fields.size() != 8)
        {
            std::cerr << "Error: Invalid thermochemistry manifest line " << line_number << ": " << line << std::endl;
            exit(1);
        }

        ThermochemistryManifestEntry entry;
        entry.index    = std::stoi(fields[0]);
        entry.category = fields[1];
        entry.phase    = fields[2];
        entry.compound = fields[3];
        entry.location = fields[4];
        entry.uom      = fields[5];
        entry.output   = std::stoi(fields[6]) != 0;
        // Optional 8th column: theoretical density (g/cm3). Defaults to 0.0
        // (unknown) for manifest lines/files that do not provide it.
        entry.density = fields.size() == 8 && !fields[7].empty() ? std::stod(fields[7]) : 0.0;

        manifest.push_back(entry);
    }

    std::sort(manifest.begin(),
              manifest.end(),
              [](const ThermochemistryManifestEntry& lhs, const ThermochemistryManifestEntry& rhs)
              { return lhs.index < rhs.index; });

    for (size_t i = 0; i < manifest.size(); ++i)
    {
        if (manifest[i].index != static_cast<int>(i))
        {
            std::cerr << "Error: Thermochemistry manifest indices must be contiguous starting from zero." << std::endl;
            exit(1);
        }
    }

    if (manifest.size() > static_cast<size_t>(thermochemistry_density_offset))
    {
        std::cerr << "Error: Thermochemistry manifest has " << manifest.size()
                  << " entries, exceeding thermochemistry_density_offset (" << thermochemistry_density_offset
                  << "). A variable's own value slot would collide with another variable's "
                     "density slot in Sciantix_thermochemistry."
                  << std::endl;
        exit(1);
    }

    return manifest;
}