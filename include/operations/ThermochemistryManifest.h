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

#ifndef THERMOCHEMISTRY_MANIFEST_H
#define THERMOCHEMISTRY_MANIFEST_H

#include <map>
#include <set>
#include <string>
#include <vector>

#include "ThermochemistrySettings.h"

struct ThermochemistryManifestEntry
{
    int                        index;
    std::string                category;
    std::string                phase;
    std::string                compound;
    std::string                location;
    std::string                uom;
    bool                       output;
    std::map<std::string, int> stoichiometry;

    std::string getLabel() const;
};

std::vector<ThermochemistryManifestEntry> loadThermochemistryManifest(const std::string& path);

std::vector<ThermochemistryManifestEntry> filterThermochemistryManifest(
    const std::vector<ThermochemistryManifestEntry>& manifest,
    const ThermochemistrySettings&                   settings
);

std::set<std::string> getThermochemistryElements(const std::vector<ThermochemistryManifestEntry>& manifest,
                                                 const std::string&                              category,
                                                 const std::string&                              location);

#endif
