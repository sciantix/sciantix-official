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
#include <string>
#include <vector>

struct ThermochemistryManifestEntry
{
    int                        index;
    std::string                category;
    std::string                phase;
    std::string                compound;
    std::string                location;
    std::string                uom;
    bool                       output;
    double                     density = 0.0;  // theoretical density, g/cm3; 0.0 if not provided
    std::map<std::string, int> stoichiometry;

    std::string getLabel() const;
};

std::vector<ThermochemistryManifestEntry> loadThermochemistryManifest(const std::string& path);

#endif
