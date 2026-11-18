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

#ifndef THERMOCHEMISTRY_MANIFEST_H
#define THERMOCHEMISTRY_MANIFEST_H

#include <map>
#include <string>
#include <vector>

// Theoretical densities are packed at the same manifest index, offset by this value, in the
// shared Sciantix_thermochemistry array (read by TU as sciantix_thermochemistry(itb + 123), see
// SetTUVariablesfromSciantix.f95). Manifest indices are contiguous from 0 (enforced in
// LoadThermochemistryManifest), so the manifest must never grow to more entries than this offset,
// or a variable's own value slot would collide with another variable's density slot.
constexpr int thermochemistry_density_offset = 123;

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

std::vector<ThermochemistryManifestEntry> LoadThermochemistryManifest(const std::string& path);

#endif
