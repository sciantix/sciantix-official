#ifndef THERMOCHEMISTRY_SETTINGS_H
#define THERMOCHEMISTRY_SETTINGS_H

#include <string>
#include <vector>

struct ThermochemistryPhaseSettings
{
    std::string              module;
    std::string              database;
    std::vector<std::string> elements;
    std::vector<std::string> locations;
    bool                     gap_settings    = false;
    double                   gap_temperature = 0.0;
    double                   gap_pressure    = 0.0;
};

struct ThermochemistrySettings
{
    bool                         kc                   = false;
    double                       kc_time              = 0.0;
    bool                         langmuir             = false;
    double                       langmuir_coefficient = 0.0;
    ThermochemistryPhaseSettings fission_products;
    ThermochemistryPhaseSettings matrix;
};

ThermochemistrySettings loadThermochemistrySettings(const std::string& path);

#endif
