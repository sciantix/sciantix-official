#ifndef OC_OUTPUT_PARSER_H
#define OC_OUTPUT_PARSER_H

#include <map>
#include <string>
#include <vector>

struct OCSpeciesData
{
    double                   moles  = 0.0;
    double                   volume = 0.0;
    std::map<std::string, double> elements;
};

struct OCPhaseData
{
    double                   moles  = 0.0;
    double                   volume = 0.0;
    std::map<std::string, OCSpeciesData> species;
    std::map<std::string, double>        elements;
};

struct OCComponentData
{
    double      moles                        = 0.0;
    double      mole_fraction                = 0.0;
    double      chemical_potential_over_rt   = 0.0;
    double      activity                     = 0.0;
    std::string reference_state;
};

struct OCOutputData
{
    std::map<std::string, OCPhaseData>     solution_phases;
    std::map<std::string, OCComponentData> components;
};

OCOutputData parseOCOutputFile(const std::string& filepath, const std::vector<std::string>& valid_elements);

#endif
