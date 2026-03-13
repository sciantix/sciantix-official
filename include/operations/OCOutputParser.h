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

struct OCOutputData
{
    std::map<std::string, OCPhaseData> solution_phases;
};

OCOutputData parseOCOutputFile(const std::string& filepath, const std::vector<std::string>& valid_elements);

#endif
