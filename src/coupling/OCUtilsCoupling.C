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

#include "OCUtilsCoupling.h"
#include "OCASIAdapter.h"
#include "MainVariables.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <exception>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <regex>
#include <set>
#include <sstream>
#include <vector>
#include <sys/wait.h>

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

std::vector<std::string> split(const std::string& input)
{
    std::vector<std::string> parts;
    std::stringstream        stream(input);
    std::string              token;

    while (stream >> token)
        parts.push_back(token);

    return parts;
}

double safeFloat(const std::string& value)
{
    try
    {
        return std::stod(value);
    }
    catch (const std::exception&)
    {
        if (value.find('E') == std::string::npos && value.find('e') == std::string::npos)
        {
            const size_t exp_pos = value.find('-', 1);
            if (exp_pos != std::string::npos)
                return std::stod(value.substr(0, exp_pos) + "E-" + value.substr(exp_pos + 1));
        }
        throw;
    }
}

bool isNumericToken(const std::string& value)
{
    static const std::regex numeric_pattern(R"(^[+-]?(?:\d+\.?\d*|\.\d+)(?:[Ee][+-]?\d+)?$)");
    return std::regex_match(value, numeric_pattern);
}

struct ParsedSublatticeHeader
{
    int    index = 0;
    int    constituents_count = 0;
    double sites = 0.0;
};

bool tryParseSublatticeHeader(const std::string& line, ParsedSublatticeHeader& header)
{
    static const std::regex sublattice_pattern(
        R"(Sublattice\s+(\d+)\s+with\s+(\d+)\s+constituents\s+and\s+([+-]?(?:\d+\.?\d*|\.\d+)(?:[Ee][+-]?\d+)?)\s+sites)");

    std::smatch match;
    if (!std::regex_search(line, match, sublattice_pattern))
        return false;

    header.index = std::stoi(match[1].str());
    header.constituents_count = std::stoi(match[2].str());
    header.sites = safeFloat(match[3].str());
    return true;
}

std::string normalizeElementCase(const std::string& token, const std::vector<std::string>& valid_elements)
{
    std::map<std::string, std::string> valid_set;
    for (const auto& element : valid_elements)
    {
        std::string upper = element;
        std::transform(upper.begin(), upper.end(), upper.begin(), [](unsigned char c) { return std::toupper(c); });
        valid_set[upper] = element;
    }

    std::string result;
    size_t      i = 0;

    while (i < token.size())
    {
        if (i + 2 <= token.size())
        {
            std::string candidate = token.substr(i, 2);
            std::transform(candidate.begin(), candidate.end(), candidate.begin(), [](unsigned char c) { return std::toupper(c); });

            const auto it = valid_set.find(candidate);
            if (it != valid_set.end())
            {
                result += it->second;
                i += 2;
                continue;
            }
        }

        std::string candidate(1, token[i]);
        std::transform(candidate.begin(), candidate.end(), candidate.begin(), [](unsigned char c) { return std::toupper(c); });
        const auto it = valid_set.find(candidate);
        if (it != valid_set.end())
        {
            result += it->second;
            ++i;
            continue;
        }

        result += token[i];
        ++i;
    }

    return result;
}

std::string normalizePhaseName(const std::string& raw_phase)
{
    std::string phase = raw_phase;

    while (!phase.empty() && phase.back() == '.')
        phase.pop_back();

    const size_t hash_pos = phase.find('#');
    if (hash_pos != std::string::npos)
        phase = phase.substr(0, hash_pos);

    const size_t auto_pos = phase.find("_AUTO");
    if (auto_pos != std::string::npos)
        phase = phase.substr(0, auto_pos);

    const size_t chkd_pos = phase.find("_CHKD");
    if (chkd_pos != std::string::npos)
        phase = phase.substr(0, chkd_pos);

    phase = trim(phase);

    static const std::set<std::string> passthrough_phases = {
        "GAS", "LIQUID", "LIQUID_IONIC", "IONIC_LIQUID", "PURE_CONDENSED", "SOLID"
    };

    if (passthrough_phases.count(phase) > 0)
    {
        std::string lower = phase;
        std::transform(lower.begin(), lower.end(), lower.begin(), [](unsigned char c) { return std::tolower(c); });
        return lower;
    }

    return "condensed";
}

std::string normalizeSpeciesName(const std::string& raw_name)
{
    std::string name = raw_name;

    while (!name.empty() && name.back() == '.')
        name.pop_back();

    const size_t hash_pos = name.find('#');
    if (hash_pos != std::string::npos)
        name = name.substr(0, hash_pos);

    const size_t auto_pos = name.find("_AUTO");
    if (auto_pos != std::string::npos)
        name = name.substr(0, auto_pos);

    const size_t chkd_pos = name.find("_CHKD");
    if (chkd_pos != std::string::npos)
        name = name.substr(0, chkd_pos);

    return trim(name);
}

std::string normalizePhaseInstanceName(const std::string& raw_name)
{
    std::string name = raw_name;

    while (!name.empty() && name.back() == '.')
        name.pop_back();

    const size_t chkd_pos = name.find("_CHKD");
    if (chkd_pos != std::string::npos)
        name.erase(chkd_pos, 5);

    const size_t auto_pos = name.find("_AUTO");
    if (auto_pos != std::string::npos)
        name.erase(auto_pos, 5);

    return trim(name);
}

std::map<std::string, double> speciesStoichiometry(const std::string& species_name,
                                                   const std::vector<std::string>& valid_elements);

double speciesStoichiometricSize(const std::string& species_name,
                                 const std::vector<std::string>& valid_elements)
{
    const std::map<std::string, double> stoichiometry =
        speciesStoichiometry(species_name, valid_elements);

    double total_size = 0.0;
    for (const auto& entry : stoichiometry)
    {
        if (entry.first != "Va")
            total_size += entry.second;
    }

    return !stoichiometry.empty() ? total_size : 1.0;
}

std::map<std::string, double> speciesStoichiometry(const std::string& species_name,
                                                   const std::vector<std::string>& valid_elements)
{
    std::map<std::string, std::string> valid_set;
    for (const auto& element : valid_elements)
    {
        std::string upper = element;
        std::transform(upper.begin(), upper.end(), upper.begin(), [](unsigned char c) { return std::toupper(c); });
        valid_set[upper] = element;
    }

    std::map<std::string, double> stoichiometry;
    size_t i = 0;
    while (i < species_name.size())
    {
        const unsigned char character = static_cast<unsigned char>(species_name[i]);
        if (species_name[i] == '+' || species_name[i] == '-')
        {
            ++i;
            while (i < species_name.size() && std::isdigit(static_cast<unsigned char>(species_name[i])))
                ++i;
            continue;
        }

        if (species_name[i] == ':' || species_name[i] == '_' || !std::isalpha(character))
        {
            ++i;
            continue;
        }

        std::string element;
        if (i + 2 <= species_name.size())
        {
            std::string candidate = species_name.substr(i, 2);
            std::transform(candidate.begin(), candidate.end(), candidate.begin(), [](unsigned char c) { return std::toupper(c); });
            if (candidate == "VA")
            {
                element = "Va";
                i += 2;
            }
            else
            {
                const auto it = valid_set.find(candidate);
                if (it != valid_set.end())
                {
                    element = it->second;
                    i += 2;
                }
            }
        }

        if (element.empty())
        {
            std::string candidate(1, species_name[i]);
            std::transform(candidate.begin(), candidate.end(), candidate.begin(), [](unsigned char c) { return std::toupper(c); });
            const auto it = valid_set.find(candidate);
            if (it != valid_set.end())
            {
                element = it->second;
                ++i;
            }
            else
            {
                ++i;
                continue;
            }
        }

        double count = 1.0;
        size_t count_begin = i;
        while (i < species_name.size() && std::isdigit(static_cast<unsigned char>(species_name[i])))
            ++i;
        if (i > count_begin)
            count = safeFloat(species_name.substr(count_begin, i - count_begin));

        stoichiometry[element] += count;
    }

    return stoichiometry;
}

void addFormulaElements(OCSpeciesData&                  species,
                        OCPhaseData&                    phase,
                        const std::string&              species_name,
                        const std::vector<std::string>& valid_elements,
                        const double                    species_formula_moles,
                        const bool                      add_to_phase)
{
    const std::map<std::string, double> stoichiometry =
        speciesStoichiometry(species_name, valid_elements);
    if (stoichiometry.empty())
        return;

    for (const auto& element_entry : stoichiometry)
    {
        const double element_moles = element_entry.second * species_formula_moles;
        species.elements[element_entry.first] += element_moles;
        if (add_to_phase)
            phase.elements[element_entry.first] += element_moles;
    }
}

std::map<std::string, double> sublatticeStoichiometry(
    const std::vector<OCSublatticeData>& sublattices,
    const std::vector<std::string>&      valid_elements)
{
    std::map<std::string, double> stoichiometry;

    for (const auto& sublattice : sublattices)
    {
        if (sublattice.sites <= 0.0)
            continue;

        for (const auto& constituent_entry : sublattice.composition)
        {
            const std::map<std::string, double> constituent_stoichiometry =
                speciesStoichiometry(constituent_entry.first, valid_elements);

            for (const auto& element_entry : constituent_stoichiometry)
            {
                stoichiometry[element_entry.first] +=
                    sublattice.sites * constituent_entry.second * element_entry.second;
            }
        }
    }

    return stoichiometry;
}

bool addSublatticeElements(OCSpeciesData&                  species,
                           OCPhaseData&                    phase,
                           const std::vector<std::string>& valid_elements,
                           const bool                      add_to_phase)
{
    const std::map<std::string, double> stoichiometry =
        sublatticeStoichiometry(species.sublattices, valid_elements);
    if (stoichiometry.empty())
        return false;

    double stoichiometric_size = 0.0;
    for (const auto& element_entry : stoichiometry)
    {
        const double element_moles = element_entry.second * species.moles;
        species.elements[element_entry.first] += element_moles;
        if (add_to_phase)
            phase.elements[element_entry.first] += element_moles;

        if (element_entry.first != "Va")
            stoichiometric_size += element_entry.second;
    }

    if (stoichiometric_size > 0.0)
    {
        species.stoichiometric_size = stoichiometric_size;
        species.atom_equivalent_moles = species.moles * stoichiometric_size;
    }

    return true;
}

std::string toLowerCopy(std::string text)
{
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) { return std::tolower(c); });
    return text;
}

std::string toUpperCopy(std::string text)
{
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) { return std::toupper(c); });
    return text;
}

bool isLiquidPhase(const std::string& phase_name)
{
    return phase_name == "liquid" || phase_name == "ionic_liquid" || phase_name == "liquid_ionic";
}

bool hasOpenCalphadSavedState(const std::string& state_file_path)
{
    return OCUtilsCoupling::fileExists(state_file_path) ||
           OCUtilsCoupling::fileExists(state_file_path + ".OCU") ||
           OCUtilsCoupling::fileExists(state_file_path + ".ocu");
}
}  // namespace

OCOutputData parseOCOutputFile(const std::string& filepath, const std::vector<std::string>& valid_elements)
{
    std::ifstream file(filepath);
    if (!file)
    {
        std::cerr << "Error: Cannot open OPENCALPHAD output file: " << filepath << std::endl;
        exit(1);
    }

    std::vector<std::string> lines;
    std::string              line;
    while (std::getline(file, line))
        lines.push_back(line);

    OCOutputData data;

    bool        in_components_section   = false;
    bool        in_phases_section       = false;
    bool        in_constitution_section = false;
    double      current_phase_moles      = 0.0;
    double      current_phase_form_units = 0.0;
    double      current_sublattice_sites = 0.0;
    int         current_sublattice_index = 0;
    std::string current_phase;
    std::string current_phase_instance;
    std::string current_condensed_species;

    for (size_t i = 0; i < lines.size(); ++i)
    {
        const std::string stripped = trim(lines[i]);

        if (stripped.find("Some data for components") != std::string::npos)
        {
            in_components_section = true;
            in_phases_section = false;
            continue;
        }

        if (in_components_section)
        {
            if (stripped.rfind("Component name", 0) == 0)
                continue;

            if (stripped.find("Some data for phases") != std::string::npos || stripped.empty())
            {
                in_components_section = false;
                in_phases_section = true;
                continue;
            }

            const std::vector<std::string> parts = split(stripped);
            if (parts.size() < 5)
                continue;

            OCComponentData* component = &data.components[normalizeElementCase(parts[0], valid_elements)];
            component->moles = safeFloat(parts[1]);
            component->mole_fraction = safeFloat(parts[2]);
            component->chemical_potential_over_rt = safeFloat(parts[3]);
            component->activity = safeFloat(parts[4]);

            continue;
        }

        if (!in_phases_section || stripped.rfind("Name", 0) == 0 || stripped.find("Moles") != std::string::npos)
            continue;

        if (stripped.rfind("Constitution:", 0) == 0)
        {
            in_constitution_section = true;
            current_sublattice_sites = 0.0;
            current_sublattice_index = 0;

            if (stripped.find("Sublattice") != std::string::npos)
            {
                ParsedSublatticeHeader header;
                if (tryParseSublatticeHeader(stripped, header))
                {
                    current_sublattice_sites = header.sites;
                    current_sublattice_index = header.index;

                    if (current_phase == "condensed" && !current_condensed_species.empty())
                    {
                        OCSublatticeData sublattice;
                        sublattice.index = header.index;
                        sublattice.constituents_count = header.constituents_count;
                        sublattice.sites = header.sites;
                        sublattice.phase_moles = current_phase_moles;
                        sublattice.phase_form_units = current_phase_form_units;
                        sublattice.phase_instance = current_phase_instance;
                        data.solution_phases[current_phase]
                            .species[current_condensed_species]
                            .sublattices.push_back(sublattice);
                    }
                    else if (!current_phase.empty())
                    {
                        OCSublatticeData sublattice;
                        sublattice.index = header.index;
                        sublattice.constituents_count = header.constituents_count;
                        sublattice.sites = header.sites;
                        sublattice.phase_moles = current_phase_moles;
                        sublattice.phase_form_units = current_phase_form_units;
                        sublattice.phase_instance = current_phase_instance;
                        data.solution_phases[current_phase].sublattices.push_back(sublattice);
                    }
                }
            }
            continue;
        }

        if (stripped.find(".. E") != std::string::npos && stripped.find("X:") != std::string::npos)
        {
            in_constitution_section = false;

            const std::vector<std::string> parts = split(stripped);
            if (parts.size() < 4 || !isNumericToken(parts[2]) || !isNumericToken(parts[3]))
                continue;

            const std::string raw_phase_name = normalizeSpeciesName(parts[0]);
            current_phase_instance = normalizePhaseInstanceName(parts[0]);
            const std::string species_name = normalizeSpeciesName(raw_phase_name);
            current_phase = normalizePhaseName(raw_phase_name);
            current_phase_moles = safeFloat(parts[2]);
            current_phase_form_units =
                (parts.size() > 4 && isNumericToken(parts[4])) ? safeFloat(parts[4]) : current_phase_moles;
            current_condensed_species = (current_phase == "condensed") ? species_name : "";

            if (current_phase != "condensed")
            {
                OCPhaseData& phase = data.solution_phases[current_phase];
                const double volume = safeFloat(parts[3]);
                phase.moles += current_phase_moles;
                phase.volume += volume;
                phase.form_units += current_phase_form_units;
            }
            else
            {
                OCPhaseData&      phase        = data.solution_phases[current_phase];
                OCSpeciesData&    species      = phase.species[species_name];
                const double      volume       = safeFloat(parts[3]);
                const double      stoichiometric_size =
                    (parts.size() > 5 && isNumericToken(parts[5])) ? safeFloat(parts[5]) : 1.0;
                
                species.moles += current_phase_moles;
                species.atom_equivalent_moles += current_phase_moles * stoichiometric_size;
                species.volume += volume;
                species.stoichiometric_size = (stoichiometric_size > 0.0) ? stoichiometric_size : 1.0;
                phase.moles += current_phase_moles;
                phase.volume += volume;
                phase.form_units += current_phase_form_units;
            }

            for (size_t j = i + 1; j < lines.size(); ++j)
            {
                const std::string next_line = trim(lines[j]);
                if (next_line.rfind("Constitution:", 0) == 0)
                    break;

                if (next_line.find("Sublattice") != std::string::npos || next_line.empty())
                    break;

                const std::vector<std::string> element_parts = split(next_line);
                for (size_t k = 0; k + 1 < element_parts.size(); k += 2)
                {
                    if (!isNumericToken(element_parts[k + 1]))
                        continue;

                    const std::string element = normalizeElementCase(element_parts[k], valid_elements);
                    const double      fraction = safeFloat(element_parts[k + 1]);

                    if (current_phase != "condensed")
                    {
                        data.solution_phases[current_phase].elements[element] +=
                            fraction * data.solution_phases[current_phase].moles;
                    }
                    else
                    {
                        const std::string species_name = normalizeSpeciesName(raw_phase_name);
                        OCPhaseData&      phase        = data.solution_phases[current_phase];
                        OCSpeciesData&    species      = phase.species[species_name];
                        const double      element_moles = fraction * species.moles * species.stoichiometric_size;

                        species.elements[element] = element_moles;
                        phase.elements[element] += element_moles;
                    }
                }
            }
        }
        else if (in_constitution_section)
        {
            const std::string next_line = stripped;

            if (next_line.empty() || next_line.rfind("Name", 0) == 0 || next_line.rfind("Some", 0) == 0 ||
                next_line.find("Output for equilibrium") != std::string::npos)
            {
                in_constitution_section = false;
                current_phase_moles = 0.0;
                current_phase_form_units = 0.0;
                current_sublattice_sites = 0.0;
                current_sublattice_index = 0;
                current_condensed_species.clear();
                current_phase_instance.clear();
                continue;
            }

            if (next_line.find("Sublattice") != std::string::npos)
            {
                ParsedSublatticeHeader header;
                if (tryParseSublatticeHeader(next_line, header))
                {
                    current_sublattice_sites = header.sites;
                    current_sublattice_index = header.index;

                    if (current_phase == "condensed" && !current_condensed_species.empty())
                    {
                        OCSublatticeData sublattice;
                        sublattice.index = header.index;
                        sublattice.constituents_count = header.constituents_count;
                        sublattice.sites = header.sites;
                        sublattice.phase_moles = current_phase_moles;
                        sublattice.phase_form_units = current_phase_form_units;
                        sublattice.phase_instance = current_phase_instance;
                        data.solution_phases[current_phase]
                            .species[current_condensed_species]
                            .sublattices.push_back(sublattice);
                    }
                    else if (!current_phase.empty())
                    {
                        OCSublatticeData sublattice;
                        sublattice.index = header.index;
                        sublattice.constituents_count = header.constituents_count;
                        sublattice.sites = header.sites;
                        sublattice.phase_moles = current_phase_moles;
                        sublattice.phase_form_units = current_phase_form_units;
                        sublattice.phase_instance = current_phase_instance;
                        data.solution_phases[current_phase].sublattices.push_back(sublattice);
                    }
                }
                else
                {
                    current_sublattice_sites = 0.0;
                    current_sublattice_index = 0;
                }
                continue;
            }

            const std::vector<std::string> species_parts = split(next_line);
            for (size_t k = 0; k + 1 < species_parts.size(); k += 2)
            {
                if (!isNumericToken(species_parts[k + 1]))
                    continue;

                const std::string species_name = normalizeSpeciesName(species_parts[k]);
                const double      mole_fraction = safeFloat(species_parts[k + 1]);

                if (current_phase == "condensed")
                {
                    if (current_condensed_species.empty())
                        continue;

                    OCSpeciesData& condensed_species =
                        data.solution_phases[current_phase].species[current_condensed_species];
                    if (condensed_species.sublattices.empty() ||
                        condensed_species.sublattices.back().index != current_sublattice_index)
                    {
                        OCSublatticeData sublattice;
                        sublattice.index = current_sublattice_index;
                        sublattice.sites = current_sublattice_sites;
                        sublattice.phase_moles = current_phase_moles;
                        sublattice.phase_form_units = current_phase_form_units;
                        sublattice.phase_instance = current_phase_instance;
                        condensed_species.sublattices.push_back(sublattice);
                    }

                    condensed_species.sublattices.back().composition[species_name] += mole_fraction;
                    continue;
                }

                OCPhaseData&      phase         = data.solution_phases[current_phase];
                if (phase.sublattices.empty() || phase.sublattices.back().index != current_sublattice_index)
                {
                    OCSublatticeData sublattice;
                    sublattice.index = current_sublattice_index;
                    sublattice.sites = current_sublattice_sites;
                    sublattice.phase_moles = current_phase_moles;
                    sublattice.phase_form_units = current_phase_form_units;
                    sublattice.phase_instance = current_phase_instance;
                    phase.sublattices.push_back(sublattice);
                }
                phase.sublattices.back().composition[species_name] += mole_fraction;

                OCSpeciesData&    species       = phase.species[species_name];
                const double      species_formula_moles =
                    (current_sublattice_sites > 0.0) ?
                        mole_fraction * current_phase_form_units * current_sublattice_sites :
                        mole_fraction * current_phase_moles;
                species.moles += species_formula_moles;
                species.stoichiometric_size = speciesStoichiometricSize(species_name, valid_elements);
                species.atom_equivalent_moles += species_formula_moles * species.stoichiometric_size;
                addFormulaElements(species, phase, species_name, valid_elements, species_formula_moles, false);
            }
        }
    }

    for (auto& phase_entry : data.solution_phases)
    {
        OCPhaseData& phase = phase_entry.second;
        const bool phase_has_element_inventory = !phase.elements.empty();
        for (auto& species_entry : phase.species)
        {
            OCSpeciesData& species = species_entry.second;
            if (!species.elements.empty() || species.moles <= 0.0)
                continue;

            if (addSublatticeElements(species, phase, valid_elements, !phase_has_element_inventory))
                continue;

            addFormulaElements(species,
                               phase,
                               species_entry.first,
                               valid_elements,
                               species.moles,
                               !phase_has_element_inventory);
        }
    }

    return data;
}

namespace OCUtilsCoupling
{
    
std::string readTextFile(const std::string& file_path)
{
    std::ifstream file(file_path);
    if (!file)
        return "<unable to open file>";

    std::ostringstream content;
    content << file.rdbuf();
    return content.str();
}

bool fileExists(const std::string& file_path)
{
    std::ifstream file(file_path);
    return static_cast<bool>(file);
}

bool hasInvalidEquilibriumResult(const std::string& output_text)
{
    return output_text.find("not a valid equilibrium as last calculation failed") != std::string::npos ||
           output_text.find("No results as no equilibrium calculated") != std::string::npos ||
           output_text.find("*** The results listed below may be inconsistent with the current conditions") != std::string::npos;
}

// Debug
void dumpParsedOcOutput(const OCOutputData& output_data)
{
    std::cout << "\n[OC parser] Parsed components" << std::endl;
    if (output_data.components.empty())
        std::cout << "  <none>" << std::endl;
    else
    {
        for (const auto& component_entry : output_data.components)
        {
            const auto& name = component_entry.first;
            const auto& data = component_entry.second;
            std::cout << "  " << name
                      << " : moles=" << data.moles
                      << ", x=" << data.mole_fraction
                      << ", mu/RT=" << data.chemical_potential_over_rt
                      << ", activity=" << data.activity
                      << std::endl;
        }
    }

    std::cout << "\n[OC parser] Parsed phases" << std::endl;
    if (output_data.solution_phases.empty())
    {
        std::cout << "  <none>" << std::endl;
        return;
    }

    for (const auto& phase_entry : output_data.solution_phases)
    {
        const auto& phase_name = phase_entry.first;
        const auto& phase_data = phase_entry.second;

        std::cout << "  Phase " << phase_name
                  << " : moles=" << phase_data.moles
                  << ", volume=" << phase_data.volume
                  << ", form_units=" << phase_data.form_units
                  << std::endl;

        if (!phase_data.elements.empty())
        {
            std::cout << "    Elements" << std::endl;
            for (const auto& element_entry : phase_data.elements)
            {
                std::cout << "      " << element_entry.first
                          << " = " << element_entry.second
                          << std::endl;
            }
        }

        if (!phase_data.sublattices.empty())
        {
            std::cout << "    Sublattices" << std::endl;
            for (const auto& sublattice : phase_data.sublattices)
            {
                std::cout << "      Sublattice " << sublattice.index
                          << " : constituents=" << sublattice.constituents_count
                          << ", sites=" << sublattice.sites
                          << ", phase_instance=" << sublattice.phase_instance
                          << ", phase_moles=" << sublattice.phase_moles
                          << std::endl;

                for (const auto& constituent_entry : sublattice.composition)
                {
                    std::cout << "        " << constituent_entry.first
                              << " = " << constituent_entry.second
                              << std::endl;
                }
            }
        }

        if (!phase_data.species.empty())
        {
            std::cout << "    Species" << std::endl;
            for (const auto& species_entry : phase_data.species)
            {
                const auto& species_name = species_entry.first;
                const auto& species_data = species_entry.second;
                std::cout << "      " << species_name
                          << " : moles=" << species_data.moles
                          << ", atom_equivalent_moles=" << species_data.atom_equivalent_moles
                          << ", stoichiometric_size=" << species_data.stoichiometric_size
                          << ", volume=" << species_data.volume
                          << std::endl;

                for (const auto& element_entry : species_data.elements)
                {
                    std::cout << "        " << element_entry.first
                              << " = " << element_entry.second
                              << std::endl;
                }

                for (const auto& sublattice : species_data.sublattices)
                {
                    std::cout << "        Sublattice " << sublattice.index
                              << " : constituents=" << sublattice.constituents_count
                              << ", sites=" << sublattice.sites
                              << ", phase_instance=" << sublattice.phase_instance
                              << ", phase_moles=" << sublattice.phase_moles
                              << std::endl;

                    for (const auto& constituent_entry : sublattice.composition)
                    {
                        std::cout << "          " << constituent_entry.first
                                  << " = " << constituent_entry.second
                                  << std::endl;
                    }
                }
            }
        }
    }
}

bool writePhaseSublatticeCompositionOutput(const std::string& file_path,
                                           double             time_hours,
                                           const std::string& location,
                                           const OCOutputData& output_data,
                                           double             content_scaling_factor)
{
    const bool write_header = !fileExists(file_path);
    std::ofstream output_file(file_path, std::ios::app);
    if (!output_file)
        return false;

    if (write_header)
    {
        output_file << "Time (h)\tLocation\tPhase\tPhase instance\tMoles (mol/m3)\t"
                    << "Form units (mol/m3)\tSublattice\tSites\tConstituent\tSite fraction\n";
    }

    output_file << std::setprecision(10);
    for (const auto& phase_entry : output_data.solution_phases)
    {
        const std::string& phase_name = phase_entry.first;
        const OCPhaseData& phase_data = phase_entry.second;

        if (phase_name == "condensed")
        {
            for (const auto& species_entry : phase_data.species)
            {
                const std::string& species_name = species_entry.first;
                const OCSpeciesData& species_data = species_entry.second;

                for (const auto& sublattice : species_data.sublattices)
                {
                    for (const auto& constituent_entry : sublattice.composition)
                    {
                        output_file << time_hours << "\t"
                                    << location << "\t"
                                    << species_name << "\t"
                                    << sublattice.phase_instance << "\t"
                                    << sublattice.phase_moles * content_scaling_factor << "\t"
                                    << sublattice.phase_form_units * content_scaling_factor << "\t"
                                    << sublattice.index << "\t"
                                    << sublattice.sites << "\t"
                                    << constituent_entry.first << "\t"
                                    << constituent_entry.second << "\n";
                    }
                }
            }
            continue;
        }

        for (const auto& sublattice : phase_data.sublattices)
        {
            for (const auto& constituent_entry : sublattice.composition)
            {
                output_file << time_hours << "\t"
                            << location << "\t"
                            << phase_name << "\t"
                            << sublattice.phase_instance << "\t"
                            << sublattice.phase_moles * content_scaling_factor << "\t"
                            << sublattice.phase_form_units * content_scaling_factor << "\t"
                            << sublattice.index << "\t"
                            << sublattice.sites << "\t"
                            << constituent_entry.first << "\t"
                            << constituent_entry.second << "\n";
            }
        }
    }

    return output_file.good();
}

std::vector<InputComponent> buildInputComponents(
     const std::set<std::string>&     selected_elements,
     SciantixArray<SciantixVariable>& sciantix_variable,
     SciantixArray<System>&           sciantix_system,
     double&                          total_content,
     const std::string& location)
{
    std::vector<InputComponent> components;
    total_content = 0.0;

    if (location == "matrix")
    {
        // Matrix component
        for (const auto& element_name : selected_elements)
        {
            InputComponent component;
            component.name = element_name;
            component.content = std::max(0.0, sciantix_variable[element_name + " content"].getFinalValue());

            if (component.content > 0.0)
            {
                total_content += component.content;
                components.push_back(component);
            }
        }
    }
    else if (location == "at grain boundary")
    {
        if (selected_elements.count("O") > 0)
        {
            InputComponent component;
            component.name = "O";
            component.content = std::max(0.0, sciantix_variable["O available content"].getFinalValue());
            
            if (component.content > 0.0)
            {
                total_content += component.content;
                components.push_back(component);
            }
        }

        // FP component
        for (auto& system : sciantix_system)
        {
            const std::string element_name = system.getFissionProductName();
            if (selected_elements.count(element_name) == 0)
                continue;

            InputComponent component;
            component.name = element_name;

            if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
            {
                const double atoms_available =
                    sciantix_variable[element_name + " produced"].getFinalValue() -
                    sciantix_variable[element_name + " decayed"].getFinalValue() -
                    sciantix_variable[element_name + " in grain"].getFinalValue() -
                    sciantix_variable[element_name + " released"].getInitialValue();

                component.content = std::max(0.0, atoms_available / avogadro_number);
            }
            else if (system.getRestructuredMatrix() == 0 && system.isMetallicFP())
            {
                const double atoms_available =
                    sciantix_variable[element_name + " produced"].getFinalValue();

                component.content = std::max(0.0, atoms_available / avogadro_number);
            }

            if (component.content > 0.0)
            {
                total_content += component.content;
                components.push_back(component);
            }
        }
    }

    if (total_content <= 0.0 || components.empty())
        return components;

    for (auto& component : components)
        component.fraction = component.content / total_content;

    components.erase(
        std::remove_if(
            components.begin(),
            components.end(),
            [](const InputComponent& component)
            {
                return component.fraction < 1.0e-8; // cut-off
            }),
        components.end());

    total_content = 0.0;
    for (const auto& component : components)
        total_content += component.content;

    if (total_content <= 0.0 || components.empty())
        return components;

    for (auto& component : components)
        component.fraction = component.content / total_content;

    return components;
}

bool writeOpenCalphadInput(const std::string& state_file_path,
                           const std::string& data_path,
                           double             pressure,
                           double             temperature,
                           OpenCalphadSolveMode solve_mode,
                           const std::string& location,
                           std::vector<InputComponent> components,
                           SciantixArray<SciantixVariable>& sciantix_variable)
{
    // Generating input file
    std::ofstream input_file(state_file_path + ".OCM");

    bool use_saved_state = false;
    #if !defined(COUPLING_TU)
        use_saved_state =
            (solve_mode == OpenCalphadSolveMode::SaveReadWarmStart) &&
            hasOpenCalphadSavedState(state_file_path);
    #endif

    if (use_saved_state)
        input_file << "r u " << state_file_path << ".OCU\n\n";
    else
    {
        input_file << "r t " << data_path;
        for (const auto& component : components)
            input_file << " " << toLowerCopy(component.name);
        input_file << "\n\n";
    }

    input_file << "set ref o gas * " << reference_oxygen_pressure_bar * 1.0e6 << "\n\n";
    input_file << "set c t=" << temperature << "\n";
    input_file << "set c p=" << pressure << "\n";
    for (const auto& component : components)
        input_file << "set c n(" << toLowerCopy(component.name) << ")=" << component.fraction << "\n";
    input_file << "c e\n";

    if (location == "at grain boundary" && solve_mode != OpenCalphadSolveMode::FixedOxygenMoles)
    {       
        input_file << "set c n(o)=none\n";
        // Oxygen potential: convert from kJ/mol O2 to J/mol O
        input_file << "set c mu(o)=" <<  sciantix_variable["Fuel oxygen potential"].getFinalValue() * 1.0e3 / 2.0 << "\n\n";
    }

    if (solve_mode == OpenCalphadSolveMode::SaveReadWarmStart || solve_mode == OpenCalphadSolveMode::GlobalEquilibrium)
        input_file << "c w\n\n";
    if (solve_mode == OpenCalphadSolveMode::PressureAxisStep)
    {
        input_file << "set c p=" << 1.0e5 << "\n";
        input_file << "c w\n";
        input_file << "set axis\n";
        input_file << "1\n";
        input_file << "p\n";
        input_file << 1.0e5 << "\n"; // start pressure
        input_file << pressure << "\n\n\n";
        input_file << "step\n";
        input_file << "normal\n\n";
        input_file << "set c p=" << pressure << "\n";
        input_file << "c e\nc w\n";
    }
    else if (solve_mode == OpenCalphadSolveMode::OnlyC1MO2)
    {
        input_file << "set st ph gas=fix 0\n";
        input_file << "set st ph *=dor\n";
        input_file << "set st ph gas=e 1\n";
        input_file << "c e\n";
        input_file << "set st ph C1_MO2=e 1\n";
        input_file << "c e\nc w\n";
    }

    #if !defined(COUPLING_TU)
        input_file << "save u " << state_file_path << " Y\n\n";
    #endif
    input_file << "l /out=" << state_file_path + ".DAT" << " r 2\n\n";
    input_file << "fin";
    return true;
}

bool runOpenCalphadCase(const std::string& executable)
{
    const std::string command =
        "timeout --signal=TERM " + std::to_string(60) + "s " + executable + " > /dev/null 2>&1";
    const int status = std::system(command.c_str());
    if (status != 0)
    {
        if (WIFEXITED(status) && WEXITSTATUS(status) == 124)
        {
            std::cerr << "Warning: OpenCalphad timed out after "
                      << 60
                      << " s."
                      << std::endl;
        }
        else
        {
            std::cerr << "Error: Execution of OPENCALPHAD failed." << std::endl;
        }
        return false;
    }
    return true;
}

bool runOpenCalphadCaseOCASI(const std::string& database_path,
                             double temperature,
                             double pressure,
                             const std::vector<InputComponent>& components,
                             const std::vector<std::string>& valid_elements,
                             OpenCalphadSolveMode solve_mode,
                             const std::string& location,
                             double oxygen_potential_kj_per_mol_o2,
                             OCOutputData& output_data)
{
    try
    {
        auto& oc = OCASIAdapter::getOpenCalphadInterface();

        if (!oc.loadDatabase(database_path, valid_elements))
        {
            std::cerr << "Error: Failed to load OpenCalphad database: " << database_path << std::endl;
            return false;
        }

        if (!oc.setReferenceState("O", "GAS", -1.0, reference_oxygen_pressure_bar * 1.0e6))
            std::cerr << "Warning: Failed to set OpenCalphad oxygen gas reference state" << std::endl;

        std::map<std::string, double> components_map;
        for (const auto& comp : components)
            components_map[comp.name] = comp.fraction;

        if (!oc.setConditions(temperature, pressure, components_map))
        {
            std::cerr << "Error: Failed to set OpenCalphad conditions" << std::endl;
            return false;
        }

        const bool use_oxygen_potential =
            location == "at grain boundary" && solve_mode != OpenCalphadSolveMode::FixedOxygenMoles;
        if (use_oxygen_potential)
        {
            if (!oc.calculateEquilibrium(false))
            {
                std::cerr << "Error: Initial OpenCalphad equilibrium calculation failed" << std::endl;
                return false;
            }

            if (!oc.removeComponentCondition("O"))
            {
                std::cerr << "Error: Failed to remove OpenCalphad oxygen amount condition" << std::endl;
                return false;
            }

            const double oxygen_potential_j_per_mol_o = oxygen_potential_kj_per_mol_o2 * 1.0e3 / 2.0;
            if (!oc.setComponentPotential("O", oxygen_potential_j_per_mol_o))
            {
                std::cerr << "Error: Failed to set OpenCalphad oxygen potential condition" << std::endl;
                return false;
            }
        }

        if (solve_mode == OpenCalphadSolveMode::PressureAxisStep)
        {
            oc.setPhaseStatus("GAS", -3, 0.0);
        }
        else if (solve_mode == OpenCalphadSolveMode::OnlyC1MO2)
        {
            oc.setPhaseStatus("GAS", 2, 0.0);
            oc.setPhaseStatus("*", -3, 0.0);
            oc.setPhaseStatus("C1_MO2", 0, 0.0);
        }

        const bool suspend_gas = !use_oxygen_potential &&
                                 (solve_mode == OpenCalphadSolveMode::GlobalEquilibrium ||
                                  solve_mode == OpenCalphadSolveMode::SaveReadWarmStart ||
                                  solve_mode == OpenCalphadSolveMode::FixedOxygenMoles);
        if (!oc.calculateEquilibrium(suspend_gas))
        {
            std::cerr << "Error: OpenCalphad equilibrium calculation failed" << std::endl;
            return false;
        }

        if (!oc.extractResults(output_data))
        {
            std::cerr << "Error: Failed to extract OpenCalphad results" << std::endl;
            return false;
        }

        return true;
    }
    catch (const std::exception& e)
    {
        std::cerr << "Exception in runOpenCalphadCaseOCASI: " << e.what() << std::endl;
        return false;
    }
}

void updateThermochemistryVariablesFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                              const std::string&                         location,
                                              double                                     content_scaling_factor,
                                              SciantixArray<ThermochemistryVariable>&    thermochemistry_variable,
                                              SciantixArray<SciantixVariable>&           sciantix_variable)
{
    auto computePhaseComposition = [](const OCPhaseData& phase_data)
    {
        std::map<std::string, double> composition;
        if (phase_data.moles <= 0.0)
            return composition;

        for (const auto& element_entry : phase_data.elements)
            composition[element_entry.first] = std::max(0.0, element_entry.second) / phase_data.moles;

        return composition;
    };

    double oxygen_with_fps = 0.0;
    for (const auto& phase_entry : solution_phases)
    {
        const std::string& phase_name = phase_entry.first;
        const OCPhaseData& phase_data = phase_entry.second;
        const bool liquid_phase = isLiquidPhase(phase_name);

        const auto oxygen = phase_data.elements.find("O");
        if (oxygen != phase_data.elements.end())
            oxygen_with_fps += oxygen->second * content_scaling_factor;

        if (liquid_phase)
        {
            const std::string liquid_variable_name = "LIQUID (" + phase_name + ", " + location + ")";
            if (thermochemistry_variable.isElementPresent(liquid_variable_name))
            {
                thermochemistry_variable[liquid_variable_name].setFinalValue(
                    phase_data.moles * content_scaling_factor);

                const std::map<std::string, double> composition = computePhaseComposition(phase_data);
                if (!composition.empty())
                    thermochemistry_variable[liquid_variable_name].setComposition(composition);
            }
        }

        if (!phase_data.species.empty())
        {
            for (const auto& species_entry : phase_data.species)
            {
                const std::string variable_name =
                    species_entry.first + " (" + phase_name + ", " + location + ")";

                if (thermochemistry_variable.isElementPresent(variable_name))
                {
                    thermochemistry_variable[variable_name].setFinalValue(
                        species_entry.second.moles * content_scaling_factor);
                    std::map<std::string, double> composition;
                    if (species_entry.second.moles > 0.0)
                    {
                        for (const auto& element_entry : species_entry.second.elements)
                            composition[element_entry.first] = element_entry.second / species_entry.second.moles;
                    }
                    thermochemistry_variable[variable_name].setComposition(composition);
                }
            }

            if (liquid_phase)
                continue;

            for (const auto& element_entry : phase_data.elements)
            {
                const std::string variable_name = element_entry.first + " (" + phase_name + ", " + location + ")";
                const std::string uppercase_variable_name =
                    toUpperCopy(element_entry.first) + " (" + phase_name + ", " + location + ")";
                const bool has_variable = thermochemistry_variable.isElementPresent(variable_name);
                const bool has_uppercase_variable =
                    thermochemistry_variable.isElementPresent(uppercase_variable_name);

                if (has_variable)
                {
                    thermochemistry_variable[variable_name].setFinalValue(
                        element_entry.second * content_scaling_factor);
                    thermochemistry_variable[variable_name].setComposition({{element_entry.first, 1.0}});
                }
                else if (has_uppercase_variable)
                {
                    thermochemistry_variable[uppercase_variable_name].setFinalValue(
                        element_entry.second * content_scaling_factor);
                    thermochemistry_variable[uppercase_variable_name].setComposition({{element_entry.first, 1.0}});
                }
            }
            continue;
        }

        if (liquid_phase)
            continue;

        for (const auto& element_entry : phase_data.elements)
        {
            const std::string variable_name = element_entry.first + " (" + phase_name + ", " + location + ")";
            const std::string uppercase_variable_name =
                toUpperCopy(element_entry.first) + " (" + phase_name + ", " + location + ")";

            if (thermochemistry_variable.isElementPresent(variable_name))
            {
                thermochemistry_variable[variable_name].setFinalValue(
                    element_entry.second * content_scaling_factor);
                thermochemistry_variable[variable_name].setComposition({{element_entry.first, 1.0}});
            }
            else if (thermochemistry_variable.isElementPresent(uppercase_variable_name))
            {
                thermochemistry_variable[uppercase_variable_name].setFinalValue(
                    element_entry.second * content_scaling_factor);
                thermochemistry_variable[uppercase_variable_name].setComposition({{element_entry.first, 1.0}});
            }
        }
    }

    if (location == "at grain boundary")
        sciantix_variable["O available content"].setFinalValue(oxygen_with_fps);
}

void updateMatrixFromOutput(const OCOutputData&              output_data,
                            double                           temperature,
                            SciantixArray<SciantixVariable>& sciantix_variable)
{
    const auto oxygen_component = output_data.components.find("O");
    double calphad_oxygen_potential(0.0), calphad_oxygen_partial_pressure(0.0);
    if (oxygen_component != output_data.components.end())
    {
        calphad_oxygen_potential =
            2.0 * oxygen_component->second.chemical_potential_over_rt * gas_constant * temperature * 1.0e-3;
        calphad_oxygen_partial_pressure =
            reference_oxygen_pressure_bar * oxygen_component->second.activity * oxygen_component->second.activity;
    }

    sciantix_variable["Fuel oxygen partial pressure - CALPHAD"].setFinalValue(calphad_oxygen_partial_pressure);
    sciantix_variable["Fuel oxygen potential - CALPHAD"].setFinalValue(calphad_oxygen_potential);

    if (calphad_oxygen_partial_pressure > 0.0)
    {
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(calphad_oxygen_partial_pressure);
        sciantix_variable["Fuel oxygen potential"].setFinalValue(calphad_oxygen_potential);
    }
}

void updateGrainBoundaryFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                   const std::set<std::string>&               selected_elements,
                                   double                                     content_scaling_factor,
                                   SciantixArray<SciantixVariable>&           sciantix_variable,
                                   SciantixArray<System>&                     sciantix_system)
{
    const auto gas_phase = solution_phases.find("gas");

    for (auto& system : sciantix_system)
    {
        const std::string element = system.getFissionProductName();
        if (selected_elements.count(element) == 0)
            continue;

        double gas_moles = 0.0;
        if (gas_phase != solution_phases.end() && gas_phase->second.elements.count(element) > 0)
            gas_moles = gas_phase->second.elements.at(element) * content_scaling_factor;

        if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
        {
            const double available = (
                sciantix_variable[element + " produced"].getFinalValue() -
                sciantix_variable[element + " decayed"].getFinalValue() -
                sciantix_variable[element + " in grain"].getFinalValue() -
                sciantix_variable[element + " released"].getInitialValue()
            );

            const double updated_atoms = std::min(available, gas_moles * avogadro_number);
            sciantix_variable[element + " at grain boundary"].setFinalValue(updated_atoms);
            sciantix_variable[element + " reacted"].setFinalValue(available - updated_atoms);
        }
        else if (system.getRestructuredMatrix() == 0 && system.isMetallicFP())
        {
            const double available =
                sciantix_variable[element + " produced"].getFinalValue();

            const double updated_atoms = std::min(available, gas_moles * avogadro_number);
            sciantix_variable[element + " in solution"].setFinalValue(updated_atoms);
            sciantix_variable[element + " reacted"].setFinalValue(available - updated_atoms);
        }
    }
}
}  // namespace OCUtilsCoupling
