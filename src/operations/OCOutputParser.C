#include "OCOutputParser.h"

#include <algorithm>
#include <cctype>
#include <exception>
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <set>
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

    return trim(name);
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
    std::string current_phase;

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

            if (!isNumericToken(parts[1]) || !isNumericToken(parts[2]) || !isNumericToken(parts[3]) ||
                !isNumericToken(parts[4]))
                continue;

            OCComponentData& component = data.components[normalizeElementCase(parts[0], valid_elements)];
            component.moles = safeFloat(parts[1]);
            component.mole_fraction = safeFloat(parts[2]);
            component.chemical_potential_over_rt = safeFloat(parts[3]);
            component.activity = safeFloat(parts[4]);

            if (parts.size() > 5)
            {
                std::ostringstream reference_state;
                for (size_t token_index = 5; token_index < parts.size(); ++token_index)
                {
                    if (token_index > 5)
                        reference_state << " ";
                    reference_state << parts[token_index];
                }
                component.reference_state = reference_state.str();
            }

            continue;
        }

        if (!in_phases_section)
            continue;

        if (stripped.rfind("Name", 0) == 0 || stripped.find("Moles") != std::string::npos)
            continue;

        if (stripped.find(".. E") != std::string::npos && stripped.find("X:") != std::string::npos)
        {
            in_constitution_section = false;

            const std::vector<std::string> parts = split(stripped);
            if (parts.size() < 4 || !isNumericToken(parts[2]) || !isNumericToken(parts[3]))
                continue;

            const std::string raw_phase_name = normalizeSpeciesName(parts[0]);
            current_phase = normalizePhaseName(raw_phase_name);

            if (current_phase != "condensed")
            {
                OCPhaseData& phase = data.solution_phases[current_phase];
                phase.moles += safeFloat(parts[2]);
                phase.volume += safeFloat(parts[3]);
            }
            else
            {
                const std::string species_name = normalizeSpeciesName(raw_phase_name);
                OCPhaseData&      phase        = data.solution_phases[current_phase];
                OCSpeciesData&    species      = phase.species[species_name];
                const double      moles        = safeFloat(parts[2]);
                const double      volume       = safeFloat(parts[3]);

                species.moles += moles;
                species.volume += volume;
                phase.moles += moles;
                phase.volume += volume;
            }

            for (size_t j = i + 1; j < lines.size(); ++j)
            {
                const std::string next_line = trim(lines[j]);
                if (next_line.rfind("Constitution: There are", 0) == 0)
                {
                    in_constitution_section = true;
                    break;
                }

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
                        data.solution_phases[current_phase].elements[element] =
                            fraction * data.solution_phases[current_phase].moles;
                    }
                    else
                    {
                        const std::string species_name = normalizeSpeciesName(raw_phase_name);
                        OCPhaseData&      phase        = data.solution_phases[current_phase];
                        OCSpeciesData&    species      = phase.species[species_name];
                        const double      element_moles = fraction * species.moles;

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
                next_line.find("with") != std::string::npos)
            {
                in_constitution_section = false;
                continue;
            }

            const std::vector<std::string> species_parts = split(next_line);
            for (size_t k = 0; k + 1 < species_parts.size(); k += 2)
            {
                if (current_phase == "condensed")
                    continue;

                if (!isNumericToken(species_parts[k + 1]))
                    continue;

                const std::string species_name = normalizeSpeciesName(species_parts[k]);
                const double      mole_fraction = safeFloat(species_parts[k + 1]);
                OCPhaseData&      phase         = data.solution_phases[current_phase];
                OCSpeciesData&    species       = phase.species[species_name];
                species.moles = mole_fraction * phase.moles;
            }
        }
    }

    return data;
}
