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

#ifndef THERMOCHEMISTRY_PARSING_UTILS_H
#define THERMOCHEMISTRY_PARSING_UTILS_H

#include <sstream>
#include <string>
#include <vector>

/**
 * @brief Shared whitespace-trim and delimiter-split helpers used when parsing the
 * pipe/key-value thermochemistry input files (manifest and settings), so the two
 * parsers cannot silently drift apart on basic tokenization behavior.
 */
namespace ThermochemistryParsingUtils
{
inline std::string trim(const std::string& input)
{
    const std::string whitespace = " \t\r\n";
    const size_t      begin      = input.find_first_not_of(whitespace);
    if (begin == std::string::npos)
        return "";

    const size_t end = input.find_last_not_of(whitespace);
    return input.substr(begin, end - begin + 1);
}

inline std::vector<std::string> split(const std::string& input, const char delimiter, bool skip_empty = false)
{
    std::vector<std::string> parts;
    std::stringstream        stream(input);
    std::string              item;

    while (std::getline(stream, item, delimiter))
    {
        item = trim(item);
        if (!skip_empty || !item.empty())
            parts.push_back(item);
    }

    return parts;
}
}  // namespace ThermochemistryParsingUtils

#endif
