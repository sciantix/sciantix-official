#include <string>
#include <vector>
#include <regex>
#include <algorithm>
#include <sstream>
#include <iostream>

static std::string trim(const std::string& str) {
    size_t first = str.find_first_not_of(" \t\n\r\f\v");
    if (std::string::npos == first) {
        return ""; 
    }
    size_t last = str.find_last_not_of(" \t\n\r\f\v");
    return str.substr(first, (last - first + 1));
}


std::vector<std::string> processTableLines(const std::vector<std::string>& rawTableLines) {
    std::vector<std::string> formattedLines;
    std::regex data_line_pattern(R"(^[^\\].*?&.*?&.*?&.*$)");
    bool explicit_header_seen = false; 

    for (const auto& rawLine : rawTableLines) {

        std::string processedLine = trim(rawLine);

        if (!processedLine.empty() && processedLine.back() == ';') {
            processedLine.pop_back();
        }

        processedLine = std::regex_replace(processedLine, std::regex(R"(\\hline)"), "");
        processedLine = std::regex_replace(processedLine, std::regex(R"(\\toprule)"), "");
        processedLine = std::regex_replace(processedLine, std::regex(R"(\\midrule)"), "");
        processedLine = std::regex_replace(processedLine, std::regex(R"(\\bottomrule)"), "");
        processedLine = std::regex_replace(processedLine, std::regex(R"(\\\\)"), ""); 
        processedLine = std::regex_replace(processedLine, std::regex(R"(^\||\|$)"), ""); 
        processedLine = std::regex_replace(processedLine, std::regex(" +"), " "); 

        
        std::string header_norm = processedLine;
        std::transform(header_norm.begin(), header_norm.end(), header_norm.begin(), ::tolower);
        header_norm.erase(std::remove(header_norm.begin(), header_norm.end(), ' '), header_norm.end());
        if (header_norm.rfind("key&value&units&type", 0) == 0) {
            explicit_header_seen = true;
            continue;
        }

        if (processedLine.empty() ||
            processedLine.rfind("{|", 0) == 0 ||              
            processedLine.find("Data:") == 0 ||               
            processedLine.find("Format:") == 0 ||
            processedLine.find("Headers:") == 0 )
        {
            continue;
        }

        if (std::regex_search(processedLine, data_line_pattern)) {

            processedLine = std::regex_replace(processedLine, std::regex(R"(&)"), ";");
            processedLine = trim(processedLine); 
            std::stringstream ss(processedLine);
            std::string segment;
            std::vector<std::string> columns;
            while(std::getline(ss, segment, ';')) {
                columns.push_back(trim(segment));
            }

            if (columns.size() >= 4) {
                formattedLines.push_back(
                    columns[0] + ";" + columns[1] + ";" + columns[2] + ";" + columns[3] + ";"
                );
            } else {
                
            }
        } else {
            
        }
    }
    return formattedLines;
}