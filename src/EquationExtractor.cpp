#include "EquationExtractor.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <filesystem>
#include <regex>

namespace fs = std::filesystem;


void extract_specific_latex_blocks(const fs::path& file_path, std::ofstream& outfile) {
    std::ifstream infile(file_path);
    if (!infile.is_open()) {
        std::cerr << "ERROR: Unable to open file" << file_path << ". Check the path and permissions.\n";
        return;
    }

    std::string line;
    std::stringstream current_block_content;
    bool in_block = false; // Indicates if we are in a targeted multi-line block (equation, tabular)
    std::string current_block_type; // Type of the block currently read
    std::string current_block_start_delimiter; // Block start delimiter
    std::string current_block_end_delimiter;   // End delimiter of the block


    std::regex bibtex_title_regex(R"(title\s*=\s*\{((?:[^\{\}]*\{[^\{\}]*\})*[^\{\}]*)\})");

    while (std::getline(infile, line)) {
        // If we are not in an equation or tabular block, we look for the start of a new targeted block or a BibTeX heading.
        if (!in_block) {
            // Detection of the "equation" environment
            if (line.find("\\begin{equation}") != std::string::npos) {
                in_block = true;
                current_block_type = "Equation environment";
                current_block_start_delimiter = "\\begin{equation}";
                current_block_end_delimiter = "\\end{equation}";
                current_block_content.str(""); // Reset the buffer
                size_t start_pos = line.find(current_block_start_delimiter) + current_block_start_delimiter.length();
                current_block_content << line.substr(start_pos) << "\n";
                continue;
            }
            // Detection of the "tabular" environment
            else if (line.find("\\begin{tabular}") != std::string::npos) {
                in_block = true;
                current_block_type = "Table (tabular)";
                current_block_start_delimiter = "\\begin{tabular}";
                current_block_end_delimiter = "\\end{tabular}";
                current_block_content.str("");
                size_t start_pos = line.find(current_block_start_delimiter) + current_block_start_delimiter.length();
                current_block_content << line.substr(start_pos) << "\n";
                continue;
            }
            else {
                std::smatch match;
                if (std::regex_search(line, match, bibtex_title_regex)) {
                    if (match.size() > 1) { 
                        std::string bib_title = match[1].str();
                        bib_title.erase(std::remove(bib_title.begin(), bib_title.end(), '\n'), bib_title.end());
                        bib_title.erase(std::remove(bib_title.begin(), bib_title.end(), '\r'), bib_title.end());

                        outfile << "File : " << file_path << "\n";
                        outfile << "Type : BibTeX Reference Title\n";
                        outfile << bib_title << "\n\n";
                    }
                }
            }
        } else { 
            if (line.find(current_block_end_delimiter) != std::string::npos) {
                in_block = false;
                outfile << "File : " << file_path << "\n";
                outfile << "Type : " << current_block_type << "\n";
                size_t end_pos = line.find(current_block_end_delimiter);
                current_block_content << line.substr(0, end_pos) << "\n";
                std::string cleaned_content = current_block_content.str();
                size_t first = cleaned_content.find_first_not_of(" \t\n\r");
                size_t last = cleaned_content.find_last_not_of(" \t\n\r");
                if (std::string::npos != first && std::string::npos != last) {
                    outfile << cleaned_content.substr(first, (last - first + 1)) << "\n\n";
                } else {
                    outfile << cleaned_content << "\n\n";
                }
                current_block_content.str(""); 
                continue;
            }
            current_block_content << line << "\n";
            continue;
        }
    }

    infile.close();
}


bool has_tex_extension(const std::string& filename) {
    return filename.size() > 4 && filename.substr(filename.size() - 4) == ".tex";
}

void extractAllEquations(const std::string& input_dir, const std::string& output_filename) {
    const std::string output_dir_path = "output";
    if (!fs::exists(output_dir_path)) {
        fs::create_directory(output_dir_path);
    }

    std::ofstream outfile(output_filename);
    if (!outfile.is_open()) {
        std::cerr << "ERROR: Unable to create output file " << output_filename << "\n";
        return;
    }

    int file_count = 0;
    std::vector<fs::path> files_with_extracted_content;
    std::vector<fs::path> files_without_extracted_content;


    for (const auto& entry : fs::recursive_directory_iterator(input_dir)) {
        if (entry.is_regular_file() && has_tex_extension(entry.path().filename().string())) {
            file_count++;

            std::string temp_file_name = "temp_extraction_check_" + entry.path().filename().string() + ".tmp";
            std::ofstream temp_outfile(temp_file_name, std::ios_base::out | std::ios_base::trunc);

            if (!temp_outfile.is_open()) {
                 std::cerr << "WARNING: Unable to create temporary file'" << temp_file_name << "'. Skipping check for this file.\n";
                 files_without_extracted_content.push_back(entry.path());
                 continue;
            }

            extract_specific_latex_blocks(entry.path(), temp_outfile);
            temp_outfile.close();

            std::stringstream temp_output_buffer;
            std::ifstream temp_infile(temp_file_name);
            temp_output_buffer << temp_infile.rdbuf();
            temp_infile.close();
            fs::remove(temp_file_name);

            if (!temp_output_buffer.str().empty()) {
                files_with_extracted_content.push_back(entry.path());
                outfile << temp_output_buffer.str();
            } else {
                files_without_extracted_content.push_back(entry.path());
            }
        }
    }

    if (file_count == 0) {
        std::cerr << "NO .tex files found in the directory'" << input_dir << "' !\n";
    } else {
        std::cout << "Total number of .tex files analyzed: " << file_count << "\n\n";

        if (!files_with_extracted_content.empty()) {
            std::cout << "Files with equations/tables/BibTeX titles detected (" << files_with_extracted_content.size() << ") :\n";
            for (const auto& p : files_with_extracted_content) {
                std::cout << "  - " << p.string() << "\n";
            }
        } else {
            std::cout << "No BibTeX equation/table/title detected in any file.\n";
        }
        std::cout << "\n";

        if (!files_without_extracted_content.empty()) {
            std::cout << "Fichiers sans équations/tableaux/titres BibTeX détectés (" << files_without_extracted_content.size() << ") :\n";
            for (const auto& p : files_without_extracted_content) {
                std::cout << "  - " << p.string() << "\n";
            }
        }
    }
    outfile.close();
}