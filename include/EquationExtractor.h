#pragma once
#include <string>
#include <filesystem> 
#include <fstream>    


void extract_specific_latex_blocks(const std::filesystem::path& file_path, std::ofstream& outfile);
void extractAllEquations(const std::string& input_dir = "Correlations", const std::string& output_filename = "output/equations.txt");
bool has_tex_extension(const std::string& filename);
