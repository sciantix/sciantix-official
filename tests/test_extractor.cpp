#include "catch.hpp" 
#include "../include/EquationExtractor.h" 
#include <fstream>
#include <filesystem>
#include <string>
#include <vector>
#include <iostream> 

namespace fs = std::filesystem;




static void create_temp_file(const fs::path& file_path, const std::string& content) {
    std::ofstream ofs(file_path);
    REQUIRE(ofs.is_open()); 
    ofs << content;
    ofs.close();
}


static std::string read_file_content(const fs::path& file_path) {
    std::ifstream ifs(file_path);
    if (!ifs.is_open()) {
        return ""; 
    }
    std::stringstream ss;
    ss << ifs.rdbuf();
    return ss.str();
}


TEST_CASE("has_tex_extension", "[utility]") {
    SECTION("Correct .tex extension") {
        REQUIRE(has_tex_extension("document.tex") == true);
        REQUIRE(has_tex_extension("my_article.tex") == true); 
    }

    SECTION("Uppercase .TEX extension (expect false as per current implementation)") {
        REQUIRE(has_tex_extension("document.TEX") == false); 
    }

    SECTION("No extension or other extension") {
        REQUIRE(has_tex_extension("document") == false);
        REQUIRE(has_tex_extension("document.pdf") == false);
        REQUIRE(has_tex_extension("image.jpg") == false);
        REQUIRE(has_tex_extension("file.bib") == false); 
    }

    SECTION("Partial .tex in name, not as extension") {
        REQUIRE(has_tex_extension("my.tex.file") == false);
    }

    SECTION("File name is exactly '.tex'") { 
        REQUIRE(has_tex_extension(".tex") == false); 
    }

    SECTION("Empty string") {
        REQUIRE(has_tex_extension("") == false);
    }
}


TEST_CASE("extract_specific_latex_blocks", "[extraction]") {
   
    fs::path input_file_path = "temp_input.tex";
    fs::path output_file_path = "temp_output.txt";


    SECTION("Extracts a simple equation environment") {
        
        std::string latex_content = R"(
\documentclass{article}
\begin{document}
This is some text.
\begin{equation}
E=mc^2
\label{eq:einstein}
\end{equation}
More text after the equation.
\end{document}
)";
        create_temp_file(input_file_path, latex_content);
        std::ofstream outfile(output_file_path); 

    
        extract_specific_latex_blocks(input_file_path, outfile);
        outfile.close(); 


        std::string expected_output =
            "File : \"" + input_file_path.string() + "\"\n" 
            "Type : Equation environment\n"
            "E=mc^2\n"
            "\\label{eq:einstein}\n\n";

        std::string actual_output = read_file_content(output_file_path);
        REQUIRE(actual_output == expected_output);

        // Cleanup
        fs::remove(input_file_path);
        fs::remove(output_file_path);
    }


    SECTION("Extracts a simple tabular environment") {
       
        std::string latex_content = R"(
\documentclass{article}
\begin{document}
Some text before table.
\begin{tabular}{|l|l|l|l|}
    \hline
    Key & Value & Units & Type \\
    \hline
    A & 1.9 & mK/W & Parameter\\
    B & 3.8 & m/W & Parameter \\
    C & 7.7 & m/W & Parameter \\
    T &    & K     & Variable\\
    k &    & W/mK & Property\\
    \hline
\end{tabular}
Text after table.
\end{document}
)";
        create_temp_file(input_file_path, latex_content);
        std::ofstream outfile(output_file_path);

        // Act
        extract_specific_latex_blocks(input_file_path, outfile);
        outfile.close();

        // Assert
        std::string expected_output =
            "File : \"" + input_file_path.string() + "\"\n" 
            "Type : Table (tabular)\n"
            "{|l|l|l|l|}\n" 
            "    \\hline\n"
            "    Key & Value & Units & Type \\\\\n"
            "    \\hline\n"
            "    A & 1.9 & mK/W & Parameter\\\\\n"
            "    B & 3.8 & m/W & Parameter \\\\\n"
            "    C & 7.7 & m/W & Parameter \\\\\n"
            "    T &    & K     & Variable\\\\\n"
            "    k &    & W/mK & Property\\\\\n"
            "    \\hline\n\n";

        std::string actual_output = read_file_content(output_file_path);
        REQUIRE(actual_output == expected_output);

        // Cleanup
        fs::remove(input_file_path);
        fs::remove(output_file_path);
    }

 
    SECTION("Extracts a BibTeX title") {
        // Arrange
        std::string bib_content = R"(
@article{SomeArticle,
  author = {John Doe},
  title  = {Study 4 on thermophysical behavior of advanced fuels},
  year    = {2023}
}
)";
        create_temp_file(input_file_path, bib_content);
        std::ofstream outfile(output_file_path);

        // Act
        extract_specific_latex_blocks(input_file_path, outfile);
        outfile.close();

        // Assert

        std::string expected_output =
            "File : \"" + input_file_path.string() + "\"\n" 
            "Type : BibTeX Reference Title\n"
            "Study 4 on thermophysical behavior of advanced fuels\n\n";

        std::string actual_output = read_file_content(output_file_path);
        REQUIRE(actual_output == expected_output);

        // Cleanup
        fs::remove(input_file_path);
        fs::remove(output_file_path);
    }

 
    SECTION("Handles file with no specific blocks") {
        // Arrange
        std::string latex_content = R"(
\documentclass{article}
\begin{document}
This document contains no equations, tables, or bibtex entries.
\end{document}
)";
        create_temp_file(input_file_path, latex_content);
        std::ofstream outfile(output_file_path);

        // Act
        extract_specific_latex_blocks(input_file_path, outfile);
        outfile.close();

        // Assert
        std::string actual_output = read_file_content(output_file_path);
        REQUIRE(actual_output.empty()); 

        // Cleanup
        fs::remove(input_file_path);
        fs::remove(output_file_path);
    }

    SECTION("Handles non-existent input file") {
        std::ofstream outfile(output_file_path);

        extract_specific_latex_blocks("non_existent_file.tex", outfile);
        outfile.close();

        // Assert
        std::string actual_output = read_file_content(output_file_path);
        REQUIRE(actual_output.empty());

        // Cleanup
        fs::remove(output_file_path);
    }
}

TEST_CASE("extractAllEquations - directory and edge scenarios", "[full_extraction][directory][empty_dir]") {

    SECTION("processes directory correctly") {
        fs::path test_input_dir = "temp_input_dir_for_full_test";
        fs::path test_output_file = "temp_full_output.txt";
        fs::path real_output_dir = "output";

        if (fs::exists(test_input_dir)) fs::remove_all(test_input_dir);
        if (fs::exists(real_output_dir)) fs::remove_all(real_output_dir);

        fs::create_directory(test_input_dir);
        fs::create_directory(test_input_dir / "subdir");

        create_temp_file(test_input_dir / "article4.tex", R"(
@article{SomeArticle,
  author = {John Doe},
  title  = {Study 4 on thermophysical behavior of advanced fuels},
  year   = {2023}
}

\documentclass{article}
\begin{document}
Some preamble.

\begin{tabular}{|l|l|l|l|}
    \hline
    Key & Value & Units & Type \\
    \hline
    A & 1.9 & mK/W & Parameter\\
    B & 3.8 & m/W & Parameter \\
    C & 7.7 & m/W & Parameter \\
    T &    & K     & Variable\\
    k &    & W/mK & Property\\
    \hline
\end{tabular}

More text.

\begin{equation}
k(T)=\frac{1}{A+B \cdot T}
\end{equation}
\end{document}
)");

        create_temp_file(test_input_dir / "doc_no_content.tex", R"(
\documentclass{article}
\begin{document}
Just text in this document, no specific content.
\end{document}
)");

        create_temp_file(test_input_dir / "subdir" / "other_refs.bib", R"(
@book{OtherBook,
  title = {Another Great Book Title}
}
)");

        extractAllEquations(test_input_dir.string(), test_output_file.string());
        std::string actual_output = read_file_content(test_output_file);

        std::string expected_bibtex_article4 =
            "File : \"" + test_input_dir.string() + "/article4.tex\"\n"
            "Type : BibTeX Reference Title\n"
            "Study 4 on thermophysical behavior of advanced fuels\n\n";
        REQUIRE(actual_output.find(expected_bibtex_article4) != std::string::npos);

        std::string expected_tabular_article4 =
            "File : \"" + test_input_dir.string() + "/article4.tex\"\n"
            "Type : Table (tabular)\n"
            "{|l|l|l|l|}\n"
            "    \\hline\n"
            "    Key & Value & Units & Type \\\\\n"
            "    \\hline\n"
            "    A & 1.9 & mK/W & Parameter\\\\\n"
            "    B & 3.8 & m/W & Parameter \\\\\n"
            "    C & 7.7 & m/W & Parameter \\\\\n"
            "    T &    & K     & Variable\\\\\n"
            "    k &    & W/mK & Property\\\\\n"
            "    \\hline\n\n";
        REQUIRE(actual_output.find(expected_tabular_article4) != std::string::npos);

        std::string expected_equation_article4 =
            "File : \"" + test_input_dir.string() + "/article4.tex\"\n"
            "Type : Equation environment\n"
            "k(T)=\\frac{1}{A+B \\cdot T}\n\n";
        REQUIRE(actual_output.find(expected_equation_article4) != std::string::npos);

        std::string unexpected_bibtex_other_refs_prefix =
            "File : \"" + test_input_dir.string() + "/subdir/other_refs.bib\"\n";
        REQUIRE(actual_output.find(unexpected_bibtex_other_refs_prefix) == std::string::npos);
        REQUIRE(actual_output.find("doc_no_content.tex") == std::string::npos);

        fs::remove_all(test_input_dir);
        fs::remove_all(real_output_dir);
        fs::remove(test_output_file);
    }

    SECTION("handles empty directory") {
        fs::path test_input_dir = "temp_empty_dir_test";
        fs::path test_output_file = "temp_empty_output.txt";
        fs::path real_output_dir = "output";

        if (fs::exists(test_input_dir)) fs::remove_all(test_input_dir);
        if (fs::exists(real_output_dir)) fs::remove_all(real_output_dir);

        fs::create_directory(test_input_dir);
        extractAllEquations(test_input_dir.string(), test_output_file.string());

        std::string actual_output = read_file_content(test_output_file);
        REQUIRE(actual_output.empty());
        REQUIRE_FALSE(fs::exists(real_output_dir / test_output_file));
        REQUIRE(fs::exists(test_output_file));

        fs::remove_all(test_input_dir);
        if (fs::exists(real_output_dir)) fs::remove_all(real_output_dir);
        fs::remove(test_output_file);
    }
}