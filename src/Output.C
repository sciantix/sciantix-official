#include "Output.h"
#include <iostream>  // for debugging

inline bool if_exist(const std::string& name)
{
    struct stat buffer;
    return (stat(name.c_str(), &buffer) == 0);
}

void Output()
{
    std::string output_name = "output.txt";
    std::fstream output_file;
    output_file.open(output_name, std::fstream::in | std::fstream::out | std::fstream::app);

    // Debugging information
    std::cout << "Entering Output function\n";

    if (int(input_variable[iv["iOutput"]].getValue()) == 1)
    {
        if (history_variable[hv["Time step number"]].getFinalValue() == 0)
        {
            std::cout << "Printing headers for iOutput == 1\n";  // Debugging information
            for (auto& variable : history_variable)
            {
                if (variable.getOutput())
                    output_file << variable.getName() << " " << variable.getUOM() << "\t";
            }
            for (auto& variable : sciantix_variable)
            {
                if (variable.getOutput())
                    output_file << variable.getName() << " " << variable.getUOM() << "\t";
            }
            output_file << "\n";
        }

        if ((int)history_variable[hv["Time step number"]].getFinalValue() % 1 == 0)
        {
            std::cout << "Printing values for iOutput == 1\n";  // Debugging information
            for (auto& variable : history_variable)
            {
                if (variable.getOutput())
                    output_file << std::setprecision(10) << variable.getFinalValue() << "\t";
            }

            for (auto& variable : sciantix_variable)
            {
                if (variable.getOutput())
                    output_file << std::setprecision(7) << variable.getFinalValue() << "\t";
            }
            output_file << "\n";
        }
    }
    else if ((int)input_variable[iv["iOutput"]].getValue() == 2)
    {
        if (history_variable[hv["Time step number"]].getFinalValue() == 0)
        {
            std::cout << "Printing headers for iOutput == 2\n";  // Debugging information
            for (auto& variable : history_variable)
            {
                output_file << variable.getName() << " " << variable.getUOM() << "\t";
            }
            for (auto& variable : sciantix_variable)
            {
                output_file << variable.getName() << " " << variable.getUOM() << "\t";
            }
            output_file << "\n";
        }

        if ((int)history_variable[hv["Time step number"]].getFinalValue() % 1 == 0)
        {
            std::cout << "Printing values for iOutput == 2\n";  // Debugging information
            for (auto& variable : history_variable)
            {
                output_file << std::setprecision(10) << variable.getFinalValue() << "\t";
            }

            for (auto& variable : sciantix_variable)
            {
                output_file << std::setprecision(7) << variable.getFinalValue() << "\t";
            }
            output_file << "\n";
        }
    }

    output_file.close();

    std::string overview_name = "overview.txt";

    if (history_variable[hv["Time step number"]].getFinalValue() == 0 && if_exist(overview_name))
        remove(overview_name.c_str());

    std::fstream overview_file;
    if (history_variable[hv["Time step number"]].getFinalValue() == 0 && !if_exist(overview_name))
    {
        overview_file.open(overview_name, std::fstream::in | std::fstream::out | std::fstream::app);

        for (auto& model_ : model)
        {
            overview_file << "Model" << "\t";
            overview_file << model_.getName() << "\t";
            overview_file << model_.getRef() << "\n";
        }

        overview_file << "\n";

        for (auto& matrix_ : matrix)
        {
            overview_file << "Matrix" << "\t";
            overview_file << matrix_.getName() << "\t";
            overview_file << matrix_.getRef() << "\n";
        }

        overview_file << "\n";

        for (auto& system : sciantix_system)
        {
            overview_file << "System" << "\t";
            overview_file << system.getName() << "\t";
            overview_file << system.getRef() << "\n";
        }

        overview_file << "\n";

        for (auto& input_variable_ : input_variable)
        {
            overview_file << "Input setting" << "\t";
            overview_file << input_variable_.getName() << " = ";
            overview_file << input_variable_.getValue() << "\n";
        }

        overview_file << "\n";
    }
    overview_file.close();

    std::cout << "Exiting Output function\n";  // Debugging information
}
