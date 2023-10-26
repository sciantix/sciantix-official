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
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Output.h"

inline bool if_exist(const std::string& name)
{
	/**
	 * @brief Function to check if a file exists.
	 * @return 0/1
	 * @author G. Zullo
	 * 
	 */
	struct stat buffer;
	return (stat(name.c_str(), &buffer) == 0);
}

/// @brief 
/// Output
/// This routine prints the output.txt file, that is the file with all the SCIANTIX code calculations.
/// The output.txt is organized in successive columns, each one starting with their header, name and unit of measure 
/// of the figure of merit (e.g., Temperature (K)), and then including the temporal evolution of the figure of merit.
/// 
/// The first columns contain the input_history.txt temporal interpolation performed by InputInerpolation.
/// The other columns contain the evolution of the sciantix variables.
/// This function contains different formatting options to print the output.txt file, according to iOutput value.
void Output()
{
	std::string output_name = "output.txt";
	std::fstream output_file;
	output_file.open(output_name, std::fstream::in | std::fstream::out | std::fstream::app);

	/// @brief
	/// iOutput == 1 --> output.txt organized in columns (header + values).
	if (int(input_variable[iv["iOutput"]].getValue()) == 1)
	{
		if (history_variable[hv["Time step number"]].getFinalValue() == 0)
		{
			for (std::vector<HistoryVariable>::size_type i = 0; i != history_variable.size(); ++i)
			{
				if (history_variable[i].getOutput())
					output_file << history_variable[i].getName() << " " << history_variable[i].getUOM() << "\t";
			}
			for (std::vector<SciantixVariable>::size_type i = 0; i != sciantix_variable.size(); ++i)
			{
				if (sciantix_variable[i].getOutput())
					output_file << sciantix_variable[i].getName() << " " << sciantix_variable[i].getUOM() << "\t";
			}
			output_file << "\n";
		}

		if ((int)history_variable[hv["Time step number"]].getFinalValue() % 1 == 0)
		{
			for (std::vector<HistoryVariable>::size_type i = 0; i != history_variable.size(); ++i)
			{
				if (history_variable[i].getOutput())
					output_file << std::setprecision(10) << history_variable[i].getFinalValue() << "\t";
			}

			for (std::vector<SciantixVariable>::size_type i = 0; i != sciantix_variable.size(); ++i)
			{
				if (sciantix_variable[i].getOutput())
					output_file << std::setprecision(7) << sciantix_variable[i].getFinalValue() << "\t";
			}
			output_file << "\n";
		}
	}

	/**
	 * @brief iOutput = 2 prints the complete output.exe file
	 * 
	 */
	else if ((int)input_variable[iv["iOutput"]].getValue() == 2)
	{
		if (history_variable[hv["Time step number"]].getFinalValue() == 0)
		{
			for (std::vector<HistoryVariable>::size_type i = 0; i != history_variable.size(); ++i)
			{
				// if (history_variable[i].getOutput())
					output_file << history_variable[i].getName() << " " << history_variable[i].getUOM() << "\t";
			}
			for (std::vector<SciantixVariable>::size_type i = 0; i != sciantix_variable.size(); ++i)
			{
				// if (sciantix_variable[i].getOutput())
					output_file << sciantix_variable[i].getName() << " " << sciantix_variable[i].getUOM() << "\t";
			}
			output_file << "\n";
		}

		if ((int)history_variable[hv["Time step number"]].getFinalValue() % 1 == 0)
		{
			for (std::vector<HistoryVariable>::size_type i = 0; i != history_variable.size(); ++i)
			{
				// if (history_variable[i].getOutput())
					output_file << std::setprecision(10) << history_variable[i].getFinalValue() << "\t";
			}

			for (std::vector<SciantixVariable>::size_type i = 0; i != sciantix_variable.size(); ++i)
			{
				// if (sciantix_variable[i].getOutput())
					output_file << std::setprecision(7) << sciantix_variable[i].getFinalValue() << "\t";
			}
			output_file << "\n";
		}
	}

	


	output_file.close();

	/**
	 * ### Writing: overview.txt
	 * 
	 */
	std::string overview_name = "overview.txt";

	if (history_variable[hv["Time step number"]].getFinalValue() == 0 && if_exist(overview_name))
		remove(overview_name.c_str()); // from string to const char*

	std::fstream overview_file;
	if (history_variable[hv["Time step number"]].getFinalValue() == 0 && !if_exist(overview_name))
	{
		overview_file.open(overview_name, std::fstream::in | std::fstream::out | std::fstream::app);

		for (std::vector<Model>::size_type i = 0; i != int(model.size()); ++i)
		{
			overview_file << "Model #" << i << ": " << "\t";
			overview_file << model[i].getName() << "\t";
			overview_file << model[i].getRef() << "\n";
		}

		overview_file << "\n";

		/**
		 * Printing the matrix
		 */
		for (std::vector<Matrix>::size_type i = 0; i != int(matrix.size()); ++i)
		{
			overview_file << "Matrix #" << i << ": " << "\t";
			overview_file << matrix[i].getName() << "\t";
			overview_file << matrix[i].getRef() << "\n";
		}

		overview_file << "\n";

		for (std::vector<System>::size_type i = 0; i != int(sciantix_system.size()); ++i)
		{
			overview_file << "System #" << i << ": " << "\t";
			overview_file << sciantix_system[i].getName() << "\t";
			overview_file << sciantix_system[i].getRef() << "\n";
		}

		overview_file << "\n";

		for (std::vector<InputVariable>::size_type i = 0; i != int(input_variable.size()); ++i)
		{
			overview_file << "Input setting #" << i << ": " << "\t";
			overview_file << input_variable[i].getName() << " = ";
			overview_file << input_variable[i].getValue() << "\n";
		}

		overview_file << "\n";

		// for (std::vector<SciantixVariable>::size_type i = 0; i != sciantix_variable.size(); ++i)
		// {
		// 	overview_file << "Sciantix variable #" << i << ": " << "\t";
		// 	overview_file << sciantix_variable[i].getName() << " ";
		// 	overview_file << sciantix_variable[i].getUOM() << "\n";
		// }
	}
	overview_file.close();
}
