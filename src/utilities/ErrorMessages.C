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
//  Authors: D. Pizzocri, T. Barani.                                                //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "ErrorMessages.h"
#include "Global.h"
#include <map>
#include "MainVariables.h"
#include <cstdlib>

namespace ErrorMessages
{

	// Put error file name into constant to avoid repetition
	const std::string Error_file_name = "error_log.txt";
	const std::string Warning_file_name = "warning_log.txt";
	std::stringstream errorMessages;
	std::map<std::string, std::pair<int, double>> exceedances;

	void clearWarningLog() {
        std::ofstream Warning_log(TestPath + Warning_file_name, std::ios::out);
        // Opening the file in std::ios::out mode without std::ios::app clears the file
        if (Warning_log.is_open()) {
            Warning_log.close();
        }
    }

	void MissingInputFile(const char *missing_file)
	{
		std::string error_message = "ERROR: Missing input file '" + (std::string)missing_file + "' \n";
		error_message += "Please check that such file exists in the current working directory.\n";
		error_message += "Execution aborted\n";

		// Write error message to the error log
		std::ofstream Error_log(TestPath + Error_file_name, std::ios::out);
		Error_log << error_message << std::endl;

		// And write it to STDERR as well
		std::cerr << error_message;
		exit(1);
	}

	void Switch(std::string routine, std::string variable_name, int variable)
	{
		errorMessages << "Warning in " << routine << "." << std::endl;
		errorMessages << "The input setting " << variable_name << " = " << variable << " is out of range." << std::endl;
	}

	void errorBounds(std::string variable_name, double value, double excess)
	{
		std::ofstream error_log(TestPath + Error_file_name, std::ios::app);
		switch(Sciantix_options[22]) {
			case 0:
				break;
			case 1:
				error_log << "Upper bound of Sciantix variable \"" << variable_name << "\" has been exceeded in "
							"with the deviation being " << excess << "." << std::endl;
				// we have to leave the program here
				std::exit(1);
			case 2:
				if (exceedances.find(variable_name) == exceedances.end()) {
					exceedances[variable_name] = {1, excess};
				} else {
					exceedances[variable_name].first += 1;
					if (exceedances[variable_name].second < excess) {
						exceedances[variable_name].second = excess;
					}
				}
				break;
			default:
				break;
		}
	}

	void writeErrorLog()
	{
		clearWarningLog();
		std::ofstream warning_log(TestPath + Warning_file_name, std::ios::app);
        for (const auto& entry : exceedances) {
            warning_log << "Upper bound of Sciantix variable \"" << entry.first << "\" has been exceeded in "
                      << entry.second.first << " instances, with the maximum deviation being " << entry.second.second << "." << std::endl;
        }
	}
}
