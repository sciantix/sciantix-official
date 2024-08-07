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

#include "ErrorMessages.h"
#include "Global.h"

namespace ErrorMessages
{

	// Put error file name into constant to avoid repetition
	const std::string Error_file_name = "error_log.txt";
	std::stringstream errorMessages;

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

	void writeErrorLog()
	{
		std::ofstream Error_log(TestPath + Error_file_name, std::ios::app);
		Error_log << errorMessages.str();
		errorMessages.str("");
	}
}
