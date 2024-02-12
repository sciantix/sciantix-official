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

namespace ErrorMessages
{
	/**
	 * @brief ErrorMessages is a namespace that contains the possibile error messages that a user can produce by providing wrong inputs to SCIANTIX.
	 */

	// Put error file name into constant to avoid repetition
	const std::string Error_file_name = "error_log.txt";

	void MissingInputFile(const char* missing_file)
	{
		/**
		 * @brief This function prints an error_log.txt file and STDERR when an input file is does not exist.
		 * 
		 */
		std::string error_message = "ERROR: Missing input file '" + (std::string)missing_file + "' \n";
		error_message += "Please check that such file exists in the current working directory.\n";
		error_message +="Execution aborted\n";

		// Write error message to the error log
		std::ofstream Error_log(Error_file_name, std::ios::out);
		Error_log << error_message << std::endl;
		
        // And write it to STDERR as well
		std::cerr << error_message;
		exit(1);
	}

	void Switch(std::string routine, std::string variable_name, int variable)
	{
		/**
		 * @brief This function prints an error_log.txt file when an input setting is out of the acceptable range of values.
		 * 
		 */
		std::ofstream Error_log(Error_file_name, std::ios::out);
		Error_log << "Error in " << routine << "." << std::endl;
		Error_log << "The input setting " << variable_name << " = " << variable << " is out of range." << std::endl;
		exit(1);
	}
}
