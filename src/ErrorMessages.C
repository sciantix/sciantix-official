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

/** @namespace ErrorMessages
 *  @brief Contains functions for error handling in SCIANTIX software.
 *
 *  Provides functions to log and manage error messages related to wrong user inputs or internal errors.
 */
namespace ErrorMessages
{

	// Put error file name into constant to avoid repetition
	const std::string Error_file_name = "error_log.txt";
	std::stringstream errorMessages;

	/**
	 * @brief Logs an error when an input file is missing and exits the program.
	 * @param missing_file The name of the missing input file.
	 *
	 * Writes a message to both an error log file and STDERR, then exits the program.
	 * The function assumes that the missing file is critical and thus stops execution.
	 */
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

	/**
     * @brief Accumulates a warning message about an out-of-range input setting.
     * @param routine The name of the routine where the error occurred.
     * @param variable_name The name of the variable that is out of range.
     * @param variable The value of the variable that triggered the warning.
     *
     * This function stores the error message in a stringstream to be written later,
     * thus minimizing file I/O operations during runtime.
     */
	void Switch(std::string routine, std::string variable_name, int variable)
	{
		errorMessages << "Warning in " << routine << "." << std::endl;
		errorMessages << "The input setting " << variable_name << " = " << variable << " is out of range." << std::endl;
	}

	/**
     * @brief Writes all accumulated error messages to the error log file.
     *
     * This function should be called to flush all error messages stored in the errorMessages stringstream
     * to the error log file at once. This approach minimizes disk I/O operations during runtime.
     */
	void writeErrorLog()
	{
		std::ofstream Error_log(TestPath + Error_file_name, std::ios::app);
		Error_log << errorMessages.str();
		errorMessages.str("");
	}
}
