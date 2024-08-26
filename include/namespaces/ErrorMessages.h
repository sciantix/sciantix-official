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
//  Year: 2023                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include <string>
#include <iostream>
#include <cstdlib>
#include <iomanip>
#include <fstream>
#include "MainVariables.h"

/**
 *  @brief Contains functions for error handling in SCIANTIX software.
 *
 *  Provides functions to log and manage error messages related to wrong user inputs or internal errors.
 * 
 * @author D. Pizzocri
 * @author T. Barani 
 * @author G. Zullo
 * @author rzehumat
 * @author F. Bastien
 * 
 */
namespace ErrorMessages
{
    /**
     * @brief Logs an error when an input file is missing and exits the program.
     * @param missing_file The name of the missing input file.
     *
     * Writes a message to both an error log file and STDERR, then exits the program.
     * The function assumes that the missing file is critical and thus stops execution.
     */
    void MissingInputFile(const char* missing_file);

    /**
     * @brief Accumulates a warning message about an out-of-range input setting.
     * @param routine The name of the routine where the error occurred.
     * @param variable_name The name of the variable that is out of range.
     * @param variable The value of the variable that triggered the warning.
     *
     * This function stores the error message in a stringstream to be written later,
     * thus minimizing file I/O operations during runtime.
     */
    void Switch(std::string routine, std::string variable_name, int variable);

    /**
     * @brief Writes all accumulated error messages to the error log file.
     *
     * This function should be called to flush all error messages stored in the errorMessages stringstream
     * to the error log file at once. This approach minimizes disk I/O operations during runtime.
     */
    void writeErrorLog();
};
