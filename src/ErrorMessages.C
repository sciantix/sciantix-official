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

	void Switch(std::string routine, std::string variable_name, int variable)
	{
		/**
		 * @brief This function prints an error_log.txt file when an input setting is out of the acceptable range of values.
		 * 
		 */
		std::ofstream Error_log;
		Error_log.open("error_log.txt", std::ios::out);
		Error_log << "Error in " << routine << "." << std::endl;
		Error_log << "The input setting " << variable_name << " = " << variable << " is out of range." << std::endl;
		exit(1);
	}
}
