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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SOURCE_READER_H
#define SOURCE_READER_H

/**
 * @brief Reads the source slope value from the "source_slope.txt" file.
 * @param Source_slope_input Reference to a double where the source slope value will be stored.
 * @author A. Zayat
 */
void ReadSourceSlope(double &Source_slope_input) 
{
    // Attempt to open the file
    std::ifstream source_slope_file(TestPath + "source_slope.txt", std::ios::in);
    
    if (source_slope_file.is_open()) 
    {
        // If the file exists, read the value
        source_slope_file >> Source_slope_input;
        source_slope_file.close();
    } 
    else 
    {
        // If the file is missing, default to 0
        Source_slope_input = 0.0;
    }
}

#endif // SOURCE_READER_H