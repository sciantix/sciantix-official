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

#include "ANN_gas_production.h"

//the following function is an ANN that reproduces the results of the bateman equation systems for the production of helium in a MOX fuel.
//(pure PuO2 or UO2 can be simulated too, given the right training for the ANN and hence the right input parameters).
//The net is a one layer feedforward one with one input and one output hence the input-hidden and hidden-output weights vectors has the same size. 
//The same goes for the biases. In the code these connections are referred to as "first" and "second" layers which can be misleading, given that, as said,
//the net has only one hidden layer. The first layer activation function is a "tansig", the second is a pure linear one (hence it is not present in the code since it is simply y = x).

double ANN_gas_production (const double& input_maxima, 
                           const double& input_minima, 
                           const double& target_maxima, 
                           const double& target_minima, 
                           const std::vector<double>& first_layer_weights, 
                           const std::vector<double>& first_layer_bias,
                           const std::vector<double>& second_layer_weights,
                           const std::vector<double>& second_layer_bias,
                           double& input_data){

                            std::vector<double> z(first_layer_weights.size());
                            double output = 0;

                                input_data = (input_data - input_minima) * ((target_maxima - target_minima) / (input_maxima - input_minima)) + target_minima;
                                for (size_t weight_counter = 0; weight_counter < first_layer_weights.size(); ++weight_counter){

                                    z[weight_counter] = first_layer_weights[weight_counter] * input_data + first_layer_bias[weight_counter];
                                    z[weight_counter] = 2/(1 + exp(-2*z[weight_counter])) - 1; //"tansig" activation function
                                    z[weight_counter] = second_layer_weights[weight_counter] * z[weight_counter] + second_layer_bias[weight_counter];
                                    output += z[weight_counter];
                                    
                                }

                                output = (output - target_minima) * ((input_maxima - input_minima)/(target_maxima - target_minima)) + input_minima;
                                return output;

                           }