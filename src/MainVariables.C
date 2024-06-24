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

#include "MainVariables.h"


clock_t timer, timer_time_step;

int    Sciantix_options[40];
double Sciantix_history[20];
double Sciantix_variables[300];
double Sciantix_scaling_factors[10];
double Sciantix_diffusion_modes[1000];

long long int Time_step_number(0);
double  Time_h(0.0), dTime_h(0.0), Time_end_h(0.0); // (h)
double  Time_s(0.0), Time_end_s(0.0); // (s)
double  Number_of_time_steps_per_interval(100);

std::ofstream Output_file;
std::ofstream Execution_file;

int Input_history_points(1000);
std::vector<double> Time_input(1000, 0.0);
std::vector<double> Temperature_input(1000, 0.0);
std::vector<double> Fissionrate_input(1000, 0.0);
std::vector<double> Hydrostaticstress_input(1000, 0.0);
std::vector<double> Steampressure_input(1000, 0.0);

void setSciantixHistory(int index, double value) {
    if (index >= 0 && index < 20) {
        Sciantix_history[index] = value;
    } else {
        throw std::out_of_range("Index out of range");
    }
}
void setSciantixHist(double sciantix[]){
    for(int i=0; i < 20 ; i++){
        Sciantix_history[i] = sciantix[i];
    }
}

// Getters
int* getSciantixOptions() {
    return Sciantix_options;
}

double* getSciantixHistory() {
    return Sciantix_history;
}

double* getSciantixVariables() {
    return Sciantix_variables;
}

double* getSciantixScalingFactors() {
    return Sciantix_scaling_factors;
}

double* getSciantixDiffusionModes() {
    return Sciantix_diffusion_modes;
}

long long int getSciantixTimeStepNumber() {
    return Time_step_number;
}

double getSciantixTimeH() {
    return Time_h;
}

double getSciantixDTimeH() {
    return dTime_h;
}

double getSciantixTimeEndH() {
    return Time_end_h;
}

double getSciantixTimeS() {
    return Time_s;
}

int getSciantixInputHistoryPoints() {
    return Input_history_points;
}

std::vector<double>& getSciantixTimeInput() {
    return Time_input;
}

std::vector<double>& getSciantixTemperatureInput() {
    return Temperature_input;
}

std::vector<double>& getSciantixFissionrateInput() {
    return Fissionrate_input;
}

std::vector<double>& getSciantixHydrostaticstressInput() {
    return Hydrostaticstress_input;
}

std::vector<double>& getSciantixSteampressureInput() {
    return Steampressure_input;
}

// Setters
void setSciantixDTimeH(double value) {
    dTime_h = value;
}

void setSciantixTimeStepNumber(long long int value) {
    Time_step_number = value;
}

void setSciantixTimeH(double value) {
    Time_h = value;
}

void setSciantixTimeS(double value) {
    Time_s = value;
}
