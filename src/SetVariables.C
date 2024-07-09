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

#include "SetVariables.h"
#include "ConstantNumbers.h"

/// SetVariables
/// This routine builds the vectors of objects:
/// - physics_variable
/// - history_variable
/// - sciantix_variable
/// - input_variable
/// together with the diffusion modes, the maps, and the scaling factors.

void SetVariables(int Sciantix_options[], double Sciantix_history[], double Sciantix_variables[], double Sciantix_scaling_factors[], double Sciantix_diffusion_modes[])
{
	// -----------------------------------------------------------------------------------------------
	// Input variable
	// The vector is used to collect all user input settings relating to the choice of SCIANTIX models
	// -----------------------------------------------------------------------------------------------

	int iv_counter(0);
	if (input_variable.empty())
	{
		input_variable.emplace_back();
		input_variable[iv_counter].setName("iOutput");
		input_variable[iv_counter].setValue(Sciantix_options[0]);
		++iv_counter;

		input_variable.emplace_back();
		input_variable[iv_counter].setName("iOption2");
		input_variable[iv_counter].setValue(Sciantix_options[1]);
		++iv_counter;

		input_variable.emplace_back();
		input_variable[iv_counter].setName("iOption3");
		input_variable[iv_counter].setValue(Sciantix_options[2]);
		++iv_counter;

	}

	MapInputVariable();

	// bool toOutputRadioactiveFG(0);
	// if (input_variable[iv["iRadioactiveFissionGas"]].getValue() != 0) toOutputRadioactiveFG = 1;

	// bool toOutputHelium(0);
	// if (input_variable[iv["iHelium"]].getValue() != 0) toOutputHelium = 1;

	// ----------------
	// Physics variable
	// ----------------
	
	int pv_counter(0);

	physics_variable.emplace_back();
	physics_variable[pv_counter].setName("Time step"); //dTime_h  * 3600
	physics_variable[pv_counter].setUOM("(s)");
	physics_variable[pv_counter].setInitialValue(Sciantix_history[10]);
	physics_variable[pv_counter].setFinalValue(Sciantix_history[10]);
	physics_variable[pv_counter].setOutput(0);
	++pv_counter;

	// ----------------
	// History variable
	// ----------------
	int hv_counter(0);

	history_variable.emplace_back();
	history_variable[hv_counter].setName("Time");
	history_variable[hv_counter].setUOM("(h)");
	history_variable[hv_counter].setInitialValue(Sciantix_history[8]);
	history_variable[hv_counter].setFinalValue(Sciantix_history[8]);
	history_variable[hv_counter].setOutput(1);
	++hv_counter;

	history_variable.emplace_back();
	history_variable[hv_counter].setName("Time step number");
	history_variable[hv_counter].setUOM("(/)");
	history_variable[hv_counter].setInitialValue(Sciantix_history[9]);
	history_variable[hv_counter].setFinalValue(Sciantix_history[9]);
	history_variable[hv_counter].setOutput(0);
	++hv_counter;

	history_variable.emplace_back();
	history_variable[hv_counter].setName("Temperature");
	history_variable[hv_counter].setUOM("(K)");
	history_variable[hv_counter].setInitialValue(Sciantix_history[0] * Sciantix_scaling_factors[4]);
	history_variable[hv_counter].setFinalValue(Sciantix_history[1] * Sciantix_scaling_factors[4]);
	history_variable[hv_counter].setOutput(0);
	++hv_counter;

	history_variable.emplace_back();
	history_variable[hv_counter].setName("Xe release rate from fuel");
	history_variable[hv_counter].setUOM("(at/m3 s)");
	history_variable[hv_counter].setInitialValue(Sciantix_history[2]);
	history_variable[hv_counter].setFinalValue(Sciantix_history[3]);
	history_variable[hv_counter].setOutput(0);
	++hv_counter;

	history_variable.emplace_back();
	history_variable[hv_counter].setName("Xe133 release rate from fuel");
	history_variable[hv_counter].setUOM("(at/m3 s)");
	history_variable[hv_counter].setInitialValue(Sciantix_history[4]);
	history_variable[hv_counter].setFinalValue(Sciantix_history[5]);
	history_variable[hv_counter].setOutput(0);
	++hv_counter;

	history_variable.emplace_back();
	history_variable[hv_counter].setName("Kr85m release rate from fuel");
	history_variable[hv_counter].setUOM("(at/m3 s)");
	history_variable[hv_counter].setInitialValue(Sciantix_history[6]);
	history_variable[hv_counter].setFinalValue(Sciantix_history[7]);
	history_variable[hv_counter].setOutput(0);
	++hv_counter;

	history_variable.emplace_back();
	history_variable[hv_counter].setName("Defect time");
	history_variable[hv_counter].setUOM("(s)");
	history_variable[hv_counter].setInitialValue(Sciantix_history[11]);
	history_variable[hv_counter].setFinalValue(Sciantix_history[11]);
	history_variable[hv_counter].setOutput(0);
	++hv_counter;

	// history_variable.emplace_back();
	// history_variable[hv_counter].setName("Steam pressure");
	// history_variable[hv_counter].setUOM("(atm)");
	// history_variable[hv_counter].setInitialValue(Sciantix_history[6]);
	// history_variable[hv_counter].setFinalValue(Sciantix_history[7]);
	// history_variable[hv_counter].setOutput(1);
	// ++hv_counter;

	// ----------------------------------------------------------------------------
	// Sciantix variable
	// ----------------------------------------------------------------------------
	int sv_counter(0);

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Xe gap");
	sciantix_variable[sv_counter].setUOM("(at/m3)");
	sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[1]);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[1]);
	sciantix_variable[sv_counter].setOutput(1);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Xe decayed");
	sciantix_variable[sv_counter].setUOM("(at/m3)");
	sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[2]);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[2]);
	sciantix_variable[sv_counter].setOutput(0);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Xe released");
	sciantix_variable[sv_counter].setUOM("(at/s)");
	sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[3]);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[3]);
	sciantix_variable[sv_counter].setOutput(0);
	++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Xe coolant");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[1]);
	// sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[1]);
	// sciantix_variable[sv_counter].setOutput(1);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Kr produced");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[7]);
	// sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[7]);
	// sciantix_variable[sv_counter].setOutput(1);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Kr released");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[12]);
	// sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[12]);
	// sciantix_variable[sv_counter].setOutput(1);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Kr decayed");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(0);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Kr concentration gap");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(0);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Kr concentration coolant");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(0);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("He produced");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[13]);
	// sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[13]);
	// sciantix_variable[sv_counter].setOutput(toOutputHelium);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("He released");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[18]);
	// sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[18]);
	// sciantix_variable[sv_counter].setOutput(toOutputHelium);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("He decayed");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(0);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("He fractional release");
	// sciantix_variable[sv_counter].setUOM("(/)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(toOutputHelium);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("He release rate");
	// sciantix_variable[sv_counter].setUOM("(at/m3 s)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(toOutputHelium);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("He concentration gap");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(0);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("He concentration coolant");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(0);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Xe133 produced");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[48]);
	// sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[48]);
	// sciantix_variable[sv_counter].setOutput(toOutputRadioactiveFG);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Xe133 decayed");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[52]);
	// sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[52]);
	// sciantix_variable[sv_counter].setOutput(toOutputRadioactiveFG);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Xe133 released");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[54]);
	// sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[54]);
	// sciantix_variable[sv_counter].setOutput(toOutputRadioactiveFG);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Xe133 R/B");
	// sciantix_variable[sv_counter].setUOM("(/)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(toOutputRadioactiveFG);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Xe133 concentration gap");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(0);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Xe133 concentration coolant");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(0);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Kr85m produced");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[57]);
	// sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[57]);
	// sciantix_variable[sv_counter].setOutput(toOutputRadioactiveFG);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Kr85m decayed");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[61]);
	// sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[61]);
	// sciantix_variable[sv_counter].setOutput(toOutputRadioactiveFG);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Kr85m released");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[63]);
	// sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[63]);
	// sciantix_variable[sv_counter].setOutput(toOutputRadioactiveFG);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Kr85m R/B");
	// sciantix_variable[sv_counter].setUOM("(/)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(toOutputRadioactiveFG);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Kr85m concentration gap");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(0);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Kr85m concentration coolant");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(0);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Hydrogen produced");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(toOutputRadioactiveFG);
	// ++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Hydrogen released");
	// sciantix_variable[sv_counter].setUOM("(at/m3)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(toOutputRadioactiveFG);
	// ++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Release to coolant");
	sciantix_variable[sv_counter].setUOM("at/s");
	sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[4]);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[4]);
	sciantix_variable[sv_counter].setOutput(0);
	++sv_counter;

	// sciantix_variable.emplace_back();
	// sciantix_variable[sv_counter].setName("Fission gas release");
	// sciantix_variable[sv_counter].setUOM("(/)");
	// sciantix_variable[sv_counter].setInitialValue(0.0);
	// sciantix_variable[sv_counter].setFinalValue(0.0);
	// sciantix_variable[sv_counter].setOutput(1);
	// ++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Gap pressure");
	sciantix_variable[sv_counter].setUOM("(MPa)");
	sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[5]);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[6]);
	sciantix_variable[sv_counter].setOutput(1);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Gap volume");
	sciantix_variable[sv_counter].setUOM("(m3)");
	sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[7]);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[7]);
	sciantix_variable[sv_counter].setOutput(0);
	++sv_counter;

	

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("H gap");
	sciantix_variable[sv_counter].setUOM("(at/m3)");
	sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[8]);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[9]);
	sciantix_variable[sv_counter].setOutput(1);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("H production rate");
	sciantix_variable[sv_counter].setUOM("(at/m3/s)");
	sciantix_variable[sv_counter].setInitialValue(2.34e21);
	sciantix_variable[sv_counter].setFinalValue(2.34e21);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("H release rate");
	sciantix_variable[sv_counter].setUOM("(at/m3/s)");
	sciantix_variable[sv_counter].setInitialValue(2.34e23);
	sciantix_variable[sv_counter].setFinalValue(2.34e23);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Gas in gap");
	sciantix_variable[sv_counter].setUOM("(at/m3/s)");
	sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[10]);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[11]);
	sciantix_variable[sv_counter].setOutput(1);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Xe133 gap");
	sciantix_variable[sv_counter].setUOM("(at/m3)");
	sciantix_variable[sv_counter].setInitialValue(0.0);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[12]);
	sciantix_variable[sv_counter].setOutput(1);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Xe133 decayed");
	sciantix_variable[sv_counter].setUOM("(at/m3)");
	sciantix_variable[sv_counter].setInitialValue(0.0);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[13]);
	sciantix_variable[sv_counter].setOutput(0);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Xe133 released");
	sciantix_variable[sv_counter].setUOM("(at/s)");
	sciantix_variable[sv_counter].setInitialValue(0.0);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[14]);
	sciantix_variable[sv_counter].setOutput(0);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Cs133 gap");
	sciantix_variable[sv_counter].setUOM("(at/m3)");
	sciantix_variable[sv_counter].setInitialValue(0.0);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[15]);
	sciantix_variable[sv_counter].setOutput(1);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Cs133 decayed");
	sciantix_variable[sv_counter].setUOM("(at/m3)");
	sciantix_variable[sv_counter].setInitialValue(0.0);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[16]);
	sciantix_variable[sv_counter].setOutput(0);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Cs133 released");
	sciantix_variable[sv_counter].setUOM("(at/s)");
	sciantix_variable[sv_counter].setInitialValue(0.0);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[17]);
	sciantix_variable[sv_counter].setOutput(0);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("He gap");
	sciantix_variable[sv_counter].setUOM("(at/m3)");
	sciantix_variable[sv_counter].setInitialValue(Sciantix_variables[18]);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[18]);
	sciantix_variable[sv_counter].setOutput(1);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("He released");
	sciantix_variable[sv_counter].setUOM("(at/m3)");
	sciantix_variable[sv_counter].setInitialValue(0.0);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[19]);
	sciantix_variable[sv_counter].setOutput(1);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Kr85m gap");
	sciantix_variable[sv_counter].setUOM("(at/m3)");
	sciantix_variable[sv_counter].setInitialValue(0.0);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[20]);
	sciantix_variable[sv_counter].setOutput(1);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Kr85m decayed");
	sciantix_variable[sv_counter].setUOM("(at/m3)");
	sciantix_variable[sv_counter].setInitialValue(0.0);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[21]);
	sciantix_variable[sv_counter].setOutput(0);
	++sv_counter;

	sciantix_variable.emplace_back();
	sciantix_variable[sv_counter].setName("Kr85m released");
	sciantix_variable[sv_counter].setUOM("(at/s)");
	sciantix_variable[sv_counter].setInitialValue(0.0);
	sciantix_variable[sv_counter].setFinalValue(Sciantix_variables[22]);
	sciantix_variable[sv_counter].setOutput(0);
	++sv_counter;




	// ------------------------------------------------------------------------------------------------
	// ------------------------------------------------------------------------------------------------

	// ---------------
	// Scaling factors
	// ---------------
	sf_temperature = Sciantix_scaling_factors[1];
	sf_cent_parameter = Sciantix_scaling_factors[2];
	sf_helium_production_rate = Sciantix_scaling_factors[3];
	sf_dummy = Sciantix_scaling_factors[4];

	// ----
	// Maps
	// ----
	MapHistoryVariable();
	MapSciantixVariable();
	MapPhysicsVariable();
}
