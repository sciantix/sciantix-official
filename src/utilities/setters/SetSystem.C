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

#include "SetSystem.h"

/// SetSystem

void setSystem(SciantixArray<System> &system, SciantixArray<Matrix> matrix, SciantixArray<InputVariable> &input_variable)
{
    switch ((int)input_variable["iFuelMatrix"].getValue())
    {
        case 0:
            system.push(Xe_in_UO2(matrix, input_variable));
            system.push(Kr_in_UO2(matrix, input_variable));
            system.push(He_in_UO2(matrix, input_variable));
            system.push(Xe133_in_UO2(matrix, input_variable));
            system.push(Kr85m_in_UO2(matrix, input_variable));
            break;

        case 1:
            system.push(Xe_in_UO2(matrix, input_variable));
            system.push(Xe_in_UO2HBS(matrix, input_variable));
            break;

        default:
            break;
    }
}




System Xe_in_UO2(SciantixArray matrix, SciantixArray input_variable)
{
    // Error handling
	if (matrix.empty() || input_variable.empty() || sma.find("UO2HBS") == sma.end())
	{
		std::cerr << "Error: Required components are not initialized in " << __FILE__  << std::endl;
		return;
	}

    System result;

    result.setName("Xe in UO2");
	result.setGasName("Xe");
	result.setMatrixName("UO2");
	result.setRestructuredMatrix(0);
	result.setYield(0.24);
	result.setRadiusInLattice(0.21e-9); // (m), from experimental data, assumed equal for Xe and Kr
	result.setVolumeInLattice(matrix[sma["UO2"]].getSchottkyVolume());
	result.setHenryConstant(0.0);
	result.setProductionRate(1);
	result.setFissionGasDiffusivity(int(input_variable["iFGDiffusionCoefficient"].getValue()));
	result.setBubbleDiffusivity(int(input_variable["iBubbleDiffusivity"].getValue()));
	result.setResolutionRate(int(input_variable["iResolutionRate"].getValue()));
	result.setTrappingRate(int(input_variable["iTrappingRate"].getValue()));
	result.setNucleationRate(int(input_variable["iNucleationRate"].getValue()));

    return result;
}

System Xe_in_UO2HBS(SciantixArray matrix, SciantixArray input_variable)
{
    // Error handling
    if (matrix.empty() || input_variable.empty() || sma.find("UO2HBS") == sma.end())
    {
        std::cerr << "Error: Required components are not initialized in " << __FILE__  << std::endl;
        return;
    }

    System result;

    result.setName("Xe in UO2HBS");
	result.setGasName("Xe");
	result.setMatrixName("UO2HBS");
	result.setRestructuredMatrix(1);
	result.setYield(0.24);
	result.setRadiusInLattice(0.21e-9);
	result.setVolumeInLattice(matrix[sma["UO2HBS"]].getSchottkyVolume());
	result.setHenryConstant(0.0);
	result.setProductionRate(5);
	result.setFissionGasDiffusivity(5);
	result.setBubbleDiffusivity(0);
	result.setResolutionRate(99);
	result.setTrappingRate(99);
	result.setNucleationRate(99);

    return result;
}

System Kr_in_UO2(SciantixArray matrix, SciantixArray input_variable)
{
    // Error handling
	if (matrix.empty() || input_variable.empty() || sma.find("UO2") == sma.end())
	{
		std::cerr << "Error: Required components are not initialized in " << __FILE__  << std::endl;
		return;
	}

    System result;

    result.setName("Kr in UO2");
	result.setGasName("Kr");
	result.setMatrixName("UO2");
	result.setRestructuredMatrix(0);
	result.setYield(0.03);
	result.setRadiusInLattice(0.21e-9);
	result.setVolumeInLattice(matrix[sma["UO2"]].getSchottkyVolume());
	result.setHenryConstant(0.0);
	result.setProductionRate(1);
	result.setFissionGasDiffusivity(int(input_variable[iv["iFGDiffusionCoefficient"]].getValue()));
	result.setBubbleDiffusivity(int(input_variable[iv["iBubbleDiffusivity"]].getValue()));
	result.setResolutionRate(int(input_variable[iv["iResolutionRate"]].getValue()));
	result.setTrappingRate(int(input_variable[iv["iTrappingRate"]].getValue()));
	result.setNucleationRate(int(input_variable[iv["iNucleationRate"]].getValue()));

    return result;
}

System He_in_UO2(SciantixArray matrix, SciantixArray input_variable)
{
    // Error handling
	if (matrix.empty() || input_variable.empty() || sma.find("UO2") == sma.end())
	{
		std::cerr << "Error: Required components are not initialized in " << __FILE__  << std::endl;
		return;
	}

    System result;

    result.setName("He in UO2");
	result.setGasName("He");
	result.setMatrixName("UO2");
	result.setRestructuredMatrix(0);
	result.setYield(0.0022); // from ternary fissions
	result.setRadiusInLattice(4.73e-11);
	result.setVolumeInLattice(matrix[sma["UO2"]].getOIS());
	result.setHeliumDiffusivity(int(input_variable[iv["iHeDiffusivity"]].getValue()));
	result.setResolutionRate(int(input_variable[iv["iResolutionRate"]].getValue()));
	result.setTrappingRate(int(input_variable[iv["iTrappingRate"]].getValue()));
	result.setNucleationRate(int(input_variable[iv["iNucleationRate"]].getValue()));
	result.setHenryConstant(4.1e+18 * exp(-7543.5 / history_variable[hv["Temperature"]].getFinalValue())); /// The Henry's constant for helium in UO<sub>2</sub>-single crystal samples is set from best estimate correlation after @ref *L. Cognini et al. Nuclear Engineering and Design 340 (2018) 240–244*. This correlation is valid in the temperature range 1073-1773 K.
	result.setProductionRate(int(input_variable[iv["iHeliumProductionRate"]].getValue()));
	result.setBubbleDiffusivity(int(input_variable[iv["iBubbleDiffusivity"]].getValue()));

    return result;
}

System Xe133_in_UO2(SciantixArray matrix, SciantixArray input_variable)
{
    // Error handling
	if (matrix.empty() || input_variable.empty() || sma.find("UO2") == sma.end())
	{
		std::cerr << "Error: Required components are not initialized in " << __FILE__  << std::endl;
		return;
	}

    System result;

    result.setName("Xe133 in UO2");
	result.setGasName("Xe133");
	result.setMatrixName("UO2");
	result.setRestructuredMatrix(0);
	result.setYield(0.066534); // from JEFF-3.3 library
	result.setRadiusInLattice(0.21e-9); // (m), number from experimental results, assumed equal for Xe and Kr
	result.setVolumeInLattice(matrix[sma["UO2"]].getSchottkyVolume());
	result.setHenryConstant(0.0);
	result.setProductionRate(1);
	result.setFissionGasDiffusivity(int(input_variable[iv["iFGDiffusionCoefficient"]].getValue()));
	result.setBubbleDiffusivity(int(input_variable[iv["iBubbleDiffusivity"]].getValue()));
	result.setResolutionRate(int(input_variable[iv["iResolutionRate"]].getValue()));
	result.setTrappingRate(int(input_variable[iv["iTrappingRate"]].getValue()));
	result.setNucleationRate(int(input_variable[iv["iNucleationRate"]].getValue()));

    return result;
}

System Kr85m_in_UO2(SciantixArray matrix, SciantixArray input_variable)
{
    System result;

    result.setName("Kr85m in UO2");
	result.setGasName("Kr85m");
	result.setMatrixName("UO2");
	result.setRestructuredMatrix(0);
	result.setYield(0.013027);
	result.setRadiusInLattice(0.21e-9);
	result.setVolumeInLattice(matrix[sma["UO2"]].getSchottkyVolume());
	result.setHenryConstant(0.0);
	result.setProductionRate(1);
	result.setFissionGasDiffusivity(int(input_variable[iv["iFGDiffusionCoefficient"]].getValue()));
	result.setBubbleDiffusivity(int(input_variable[iv["iBubbleDiffusivity"]].getValue()));
	result.setResolutionRate(int(input_variable[iv["iResolutionRate"]].getValue()));
	result.setTrappingRate(int(input_variable[iv["iTrappingRate"]].getValue()));
	result.setNucleationRate(int(input_variable[iv["iNucleationRate"]].getValue()));

    return result;
}