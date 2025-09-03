#ifndef MATRIX_H
#define MATRIX_H

#include <cmath>
#include "Material.h"
#include "Constants.h"
#include "ErrorMessages.h"
#include "SciantixArray.h"
#include "SciantixVariable.h"

class Matrix : virtual public Material
{
public:
    double GrainBoundaryMobility;
    double GrainBoundaryVacancyDiffusivity;
    double PoreNucleationRate;
    double PoreResolutionRate;
    double PoreTrappingRate;
    double ChromiaPrecipitate;
    double ChromiaSolution;
    double ChromiumPrecipitate;
    double ChromiumSolution;
    double ChromiaSolubility;
    double ChromiumSolubility;
    double ChromiumContent;
    double HealingTemperatureThreshold;
    double GrainRadius;
    double NucleationRate;
    double LenticularShapeFactor;
    double GrainBoundaryThickness;
    double SemidihedralAngle;
    double FissionFragmentInfluenceRadius;
    double FissionFragmentRange;
    double OctahedralInterstitialSite;
    double SchottkyVolume;
    double SurfaceTension;
    double LatticeParameter;
    double TheoreticalDensity;

    void setGrainBoundaryMobility(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices);
    void setGrainBoundaryVacancyDiffusivity(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices);
    void setPoreNucleationRate(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices);
    void setPoreResolutionRate(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices);
    void setPoreTrappingRate(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices);

    double getGrainBoundaryMobility()
    {
        return GrainBoundaryMobility;
    }

    double getGrainBoundaryVacancyDiffusivity()
    {
        return GrainBoundaryVacancyDiffusivity;
    }

    double getPoreNucleationRate()
    {
        return PoreNucleationRate;
    }

    double getPoreResolutionRate()
    {
        return PoreResolutionRate;
    }

    double getPoreTrappingRate()
    {
        return PoreTrappingRate;
    }

    void setChromiaPrecipitate(double val)
    {
        ChromiaPrecipitate = val;
    }

    double getChromiaPrecipitate()
    {
        return ChromiaPrecipitate;
    }

    void setChromiaSolution(double val)
    {
        ChromiaSolution = val;
    }

    double getChromiaSolution()
    {
        return ChromiaSolution;
    }

    void setChromiumPrecipitate(double val)
    {
        ChromiumPrecipitate = val;
    }

    double getChromiumPrecipitate()
    {
        return ChromiumPrecipitate;
    }

    void setChromiumSolution(double val)
    {
        ChromiumSolution = val;
    }

    double getChromiumSolution()
    {
        return ChromiumSolution;
    }

    void setChromiaSolubility(double val)
    {
        ChromiaSolubility = val;
    }

    double getChromiaSolubility()
    {
        return ChromiaSolubility;
    }

    void setChromiumSolubility(double val)
    {
        ChromiumSolubility = val;
    }

    double getChromiumSolubility()
    {
        return ChromiumSolubility;
    }

    void setChromiumContent(double val)
    {
        ChromiumContent = val;
    }

    double getChromiumContent()
    {
        return ChromiumContent;
    }

    void setHealingTemperatureThreshold(double val)
    {
        HealingTemperatureThreshold = val;
    }

    double getHealingTemperatureThreshold()
    {
        return HealingTemperatureThreshold;
    }

    void setGrainRadius(double val)
    {
        GrainRadius = val;
    }

    double getGrainRadius()
    {
        return GrainRadius;
    }

    void setNucleationRate(double val)
    {
        NucleationRate = val;
    }

    double getNucleationRate()
    {
        return NucleationRate;
    }

    void setLenticularShapeFactor(double val)
    {
        LenticularShapeFactor = val;
    }

    double getLenticularShapeFactor()
    {
        return LenticularShapeFactor;
    }

    void setGrainBoundaryThickness(double val)
    {
        GrainBoundaryThickness = val;
    }

    double getGrainBoundaryThickness()
    {
        return GrainBoundaryThickness;
    }

    void setSemidihedralAngle(double val)
    {
        SemidihedralAngle = val;
    }

    double getSemidihedralAngle()
    {
        return SemidihedralAngle;
    }

    void setFissionFragmentInfluenceRadius(double val)
    {
        FissionFragmentInfluenceRadius = val;
    }

    double getFissionFragmentInfluenceRadius()
    {
        return FissionFragmentInfluenceRadius;
    }

    void setFissionFragmentRange(double val)
    {
        FissionFragmentRange = val;
    }

    double getFissionFragmentRange()
    {
        return FissionFragmentRange;
    }

    void setOctahedralInterstitialSite(double val)
    {
        OctahedralInterstitialSite = val;
    }

    double getOctahedralInterstitialSite()
    {
        return OctahedralInterstitialSite;
    }

    void setSchottkyVolume(double val)
    {
        SchottkyVolume = val;
    }

    double getSchottkyVolume()
    {
        return SchottkyVolume;
    }

    void setSurfaceTension(double val)
    {
        SurfaceTension = val;
    }

    double getSurfaceTension()
    {
        return SurfaceTension;
    }

    void setLatticeParameter(double val)
    {
        LatticeParameter = val;
    }

    double getLatticeParameter()
    {
        return LatticeParameter;
    }

    void setTheoreticalDensity(double val)
    {
        TheoreticalDensity = val;
    }

    double getTheoreticalDensity()
    {
        return TheoreticalDensity;
    }

    /**
     * @brief Constructor
     */
    Matrix() {}

    /**
     * @brief Destructor
     */
    ~Matrix() {}
}; // class Matrix

#endif // MATRIX_H
