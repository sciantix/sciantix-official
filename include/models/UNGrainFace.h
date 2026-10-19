#ifndef SCIANTIX_UN_GRAIN_FACE_H
#define SCIANTIX_UN_GRAIN_FACE_H
#include "UNParameters.h"
#include "UNState.h"
#include <array>
namespace un_l3
{
double grainFaceVolume(const PhysicsOptions& o,double radius);
double grainFaceRadius(const PhysicsOptions& o,double volume);
double grainFaceArea(const PhysicsOptions& o,double radius);
double grainFaceZeta(double coverage);
void refreshGrainFace(const Parameters& p,const PhysicsOptions& o,GrainFaceState& state);
GrainFaceState initializeGrainFace(const Parameters& p,const PhysicsOptions& o);
double grainFaceLimitedGrowth(double initial,double growth,double equilibrium,double dt);
std::array<double,3> solveGrainFace3x3(double v0,double a0,double n0,double fc0,double gasvol0,double gasvol1,
    double delta_nv,double omega,double kappa,bool saturated);
void advanceGrainFace(const Parameters& p,const PhysicsOptions& o,GrainFaceState& state,double gas_in,double dt,double bulk_vacancy_first);
}
#endif
