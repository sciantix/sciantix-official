#ifndef SCIANTIX_UN_BUBBLE_PHYSICS_H
#define SCIANTIX_UN_BUBBLE_PHYSICS_H
#include "UNParameters.h"
#include <utility>
namespace un_l3
{
double matrixVolume(const Parameters& p);
double sphereVolume(double radius);
double radiusFromVolume(double volume);
double phiPopulation(double gas, double number);
double coalescenceLambda(double volume,double number);
double internalPressure(const Parameters& p,double gas,double vacancies);
double equilibriumPressure(const Parameters& p,double radius);
std::pair<double,double> pressureSeed(const Parameters& p,double atoms_per_bubble,double factor);
std::pair<double,double> seedVacancyInventory(const Parameters& p,double gas,double number,double factor);
double wignerSeitzDelta(double number);
double zetaGeometry(const PhysicsOptions& o,double radius,double number);
struct VacancyStep { double inventory, rate; };
VacancyStep advanceVacancies(const Parameters& p,const PhysicsOptions& o,double dv,double radius,double number,double gas,double old,double dt);
}
#endif
