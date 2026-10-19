#ifndef SCIANTIX_UN_L3_TRANSPORT_H
#define SCIANTIX_UN_L3_TRANSPORT_H
#include "UNParameters.h"
#include <utility>
#include <vector>

namespace un_l3
{
struct Diffusivity { double total, first, second, third, dislocation; };
struct Trapping { double bulk, dislocation, gamma, denominator, free_line, bubble_term, line_term; };
double safeExp(double x);
double lookupDiffusivity(double temperature, const std::vector<std::pair<double, double>>& table);
Diffusivity xeDiffusivity(const Parameters& p);
Diffusivity vacancyDiffusivity(const Parameters& p);
double grainBoundaryDiffusivity(const Parameters& p, const PhysicsOptions& o, double bulk_first);
std::pair<double, double> resolutionRates(const Parameters& p, double rb, double rd);
Trapping trappingRates(const Parameters& p, const PhysicsOptions& o, double dg, double rb, double nb, double rd, double nd);
double productionRate(const Parameters& p);
double nucleationRate(const Parameters& p, double dg, double concentration);
}
#endif
