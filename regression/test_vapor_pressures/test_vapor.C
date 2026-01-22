#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>

// constant
const double R = 8.314;

// pressures calculation
void calculate_pressures(double T, double q, double om, std::ofstream &file) {
    
    // Kato's equation NEA: p_o2
    double target_om = om;
    double log_low = -80.0;
    double log_high = 10.0;
    double log_mid = 0.0;
    double p_o2 = 0.0;

    // bisection method
    for (int i = 0; i < 100; i++) {
        log_mid = (log_low + log_high) / 2.0;
        double current_po2 = std::pow(10.0, log_mid);
        double ln_po2 = std::log(current_po2);

        double log_v1 = -5.0 * ((44.0 + 55.8 * q) / R - 376000.0 / (R * T) - 0.5 * ln_po2);
        double log_v2 = -5.0 * (0.5 * (68.8 + 131.3 * q) / R - 0.5 * 515000.0 / (R * T) - 0.25 * ln_po2);
        double log_v3 = -5.0 * (((1.0/3.0) * (std::log(2.0) + (153.5 - 96.5 * q + 331.0 * std::pow(q, 2)) / R - 891000.0 / (R * T))) - (1.0/3.0) * ln_po2);
        double log_v4 = -5.0 * std::log(0.5 * q);

        double max_log = std::max({log_v1, log_v2, log_v3, log_v4});
        double sum_exp = std::exp(log_v1 - max_log) + std::exp(log_v2 - max_log) + std::exp(log_v3 - max_log) + std::exp(log_v4 - max_log);
        double S = std::exp(-0.2 * (max_log + std::log(sum_exp)));
        double term5 = std::exp((-22.8 - 84.5 * q) / R + 105000.0 / (R * T) + 0.5 * ln_po2);
        
        double calculated_om = 2.0 - S + term5;

        if (calculated_om > target_om) log_high = log_mid;
        else log_low = log_mid;
    }
    p_o2 = std::pow(10.0, log_mid);

    // concentrations calculation
    double n_o = 2.0;
    double K_u24 = std::exp(-(78.3e3 / T) + 13.6); // Olander constants
    double K_u46 = std::exp(-(16.4e3 / T) + 5.0);
    double K_pu34 = std::exp(-(50.1e3 / T) + 10.3);
    double K_pu23 = std::exp(-(92.5e3 / T) + 21.3);
    
    double sqrt_po2 = std::sqrt(p_o2);
    
    // U
    double denom_u = 1.0 + ((K_u24 * n_o) / sqrt_po2) + (sqrt_po2 / (K_u46 * n_o));
    double n_u4 = (1.0 - q) / denom_u;
    double n_u2 = ((K_u24 * n_o) / sqrt_po2) * n_u4;
    double n_u6 = (sqrt_po2 / (K_u46 * n_o)) * n_u4;
    double n_u = 0.0;

    // Pu
    double ratio_34 = std::sqrt((K_pu34 * n_o) / sqrt_po2);
    double ratio_23 = std::sqrt((K_pu23 * n_o) / sqrt_po2);
    double denom_pu = 1.0 + ratio_34 + (ratio_23 * ratio_34);
    double n_pu4 = q / denom_pu;
    double n_pu3 = ratio_34 * n_pu4;
    double n_pu2 = ratio_23 * n_pu3;

    // partial pressures calculation [atm]
    double K_UO = std::exp(-49500.0 / T + 11.9); 
    double K_UO2 = std::exp(-74000.0 / T + 19.9);
    double K_UO3 = std::exp(-44000.0 / T + 11.9);
    double K_PuO = std::exp(-44100.0 / T + 11.5);
    double K_PuO2= std::exp(-72500.0 / T + 18.8);
    
    double p_uo = 2.0 * K_UO * n_u2;
    double p_uo2 = 4.0 * K_UO2 * n_u4;
    double p_uo3 = 8.0 * K_UO3 * n_u6;
    double p_puo = 2.0 * K_PuO * n_pu2;
    double p_puo2 = 4.0 * K_PuO2 * n_pu4;

    // Scrittura su file
    file << T << "," << q << "," << om << "," 
         << p_o2 << "," << p_uo << "," << p_uo2 << "," << p_uo3 << "," 
         << p_puo << "," << p_puo2 << "\n";
}

int main() {
    std::ofstream file("vapor_results.csv");
    file << "T,q,OM,p_O2,p_UO,p_UO2,p_UO3,p_PuO,p_PuO2\n";

    // variation of T: from T = 1500K to 3000K
    double q_fix = 0.244;
    double om_fix = 1.98; // hypostoichiometric case
    for (double T = 1500; T <= 3000; T += 50) {
        calculate_pressures(T, q_fix, om_fix, file);
    }

    // variation of O/M
    double T_fix = 2000;
    for (double om = 1.90; om <= 2.15; om += 0.001) {
        calculate_pressures(T_fix, q_fix, om, file);
    }

    file.close();
    std::cout << "Completed test. Results in 'vapor_results.csv'" << std::endl;
    return 0;
}