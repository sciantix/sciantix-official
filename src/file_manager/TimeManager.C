#include "TimeManager.h"
#include <fstream>
#include <iostream>
#include <iomanip>
#include <limits>

void readFirstColumn(const std::string& filename, std::vector<double>& first_column) {
    std::ifstream input_file(filename);
    if (!input_file.is_open()) {
        std::cerr << "Could not open the file for reading!" << std::endl;
        return;
    }

    double col1;
    while (input_file >> col1) {
        first_column.push_back(col1);
        input_file.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    }
}

void interpolateTimes(const std::vector<double>& time_vector,
                      std::vector<double>& interpolated_times, double num_bins) {
    if (time_vector.size() < 2) {
        std::cerr << "Error: time_vector must contain at least two values!" << std::endl;
        return;
    }

    double step_size = (time_vector[1] - time_vector[0]) / num_bins;
    interpolated_times.clear();
    for (int i = 0; i <= num_bins; ++i)
        interpolated_times.push_back(time_vector[0] + i * step_size);
}

void writeInterpolatedTimes(const std::string& filename,
                            const std::vector<double>& time,
                            const std::vector<double>& interpolated_times) {
    if (time.size() != interpolated_times.size()) {
        std::cerr << "Error: vectors must have the same size!" << std::endl;
        return;
    }

    std::ofstream output_file(filename);
    if (!output_file.is_open()) {
        std::cerr << "Could not open the file for writing!" << std::endl;
        return;
    }

    output_file << "Time (h)\tManufactured Solution (at/m³)\n";
    for (size_t i = 0; i < time.size(); ++i)
        output_file << std::setprecision(12) << std::fixed
                    << time[i] << "\t" << interpolated_times[i] << "\n";
}
