#pragma once
#include <string>
#include <map>
#include <vector>
#include "CorrelationProcessor.h" 

std::map<std::string, int> parseSettingsFile(const std::string& filename);


void writeSelectedCorrelations(
    const std::string& allCorrsFile,
    const std::map<std::string, int>& settings,
    const std::string& outputSelectionFile
);