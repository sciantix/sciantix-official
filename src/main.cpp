#include "EquationExtractor.h"
#include "LatexTranslator.h"
#include "CorrelationProcessor.h"
#include "CorrelationSelector.h" 
#include "OutputGenerator.h"

#include <iostream>
#include <string> 

int main() {

    extractAllEquations();
    processLatexEquations("output/equations.txt", "output/translated_equations.txt");
    getAllCorrelations("output/translated_equations.txt", "output/correlations_parsed.txt");

    auto settings = parseSettingsFile("input/settings_correlation.txt");
    writeSelectedCorrelations("output/correlations_parsed.txt", settings, "output/Selected_correlation.txt");

    std::string variableMappingFile = "input/variable_mapping.txt";
    generateOutputsFromSelected("output/Selected_correlation.txt", variableMappingFile);

    return 0;
}