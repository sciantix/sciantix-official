#ifndef CORRELATION_PROCESSOR_H
#define CORRELATION_PROCESSOR_H

#include <string>
#include <vector>
#include <map>
#include <utility> 


struct Correlation {
    std::string property;
    std::string category;
    std::vector<std::string> variables;
    std::vector<std::string> coefficients;
    std::map<std::string, std::string> units; 
    std::vector<std::string> eqs;
    std::string title; 
    std::string moduleName; 


    Correlation(
        std::string prop = "",
        std::string cat = "",
        std::vector<std::string> equations = {},
        std::vector<std::string> vars = {},
        std::vector<std::string> coeffs = {},
        std::string t = "", 
        std::string module = ""
    ) : property(std::move(prop)),
        category(std::move(cat)),
        variables(std::move(vars)),      
        coefficients(std::move(coeffs)), 
        units({}),                      
        eqs(std::move(equations)),       
        title(std::move(t)),             
        moduleName(std::move(module))    
    {}

};


std::map<std::string, std::vector<Correlation>> getAllCorrelations(
    const std::string& filename,
    const std::string& out_filename
);

std::map<std::string, std::vector<Correlation>> loadAllCorrelations(
    const std::string& in_filename
);

#endif 