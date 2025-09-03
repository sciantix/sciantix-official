#include <fstream>
#include <iostream>
#include <sstream>
#include <algorithm>
#include <string>
#include <cctype>
#include <map>
#include <vector>
#include <filesystem>
#include <tuple> // Pour std::tuple

#include "CorrelationProcessor.h" // Utilise la struct Correlation (avec moduleName)
#include "OutputGenerator.h"



struct ParsedCoefficient {
    std::string name;
    std::string value;
    std::string unit;
};


static std::string trim(const std::string& s) {
    size_t start = s.find_first_not_of(" \t\r\n#");
    size_t end = s.find_last_not_of(" \t\r\n#");
    if (start == std::string::npos || end == std::string::npos)
        return "";
    return s.substr(start, end - start + 1);
}


static std::string cleanFunctionName(const std::string& name) {
    std::string cleaned_name = name;
    std::transform(cleaned_name.begin(), cleaned_name.end(), cleaned_name.begin(),
                   [](unsigned char c){ return std::isalnum(c) ? c : '_'; });
    return cleaned_name;
}


static std::map<std::string, std::string> loadVariableMapping(const std::string& filename) {
    std::map<std::string, std::string> mapping;
    std::ifstream infile(filename);
    if (!infile) {
        std::cerr << "Warning: Impossible d'ouvrir le fichier de mappage de variables: " << filename << std::endl;
        std::cerr << "Les variables dans les équations NE SERONT PAS remplacées par leurs équivalents Sciantix." << std::endl;
        return mapping;
    }
    std::string line;
    while (std::getline(infile, line)) {
        line = trim(line);
        if (line.empty() || line[0] == '#') continue; // Ignorer les lignes vides ou commentées
        size_t eq_pos = line.find("=");
        if (eq_pos != std::string::npos) {
            std::string var_name = trim(line.substr(0, eq_pos));
            std::string sciantix_expr = trim(line.substr(eq_pos + 1));
            if (!var_name.empty() && !sciantix_expr.empty()) {
                mapping[var_name] = sciantix_expr;
            }
        }
    }
    return mapping;
}


static std::vector<Correlation> loadSelectedCorrelations(const std::string& filename) {
    std::vector<Correlation> corrs;
    std::ifstream in(filename);
    if (!in) {
        std::cerr << "Erreur: Impossible d'ouvrir le fichier de corrélations sélectionnées: " << filename << std::endl;
        return corrs;
    }
    std::string line;
    Correlation corr; 
    while (std::getline(in, line)) {
        line = trim(line);
        if (line.empty()) continue; 

        if (line.rfind("Property:", 0) == 0) {
            if (!corr.property.empty()) {
                corrs.push_back(corr);
                corr = Correlation(); 
            }
            corr.property = trim(line.substr(9));
        } else if (line.rfind("Module:", 0) == 0) {
            corr.moduleName = trim(line.substr(7));
        } else if (line.rfind("Title:", 0) == 0) {
            corr.title = trim(line.substr(6));
        } else if (line.rfind("Variables:", 0) == 0) {
            std::string vars = trim(line.substr(10));
            corr.variables.clear();
            std::stringstream ss(vars);
            std::string var;
            while (std::getline(ss, var, ',')) {
                corr.variables.push_back(trim(var));
            }
        } else if (line.rfind("Coefficients:", 0) == 0) {
            std::string coefs = trim(line.substr(12));
            corr.coefficients.clear();
            corr.coefficients.push_back(coefs); 
        } else if (line.rfind("Equation:", 0) == 0) {
            std::string eq = trim(line.substr(9));
            corr.eqs.clear();
            corr.eqs.push_back(eq);
        } else if (line.rfind("Units:", 0) == 0) {
            std::string units_str = trim(line.substr(6));
            corr.units.clear();
            std::stringstream ss(units_str);
            std::string item;
            while (std::getline(ss, item, ',')) {
                size_t pos = item.find(':');
                if (pos != std::string::npos) {
                    std::string key = trim(item.substr(0, pos));
                    std::string val = trim(item.substr(pos+1));
                    if (!key.empty()) corr.units[key] = val;
                }
            }
        } else if (line == "---") {
      
            if (!corr.property.empty()) {
                corrs.push_back(corr);
                corr = Correlation(); 
            }
        }
    }
    if (!corr.property.empty()) {
        corrs.push_back(corr);
    }
    return corrs;
}


static std::vector<ParsedCoefficient> parseCoefficients(
    const std::string& coef_str,
    const std::map<std::string, std::string>& units,
    const std::string& default_unit)
{
    std::vector<ParsedCoefficient> parsed_coefs;
    std::stringstream coef_ss(coef_str);
    std::string segment;
    while (std::getline(coef_ss, segment, ',')) {
        size_t eq_pos = segment.find("=");
        if (eq_pos != std::string::npos) {
            std::string coef_name_raw = segment.substr(0, eq_pos);
            std::string coef_value = trim(segment.substr(eq_pos + 1));

            // Nettoyer le nom du coefficient en supprimant les ':' ou espaces en début
            size_t start = 0;
            while (start < coef_name_raw.length() && (coef_name_raw[start] == ':' || std::isspace(coef_name_raw[start]))) {
                start++;
            }
            std::string coef_name = trim(coef_name_raw.substr(start));

            if (!coef_name.empty()) {
                std::string coef_unit = "?";
                auto uit = units.find(coef_name);
                if (uit != units.end() && !uit->second.empty()) {
                    coef_unit = uit->second;
                } else if (!default_unit.empty()) {
                    coef_unit = default_unit;
                }
                parsed_coefs.push_back({coef_name, coef_value, coef_unit});
            }
        }
    }
    return parsed_coefs;
}

static std::vector<std::string> loadModuleProperties(const std::string& filename) {
    std::vector<std::string> properties;
    std::ifstream infile(filename);
    if (!infile) {
        std::cerr << "Warning: Impossible d'ouvrir le fichier de propriétés du module: " << filename << std::endl;
        return properties;
    }
    std::string line;
    while (std::getline(infile, line)) {
        line = trim(line);
        if (!line.empty() && line[0] != '#') { // Ignorer les lignes vides et les commentaires
            properties.push_back(line);
        }
    }
    return properties;
}


void generateOutputsFromSelected(
    const std::string& selectedCorrFile,
    const std::string& variableMappingFile
) {
    
    // Définition des chemins de sortie en tant que constantes
    const std::string INTERPRETATION_REPORT_PATH = "output/interpretation_report.txt";
    const std::string OUTPUT_BASE_DIR = "output"; // Pour le répertoire du rapport
    const std::string INPUT_DIR = "input/"; // Répertoire des fichiers de propriétés additionnelles

    // Charger les données nécessaires
    auto selectedCorrs = loadSelectedCorrelations(selectedCorrFile);
    auto variable_mapping = loadVariableMapping(variableMappingFile);

    // Construire les chemins complets des répertoires de sortie
    const std::string cppOutputDir = "src/generated_correlations";
    const std::string hOutputDir = "include/generated_correlations";

    // Créer les répertoires de sortie si ils n'existent pas
    std::filesystem::create_directories(cppOutputDir);
    std::filesystem::create_directories(hOutputDir);
    std::filesystem::create_directories(OUTPUT_BASE_DIR); // Pour le rapport d'interprétation

    // Ouvrir le fichier de rapport d'interprétation
    std::ofstream report_out(INTERPRETATION_REPORT_PATH, std::ios::trunc);
    if (!report_out) {
        std::cerr << "Erreur: Impossible de créer le fichier de rapport d'interprétation: " << INTERPRETATION_REPORT_PATH << std::endl;
        return;
    }

    // moduleClassMembers stocke:
    // tuple.get<0>: vector<string> des déclarations de fonctions set (pour les corrélations, définies dans .cpp)
    // tuple.get<1>: string des déclarations de membres double (pour toutes les propriétés)
    // tuple.get<2>: string des implémentations de fonctions get (pour les corrélations et propriétés additionnelles)
    //               et des implémentations de fonctions set (pour les propriétés additionnelles)
    std::map<std::string, std::tuple<std::vector<std::string>, std::string, std::string>> moduleClassMembers;

    // --- Première passe: Traiter les corrélations sélectionnées (génération des .cpp et préparation des .h) ---
    for (const auto& corr : selectedCorrs) {
        std::string cleanModuleName = cleanFunctionName(corr.moduleName);
        std::string propertyName = cleanFunctionName(corr.property);

        if (propertyName.empty()) {
            continue;
        }

        // --- Logique de rapport d'interprétation (inchangée) ---
        std::string unit_prop = "?";
        auto unit_it = corr.units.find(corr.property);
        if (unit_it != corr.units.end())
            unit_prop = unit_it->second;
        else if (!corr.units.empty())
            unit_prop = corr.units.begin()->second;

        report_out << "For " << corr.property << ", Unit :" << unit_prop << ",\n";
        for (const auto& var : corr.variables) {
            if (var == corr.property) continue;
            std::string replacement = variable_mapping.count(var) ? variable_mapping.at(var) : var;
            std::string var_unit = "?";
            auto uit = corr.units.find(var);
            if (uit != corr.units.end() && !uit->second.empty()) var_unit = uit->second;
            else var_unit = unit_prop;
            report_out << "- " << var << " was assumed as " << replacement << ". Unit : " << var_unit << "\n";
        }
        if (!corr.coefficients.empty()) {
            std::string coef_str = corr.coefficients[0];
            std::stringstream coef_ss(coef_str);
            std::string segment;
            while (std::getline(coef_ss, segment, ',')) {
                size_t eq_pos = segment.find("=");
                if (eq_pos != std::string::npos) {
                    std::string coef_name = trim(segment.substr(0, eq_pos));

                    // Enlève tout ':' ou espace en début de nom (corrige le bug du rapport)
                    while (!coef_name.empty() && (coef_name.front() == ':' || std::isspace(coef_name.front()))) {
                        coef_name.erase(coef_name.begin());
                    }

                    if (std::find(corr.variables.begin(), corr.variables.end(), coef_name) == corr.variables.end() &&
                        coef_name != corr.property && !coef_name.empty()) {

                        std::string coef_unit = "?";
                        auto uit = corr.units.find(coef_name);
                        if (uit != corr.units.end() && !uit->second.empty()) {
                            coef_unit = uit->second;
                        } else if (!unit_prop.empty()) {
                            coef_unit = unit_prop;
                        }
                        report_out << "- " << coef_name << " was assumed as a parameter. Unit: " << coef_unit << "\n";
                    }
                }
            }
        }
        report_out << "\n";
        // --- Fin logique de rapport d'interprétation ---

        if (!cleanModuleName.empty()) {
            // Ajouter le membre double pour la propriété de corrélation
            std::get<1>(moduleClassMembers[cleanModuleName]) += "    double " + propertyName + ";\n";
            // Ajouter l'implémentation du getter pour la propriété de corrélation (inline dans .h)
            std::get<2>(moduleClassMembers[cleanModuleName]) += "    double get" + propertyName + "()\n";
            std::get<2>(moduleClassMembers[cleanModuleName]) += "    {\n";
            std::get<2>(moduleClassMembers[cleanModuleName]) += "        return " + propertyName + ";\n";
            std::get<2>(moduleClassMembers[cleanModuleName]) += "    }\n\n";
            // Ajouter la déclaration du setter pour la propriété de corrélation (implémentée dans .cpp)
            std::get<0>(moduleClassMembers[cleanModuleName]).push_back("void set" + propertyName + "("
                                        + "SciantixArray<SciantixVariable> &sciantix_variable, "
                                        + "SciantixArray<SciantixVariable> &history_variable,"
                                        + "SciantixArray<Matrix> &matrices);");
        }

        // --- Génération du fichier .cpp pour la corrélation (inchangée) ---
        std::string outFilename = cppOutputDir + "/" + propertyName + ".cpp";
        std::ofstream output(outFilename);
        if (!output) {
            std::cerr << "Error opening file " << outFilename << ". Make sure that the directory '" << cppOutputDir << "' exists and you have write permissions.\n";
            continue;
        }

        output << "#include <cmath>\n#include <iostream>\n#include <string>\n\n";
        if (!cleanModuleName.empty()) {
            output << "#include \"" << cleanModuleName << ".h\"\n";
        }
        output << "\n";

        std::string classPrefix = "";
        if (!cleanModuleName.empty()) {
            classPrefix = cleanModuleName + "::";
        }

        std::string functionSignature = "void " + classPrefix + "set" + propertyName + "("
                                        + "SciantixArray<SciantixVariable> &sciantix_variable, "
                                        + "SciantixArray<SciantixVariable> &history_variable,"
                                        + "SciantixArray<Matrix> &matrices)";

        output << functionSignature << " {\n";

        if (!corr.title.empty()) {
            output << "    // Source: " << corr.title << "\n";
            output << "    std::string reference = \"\";\n";
            output << "    reference += \"" << corr.title << "\";\n";
        } else {
            output << "    std::string reference = \"\";\n";
        }

        if (!corr.units.empty()) {
            output << "    // Units:\n";
            std::vector<std::pair<std::string, std::string>> sorted_units(corr.units.begin(), corr.units.end());
            std::sort(sorted_units.begin(), sorted_units.end(), [](const auto& a, const auto& b){
                return a.first < b.first; 
            });
            for (const auto& pair : sorted_units) {
                if (!pair.second.empty()) {
                    output << "    //   " << pair.first << ": " << pair.second << "\n";
                }
            }
        }
        if (!corr.coefficients.empty()) {
            std::string coef_str = corr.coefficients[0];
            std::stringstream coef_ss(coef_str);
            std::string segment;
            while (std::getline(coef_ss, segment, ',')) {
                size_t eq_pos = segment.find("=");
                if (eq_pos != std::string::npos) {
                    std::string coef_name = trim(segment.substr(0, eq_pos));
                    std::string coef_value = trim(segment.substr(eq_pos + 1));
                    while (!coef_name.empty() && (coef_name.front() == ':' || std::isspace(coef_name.front()))) {
                        coef_name.erase(coef_name.begin());
                    }
                    if (std::find(corr.variables.begin(), corr.variables.end(), coef_name) == corr.variables.end() &&
                        coef_name != corr.property) {
                        output << "    const double " << coef_name << " = " << coef_value << ";\n";
                    }
                }
            }
        }
        output << "\n";

        std::vector<std::string> sorted_keys;
        for(auto const& [key, val] : variable_mapping) {
            sorted_keys.push_back(key);
        }
        std::sort(sorted_keys.begin(), sorted_keys.end(), [](const std::string& a, const std::string& b) {
            return a.length() > b.length(); // Tri par longueur décroissante pour éviter les remplacements partiels
        });

        std::string result_variable_name = propertyName;

        bool first_eq_processed = false;
        for (const auto& eq : corr.eqs) {
            std::string processed_eq = eq;
            if (!first_eq_processed && (processed_eq.find(propertyName + " =") == 0 || processed_eq.find(propertyName + "=") == 0)) {
                processed_eq.replace(0, propertyName.length(), result_variable_name);
                first_eq_processed = true;
            }
            for (const auto& var_name_raw : sorted_keys) {
                if (var_name_raw == propertyName && (eq.find(propertyName + " =") == 0 || eq.find(propertyName + "=") == 0)) {
                    continue;
                }
                const std::string& replacement_str = variable_mapping[var_name_raw];
                size_t pos = processed_eq.find(var_name_raw);
                while (pos != std::string::npos) {
                    bool is_whole_word = true;
                    // Vérifier le caractère précédent
                    if (pos > 0) {
                        char prev_char = processed_eq[pos-1];
                        if (std::isalnum(prev_char) || prev_char == '_') {
                            is_whole_word = false;
                        }
                    }
                    // Vérifier le caractère suivant
                    if (pos + var_name_raw.length() < processed_eq.length()) {
                        char next_char = processed_eq[pos + var_name_raw.length()];
                        if (std::isalnum(next_char) || next_char == '_') {
                            is_whole_word = false;
                        }
                    }
                    if (is_whole_word) {
                        processed_eq.replace(pos, var_name_raw.length(), replacement_str);
                        pos = processed_eq.find(var_name_raw, pos + replacement_str.length());
                    } else {
                        pos = processed_eq.find(var_name_raw, pos + 1);
                    }
                }
            }
            output << "    " << processed_eq << "\n";
        }

        output << "}\n\n";
        output.close();
    }

    // --- Deuxième passe: Ajouter les propriétés des fichiers NomDuModule_properties.txt ---
    // Cette boucle doit être exécutée après que moduleClassMembers ait été initialisé avec tous
    // les noms de modules provenant des corrélations.
    for (auto& pair : moduleClassMembers) { // Utiliser auto& pour pouvoir modifier la valeur du tuple
        const std::string& moduleName = pair.first;
        std::string properties_filename = INPUT_DIR + moduleName + "_properties.txt";
        auto additional_properties = loadModuleProperties(properties_filename);

        for (const auto& prop_name : additional_properties) {
            std::string cleanPropName = cleanFunctionName(prop_name);
            if (cleanPropName.empty()) continue; // Ignorer les noms de propriétés nettoyés vides

            // Ajouter le membre double pour la propriété additionnelle
            std::get<1>(pair.second) += "    double " + cleanPropName + ";\n";

            // Ajouter l'implémentation du setter (inline dans .h)
            std::get<2>(pair.second) += "    void set" + cleanPropName + "(double val)\n";
            std::get<2>(pair.second) += "    {\n";
            std::get<2>(pair.second) += "        " + cleanPropName + " = val;\n";
            std::get<2>(pair.second) += "    }\n\n";

            // Ajouter l'implémentation du getter (inline dans .h)
            std::get<2>(pair.second) += "    double get" + cleanPropName + "()\n";
            std::get<2>(pair.second) += "    {\n";
            std::get<2>(pair.second) += "        return " + cleanPropName + ";\n";
            std::get<2>(pair.second) += "    }\n\n";
        }
    }


    // --- Génération des headers modules (.h) ---
    for (const auto& pair : moduleClassMembers) {
        const std::string& moduleName = pair.first;
        const std::vector<std::string>& functionDeclarations = std::get<0>(pair.second); // Déclarations des setters de corrélations
        const std::string& classMemberVariables = std::get<1>(pair.second); // Tous les membres double
        const std::string& getterAndSetterImplementations = std::get<2>(pair.second); // Tous les getters et les setters des propriétés additionnelles

        std::string h_filename = hOutputDir + "/" + moduleName + ".h";
        std::ofstream h_output(h_filename);
        if (!h_output) {
            std::cerr << "Error: Unable to create header file:" << h_filename << "\n";
            continue;
        }

        std::string define_guard = moduleName;
        std::transform(define_guard.begin(), define_guard.end(), define_guard.begin(), ::toupper);
        define_guard += "_H";

        h_output << "#ifndef " << define_guard << "\n";
        h_output << "#define " << define_guard << "\n\n";
        h_output << "#include <cmath>\n";
        h_output << "#include \"Material.h\"\n";
        h_output << "#include \"Constants.h\"\n";
        h_output << "#include \"ErrorMessages.h\"\n";
        h_output << "#include \"SciantixArray.h\"\n";
        h_output << "#include \"SciantixVariable.h\"\n\n";

        h_output << "class " << moduleName << " : virtual public Material\n";
        h_output << "{\n";
        h_output << "public:\n";
        h_output << classMemberVariables; // Déclarations des membres (double propertyName;)
        h_output << "\n";
        for (const std::string& decl : functionDeclarations) { // Déclarations des setters de corrélations
            h_output << "    " << decl << "\n";
        }
        h_output << "\n";
        h_output << getterAndSetterImplementations; // Implémentations des getters et des setters additionnels
        h_output << "    /**\n";
        h_output << "     * @brief Constructor\n";
        h_output << "     */\n";
        h_output << "    " << moduleName << "() {}\n\n";
        h_output << "    /**\n";
        h_output << "     * @brief Destructor\n";
        h_output << "     */\n";
        h_output << "    ~" << moduleName << "() {}\n";
        h_output << "}; // class " << moduleName << "\n\n";
        h_output << "#endif // " << define_guard << "\n";
        h_output.close();

    }
}
