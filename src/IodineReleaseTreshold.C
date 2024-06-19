// This function can be used to define a threshold value above which the iodine 
// contained at the grain boundaries (precipitate) is released completely outside 
// the grain.
// Here : 1300°C = CsI boiling Point

#include "IodineReleaseTreshold.h"
void IodineReleaseTreshold()
    {
    if (history_variable[hv["Temperature"]].getFinalValue() > 1300);

    model.emplace_back();
    int model_index = int(model.size()) - 1;
    model[model_index].setName("IodineReleaseTreshold");
    
    std::string reference;
    reference = "Kleykamp (1985) J. Nucl. Mater., 131, 221-246.";
    
    model[model_index].setRef(reference);
    }