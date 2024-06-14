//this function can be used to define a threshold value above which the iodine 
// contained at the grain boundaries is released completely outside the grain.

#include "IodineReleaseTreshold.h"
void IodineReleaseTreshold()
    {
    if (history_variable[hv["Temperature"]].getFinalValue() > 1300);

    model.emplace_back();
    int model_index = int(model.size()) - 1;

    model[model_index].setName("IodineReleaseTreshold");
    
    std::string reference;
    reference = "Cappia (2022) J. Nucl. Mater., 569, 153-881.";
    
    model[model_index].setRef(reference);
    }