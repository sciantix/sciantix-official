# Training Case 2 — Transient Irradiation (White 2004)

This training case demonstrates fission-gas behaviour during a **temperature transient** and is based on the classical separate-effect study by *White (2004)*.

It corresponds to the **transient example** used in Module 3.2 of the SCIANTIX MOOC.

The case illustrates:

- steady grain-boundary swelling during base irradiation  
- grain-face saturation  
- burst release triggered by thermal stresses  
- competition between gas accumulation and micro-cracking  

## Input conditions

Two irradiation phases:

### **1. Base irradiation**
- Temperature: 1157 K  
- Duration: experiment-dependent  
- Observed effect: grain-boundary swelling increases steadily due to vacancy absorption.

### **2. Temperature ramp**
- Temperature increased up to **~2048 K**  
- Burst release occurs  
- Driven by **micro-cracking** of grain boundaries  
- Followed by a cooling phase

## Input files

- `input_history.txt` — time evolution of temperature  
- `input_settings.txt` — models for GB saturation and burst release  
- `input_initial_conditions.txt` — initial grain properties  
- `input_scaling_factors.txt` — optional  

## Running the case

```bash
./sciantix.x
```

## Expected behaviour

* During base irradiation: **smooth increase** in grain-boundary swelling
* During transient:

  * sharp temperature rise
  * **burst release** (rapid drop in GB gas inventory)
  * structural changes linked to micro-cracking

SCIANTIX reproduces both trends and provides a predictive description of transient fission-gas behaviour.

## Reference

White, R.J. (2004). *The development of grain-face porosity in irradiated oxide fuel.*
Journal of Nuclear Materials, 325, 61–77.
[https://doi.org/10.1016/j.jnucmat.2003.10.008](https://doi.org/10.1016/j.jnucmat.2003.10.008)