# Training Case 1 — Stationary Irradiation (Baker 1977)

This training case illustrates the behaviour of fission gases under **steady irradiation conditions**.  
It is based on conditions representative of the separate-effect experiments described in *Baker (1977)*.

The goal is to observe:

- xenon production under constant fission rate  
- intra-granular diffusion  
- bubble nucleation  
- trapping and re-solution  
- grain-face saturation  
- onset of fission gas release  

This case corresponds to the **stationary example** used in Module 3.1 of the SCIANTIX MOOC.

## Input conditions

The simulation represents a UO₂ fuel grain irradiated at:

- **Constant temperature:** 1373 K  
- **Constant fission rate density:** 1×10¹⁹ fiss/m³/s  
- **Hydrostatic stress:** 0 Pa  
- **Irradiation time:** 5500 h  

These conditions highlight the competition between **bubble nucleation** (early stage) and **re-solution** (late stage).

## Input files

- `input_history.txt` — total irradiation time and time grid  
- `input_settings.txt` — activated physical models  
- `input_initial_conditions.txt` — grain radius, initial xenon concentration  
- `input_scaling_factors.txt` — optional scaling factors (defaults provided)

## Running the case

```bash
./sciantix.x
```

output,txt will appear in the same directory.


## Expected behaviour

* Xenon production increases **linearly** with time.
* Bubble density shows an **initial rise** (nucleation) followed by a **decline** (re-solution).
* Grain-boundary gas increases until **saturation**, after which **release** begins.

## Reference

Baker, C. (1977). *The fission gas bubbles distribution in uranium dioxide from high temperature irradiated SGHWR fuel pins.*
Journal of Nuclear Materials, 66, 283–291.
[https://doi.org/10.1016/0022-3115(77)90117-9](https://doi.org/10.1016/0022-3115%2877%2990117-9)