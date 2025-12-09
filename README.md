# SCIANTIX Training Repository

The **material/** directory contains the official SCIANTIX training cases.  
These examples allow users to familiarise themselves with the SCIANTIX input/output structure, validate their installation, and reproduce reference simulations from the scientific literature.

The training cases are also used as optional exercises in courses, but they are fully self-contained and can be run independently.

## Available training cases

| Folder | Description |
|--------|-------------|
| `1_baker1977_stationary` | Stationary irradiation case. Demonstrates intra-granular diffusion, trapping, re-solution and grain-boundary saturation. Based on conditions representative of Baker (1977). |
| `2_white2004_transient` | Transient temperature-ramp case. Demonstrates grain-boundary swelling, saturation and burst release. Based on White (2004). |
| `3_uo2_hbs` | Example case for high-burnup structure (HBS) formation. For advanced users and model development. |

## How to run a training case

Inside any case directory:

```bash
./sciantix.x
```

Output files will be written in the same folder.

## Plotting results
Copy-paste into excel/csv files is suggested to elaborate quickly the results.

## Useful links

* SCIANTIX main repository: [https://github.com/sciantix/sciantix-official](https://github.com/sciantix/sciantix-official)
* SCIANTIX documentation (installation & structure): [https://sciantix.github.io/sciantix-official/](https://sciantix.github.io/sciantix-official/)
<!-- * SCIANTIX MOOC (optional learning resource): *link to be added when public* -->

## Notes

* All input files follow the standard SCIANTIX format (history, settings, initial conditions, scaling factors).
* Users are encouraged to modify temperature, fission rate and material properties to explore parameter sensitivity.
* These cases rely only on the physics available in the standard open-source version of SCIANTIX.