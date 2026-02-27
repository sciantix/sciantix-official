# SCIANTIX
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17748425.svg)](https://doi.org/10.5281/zenodo.17748425)
![CI](https://github.com/sciantix/sciantix-official/actions/workflows/pages.yml/badge.svg)
![paper](https://github.com/sciantix/sciantix-official/actions/workflows/paper.yml/badge.svg)

SCIANTIX is an open-source 0D simulation code developed at Politecnico di Milano, designed to model the behavior of a single grain of nuclear fuel, with a particular focus on fission gas behavior. 

The code employs physics-based rate-theory models rather than empirical correlations, enabling better integration with lower-length scale calculations and improved predictive capability. SCIANTIX is designed to operate both as an independent tool and as an embedded module within industrial fuel performance codes (FPCs) such as TRANSURANUS, FRAPCON/FRAPTRAN, and OFFBEAT.

Currently, SCIANTIX is validated against experimental data for:
- Intragranular gaseous swelling
- Intergranular gaseous swelling
- Fission gas release (Xe, Kr)
- Helium behavior and release under annealing conditions
- High-burnup structure (HBS) formation and porosity evolution

The validation database and regression suite are available in the `regression/` directory.

# Installation

Recommended requirements:
- C++17 compatible compiler (tested: GCC ≥ 9, Clang ≥ 10)
- CMake ≥ 3.6
- Python 3.8+ (for regression suite)

## Quick installation (Linux/WSL2)

1. **Obtain the code:**
   ```bash
   git clone https://github.com/sciantix/sciantix-official.git
   cd sciantix-official
   ```
2. **Build the code:**
   ```bash
   ./Allmake.sh
   ```
   The compiled executable `sciantix.x` will be located in the `build/` directory.

## Manual installation

If you prefer to build manually or need custom CMake flags:
```bash
mkdir build && cd build
cmake ..
make -j$(nproc)
```

## Windows installation
The recommended approach for Windows users is to use the [Windows Subsystem for Linux (WSL2)](https://learn.microsoft.com/en-us/windows/wsl/install). Follow the Quick installation steps within your WSL terminal.

# Coupling with fuel performance codes
To compile SCIANTIX as a static library for coupling with codes like TRANSURANUS:
```bash
cd build
cmake -DCOUPLING_TU=ON ..
make -j$(nproc)
```
The static library `libsciantix.a` will be generated in the `build/` directory.

# Usage
Execute `sciantix.x` within the directory containing your input files, or provide the path to the input folder:
```bash
./build/sciantix.x [path_to_input_folder]
```

### Input preparation
Refer to the [Input File Explanation](utilities/InputExplanation.md) for detailed syntax.
Generation scripts for template input files are available in `utilities/inputExample/`:
```bash
python3 utilities/inputExample/print_input_settings.py
python3 utilities/inputExample/print_input_initial_conditions.py
python3 utilities/inputExample/print_input_scaling_factors.py
```

# Regression tests

To verify the installation and physics:
```bash
./runRegression.sh
```
Alternatively, run the runner directly:
```bash
python3 -m regression.runner --all -j $(nproc)
```

# Documentation

Online documentation is available at [sciantix.github.io/sciantix-official](https://sciantix.github.io/sciantix-official/).
To generate local Doxygen documentation:
```bash
doxygen Doxyfile
```

# How to cite

Please cite SCIANTIX using the Zenodo DOI:

```bibtex
@software{SCIANTIX_v2,
  title        = {SCIANTIX},
  year         = {2024},
  publisher    = {Zenodo},
  doi          = {10.5281/zenodo.17748425},
  url          = {https://doi.org/10.5281/zenodo.17748425}
}
```

We also encourage citing the scientific publications associated with SCIANTIX, listed below and in `references/references.md`, when relevant.
Citation metadata is also available in the `CITATION.cff` file.

# Theory and References

For a deeper understanding of the SCIANTIX code and its underlying models, refer to the following publications:

- [Zullo G. et al. (2023). Journal of Nuclear Materials, 587, 154744.](https://www.sciencedirect.com/science/article/pii/S0022311523005111)

```bibtex
@article{Zullo2023,
   author    = {Zullo, Giovanni and Pizzocri, Davide and Luzzi, Lelio},
   title     = {The SCIANTIX code for fission gas behaviour: Status, upgrades, separate-effect validation, and future developments},
   journal   = {Journal of Nuclear Materials},
   volume    = {587},
   pages     = {154744},
   year      = {2023},
   issn      = {0022-3115},
   doi       = {10.1016/j.jnucmat.2023.154744},
   url       = {https://www.sciencedirect.com/science/article/pii/S0022311523005111}
   }
```

- [Pizzocri D. et al. (2020). Journal of Nuclear Materials, 532, 152042.](https://www.sciencedirect.com/science/article/pii/S0022311519313868)

```bibtex
@article{Pizzocri2020,
   author    = {Pizzocri, Davide and Barani, Tommaso and Luzzi, Lelio},
   title     = {SCIANTIX: A new open source multi-scale code for fission gas behaviour modelling designed for nuclear fuel performance codes},
   journal   = {Journal of Nuclear Materials},
   volume    = {532},
   pages     = {152042},
   year      = {2020},
   issn      = {0022-3115},
   doi       = {10.1016/j.jnucmat.2020.152042},
   url       = {https://www.sciencedirect.com/science/article/pii/S0022311519313868}
   }
```

# Authors

SCIANTIX is developed and maintained by:

- Giovanni Zullo
- Elisa Cappellari
- Giovanni Nicodemo
- Aya Zayat
- Davide Pizzocri
- Lelio Luzzi

Politecnico di Milano, Nuclear Engineering Division.
