# SCIANTIX

SCIANTIX is a 0D code developed at Politecnico di Milano.
The objective of SCIANTIX is to represent the behaviour of a single grain of nuclear fuel.
The modelling of fission gases is the main aspect considered.
Engineering models are used, allowing for integration in industrial fuel performance codes.
Nevertheless, physics-based model are preferred to empirical models.
This facilitates the incorporation of information from lower length scale calculations.

At present, this version of the code is validated against experiments for
 - intragranular gaseous swelling.
 - intergranular gaseous swelling
 - helium behaviour and release in annealing conditions
 - release of radioactive fission gases
 - high-burnup structure porosity formation and evolution

The validation database is accessible in the *regression* folder.

# Installation

## On Linux
1. Obtain the code (either by `git clone` or by downloading the `zip`)
2. Install dependencies
    - Ubuntu: `sudo apt install build-essential`
3. Create a build directory (`mkdir build`), enter it (`cd build`) and create a Makefile with cmake tool (`cmake ..`)
4. Compile the code by typing `make` in the build directory
    - to utilize more cores, use `make -j`
    - in the `build` directory, look for the executable `sciantix.x`

## On Windows

A possible approach is throught the [Windows Subsystem for Linux (WSL)](https://learn.microsoft.com/en-us/windows/wsl/install).

1. [Install Windows Subsystem for Linux (WSL)](https://learn.microsoft.com/en-us/windows/wsl/install)

2. Run WSL (by typing `wsl` in `cmd.exe` or as `Ubuntu` application in Start menu). Navigate to the unpacked code directory (possibly by  `cd /mnt/c/...`, where `...` should be changed to code directory path).

3. Follow instructions for Linux from point 2.

# Usage

To run Sciantix, execute call the `sciantix.x` executable inside directory with the input files.

Refer to [Input file Explanation](utilities/InputExplanation.md) for input syntax manual.

Some examples of input files can be found
- in the `regression` directory,
- in the `utilities/inputExample` directory by running the Python files
    ```
    python utilities/inputExample/print_input_initial_conditions.py
    python utilities/inputExample/print_input_scaling_factors.py
    python utilities/inputExample/print_input_settings.py
    ```

# Regression
Run regression tests to verify everything works correctly
    ```
    cd regression
    python regression.py
    ```

# Documentation
Compile the code documentation by typing `doxygen` in the code directory
    - if `doxygen` is not found, install by `sudo apt install doxygen`

# Theory, papers

<a href="../../references/pdf_link/Zullo_et_al_2023.pdf" target="_blank">Zullo G. et al (2023). Journal of Nuclear Materials, 587, 154744.</a>.

```
G. Zullo, D. Pizzocri, L. Luzzi,
The SCIANTIX code for fission gas behaviour: Status, upgrades, separate-effect validation, and future developments,
Journal of Nuclear Materials,
Volume 587,
2023,
154744,
ISSN 0022-3115,
https://doi.org/10.1016/j.jnucmat.2023.154744.
(https://www.sciencedirect.com/science/article/pii/S0022311523005111)
```

<a href="../../references/pdf_link/Pizzocri_et_al_2020.pdf" target="_blank">Pizzocri D. et al (2020). Journal of Nuclear Materials, 532, 152042.</a>.

```
D. Pizzocri, T. Barani, L. Luzzi,
SCIANTIX: A new open source multi-scale code for fission gas behaviour modelling designed for nuclear fuel performance codes,
Journal of Nuclear Materials,
Volume 532,
2020,
152042,
ISSN 0022-3115,
https://doi.org/10.1016/j.jnucmat.2020.152042.
(https://www.sciencedirect.com/science/article/pii/S0022311519313868)
```



