# SCIANTIX

SCIANTIX is a 0D simulation code developed at Politecnico di Milano, designed to model the behavior of a single grain of nuclear fuel, with a particular focus on fission gas behavior. The code primarily uses physics-based models, which enhances the integration of lower-length scale calculations and improves accuracy over empirical models. The engineering models facilitate the code integration into industrial fuel performance codes.

Currently, SCIANTIX is validated against experimental data for the following phenomena:
- Intragranular gaseous swelling
- Intergranular gaseous swelling
- Helium behavior and release under annealing conditions
- Release of radioactive fission gases
- Formation and evolution of high-burnup structure (HBS) porosity

The validation database is available in the `regression` folder.

# Installation

## Linux Installation

1. **Obtain the Code:** Download the source code by cloning the GitHub repository or by downloading the zip folder.
2. **Install Dependencies:**
   - On Ubuntu, run: `sudo apt install build-essential cmake`
3. **Build the Code:**
   - Create a build directory: `mkdir build`
   - Navigate to the build directory: `cd build`
   - Generate the Makefile with CMake: `cmake ..`
   - Compile the code: `make` (to speed up the process using multiple cores, use: `make -j`)

The compiled executable `sciantix.x` will be located in the `build` directory.

## Windows Installation

The recommended approach for Windows users is to use the [Windows Subsystem for Linux (WSL)](https://learn.microsoft.com/en-us/windows/wsl/install).

1. **Install WSL:**
   - Follow the [official installation guide](https://learn.microsoft.com/en-us/windows/wsl/install).
2. **Run WSL:**
   - Open WSL by typing `wsl` in `cmd.exe` or by launching the `Ubuntu` application from the Start menu.
   - Navigate to the code directory using: `cd /mnt/c/...` (replace `...` with your code directory path).
3. **Follow Linux Installation Instructions:**
   - Proceed with the Linux installation steps starting from step 2.

# Coupling with fuel performance codes
The code can be coupled with thermo-mechanical fuel performance codes (<a href="https://www.sciencedirect.com/science/article/pii/S0022311524004070" target="_blank">Zullo G. et al (2024). Journal of Nuclear Materials, 601, 155305.</a>). Here, it is detailed that the code can be compiled as a static library and coupled with TRANSURANUS (currently at version v1m1j24) distributed by JRC-EC Karlsruhe To generate SCIANTIX as a static libray, in the build folder:

   - Generate the Makefile with CMake: `cmake -DCOUPLING_TU=ON ..`
   - Compile the code: `make` (or `make -j`)

The static library, `libsciantix.a`, will be located in the `build` directory.

# Usage
To run SCIANTIX, execute `sciantix.x` within the directory containing your input files, or execute `./sciantix.x /path_to_folder_with_input_files/` in the working directory with the executable.

Refer to the [Input File Explanation](utilities/InputExplanation.md) for detailed instructions on input syntax.

Examples of input files can be found in:
- The `regression` directory
- The `utilities/inputExample` directory, by running the following Python scripts:

```sh
python3 utilities/inputExample/print_input_initial_conditions.py
python3 utilities/inputExample/print_input_scaling_factors.py
python3 utilities/inputExample/print_input_settings.py
```

# Regression Tests

To verify the correct operation of SCIANTIX, run the regression tests:

```sh
cd regression
python3 regression.py
```

# Documentation

To generate the code documentation, run `doxygen` in the root directory of the code. The Doxygen software can be installed, e.g., on Linux systems, with `sudo apt install doxygen`.

# Theory and References

For a deeper understanding of the SCIANTIX code and its underlying models, refer to the following publications:

- [Zullo G. et al. (2023). Journal of Nuclear Materials, 587, 154744.](https://www.sciencedirect.com/science/article/pii/S0022311523005111)

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

- [Pizzocri D. et al. (2020). Journal of Nuclear Materials, 532, 152042.](https://www.sciencedirect.com/science/article/pii/S0022311519313868)

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