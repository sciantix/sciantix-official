# Code folder

This directory contains the source code of SCIANTIX.

## Quick compilation (Linux / WSL)

If you have CMake installed, use the standard procedure:

```bash
mkdir build
cd build
cmake ..
make

```

Alternatively, you can use the provided scripts:

* `./Allmake`: To compile the code and the regression suite.
* `./Allclean`: To remove build artifacts and clean the directory.

## Structure

* **`src/`**: C++ source files.
* **`include/`**: Header files.
* **`regression/`**: Scripts and cases for code verification.
* **`GPregression`**: Gaussian Process tools for regression analysis.