Installation
============

System Requirements
-------------------

SCIANTIX requires the following minimum specifications:

- **C++ Compiler**: C++17 compatible compiler
  
  - GCC ≥ 9
  - Clang ≥ 10

- **Build System**: CMake ≥ 3.6

- **Operating System**:
  
  - Ubuntu ≥ 20.04 (native)
  - macOS Ventura
  - Windows 10/11 via Windows Subsystem for Linux 2 (WSL2)

Obtaining the Code
-------------------

You can obtain SCIANTIX in two ways:

**Option 1: Clone from GitHub**

.. code-block:: bash

    git clone https://github.com/sciantix/sciantix-official.git
    cd sciantix-official

**Option 2: Download the Source Archive**

Download the zip folder containing the source code from the GitHub repository and extract it to your desired location.

Linux Installation
-------------------

Follow these steps to build SCIANTIX on Linux systems:

**Step 1: Install Dependencies**

On Ubuntu-based systems:

.. code-block:: bash

    sudo apt install build-essential cmake

- `build-essential`: Provides the GCC compiler and related build tools
- `cmake`: Required for configuring the build system

**Step 2: Create and Navigate to Build Directory**

.. code-block:: bash

    mkdir build
    cd build

**Step 3: Configure with CMake**

.. code-block:: bash

    cmake ..

This generates the Makefile from the CMakeLists.txt configuration.

**Step 4: Compile the Code**

.. code-block:: bash

    make

To speed up compilation using multiple cores:

.. code-block:: bash

    make -j

where `` is the number of cores to use (e.g., `make -j4` for 4 cores or `make -j` to use all available cores).

**Step 5: Verify the Build**

After successful compilation, the executable `sciantix.x` will be located in the `build/` directory:

.. code-block:: bash

    ls -l build/sciantix.x

Windows Installation (via WSL)
-------------------------------

Windows users are recommended to use Windows Subsystem for Linux (WSL). This provides a native Linux environment on Windows without requiring virtual machines.

**Step 1: Install WSL**

Follow the `official WSL installation guide <https://learn.microsoft.com/en-us/windows/wsl/install>`_.

**Step 2: Launch WSL**

Open WSL by:

- Typing `wsl` in `cmd.exe` (Command Prompt), or
- Launching `Ubuntu` from the Start menu (after WSL installation)

**Step 3: Navigate to Code Directory**

Use the `/mnt/` mountpoint to access Windows directories:

.. code-block:: bash

    cd /mnt/c/path/to/sciantix-official

Replace `path/to/sciantix-official` with your actual installation path.

**Step 4: Follow Linux Installation Steps**

Proceed with the Linux installation steps starting from **Step 1: Install Dependencies**.

Building with Coupling Support
-------------------------------

To build SCIANTIX as a static library for coupling with external fuel performance codes (such as TRANSURANUS), use CMake with the coupling flag:

**Step 1: Configure with Coupling Enabled**

.. code-block:: bash

    cd build
    cmake -DCOUPLING_TU=ON ..

**Step 2: Compile**

.. code-block:: bash

    make

or with parallelization:

.. code-block:: bash

    make -j

**Step 3: Verify the Library**

The static library `libsciantix.a` will be created in the `build/` directory:

.. code-block:: bash

    ls -l build/libsciantix.a

This library can now be linked with external fuel performance codes. For detailed coupling instructions, contact the SCIANTIX development team or refer to the coupling documentation.

Running SCIANTIX
----------------

**Basic Usage**

To run SCIANTIX with input files in the current directory:

.. code-block:: bash

    ./sciantix.x

**Custom Input Directory**

To run SCIANTIX with input files in a specific directory:

.. code-block:: bash

    ./sciantix.x /path/to/input/directory/

SCIANTIX will read the following input files from the specified directory:

- `input_settings.txt`: Model and solver configuration
- `input_history.txt`: Time-dependent boundary conditions
- `input_initial_conditions.txt`: Initial simulation parameters
- `input_scaling_factors.txt` (optional): Parameter scaling factors

Verification and Testing
------------------------

To verify that SCIANTIX is correctly installed and functioning:

**Method 1: Run Regression Tests**

The repository includes a comprehensive regression testing suite:

.. code-block:: bash

    cd regression
    python3 regression.py

This executes all validation tests and compares results against established benchmarks. Successful completion indicates a correct installation.

**Method 2: Run a Single Example**

Navigate to one of the provided example directories and run a single simulation:

.. code-block:: bash

    cd regression/baker
    ../../build/sciantix.x

Check the output files to verify correct execution.

Generating Documentation
------------------------

To generate the Doxygen source code documentation:

.. code-block:: bash

    doxygen

This requires Doxygen to be installed:

.. code-block:: bash

    sudo apt install doxygen

The generated documentation will be available in the `doxygen/` directory.

Troubleshooting
---------------
**CMake not found**

Ensure CMake is installed:

.. code-block:: bash

    sudo apt install cmake

**Compiler not found**

Install the C++ compiler:

.. code-block:: bash

    sudo apt install build-essential

**Build fails with C++17 errors**

Verify your compiler supports C++17. Update your compiler if necessary:

.. code-block:: bash

    # Update GCC
    sudo apt update
    sudo apt install g++-9  # or later version

**Python regression tests fail**

Ensure Python 3 is installed:

.. code-block:: bash

    sudo apt install python3

For additional support, contact the SCIANTIX development team or open an issue on the GitHub repository.

Next Steps
----------

After successful installation:

1. Review the :doc:`overview` section for conceptual understanding
2. Examine the :doc:`examples` for practical usage
3. Run the :doc:`regression` tests to validate your installation
4. Refer to the :doc:`models` documentation for model details

Contributing & development workflow
----------------------------------

Follow the repository `CONTRIBUTING.md` guidance when contributing changes
to SCIANTIX. A concise development workflow is provided here for convenience:

1. **Fork the repository** on GitHub and clone your fork locally.

2. **Create a feature branch** for your changes:

.. code-block:: bash

     git checkout -b my-feature-branch

3. **Make small, focused commits** describing the changes you implemented.

4. **Run regression tests** before opening a Pull Request to ensure no
    regressions are introduced:

.. code-block:: bash

     python3 regression/regression.py

5. **Build the documentation locally** to verify formatting and links:

.. code-block:: bash

     cd docs
     make html

6. **Push the branch** to your fork and open a Pull Request against the
    `main` branch of the upstream repository:

.. code-block:: bash

     git push -u origin my-feature-branch

7. **Pull Request checklist** (recommended):

- Ensure the code compiles on the supported platforms (C++17).
- Ensure regression tests pass (or failures are justified and documented).
- Document new models, input options, and variables in Doxygen and the
  `docs/` directory.
- Use clear commit messages and reference any related issue numbers.

If you are unsure about the scope of a change, open an issue first to discuss
the proposal with the maintainers.
5. Check the :doc:`api/index_api` for API information
