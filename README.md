# Development/PyBind11

This branch is used for the implementation of PyBind11 in the sciantix 2.0 code.

## How to Use PyBind11 with the Sciantix Code

### Installation

Install PyBind11 using pip:

```sh
pip install "pybind11[global]"
```

<b>Create the build folder and go inside it with this command : </b>
```shell
mkdir build 
cd build
```

<b>For compilation From the Build folder :</b>

```shell
cmake .. && make
```

<b>All in one : </b>

```shell
mkdir build && cd build && cmake .. && make && cd ../pythonBind/ && python3 ../pythonBind/main.py ../regression/test_Baker1977__1273K/
```
<b>Only Python : </b> <br>
For the `test_Barker1977__1273K`
```shell
python3 ../pythonBind/main.py ../regression/test_Baker1977__1273K/
```

### Explanation :
- Create the build folder: This sets up a separate directory for building the project.<br>
- Navigate into the build folder: Change the current directory to the newly created build directory.<br>
- Run cmake and make: CMake generates the necessary Makefile, and make compiles the project. <br>

- Run the Python main file: This command compiles the module and then runs the specified Python script.


> [!WARNING]  
>  Ensure the input files are present in the pythonBind folder or specify the correct path to the test files to execute the code properly.