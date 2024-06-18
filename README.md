# Development/PyBind11 

This branch is used for the implementation of PyBind11 in the sciantix 2.0 code. 

How to use PyBind11 with the sciantix code
> [!NOTE]  
> install with pip of pyBind11
### How to use the code :
> [!WARNING]  
> If you don't use linux you need to change Python3 to python.
>
Install pyBind11 : 
```python
pip install pybind11
```
<b>Create the build folder and go inside it with this command : </b>
```shell
mkdir build 
cd build
```

<b>For compilation From the Build folder :</b>

```shell
cmake .. && make && python3 ../pythonBind/main.py
```

<b>All in one : </b>

```shell
mkdir build && cd build && cmake .. && make && python3 ../src/pythonBind/[main_file].py
```
Explantion : 

This creates the build folder, inside this folder cmake create the makefile. The module is then compiled with the <u>MakeFile</u>. Then it runs the python <b>Main file</b>.