# Development/PyBind11 

This branch is used for the implementation of PyBind11 in the sciantix 2.0 code. 

How to use PyBind11 with the sciantix code
> [!NOTE]  
> install with pip of pyBind11 the global version
### How to use the code :

Install pyBind11 : 
```python
pip install "pybind11[global]"
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
mkdir build && cd build && cmake .. && make && python3 ../pythonBind/main.py
```
Explantion : 

This creates the build folder, inside this folder cmake create the makefile. The module is then compiled with the <u>MakeFile</u>. Then it runs the python <b>Main file</b>.

> [!WARNING]  
>  The inputs files are required in the pythonBind folder in order to execute correctly 