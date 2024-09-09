# MMS verification with Pybind11 Module

## How it's working : 

> [!WARNING]  
> You need to compile the Pybind11 Module before lauching the code check the main README.md to see how.
> 

The Verification is launched with the <u>mainVerification.py</u> after starting the script a Menu shows up and you need to choose between all the solver and functions. 

Here is the command for Linux users : 
```bash
python3 ./mainVerification.py
```
Here are all the functions used for the solvers : 

- $\ F0: y = t^3$
- $\ F1: y = t \sin(t)$
- $\ F2: y = t  e^t \sin(t)$

> [!NOTE] 
> This are the functions used for theses solvers : Integretor, Binary interaction, Decay Limited Growth. For the spectral verification we don't have yet a correct solution. 

> [!TIP]
> If you need more information check the paper : {link}
