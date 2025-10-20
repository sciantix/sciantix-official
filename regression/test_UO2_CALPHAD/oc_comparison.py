import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from scipy.interpolate import griddata
from io import StringIO

import subprocess

subprocess.run(["gnuplot", "-persist", "ocgnu.plt"])
