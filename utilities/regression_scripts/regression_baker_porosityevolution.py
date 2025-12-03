import sys 
import os
import shutil
import subprocess
import pandas as pd
import matplotlib.pyplot as plt

# Define the directories
test_baker_dir = 'test_Baker1977_1273K_porosityevolution'
build_dir = '../build'
sciantix_executable = os.path.join(build_dir, 'sciantix.x')

def copy_sciantix_executable():
    try:
        shutil.copy(sciantix_executable, test_baker_dir)
        print(f"Copied {sciantix_executable} to {test_baker_dir}")
    except Exception as e:
        print(f"Error copying sciantix.x: {e}")

def run_sciantix():
    try:
        subprocess.run(["./sciantix.x"], cwd=test_baker_dir, check=True)
        print("Sciantix execution completed successfully.")
    except subprocess.CalledProcessError as e:
        print(f"Error running Sciantix: {e}")

copy_sciantix_executable()
run_sciantix()

# Initialize the data
data1 = pd.read_csv(os.path.join(test_baker_dir, 'output.txt'), delimiter='\t')
data2 = pd.read_csv(os.path.join(test_baker_dir, 'output_gold.txt'), delimiter='\t')

data = [data1, data2]
labels = ['Current work', 'Gold']
kinds = ['-', ':'] 

# Function to safely create new columns if the required ones exist
def safe_create_column(df, new_col, base_col, factor):
    if base_col in df.columns:
        df[new_col] = df[base_col] * factor

for i in range(2):  
    safe_create_column(data[i], "Fission gas release (%)", "Fission gas release (/)", 100)

# Function to plot if both x and y columns exist
def safe_plot(df, x_col, y_col, linestyle, ax, label=None):
    if x_col in df.columns and y_col in df.columns:
        df.plot(x=x_col, y=y_col, kind="line", linestyle=linestyle, ax=ax, label=label)

# Fission Gas Release plot
fig, ax = plt.subplots(figsize=(11, 8))
for i in range(2):
    safe_plot(data[i], "Burnup (MWd/kgUO2)", "Fission gas release (%)", kinds[i], ax, labels[i])
ax.set_xlabel("Burnup (MWd/kgUO2)")
ax.set_ylabel("Fission Gas Release (%)")
plt.tight_layout()

# Intergranular Fractional Coverage plot
fig, ax = plt.subplots(figsize=(11, 8))
for i in range(2):
    safe_plot(data[i], "Burnup (MWd/kgUO2)", "Intergranular fractional coverage (/)", kinds[i], ax, labels[i])
ax.set_xlabel("Burnup (MWd/kgUO2)")
ax.set_ylabel("Intergranular fractional coverage (/)")
plt.tight_layout()

for i in range(2):
    safe_create_column(data[i], "Porosity (%)", "Porosity (/)", 100)
    safe_create_column(data[i], "Residual porosity (%)", "Residual porosity (/)", 100)
    safe_create_column(data[i], "Open porosity (%)", "Open porosity (/)", 1000)
    safe_create_column(data[i], "Fabrication porosity (%)", "Fabrication porosity (/)", 100)

# Porosity plot
fig, ax = plt.subplots(figsize=(11, 8))
for i in range(2):
    safe_plot(data[i], "Burnup (MWd/kgUO2)", "Porosity (%)", kinds[i], ax)
    safe_plot(data[i], "Burnup (MWd/kgUO2)", "Fabrication porosity (%)", kinds[i], ax)
    safe_plot(data[i], "Burnup (MWd/kgUO2)", "Open porosity (%)", kinds[i], ax, label='Open porosity(%) *10')
    safe_plot(data[i], "Burnup (MWd/kgUO2)", "Residual porosity (%)", kinds[i], ax, label='Residual porosity(%)')
ax.set_xlabel("Burnup (MWd/kgUO2)")
ax.set_ylabel("Porosity (%)")
plt.tight_layout()
plt.show()
