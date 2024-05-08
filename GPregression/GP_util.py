"""

This is a python script to perform the correlation update in sciantix.

@author G. Nicodemo

"""

""" ------------------- Import requiered depedencies ------------------- """
import numpy as np
import math
import os
import shutil

""" ------------------- Some useful geometric functions ------------------ """
def read_one_parameter(input_file, output_file):
    # Load settings
    with open(input_file, 'r') as input_file:
        settings_list = []
        for line in input_file:
            parts = line.split('#')
            if len(parts) > 1:
                value_part = parts[0].strip()  # Extract the value part before the comment
                setting_value = int(value_part.split()[0])  # Extract the value
                settings_list.append(setting_value)

    # Convert the list to a NumPy array
    settings = np.array(settings_list)
    
    # Write the loaded settings to a file with an index
    with open(output_file, 'w') as output_file:
        for i, setting in enumerate(settings):
            output_file.write(f"Input setting #{i}: \tXeDiffusionCoefficient = {setting}\n")
    return settings

def write_matrix_to_txt(matrix, folder_path, file_name):
    file_path = os.path.join(folder_path, file_name)
    
    # Write the matrix to the text file
    np.savetxt(file_path, matrix, fmt='%.5f') 

def parse_value(value_str):
    try:
        # Try converting to int
        value = int(value_str)
        return value
    except ValueError:
        try:
            # Try converting to float
            value = float(value_str)
            return value
        except ValueError:
            # If neither int nor float, return as string
            return value_str.strip()

def load_settings(filepath):
    settings = []
    with open(filepath, 'r') as file:
        for line in file:
            line = line.strip()
            if not line:
                continue
            parts = line.split('#')
            value_str = parts[0].strip()
            value = parse_value(value_str)
            settings.append(value)
    return settings

def move_all_images(source_folder, destination_folder):
    # Make sure source and destination folders exist
    if not os.path.exists(destination_folder):
        os.makedirs(destination_folder)

    # Get all files in the source folder
    files = os.listdir(source_folder)

    # Iterate through each file
    for file in files:
        # Check if the file is an image file (you can add more extensions if needed)
        if file.lower().endswith(('.png', '.jpg', '.jpeg', '.gif', '.bmp')):
            source_file_path = os.path.join(source_folder, file)
            destination_file_path = os.path.join(destination_folder, file)
            shutil.move(source_file_path, destination_file_path)