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
def euclidean_distance(point1, point2):
    if len(point1) != len(point2):
        raise ValueError("Points must have the same dimensionality")

    squared_distance = sum((x - y) ** 2 for x, y in zip(point1, point2))
    distance = math.sqrt(squared_distance)
    return distance

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
    
def move_file(source_folder, destination_folder, file_name):
    source_file_path = os.path.join(source_folder, file_name)
    if os.path.exists(source_file_path):
        
        destination_file_path = os.path.join(destination_folder, file_name)
        shutil.move(source_file_path, destination_file_path)


def vertical_distance_with_uncertainty(points, a, b, c, dc, dz): 
    
    N = points.shape[0]
    z_nominal_plane = a * points[:, 0] + b * points[:, 1] + c
    
    vertical_distances_nominal = (points[:, 2] - z_nominal_plane)
    
    # Compute the Z-coordinate of the point on the perturbed planes
    z_min_plane = a * points[:, 0] + b * points[:, 1] + c - dc
    z_max_plane = a * points[:, 0] + b * points[:, 1] + c + dc
    
    vertical_distances_max = np.array(np.zeros(N)).reshape(-1,1)
    vertical_distances_min = np.array(np.zeros(N)).reshape(-1,1)
    
    for i in range(N):
        vertical_distances_max[i] = np.max(points[i, 2] + dz[i] - z_min_plane, points[:, 2] - dz[i] + z_max_plane)
        vertical_distances_min[i] = np.min(points[i, 2] - dz[i] - z_max_plane, points[:, 2] + dz[i] - z_min_plane)
    
    uncertainty = np.abs((vertical_distances_max - vertical_distances_min) / 2)
    
    # Return the nominal distances and uncertainties
    return vertical_distances_nominal, uncertainty



