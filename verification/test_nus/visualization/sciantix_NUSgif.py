import pandas as pd
import matplotlib.pyplot as plt
import imageio.v2 as imageio  # Avoid deprecation warning
import os

# Read the data
df = pd.read_csv("source_gif.txt", sep="\t", engine="python")

# Extract radius column and time step columns
r = df.iloc[:, 0]
time_steps = df.columns[1:]

# Create output directory for frames
os.makedirs("frames", exist_ok=True)

filenames = []
for i, col in enumerate(time_steps):
    # Create plot
    plt.figure()
    plt.plot(r, df[col])
    plt.xlabel("r (micron)")
    plt.ylabel("Source (at/m³/s)")
    plt.title(col)
    plt.ylim(bottom=0)  # assuming source is always positive

    # Save frame
    filename = f"frames/frame_{i:03}.png"
    plt.savefig(filename)
    plt.close()
    filenames.append(filename)

# Create GIF
with imageio.get_writer("source_evolution.gif", mode="I", duration=0.05) as writer:
    for filename in filenames:
        image = imageio.imread(filename)
        writer.append_data(image)

print("GIF created: source_evolution.gif")