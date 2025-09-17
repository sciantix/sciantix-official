import os
import numpy as np
import matplotlib.pyplot as plt

def plot_txt_files(folder="."):
    txt_files = [f for f in os.listdir(folder) if f.endswith(".txt")]
    txt_files.sort()

    plt.figure(figsize=(10, 6))

    for file in txt_files:
        path = os.path.join(folder, file)
        try:
            # Carico provando a saltare un'eventuale riga di header
            try:
                data = np.loadtxt(path, comments="#", skiprows=1)
            except ValueError:
                # fallback: forse non c'era header
                data = np.loadtxt(path, comments="#")

            # Se è un solo numero → trasformalo in array
            if data.ndim == 0:
                data = np.array([data])

            # Caso 1: una colonna → y vs indice
            if data.ndim == 1:
                x = np.arange(len(data))
                y = data
                plt.plot(x, y, label=file)

            # Caso 2: due o più colonne → prima colonna = x, seconda = y
            elif data.ndim == 2 and data.shape[1] >= 2:
                x = data[:, 0]
                y = data[:, 1]
                plt.scatter(x, y, label=file)
                plt.xlabel("X", fontsize=12)
                plt.ylabel("Y", fontsize=12)
                plt.title("Plot automatico dei file .txt", fontsize=14)
                plt.legend(fontsize=8)
                plt.grid(True)
                plt.tight_layout()
                plt.show()
                
        except Exception as e:
            print(f"Errore con {file}: {e}")

if __name__ == "__main__":
    plot_txt_files(".")
