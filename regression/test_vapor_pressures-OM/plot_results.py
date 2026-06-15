import pandas as pd
import matplotlib.pyplot as plt

# Leggiamo i dati (assicurati che il CSV sia stato appena rigenerato)
df = pd.read_csv("vapor_results.csv")
df.columns = df.columns.str.strip()

# Rimuoviamo eventuali duplicati se hai lanciato il codice più volte
df = df.drop_duplicates(subset=['OM'])

# FORMATO VERTICALE (stile Olander)
plt.figure(figsize=(7, 10))

species_to_plot = {
    'p_U': 'gray',       # Nuova
    'p_Pu': 'magenta',   # Nuova
    'p_UO': 'blue',      # Nuova
    'p_UO2': 'green',
    'p_UO3': 'cyan',
    'p_PuO': 'red',
    'p_PuO2': 'orange'
}

for sp, col in species_to_plot.items():
    if sp in df.columns:
        # Linewidth alto per rendere le curve "lisce" alla vista
        plt.plot(df['OM'], df[sp], label=sp, color=col, linewidth=2.5)

plt.yscale('log')
plt.axvline(x=2.0, color='black', linestyle='-', linewidth=1, alpha=0.5)

plt.xlim(1.95, 2.12)
plt.ylim(1e-14, 1e-2)

plt.xlabel('O/M RATIO', fontsize=14, fontweight='bold')
plt.ylabel('LOG p ATM', fontsize=14, fontweight='bold')
plt.title('Partial Pressures at 2000 K', fontsize=12, pad=20)

# Estetica pulita
plt.grid(True, which="both", linestyle=':', alpha=0.3)
plt.tick_params(direction='in', top=True, right=True, which='both')
plt.legend(loc='lower right', frameon=True)

plt.tight_layout()
plt.show()