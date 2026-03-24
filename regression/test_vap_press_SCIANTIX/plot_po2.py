import pandas as pd
import matplotlib.pyplot as plt

filename = "output.txt"

try:
    # Load data from SCIANTIX output
    df = pd.read_csv(filename, sep='\t')
    df.columns = df.columns.str.strip()
    
    # Define columns for plotting
    col_T = "Temperature (K)"
    col_po2 = "Fuel oxygen partial pressure (MPa)"
    
    # Conversion from MPa to atm (1 MPa = 9.86923 atm)
    df['p_O2 (atm)'] = df[col_po2] * 9.86923
    print(df["Stoichiometry deviation (/)"])

    # Plotting
    plt.figure(figsize=(8, 6))
    
    # Sort by temperature to ensure a continuous line plot
    df_plot = df.sort_values(by=col_T)
    
    plt.plot(df_plot[col_T], df_plot['p_O2 (atm)'], color='blue', linewidth=2, label='p$_{O_2}$ (Kato Model)')
    
    # Graph
    plt.title("Oxygen Partial Pressure vs Temperature", fontsize=14)
    plt.xlabel("Temperature (K)", fontsize=12)
    plt.ylabel("p$_{O_2}$ (atm)", fontsize=12)
    
    # Use logarithmic scale for pressure
    plt.yscale('log')
    
    plt.grid(True, which="both", linestyle=':', alpha=0.6)
    plt.legend()
    plt.tight_layout()
    
    # Display the plot
    plt.show()

except Exception as e:
    print(f"Error: {e}")