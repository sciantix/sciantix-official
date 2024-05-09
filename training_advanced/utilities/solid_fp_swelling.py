import numpy as np

# Olander 1976 - Formula for swelling due to solid FPs

fima_burnup = np.linspace(0, 4, 100)

# UO2 - Elemental fission product yields in fast-neutron spectrum
y = np.zeros(8)
y[0] = 0.149  # Zr + Nb
y[1] = 0.534  # Y + rare earths
y[2] = 0.149  # Ba + Sr
y[3] = 0.240  # Mo
y[4] = 0.263  # Ru + Te + Rh + Pd
y[5] = 0.226  # Cs + Rb
y[6] = 0.012  # I + Te
y[7] = 0.27   # Xe + Kr

# Swelling due to solid fission products in pure UO2
y_vi_vu = np.zeros(5)
y_vi_vu[0] = 0.683  # total soluble fission products (Nb + soluble Zr + Y + rare earths)
y_vi_vu[1] = 0.181  # total metallic inclusions (Mo + Ru + Te + Rh + Pd)
y_vi_vu[2] = 0.258  # Ba + Sr (as zirconates)
y_vi_vu[3] = 0.181  # Cs + Rb + I + Te
y_vi_vu[4] = 0.021  # Other fission products (to be excluded, Xe + Kr)

# Solid swelling
solid_swelling = sum(y_vi_vu) - y_vi_vu[4] - 1
swelling = fima_burnup * solid_swelling

print(solid_swelling)
