import numpy as np
import matplotlib.pyplot as plt

# Generate 10 random values for A and n
np.random.seed(42)  # for reproducibility
A_values = np.random.uniform(low=1e-7, high=1e-6, size=10)
n_values = np.random.uniform(low=3.0, high=5.0, size=10)

# Define the burnup range
bueff_values = np.linspace(0, 100, 1000)

# Plot alpha curves for each A, n pair
plt.figure(figsize=(10, 6))
for i in range(10):
    A = A_values[i]
    n = n_values[i]
    
    def compute_alpha(bueff):
        return 1 - np.exp(-A * bueff**n)
    
    def alpha_ref(bueff):
        return 1 - np.exp(-2.77e-7 * bueff**3.54)
    
    alpha_values = compute_alpha(bueff_values)
    alpha_values_ref = alpha_ref(bueff_values)

    plt.plot(bueff_values, alpha_values, label=f"A={A:.1e}, n={n:.2f}")

plt.plot(bueff_values, alpha_values_ref, label=f"Baranui et al. 2022", color='black', linestyle='--', linewidth=2)
plt.xlabel("Effective Burnup (bueff)")
plt.ylabel("Alpha")
plt.title("Alpha vs Effective Burnup for Random A and n")
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()

# Plot derivative curves for each A, n pair
# plt.figure(figsize=(10, 6))
# for i in range(10):
#     A = A_values[i]
#     n = n_values[i]
    
#     def compute_dalpha_db(bueff):
#         return A * n * bueff**(n - 1) * np.exp(-A * bueff**n)
    
#     dalpha_db_values = compute_dalpha_db(bueff_values)
#     plt.plot(bueff_values, dalpha_db_values, label=f"A={A:.1e}, n={n:.2f}")

# plt.xlabel("Effective Burnup (bueff)")
# plt.ylabel("d(alpha)/db")
# plt.title("Derivative of Alpha for Random A and n")
# plt.grid(True)
# plt.legend()
# plt.tight_layout()
# plt.show()
