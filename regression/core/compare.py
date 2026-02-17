"""
sciantix regression suite
author: Giovanni Zullo
"""

import numpy as np

def compare_arrays(a: np.ndarray, b: np.ndarray,
                   abs_tol: float = 1e-8,
                   rel_tol: float = 1e-6):
    """
    Element-wise numerical comparison between two arrays with
    absolute and relative tolerances.

    Returns:
        ok (bool): True if all elements match within tolerance.
        diff (ndarray): absolute differences |a - b|.
        bad_mask (ndarray): boolean mask marking elements that fail.
    """

    if a.shape != b.shape:
        return False, None, None

    diff = np.abs(a - b)
    rel = diff / np.maximum(abs_tol, np.abs(b))

    bad_mask = (diff > abs_tol) & (rel > rel_tol)
    ok = not np.any(bad_mask)

    return ok, diff, bad_mask


def compare_outputs(out, gold,
                    abs_tol=1e-8,
                    rel_tol=1e-6,
                    verbose=False):
    """
    Compare two SciantixOutput objects.

    Args:
        out (SciantixOutput)
        gold (SciantixOutput)
        abs_tol, rel_tol: tolerances
        verbose: print mismatch locations

    Returns:
        bool: True if match within tolerance
    """

    if out.data.shape != gold.data.shape:
        if verbose:
            print(f"[compare] Shape mismatch: "
                  f"output={out.data.shape}, gold={gold.data.shape}")
        return False

    ok, diff, bad_mask = compare_arrays(out.data, gold.data,
                                        abs_tol, rel_tol)

    if not ok and verbose:
        print("[compare] Differences beyond tolerance:")
        rows, cols = np.where(bad_mask)
        for r, c in zip(rows, cols):
            print(f"  row={r}, col={c}, "
                  f"output={out.data[r,c]}, gold={gold.data[r,c]}, "
                  f"diff={diff[r,c]}")

    return ok
