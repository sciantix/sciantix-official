"""
sciantix testing suite
author: Giovanni Zullo, Elisa Cappellari
"""

import numpy as np


def _drop_columns(header, data, ignore_columns):
    """
    Return (header, data) with any column whose name contains one of the
    ignore_columns substrings removed. Used to exclude OpenCalphad-derived
    columns (e.g. "CALPHAD") from the comparison when OC is unavailable.
    """
    if not ignore_columns:
        return header, data

    keep = [i for i, name in enumerate(header)
            if not any(marker in name for marker in ignore_columns)]
    if len(keep) == len(header):
        return header, data

    new_header = np.asarray([header[i] for i in keep])
    new_data = data[:, keep]
    return new_header, new_data


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

    one_sided_nan = np.isnan(a) ^ np.isnan(b)
    bad_mask = ((diff > abs_tol) & (rel > rel_tol)) | one_sided_nan
    ok = not np.any(bad_mask)

    return ok, diff, bad_mask


def compare_outputs(out, gold,
                    abs_tol=1e-8,
                    rel_tol=1e-6,
                    verbose=False,
                    ignore_columns=()):
    """
    Compare two SciantixOutput objects.

    Args:
        out (SciantixOutput)
        gold (SciantixOutput)
        abs_tol, rel_tol: tolerances
        verbose: print mismatch locations
        ignore_columns: substrings; any column whose header name contains one
            of these is excluded from the comparison before it runs (e.g.
            ["CALPHAD"] to skip OpenCalphad-derived columns when OC is
            unavailable). Empty by default -- compares every column, as
            before.

    Returns:
        bool: True if match within tolerance
    """

    out_header, out_data = _drop_columns(out.header, out.data, ignore_columns)
    gold_header, gold_data = _drop_columns(gold.header, gold.data, ignore_columns)

    if not np.array_equal(out_header, gold_header):
        if verbose:
            print("[compare] Header mismatch:")
            for i in range(max(len(out_header), len(gold_header))):
                o = out_header[i] if i < len(out_header) else "<missing>"
                g = gold_header[i] if i < len(gold_header) else "<missing>"
                if o != g:
                    print(f"  col={i}, output={o!r}, gold={g!r}")
        return False

    if out_data.shape != gold_data.shape:
        if verbose:
            print(f"[compare] Shape mismatch: "
                  f"output={out_data.shape}, gold={gold_data.shape}")
        return False

    ok, diff, bad_mask = compare_arrays(out_data, gold_data,
                                        abs_tol, rel_tol)

    if not ok and verbose:
        print("[compare] Differences beyond tolerance:")
        rows, cols = np.where(bad_mask)
        for r, c in zip(rows, cols):
            print(f"  row={r}, col={c}, "
                  f"output={out_data[r,c]}, gold={gold_data[r,c]}, "
                  f"diff={diff[r,c]}")

    return ok
