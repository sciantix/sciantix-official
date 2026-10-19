"""
Analysis of the tests of the RVE on the cubes of test_rve.py once MMM has solved them: the size of the cube and the size of
the mesh, for s_hat, the stress factor of every pore. No Mérope is needed here. The choices (RUN, EPS_R, EPS_MESH, PRESSURE)
are those at the top of test_rve.py; the side of the cube and the mesh of the runs are read from cubes.txt.

[?] PROVISIONAL: the output of MMM on these cubes is not fixed yet. read_cube below assumes the two files that test-bubble
of MMM leaves today (bubbles_and_stresses_selected.txt, sig_hydro.txt) and the rescaling of OperaHPC; it is the one function
to rewrite when the output is known. Everything after it works on (pore id, radius, s_hat) per cube.

Size of the cube: Kanit et al., Int. J. Solids Struct. 40 (2003) 3647 (K03, ../464-kanit03.pdf), Sections 5.2 and 6.

  1 property       Z: mean of s_hat over the pores of a class in a cube; precision EPS_R on its mean
  2 realisations   the n cubes of the size test, the cube of the runs with n seeds: mean and variance D^2(V) of Z
  3 integral range D^2(V) = S^2 A / V: K03 Eq. 62 (Matheron 1971, Lantuejoul 1991), stated for a quantity additive over V,
                   which the mean of s_hat over the pores is. S^2 is the point variance: K03 Eq. 64 gives it as
                   p (1 - p) (Z1 - Z2)^2 for a two-phase property; here it is the variance of s_hat between the pores of the
                   class [R]. A = D^2 V / S^2 (K03 fits A3 the same way, Eq. 61 and Fig. 9); A * pore density of the class
                   is 1 for independent pores
  4 precision      eps_abs = 2 D(V) / sqrt(n) for n cubes of volume V: K03 Eq. 68; n = 4 D^2(V) / eps_abs^2. With n = 1
                   this is the test: one cube of the runs is an RVE for the class if 2 D / Z <= EPS_R, the precision
                   relative to the mean of the class (K03 Eq. 69)
  5 RVE            the volume for which one cube gives the mean within the precision, n(V = RVE) = 1: K03 Section 6; with
                   Eq. 62 in Eq. 70, RVE = 4 S^2 A / (EPS_R Z)^2
  6 cubes          otherwise (2 D / (Z EPS_R))^2 cubes of the runs give the precision asked: K03 Eq. 70

Size of the mesh: one pore of the mean size of the run (R_V) at its porosity, meshed at every size of MESH_SIZES. The test is
on s_hat of the pore against the finest mesh: on the mesh of the runs it must agree within EPS_MESH. K03 Section 4.1.2 does
the same on one geometry at several resolutions: converged within 1 % at 50 quadratic elements per grain, better than 5 % at
the density retained there.

Usage
    python3 analyse_rve.py           no arguments. For test_rve/run_<RUN>/ of the current directory it reads the solved cubes
                                     listed in cubes.txt and writes rve_size.txt and rve_size.png (size of the cube),
                                     mesh_test.txt and mesh_test.png (size of the mesh). Cubes not solved yet are skipped.
                                     Exit 1 if a class needs more than one cube of the runs, or if s_hat on the mesh of the
                                     runs is more than EPS_MESH from the finest mesh.

@author E. Cappellari
@date   October 2026
"""

import glob
import os
import sys

import matplotlib.pyplot as plt
import numpy as np

from test_rve import EPS_MESH, EPS_R, OUT, PRESSURE, RUN, listed_cubes

SEQ_BLUE, GRID = ("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281"), "#e1e0d9"


# ---- Reading a solved cube [?] provisional: the output of MMM is not fixed yet ------------------------------------------

STRESSES, HYDRO = "bubbles_and_stresses_selected.txt", "sig_hydro.txt"  # files that test-bubble leaves in the folder of a cube


def read_cube(folder, side):
    """Pore ids, radii and s_hat of every pore of a solved cube; None if MMM has not run in it.

    Assumed output [?]: test-bubble with -f bubbles_<file>.txt and -pr PRESSURE, leaving bubbles_and_stresses_selected.txt
    (csv with a header; column 1 the pore id, column 5 the largest first principal stress near the pore) and sig_hydro.txt
    (csv with a header: volume of the matrix, integral and average of its hydrostatic stress). test-bubble looks for the
    maximum within one distance -dm of every centre, among the points above -sf of the largest stress of the cube: pores of
    several radii need -sf 0 and a distance per pore [?].

    s_hat is the factor at zero macroscopic stress of the rescaling of OperaHPC (README_HBFFmodel.md, 4), exact for one
    pressure p in all the pores [P]: (sigma / p + h) / (1 - h), with sigma the largest first principal stress near the pore
    at zero macroscopic strain and h p the hydrostatic compression of the cube in that state. h is that of the whole cube,
    pores at -p included: h = xi - (1 - xi) <sigma_h> / p, with <sigma_h> the average over the matrix of sig_hydro.txt and xi
    the porosity of the mesh. It gives the free hollow sphere, (1 + 2 xi) / (2 (1 - xi)), from the one with a fixed outer
    radius; the average over the matrix alone gives 1/2 at any porosity. [?] D5.2 does not say which one it rescales with."""
    if not all(os.path.exists(os.path.join(folder, name)) for name in (STRESSES, HYDRO)):
        return None
    pores = [p for p in glob.glob(os.path.join(folder, "bubbles_*.txt")) if os.path.basename(p) != STRESSES]
    if len(pores) != 1:
        raise SystemExit(f"{folder}: one bubbles_<file>.txt expected, found {len(pores)}")
    bubbles = np.atleast_2d(np.loadtxt(pores[0]))  # id, x, y, z, radius, pressure
    stress = dict(np.atleast_2d(np.genfromtxt(os.path.join(folder, STRESSES), delimiter=",", skip_header=1, usecols=(0, 4))))
    hydro = np.atleast_1d(np.genfromtxt(os.path.join(folder, HYDRO), delimiter=",", skip_header=1))  # volume, integral, average
    if hydro.size != 3:
        return None  # test-bubble without post-processing
    sigma = np.array([stress.get(bubble, 0.0) for bubble in bubbles[:, 0]])
    if not sigma.all():
        raise SystemExit(f"{folder}: {np.count_nonzero(sigma == 0.0)} pores of {len(sigma)} without a stress in {STRESSES} [?]")
    volume, _, mean = hydro
    if mean / PRESSURE >= 0.0:
        raise SystemExit(f"{folder}: mean hydrostatic stress of the matrix {mean:g} in {HYDRO}: a compression (negative) is expected "
                         "at zero macroscopic strain [?]")
    xi = 1.0 - volume / side**3
    h = xi - (1.0 - xi) * mean / PRESSURE
    return bubbles[:, 0], bubbles[:, 4], (sigma / PRESSURE + h) / (1.0 - h)


# ---- Size of the cube ------------------------------------------------------------------------------------------------

def statistics(cubes, side):
    """Steps 2 to 6 for every class, from the (radius, s_hat) of the solved cubes of the given side. A class is a radius."""
    results = []
    for R in np.unique(np.concatenate([np.round(radius, 9) for radius, _ in cubes])):
        groups = [s_hat[np.round(radius, 9) == R] for radius, s_hat in cubes]
        S2 = np.concatenate(groups).var(ddof=1)  # point variance [R]
        result = {"R": R, "S": np.sqrt(S2), "n": len(groups), "pores": np.mean([len(g) for g in groups]), "cubes": np.nan}
        if len(groups) > 1 and all(len(g) for g in groups):  # the class is in every cube
            Z = np.array([g.mean() for g in groups])
            mean, D2 = Z.mean(), Z.var(ddof=1)
            A = D2 * side**3 / S2                     # step 3, K03 Eq. 62 inverted
            eps_cube = 2.0 * np.sqrt(D2) / mean        # step 4, K03 Eq. 68-69 with n = 1: the test
            result.update(mean=mean, D2=D2, A=A, independent=A * result["pores"] / side**3, eps_cube=eps_cube,
                          rve_side=np.cbrt(4.0 * S2 * A / (EPS_R * mean) ** 2),  # step 5
                          cubes=(eps_cube / EPS_R) ** 2)                          # step 6, K03 Eq. 70
        results.append(result)
    return results


def report(run, side, solved, unsolved, results):
    """Table of the statistics: the lines of rve_size.txt."""
    lines = [f"# size of the RVE of {run} for s_hat (python3 utilities/HBFF/Merope/analyse_rve.py): {solved} cubes of {side:g} um solved, "
             f"{unsolved} not solved",
             f"# load: pressure {PRESSURE:g} in all the pores; precision asked on the mean of a class: {100 * EPS_R:g} %",
             "# Kanit 2003: D^2(V) = S^2 A / V (Eq. 62), eps = 2 D / sqrt(n) (Eq. 68), relative to the mean (Eq. 69), n = 4 D^2 / eps^2 (Eq. 70);",
             "# RVE = 4 S^2 A / eps^2, the volume for n = 1 (Section 6)",
             "#",
             "# per class: cubes n, pores of the class per cube, mean of s_hat with the half-width 2 D / sqrt(n) of its 95 % interval, standard",
             "# deviation D between the cubes, S between the pores, A = D^2 V / S^2 and A x pore density of the class (1: independent pores),",
             f"# relative error of one cube 2 D / mean, cubes of {side:g} um for the precision asked, side of the RVE",
             f"# {'class':>5s}{'radius_um':>10s}{'n':>4s}{'pores':>8s}{'mean':>9s}{'+/-':>9s}{'D':>10s}{'S':>9s}{'A_um3':>10s}{'A_x_dens':>10s}"
             f"{'eps_cube':>10s}{'cubes':>8s}{'RVE_um':>8s}"]
    for k, r in enumerate(results, 1):
        if np.isnan(r["cubes"]):
            lines.append(f"{k:7d}{r['R']:10.4f}{r['n']:4d}{r['pores']:8.1f}{'-':>9s}{'-':>9s}{'-':>10s}{r['S']:9.4f}" + "".join(f"{'-':>{w}s}" for w in (10, 10, 10, 8, 8)))
        else:
            lines.append(f"{k:7d}{r['R']:10.4f}{r['n']:4d}{r['pores']:8.1f}{r['mean']:9.4f}{2.0 * np.sqrt(r['D2'] / r['n']):9.4f}"
                         f"{np.sqrt(r['D2']):10.2e}{r['S']:9.4f}{r['A']:10.2e}{r['independent']:10.2f}{r['eps_cube']:10.4f}{r['cubes']:8.1f}{r['rve_side']:8.1f}")
    more = [f"{k} ({np.ceil(r['cubes']):.0f} cubes)" for k, r in enumerate(results, 1) if r["cubes"] > 1.0]
    unknown = [str(k) for k, r in enumerate(results, 1) if np.isnan(r["cubes"])]
    lines.append("#")
    if more:
        lines.append(f"# one cube of {side:g} um is not an RVE at {100 * EPS_R:g} % for the classes: {', '.join(more)}")
    if unknown:
        lines.append(f"# not determined, the class is missing in a cube: classes {', '.join(unknown)}")
    if not more and not unknown:
        lines.append(f"# one cube of {side:g} um is an RVE at {100 * EPS_R:g} % for every class")
    return lines


def figure(run, side, results, name):
    """Mean of s_hat of every class with the interval of one cube and of the mean of the n cubes; relative error of one cube
    against the precision asked."""
    done = [r for r in results if not np.isnan(r["cubes"])]
    R = np.array([r["R"] for r in done])
    mean, D = np.array([r["mean"] for r in done]), np.sqrt([r["D2"] for r in done])
    fig, ax = plt.subplots(1, 2, figsize=(12.0, 4.6))
    ax[0].errorbar(R, mean, yerr=2.0 * D, color=SEQ_BLUE[1], marker="o", ms=5, lw=0, elinewidth=1.2, capsize=3, label="one cube, $\\pm\\,2D$")
    ax[0].errorbar(R, mean, yerr=2.0 * D / np.sqrt([r["n"] for r in done]), color=SEQ_BLUE[4], marker="o", ms=5, lw=0, elinewidth=2.0,
                   capsize=0, label=r"mean of the cubes, $\pm\,2D/\sqrt{n}$")
    ax[1].axhline(100 * EPS_R, color=GRID, lw=6, label=f"precision asked, {100 * EPS_R:g} %")
    ax[1].plot(R, 100 * np.array([r["eps_cube"] for r in done]), "o-", color=SEQ_BLUE[3], ms=5, lw=1.2)
    ax[0].set(xlabel="radius of the class (um)", ylabel=r"mean of $\hat s$ of the class", title=f"{run}: cubes of {side:g} um")
    ax[1].set(xlabel="radius of the class (um)", ylabel="relative error of one cube, 2 D / mean (%)", ylim=(0.0, None),
              title="one cube is an RVE below the line")
    for ax_ in ax:
        ax_.grid(True, color=GRID)
        ax_.legend(frameon=False)
    fig.tight_layout()
    fig.savefig(name, dpi=150)
    plt.close(fig)


def analyse_size(out, run, rows):
    """Size test on the solved cubes of a run folder: rve_size.txt and rve_size.png. Returns 1 if one cube of the runs is not
    an RVE for a class, 0 otherwise (also when nothing is solved yet)."""
    if not rows:
        print(f"{out}: no cube of the size test listed")
        return 0
    side = rows[0]["side"]
    assert all(row["side"] == side for row in rows), "the cubes of the size test have one side"
    cubes = [read_cube(os.path.join(out, row["folder"]), side) for row in rows]
    cubes = [(data[1], data[2]) for data in cubes if data is not None]
    if not cubes:
        print(f"{out}: no cube of the size test solved yet")
        return 0
    results = statistics(cubes, side)
    lines = report(run, side, len(cubes), len(rows) - len(cubes), results)
    with open(os.path.join(out, "rve_size.txt"), "w") as f:
        f.write("\n".join(lines) + "\n")
    figure(run, side, results, os.path.join(out, "rve_size.png"))
    print("\n".join(lines))
    print(f"wrote {os.path.join(out, 'rve_size.txt')} and {os.path.join(out, 'rve_size.png')}")
    return 0 if all(r["cubes"] <= 1.0 for r in results) else 1


# ---- Size of the mesh ------------------------------------------------------------------------------------------------

def mesh_report(run, radius, side, mesh, s_hat, mesh_runs):
    """Table of the mesh test: the lines of mesh_test.txt."""
    lines = [f"# size of the mesh for s_hat, {run} (python3 utilities/HBFF/Merope/analyse_rve.py): one pore of R = {radius:.4f} um, the R_V "
             f"of the run, in a cube of {side:.4f} um (the porosity of the run), meshed at " + ", ".join(f"{h:g}" for h in mesh) + " um",
             f"# load: pressure {PRESSURE:g}; tolerance on s_hat between the mesh of the runs ({mesh_runs:g} um) and the finest: {100 * EPS_MESH:g} % [?]",
             "# Kanit 2003, Section 4.1.2: one geometry at several mesh densities, converged within 1 % at 50 quadratic elements per grain, 5 % at the density retained",
             "#",
             f"# {'mesh_um':>7s}{'h/2R':>8s}{'s_hat':>10s}{'d_finest':>10s}"]
    lines += [f"{h:9g}{h / (2.0 * radius):8.3f}{s:10.4f}{100 * (s / s_hat[0] - 1.0):9.2f}%" for h, s in zip(mesh, s_hat)]
    d = s_hat[np.isclose(mesh, mesh_runs)][0] / s_hat[0] - 1.0
    lines += ["#", f"# the mesh of the runs, {mesh_runs:g} um, moves s_hat by {100 * d:+.2f} % from the finest, {mesh[0]:g} um: "
                   + ("converged" if abs(d) <= EPS_MESH else "not converged") + f" at {100 * EPS_MESH:g} %"]
    return lines


def mesh_figure(run, radius, mesh, s_hat, mesh_runs, name):
    """s_hat of the pore against the mesh size, with the tolerance around the finest mesh."""
    fig, ax = plt.subplots(figsize=(6.4, 4.4))
    ax.axhspan(s_hat[0] * (1.0 - EPS_MESH), s_hat[0] * (1.0 + EPS_MESH), color=GRID, alpha=0.6, lw=0,
               label=rf"finest mesh $\pm${100 * EPS_MESH:g} %")
    ax.axvline(mesh_runs, color=SEQ_BLUE[1], lw=1.0, ls="--", label="mesh of the runs")
    ax.plot(mesh, s_hat, "o-", color=SEQ_BLUE[3], ms=6, lw=1.2)
    ax.set(xlabel="mesh size (um)", ylabel=r"$\hat s$", title=f"{run}: one pore of R = {radius:.2f} um")
    ax.grid(True, color=GRID)
    ax.legend(frameon=False)
    fig.tight_layout()
    fig.savefig(name, dpi=150)
    plt.close(fig)


def analyse_mesh(out, run, rows, mesh_runs):
    """Mesh test on a run folder, from the rows of cubes.txt: s_hat of the pore of every mesh cube against the finest one.
    mesh_test.txt and mesh_test.png. Returns 1 if s_hat on mesh_runs, the mesh of the runs, is more than EPS_MESH from the
    finest mesh, 0 otherwise (also when a mesh cube is not solved yet)."""
    rows = sorted((row for row in rows if row["test"] == "mesh"), key=lambda row: row["mesh"])
    if len(rows) < 2 or not any(np.isclose(row["mesh"], mesh_runs) for row in rows[1:]):
        print(f"{out}: the mesh test needs the mesh of the runs ({mesh_runs:g} um) and a finer one")
        return 0
    data = [read_cube(os.path.join(out, row["folder"]), row["side"]) for row in rows]
    if any(d is None for d in data):
        print(f"{out}: the mesh test waits for MMM in {', '.join(row['folder'] for row, d in zip(rows, data) if d is None)}")
        return 0
    if any(len(d[2]) != 1 for d in data):
        raise SystemExit(f"{out}: one pore per mesh cube expected")
    mesh, s_hat = np.array([row["mesh"] for row in rows]), np.array([d[2][0] for d in data])
    radius = data[0][1][0]
    lines = mesh_report(run, radius, rows[0]["side"], mesh, s_hat, mesh_runs)
    with open(os.path.join(out, "mesh_test.txt"), "w") as f:
        f.write("\n".join(lines) + "\n")
    mesh_figure(run, radius, mesh, s_hat, mesh_runs, os.path.join(out, "mesh_test.png"))
    print("\n".join(lines))
    print(f"wrote {os.path.join(out, 'mesh_test.txt')} and {os.path.join(out, 'mesh_test.png')}")
    return 0 if abs(s_hat[np.isclose(mesh, mesh_runs)][0] / s_hat[0] - 1.0) <= EPS_MESH else 1


# ---- Main ---------------------------------------------------------------------------------------------------------

def main():
    """Size and mesh tests on the folder of RUN. Returns the worst status."""
    out, run = os.path.join(OUT, f"run_{RUN}"), f"run_{RUN}"
    if not os.path.exists(os.path.join(out, "cubes.txt")):
        print(f"no cubes.txt in {out}: nothing to analyse (python3 test_rve.py writes the cubes)")
        return 0
    rows = listed_cubes(out)
    size = [row for row in rows if row["test"] == "size"]
    print(f"\n== {run}: size of the cube ==")
    status = analyse_size(out, run, size)
    print(f"\n== {run}: size of the mesh ==")
    if size:
        status = max(status, analyse_mesh(out, run, rows, size[0]["mesh"]))
    else:
        print(f"{out}: no cube of the size test listed, so no mesh of the runs to test")
    return status


if __name__ == "__main__":
    sys.exit(main())
