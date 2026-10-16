"""
Cubes for two tests of the RVE of the runs for MMM:
- the size of the cube, on the reference run NFIR of ../mmm_inputs: the NFIR disc before the anneal, the log-normal fit of
  its measured pores (not the SCIANTIX state);
- the size of the mesh, on one pore of the mean size of the disc.
This script writes the geometry only (bubbles, .geo, summary) with Mérope.

Both tests follow Kanit et al., Int. J. Solids Struct. 40 (2003) 3647: the size of the cube is
Sections 5.2 and 6 (Eq. 61-64, 66, 68-72), the size of the mesh is Section 4.1.2.

Size of the cube, base: SEEDS cubes of the side of the runs, SIDE, meshed at MESH_SIZE, with the classes of RUN. Over them,
the mean Z and the standard deviation D of the mean of s_hat over the pores of a class; one cube gives the mean within 2 D / Z
(K03 Eq. 68-69 with n = 1), and SIDE is an RVE at EPS_R for the class if 2 D / Z <= EPS_R (K03 Section 6). If it is not, the
number of cubes needed is n = (2 D / (Z EPS_R))^2 (Eq. 70), and the side of the RVE follows from D^2(V) = S^2 A / V (Eq. 62).

Size of the cube, exploration (EXPLORE = True, only if the base is not enough or to confirm it): EXPLORE_CUBES, a smaller
side, to check that D^2 scales as 1 / V (the exponent alpha of K03 Eq. 63) before extrapolating, and a larger side, to check
that the mean of SIDE has no bias (a bias does not show in the scatter of cubes of one side).

Size of the mesh: one pore of radius R_V, the radius of the mean pore volume of RUN, in a periodic cube at the porosity of RUN
[R], meshed at every size of MESH_SIZES with the same pore. K03 Section 4.1.2 does the same on one geometry at several
resolutions. The finest mesh is the reference: the mesh of the runs, MESH_SIZE, is converged if s_hat moves by less than
EPS_MESH from it.

Load of the cubes: one pressure in all the pores, PRESSURE, as in OperaHPC D5.2 and in test-bubble of MMM; it is written in
the sixth column of bubbles_<run>.txt, as the class pressures of the runs by distributedpore.py.

Usage
    python3 test_rve.py             Writes the cubes in test_rve/<run>/ of the current directory (needs Mérope): the base
                                     (SEEDS cubes), the mesh cubes, and the exploration if EXPLORE. A group whose cubes are
                                     listed in cubes.txt already is skipped, so EXPLORE can be switched on later.

A cube is a folder side<L>_seed<n> (base and exploration) or mesh<h> (mesh test) with bubbles_<run>.txt (id, x, y, z,
radius, pressure: the columns of distributedpore.py), mesh_<run>.geo and summary_<run>.txt; cubes.txt lists them (test, side,
seed, mesh, folder), test being base, explore or mesh. [?] test-bubble -f of MMM reads lines of five columns only
(BubbleDescription.cxx): its reader has to take the sixth, as for the runs.

@author E. Cappellari
@date   October 2026
"""

import os
import sys

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
RUN_DIR = os.path.join(HERE, "..", "mmm_inputs")
OUT = "test_rve"  # folder of the cubes, in the current directory

# Choices
RUN = "NFIR"                      # run of ../mmm_inputs: the reference, the NFIR disc before the anneal (pore_distribution.py)
SIDE = 30.0                       # um: side of the base cubes, that of the runs (distributedpore.dimensions_side)
SEEDS = 10                        # seeds tested, as in OperaHPC D5.2
EPS_R = 0.05                      # relative precision asked on the mean of s_hat of a class
EXPLORE = False                   # True: also the cubes of EXPLORE_CUBES
EXPLORE_CUBES = {20.0: 10, 40.0: 2}
MESH_SIZE = 0.15                  # um: mesh of the runs (distributedpore.mesh_size)
MESH_SIZES = (0.30, 0.15, 0.075)  # um: meshes of the pore of the mesh test; the finest is the reference
EPS_MESH = EPS_R                  # tolerance on s_hat between MESH_SIZE and the finest mesh
DIST_MIN = 0.3                    # um: repulsion distance between the surfaces of two pores
PRESSURE = 1.0                    # unit pressure of every pore (s_hat = sigma_I / p), no distribution


# ---- Run files and Mérope ---------
def read_run(path):
    """Classes of a run file: radius (um), pore fraction (-), pressure (MPa), confining pressure (MPa); largest radius first."""
    try:
        data = np.atleast_2d(np.loadtxt(path))
    except ValueError:
        raise SystemExit(f"{path}: not a run file (rows of 4 numbers, comments starting with #)")
    if data.shape[1] != 4:
        raise SystemExit(f"{path}: 4 columns expected (radius, porosity, pressure, confinement), found {data.shape[1]}")
    if np.ptp(data[:, 3]) > 0.0:
        raise SystemExit(f"{path}: the confining pressure differs between the rows")
    data = data[np.argsort(-data[:, 0])]
    return data[:, 0], data[:, 1], data[:, 2], float(data[0, 3])


def pores_in_cube(radius, pore_fraction, side):
    """Radius of the mean pore volume and expected number of pores per class in a cube of the given side: a class of porosity
    pf and pore volume V has pf / V pores per unit volume."""
    volume = 4.0 / 3.0 * np.pi * radius**3
    density = pore_fraction / volume
    r_v = np.cbrt(3.0 * pore_fraction.sum() / density.sum() / (4.0 * np.pi))
    return r_v, density * side**3


def smallest_gap(centres, radii, side):
    """Smallest distance between the surfaces of two pores, with the periodicity of the cube (a pore and its own image too)."""
    gap = (side - 2.0 * radii).min()
    for i in range(len(radii) - 1):
        d = centres[i + 1:] - centres[i]
        d -= side * np.round(d / side)
        gap = min(gap, (np.linalg.norm(d, axis=1) - radii[i + 1:] - radii[i]).min())
    return gap


def generate(run, radius, pore_fraction, counts, side, seed, mesh_size, folder):
    """Pores in a periodic cube, counts of them per class of radius and pore fraction (largest radius first), and the files of
    the cube: bubbles_<run>.txt (id, x, y, z, radius, pressure PRESSURE), mesh_<run>.geo at the given mesh size,
    summary_<run>.txt."""
    import sac_de_billes as sdb
    import merope

    expected = pores_in_cube(radius, pore_fraction, side)[1]
    L = [side, side, side]
    os.makedirs(folder, exist_ok=True)

    pores = [float(r) for r, n in zip(radius, counts) for _ in range(int(n))]  # one radius per pore
    theSpheres = sdb.throwSpheres_3D(sdb.TypeAlgo.RSA, sdb.NameShape.Tore, L, seed, pores, [2] * len(pores), DIST_MIN)
    classes = [int(np.argmin(np.abs(radius - sphere.radius))) for sphere in theSpheres]
    tab_phase_not_to_mesh = []
    for j, sphere in enumerate(theSpheres):
        sphere.phase = 2 + j  # id of the pore
        tab_phase_not_to_mesh.append(sphere.phase)

    with open(os.path.join(folder, f"bubbles_{run}.txt"), "w") as bbl:
        for sphere in theSpheres:
            bbl.write(f"{sphere.phase} {sphere.center[0]} {sphere.center[1]} {sphere.center[2]} {sphere.radius} {PRESSURE}\n")

    gap = smallest_gap(np.array([sphere.center for sphere in theSpheres]), np.array([sphere.radius for sphere in theSpheres]), side)
    placed = np.bincount(classes, minlength=len(radius))
    obtained = placed * 4.0 / 3.0 * np.pi * radius**3 / side**3
    with open(os.path.join(folder, f"summary_{run}.txt"), "w") as out:
        out.write(f"# {run}: seed {seed}, cube side {side:.6f} um, distMin {DIST_MIN:.6f} um, mesh size {mesh_size:.6f} um "
                  f"({mesh_size / (2.0 * radius.min()):.3f} of the smallest pore diameter)\n")
        out.write(f"# pores {len(theSpheres)} (asked {int(np.sum(counts))}, expected {expected.sum():.1f}), porosity {obtained.sum():.6f} "
                  f"(asked {pore_fraction.sum():.6f})\n")
        out.write(f"# smallest gap between two pore surfaces {gap:.6f} um" + (" [?] below distMin" if gap < DIST_MIN - 1.0e-9 else "") + "\n")
        out.write(f"# load: pressure {PRESSURE:g} in every pore\n")
        out.write(f"# {'radius_um':>10s}{'porosity_asked':>16s}{'porosity':>14s}{'pores_asked':>13s}{'pores':>8s}\n")
        for r, pf, got, n, m in zip(radius, pore_fraction, obtained, counts, placed):
            out.write(f"{r:12.6f}{pf:16.6e}{got:14.6e}{int(n):13d}{m:8d}\n")

    sphInc = merope.SphereInclusions_3D()
    sphInc.setLength(L)
    sphInc.setSpheres(theSpheres)
    mi = merope.MultiInclusions_3D()
    mi.setInclusions(sphInc)
    mi.setMatrixPhase(1)
    meshGenerator = merope.mesh.MeshGenerator()
    meshGenerator.setMeshOrder(2)  # perfect spheres
    meshGenerator.setMeshSize(mesh_size)
    meshGenerator.setMultiInclusions(mi)
    meshGenerator.do_not_mesh(tab_phase_not_to_mesh)
    meshGenerator.write(os.path.join(folder, f"mesh_{run}.geo"))
    print(f"{folder}: {len(theSpheres)} pores (asked {int(np.sum(counts))}), porosity {obtained.sum():.4f}, smallest gap {gap:.4f} um, "
          f"mesh {mesh_size:g} um")


# ---- Cubes --------------------------------------------------------------------------------------------------------

def pores(expected, rng):
    """Pores per class of a cube [N]: the integer part of the expected number, plus one with the probability of the fractional
    part. Porosity and size distribution are then those of the run on average at every side; with the integer part alone a
    class with less than one pore expected is absent from every cube."""
    whole = np.floor(expected)
    return (whole + (rng.random(len(expected)) < expected - whole)).astype(int)


def counts_of(radius, pore_fraction, side, seed):
    """Pores per class of the cube (side, seed)."""
    return pores(pores_in_cube(radius, pore_fraction, side)[1], np.random.default_rng([round(1000 * side), seed]))


def mesh_pore(radius, pore_fraction):
    """The pore of the mesh test [R]: radius R_V of the run, in the cube that gives it the porosity of the run."""
    r_v = pores_in_cube(radius, pore_fraction, 1.0)[0]
    return r_v, np.cbrt(4.0 / 3.0 * np.pi * r_v**3 / pore_fraction.sum())


def listed_cubes(out):
    """Rows of cubes.txt of a run folder: dicts with test, side, seed, mesh, folder; [] if there is no cubes.txt."""
    path = os.path.join(out, "cubes.txt")
    if not os.path.exists(path):
        return []
    with open(path) as f:
        rows = [line.split() for line in f if line.strip() and not line.startswith("#")]
    return [{"test": t, "side": float(L), "seed": int(n), "mesh": float(h), "folder": f} for t, L, n, h, f in rows]


def write_cubes(run):
    """Cubes of the run: the base, the mesh cubes and, if EXPLORE, the exploration; one folder per cube, listed in cubes.txt.
    A group whose cubes are listed already is not written again."""
    path = os.path.join(RUN_DIR, f"run_{run}.txt")
    if not os.path.exists(path):
        raise SystemExit(f"{os.path.normpath(path)} is not there: python3 ../pore_distribution.py")
    run = f"run_{run}"
    radius, pore_fraction = read_run(path)[:2]
    out = os.path.join(OUT, run)
    os.makedirs(out, exist_ok=True)
    rows = listed_cubes(out)
    done = {row["test"] for row in rows}

    def write(test, cubes):
        """cubes: (folder, radius, pore fraction, counts, side, seed, mesh) of one group."""
        if test in done:
            print(f"{test} cubes of {run} already in {out}: delete their folders and rows in cubes.txt to write them again")
            return
        for folder, radius_, pore_fraction_, counts, side, seed, mesh in cubes:
            generate(run, radius_, pore_fraction_, counts, side, seed, mesh, os.path.join(out, folder))
            rows.append({"test": test, "side": side, "seed": seed, "mesh": mesh, "folder": folder})

    assert SIDE not in EXPLORE_CUBES, "the exploration has other sides than the base"
    assert MESH_SIZE in MESH_SIZES and MESH_SIZE > min(MESH_SIZES), "the mesh of the runs is tested against a finer one"
    sides = [SIDE] + (list(EXPLORE_CUBES) if EXPLORE else [])
    rarest = {side: pores_in_cube(radius, pore_fraction, side)[1].min() for side in sides}
    print(f"{run}: pores expected in the rarest class, " + ", ".join(f"{n:.2f} in {L:g} um" for L, n in rarest.items()))
    for side, n in rarest.items():
        if n < 1.0:
            print(f"[?] the rarest class of {run} has {n:.2f} pores expected in {side:g} um: it is missing in some cubes and drops "
                  "out of the statistics of that side; choose a larger side")
    print(f"\nbase of {run}: {SEEDS} cubes of {SIDE:g} um, mesh {MESH_SIZE:g} um")
    write("base", [(f"side{SIDE:g}_seed{seed:02d}", radius, pore_fraction, counts_of(radius, pore_fraction, SIDE, seed), SIDE, seed,
                    MESH_SIZE) for seed in range(1, SEEDS + 1)])
    r_v, side = mesh_pore(radius, pore_fraction)
    print(f"\nmesh test of {run}: one pore of R_V = {r_v:.4f} um in a cube of {side:.4f} um (porosity {pore_fraction.sum():.4f}), "
          f"meshes " + ", ".join(f"{h:g}" for h in MESH_SIZES) + " um")
    write("mesh", [(f"mesh{h:g}", np.array([r_v]), np.array([pore_fraction.sum()]), [1], side, 1, h) for h in MESH_SIZES])
    texts = []
    for h in MESH_SIZES:
        with open(os.path.join(out, f"mesh{h:g}", f"bubbles_{run}.txt")) as f:
            texts.append(f.read())
    assert all(text == texts[0] for text in texts), "the mesh cubes hold other pores: same seed and inputs expected"
    if EXPLORE:
        print(f"\nexploration of {run}: " + ", ".join(f"{n} cubes of {L:g} um" for L, n in EXPLORE_CUBES.items()))
        write("explore", [(f"side{L:g}_seed{seed:02d}", radius, pore_fraction, counts_of(radius, pore_fraction, L, seed), L, seed,
                           MESH_SIZE) for L, n in EXPLORE_CUBES.items() for seed in range(1, n + 1)])

    lines = [(f"# cubes of {run} for the tests of the RVE (python3 utilities/HBFF/Merope/test_rve.py): MMM solves every folder, "
              "analyse_rve.py reads the results"),
             f"# {'test':>6s}{'side_um':>9s}{'seed':>6s}{'mesh_um':>9s}  folder"]
    lines += [f"{row['test']:>8s}{row['side']:9g}{row['seed']:6d}{row['mesh']:9g}  {row['folder']}" for row in rows]
    with open(os.path.join(out, "cubes.txt"), "w") as f:
        f.write("\n".join(lines) + "\n")
    print(f"{len(rows)} cubes of {run} listed in {os.path.join(out, 'cubes.txt')}")


def main():
    """The cubes not written yet."""
    try:
        import sac_de_billes  # noqa: F401
    except ModuleNotFoundError as err:
        raise SystemExit(f"{err}: the cubes are written with Mérope")
    write_cubes(RUN)
    return 0


if __name__ == "__main__":
    sys.exit(main())
