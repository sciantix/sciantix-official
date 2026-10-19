"""
Cubes for two tests of the RVE of the runs for MMM, written by generate() of merope_pore_pressure.py
- the size of the cube, on the reference run NFIR of ../mmm_inputs: the NFIR disc before the anneal, the log-normal fit of
  its measured pores (not the SCIANTIX state);
- the size of the mesh, on one pore of the mean size of the disc.
This script writes the geometry only (bubbles, .geo, summary) with Mérope.

Both tests follow Kanit et al., Int. J. Solids Struct. 40 (2003) 3647: the size of the cube is Sections 5.2 and 6 (Eq. 62, 68-70), the size of the mesh is Section 4.1.2.
Usage
    python3 test_rve.py           

@author E. Cappellari
@date   October 2026
"""

import os
import sys
import merope_pore_pressure
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = "test_rve"  # folder of the cubes, in the current directory

# Choices
RUN = "NFIR"                      # run of ../mmm_inputs: the reference, the NFIR disc before the anneal (pore_distribution.py)
SEEDS = 10                        # cubes of the size test, as the ten seeds of OperaHPC D5.2
EPS_R = 0.05                      # relative precision asked on the mean of s_hat of a class
MESH_SIZES = (0.15, 0.30)         # um: meshes of the pore of the mesh test, that of the runs among them; the finest is the reference
EPS_MESH = EPS_R                  # tolerance on s_hat between the mesh of the runs and the finest mesh
PRESSURE = 1.0                    # unit pressure of every pore (s_hat = sigma_I / p), no distribution


def write_run(path, radius, pore_fraction, title):
    """A run file in the format of ../mmm_inputs (radius, porosity, pressure, confinement), with PRESSURE in every class."""
    lines = [f"# {title}", f"# pressure {PRESSURE:g} in every class, no confinement",
             f"# {'radius_um':>10s}{'porosity':>14s}{'pressure_MPa':>14s}{'confinement_MPa':>17s}"]
    lines += [f"{r:12.6f}{pf:14.6e}{PRESSURE:14.4f}{0.0:17.4f}" for r, pf in zip(radius, pore_fraction)]
    with open(path, "w") as f:
        f.write("\n".join(lines) + "\n")


def listed_cubes(out):
    """Rows of cubes.txt of a run folder: dicts with test, side, seed, mesh, folder; [] if there is no cubes.txt."""
    path = os.path.join(out, "cubes.txt")
    if not os.path.exists(path):
        return []
    with open(path) as f:
        rows = [line.split() for line in f if line.strip() and not line.startswith("#")]
    return [{"test": t, "side": float(L), "seed": int(n), "mesh": float(h), "folder": f} for t, L, n, h, f in rows]


def run_file(mpp, run):
    """The run file of the run: where merope_pore_pressure.py takes the runs (RUN_FILES), or ../mmm_inputs of this folder."""
    for folder in (os.path.dirname(mpp.RUN_FILES), os.path.join(HERE, "..", "mmm_inputs")):
        path = os.path.normpath(os.path.join(folder, f"run_{run}.txt"))
        if os.path.exists(path):
            return path
    raise SystemExit(f"run_{run}.txt is not in the folder of RUN_FILES of merope_pore_pressure.py nor in ../mmm_inputs: "
                     "python3 ../pore_distribution.py")


def new_folder(out, folder):
    """out/folder, which must not exist: a cube is not written over."""
    path = os.path.join(out, folder)
    return path


def write_cubes(mpp, run):
    """Cubes of the run with mpp, the module merope_pore_pressure."""
    path = run_file(mpp, run)
    radius, pore_fraction = mpp.read_run(path)[:2]
    out = os.path.abspath(os.path.join(OUT, f"run_{run}"))
    os.makedirs(out, exist_ok=True)
    rows = listed_cubes(out)
    done = {row["test"] for row in rows}

    # size of the cube: the classes of the run at unit pressure, in the cube of the runs
    side = mpp.dimensions_side
    mesh_size = mpp.mesh_size
    unit = os.path.join(out, f"unit_{run}.txt")
    write_run(unit, radius, pore_fraction, f"classes of {os.path.basename(path)} for the size test of the RVE (test_rve.py)")
    expected = mpp.pores_in_cube(radius, pore_fraction, side)[1]
    print(f"run_{run} ({path}): {expected.sum():.0f} pores expected in the cube of {side:g} um, {expected.min():.2f} in the rarest class")
    if expected.min() < 2.0:
        print(f"Mérope places the integer part of the expected number of pores of a class: the rarest class of run_{run} "
              "is missing or has one pore in a cube")
    if "size" in done:
        print(f"size cubes of run_{run} already in {out}: delete their folders and rows in cubes.txt to write them again")
    else:
        for seed in range(1, SEEDS + 1):
            folder = f"side{side:g}_seed{seed:02d}"
            mpp.generate(unit, seed=seed, folder=new_folder(out, folder))
            rows.append({"test": "size", "side": side, "seed": seed, "mesh": mesh_size, "folder": folder})
        
        # mesh test
        seed = 1
        meshfolder = f"side{side:g}_seed{seed:02d}_meshdouble"
        mpp.generate(unit, seed=seed, mesh=(mesh_size*2), folder=new_folder(out, meshfolder))
        rows.append({"test": "mesh", "side": side, "seed": 1, "mesh": mesh_size*2, "folder": f"mesh{mesh_size*2:g}"})

    lines = [(f"# cubes of run_{run} for the tests of the RVE (python3 utilities/HBFF/Merope/test_rve.py): MMM solves every folder "
              f"with the pressure {PRESSURE:g} in all the pores, analyse_rve.py reads the results"),
             f"# {'test':>6s}{'side_um':>11s}{'seed':>6s}{'mesh_um':>9s}  folder"]
    lines += [f"{row['test']:>8s}{row['side']:11.6f}{row['seed']:6d}{row['mesh']:9g}  {row['folder']}" for row in rows]
    with open(os.path.join(out, "cubes.txt"), "w") as f:
        f.write("\n".join(lines) + "\n")
    print(f"{len(rows)} cubes of run_{run} listed in {os.path.join(out, 'cubes.txt')}")


def main():
    """The cubes not written yet."""
    write_cubes(merope_pore_pressure, RUN)
    return 0


if __name__ == "__main__":
    sys.exit(main())
