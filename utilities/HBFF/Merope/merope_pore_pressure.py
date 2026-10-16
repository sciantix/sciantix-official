"""
Generator of pore classes in a cube with each pore class defined by its
   -   radius
   -   porosity fraction
   -   pressure
and with the confining pressure of the cube, defined by an external script: the run files
../mmm_inputs/run_*.txt written by `python3 ../pore_distribution.py`. run_NFIR.txt is the reference, the NFIR disc before the anneal (log-normal fit of its measured pores); run_R*.txt are the Latin hypercube, at its R_V and temperature.

This script will feed the future MMM micromechanical calculations.

For every run it writes, in the folder <run>_seed_<seed>_dmin<distMin> of the current directory:
    bubbles_<run>.txt   one row per pore: phase, x, y, z, radius, pressure
    mesh_<run>.geo      geometry for the mesh generator
    summary_<run>.txt   cube side, confining pressure, and per class the porosity

@author E. Cappellari
@date   October 2026
"""

###### Libraries ######
import glob
import os
import sac_de_billes as sdb # Sphere generator
import merope
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
RUN_FILES = os.path.join(HERE, "..", "sciantix-official/utilities/HBFF/mmm_inputs", "run_*.txt")  # every run file of the folder is processed

###### Matrix ######
# The RVE is dimensionless, nevertheless dimensions of objects within the RVE should stay consistent through each other: lengths are in um, as the radii of the run file. The side of the cube, the repulsion distance and the mesh size are fixed.
dimensions_side = 30  # um - side of the cube
distMin = 0.3         # um - repulsion distance from OperaHPC D5.2 - to be verified a posteriori
mesh_size = 0.15      # um - mesh size, should be fine enough but it can be changed
SEED = 1  # Single seed value - random positioning of the pores, however at low
          # cube size / defect size the seed could be meaningless.


###### Set of all triples between radius, pore fraction, pressure ######
def read_run(path):
    """Classes of a run file: 
    - radius (um), 
    - pore fraction (-), 
    - pressure (MPa),
    - confining pressure (MPa)."""
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
    """Radius of the mean pore volume and expected number of pores per class in a cube of the given side. A class of porosity pf and pore volume V has pf / V pores per unit volume."""
    volume = 4.0 / 3.0 * np.pi * radius**3
    density = pore_fraction / volume  # pores per um3, per class
    r_v = np.cbrt(3.0 * pore_fraction.sum() / density.sum() / (4.0 * np.pi))
    return r_v, density * side**3


def generate(path, side=dimensions_side, seed=SEED, mesh=mesh_size, folder=None):
    """Pores of a run file in a periodic cube of the given side (um), placed with the given seed and meshed at the given
    size (um), and the files of the cube, written in folder (by default mmm_inputs/<run>_seed_<seed>_dmin<distMin> of the
    current directory). The defaults are the choices at the top of this file."""
    run = os.path.splitext(os.path.basename(path))[0]
    radius, pore_fraction, pressure, confinement = read_run(path)
    r_v, expected = pores_in_cube(radius, pore_fraction, side)
    L = [side, side, side]  # cube

    print(f"\n{run}: porosity {pore_fraction.sum():.4f}, R_V = {r_v:.4f} um, cube side {side:g} um with {expected.sum():.0f} pores, "
          f"distMin {distMin:.3f} um, mesh size {mesh:.3f} um, confining pressure {confinement:g} MPa")
    print(f"{'radius_um':>10s}{'porosity':>12s}{'pressure_MPa':>14s}{'pores expected':>16s}")
    for r, pf, p, n in zip(radius, pore_fraction, pressure, expected):
        print(f"{r:10.4f}{pf:12.4e}{p:14.2f}{n:16.1f}")
    if mesh > 0.5 * radius.min():
        print(f"mesh size above half of the smallest radius ({radius.min():.3f} um): the smallest class is poorly resolved")

    ###### Folder for the case: run, seed, dmin ######
    parent_folder = "mmm_inputs/"
    folder_name = folder or parent_folder + f"{run}_seed_{seed}_dmin{distMin:.3f}"
    os.makedirs(folder_name, exist_ok=True)

    ###### Generation of spheres for each triple between radius, pore fraction, pressure ######
    radius_pore_fraction = [[float(r), float(pf)] for r, pf in zip(radius, pore_fraction)]
    theSpheres = sdb.throwSpheres_3D(
        sdb.TypeAlgo.RSA,     # RSA algorithm will try to fullfil the porosity required by the user (upper limit),
                              # RSAmax instead will reach the theoretical limit of maximum porosity (approx 40%)
        sdb.NameShape.Tore,   # Tore - cubic shape for covering surface pores
        L,
        seed,
        radius_pore_fraction,
        [2] * len(radius),    # TBC, phase name, 1 = solid matrix, 2 = pores
        distMin
        )

    # the class of a sphere is the one with the nearest radius: its pressure follows
    classes = [int(np.argmin(np.abs(radius - sphere.radius))) for sphere in theSpheres]

    # pores are not meshed by including their id into a list
    tab_phase_not_to_mesh = []
    for j in range(0, len(theSpheres)):
        sphere = theSpheres[j]  # sphere is a structure containing the sphere properties
        sphere.phase = 2 + j    # id for each sphere
        tab_phase_not_to_mesh.append(sphere.phase)

    # Write sphere information to bubbles_<run>.txt in the dedicated folder
    with open(os.path.join(folder_name, f"bubbles_{run}.txt"), 'w') as bbl:
        for sphere, k in zip(theSpheres, classes):
            bbl.write(str(sphere.phase) + " ")
            bbl.write(str(sphere.center[0]) + " " +
                      str(sphere.center[1]) + " " +
                      str(sphere.center[2]) + " ")
            bbl.write(str(sphere.radius) + " ")
            bbl.write(str(pressure[k]) + "\n")

    # Porosity obtained per class, against the one asked (RSA stays below it), and the load of the cube
    placed = np.bincount(classes, minlength=len(radius))
    obtained = placed * 4.0 / 3.0 * np.pi * radius**3 / side**3
    with open(os.path.join(folder_name, f"summary_{run}.txt"), 'w') as out:
        out.write(f"# {run}: seed {seed}, cube side {side:.6f} um, distMin {distMin:.6f} um, mesh size {mesh:.6f} um\n")
        out.write(f"# confining pressure {confinement:g} MPa\n")
        out.write(f"# pores {len(theSpheres)} (expected {expected.sum():.0f}), porosity {obtained.sum():.6f} (asked {pore_fraction.sum():.6f})\n")
        out.write(f"# {'radius_um':>10s}{'pressure_MPa':>14s}{'porosity_asked':>16s}{'porosity':>14s}{'pores_expected':>16s}{'pores':>8s}\n")
        for r, p, pf, got, n, m in zip(radius, pressure, pore_fraction, obtained, expected, placed):
            out.write(f"{r:12.6f}{p:14.4f}{pf:16.6e}{got:14.6e}{n:16.1f}{m:8d}\n")
    print(f"{len(theSpheres)} pores placed, porosity {obtained.sum():.4f} (asked {pore_fraction.sum():.4f})")

    # Mesh generation setup (do not change)
    sphInc = merope.SphereInclusions_3D()
    sphInc.setLength(L)
    sphInc.setSpheres(theSpheres)
    mi = merope.MultiInclusions_3D()
    mi.setInclusions(sphInc)
    mi.setMatrixPhase(1)
    meshGenerator = merope.mesh.MeshGenerator()
    meshGenerator.setMeshOrder(2)  # perfect spheres
    meshGenerator.setMeshSize(mesh)  # mesh size Z
    meshGenerator.setMultiInclusions(mi)
    meshGenerator.do_not_mesh(tab_phase_not_to_mesh)

    # Set output file names
    # meshGenerator.set_nameOutput([os.path.join(folder_name, "mesh_" + run + ".vtk")])
    meshGenerator.write(os.path.join(folder_name, "mesh_" + run + ".geo"))

    print(f"Geometry generation completed: {folder_name}")


def main():
    paths = sorted(glob.glob(RUN_FILES))
    if not paths:
        raise SystemExit(f"no run file in {os.path.normpath(os.path.dirname(RUN_FILES))}: python3 ../pore_distribution.py")
    for path in paths:
        generate(path)


if __name__ == "__main__":
    main()