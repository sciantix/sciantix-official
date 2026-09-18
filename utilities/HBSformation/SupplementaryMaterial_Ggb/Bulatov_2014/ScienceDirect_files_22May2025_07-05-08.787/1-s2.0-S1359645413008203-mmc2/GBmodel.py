import numpy as np

def GB5DOF(P, Q, AlCuParameter, eRGB=None):
    """
    Compute the energy of an arbitrary grain boundary in FCC metals.

    Parameters
    ----------
    P : np.ndarray
        3x3 rotation matrix of grain P.
    Q : np.ndarray
        3x3 rotation matrix of grain Q.
    AlCuParameter : str or float
        Either a string ('Al', 'Cu', 'Ni', 'Au') or a float (0.0 to 1.0) if eRGB defined.
    eRGB : float, optional
        Defines the energy scale in J/m² for hypothetical FCC metals.

    Returns
    -------
    en : float
        Computed grain boundary energy in J/m².
    """

    # Generate geometry parameters
    geom100 = distances_to_set(P, Q, '100')
    geom110 = distances_to_set(P, Q, '110')
    geom111 = distances_to_set(P, Q, '111')

    # Get parameter vector
    if eRGB is None:
        parvec = makeparvec(AlCuParameter)  # Option 2: real element
    else:
        parvec = makeparvec(AlCuParameter, eRGB)  # Option 1: synthetic metal

    # Compute energy
    en = weightedmeanenergy(geom100, geom110, geom111, parvec)

    return en

import numpy as np

def distances_to_set(P, Q, whichaxes, dismax=0.999999):
    """
    Calculates geometry parameters for a given grain boundary relative to a set of axes.
    
    Parameters
    ----------
    P : np.ndarray
        3x3 rotation matrix for grain P.
    Q : np.ndarray
        3x3 rotation matrix for grain Q.
    whichaxes : str
        One of '100', '110', or '111'.
    dismax : float, optional
        Maximum distance to include (default ~1).
    
    Returns
    -------
    geom : np.ndarray
        4 x n array with rows: distance, ksi, eta, phi.
    """

    if whichaxes == '110':
        axes = np.array([
            [1, 1, 1, 1, 0, 0],
            [1, -1, 0, 0, 1, 1],
            [0, 0, 1, -1, 1, -1]
        ]) / np.sqrt(2)

        dirs = np.array([
            [0, 0, 0, 0, 1, 1],
            [0, 0, 1, 1, 0, 0],
            [1, 1, 0, 0, 0, 0]
        ])

    elif whichaxes == '111':
        axes = np.array([
            [1, 1, -1, -1],
            [1, -1, 1, -1],
            [1, -1, -1, 1]
        ]) / np.sqrt(3)

        dirs = np.array([
            [1, 1, 1, 1],
            [-1, 1, 1, -1],
            [0, 0, 0, 0]
        ]) / np.sqrt(2)

    elif whichaxes == '100':
        axes = np.array([
            [1, 0, 0],
            [0, 1, 0],
            [0, 0, 1]
        ])

        dirs = np.array([
            [0, 0, 1],
            [1, 0, 0],
            [0, 1, 0]
        ])
    else:
        raise ValueError('Undefined axis set')

    naxes = axes.shape[1]
    period = np.pi * naxes / 6

    # Define symmetry rotations
    rotX90 = np.array([
        [1, 0, 0],
        [0, 0, -1],
        [0, 1, 0]
    ])

    rotY90 = np.array([
        [0, 0, 1],
        [0, 1, 0],
        [-1, 0, 0]
    ])

    rotZ90 = np.array([
        [0, -1, 0],
        [1, 0, 0],
        [0, 0, 1]
    ])

    rotZ90m = np.array([
        [0, 1, 0],
        [-1, 0, 0],
        [0, 0, 1]
    ])

    # Generate 24 symmetry equivalent variants of Q
    # % This is the coset appropriate for the rotation convention where Q'*P
    # % is the misorientation represented in the grain frame.  If you're
    # % getting odd results, e.g. misorientations that you know are CSL are
    # % coming out entirely wrong, you may be using the opposite convention;
    # % try replacing P and Q with P' and Q'.
    V = [None] * 24
    V[0] = Q.copy()

    V[1] = V[0] @ rotX90
    V[2] = V[1] @ rotX90
    V[3] = V[2] @ rotX90

    for j in range(12):
        V[j + 4] = V[j] @ rotY90

    for j in range(4):
        V[j + 16] = V[j] @ rotZ90
        V[j + 20] = V[j] @ rotZ90m

    distances = np.zeros(24 * naxes)
    ksis = np.zeros(24 * naxes)
    etas = np.zeros(24 * naxes)
    phis = np.zeros(24 * naxes)

    thisindex = 0

    # Step through all combinations of symmetrically-equivalent axes and coset
    for i in range(naxes):
        ax = axes[:, i]
        dir = dirs[:, i]
        dir2 = np.cross(ax, dir)

        for j in range(24):
            Qv = V[j]
            Rmat = Qv.T @ P

            r = Rmat.from_matrix(Rmat)
            q = r.as_quat()  # [x, y, z, w]
            q = np.roll(q, 1)  # Convert to [w, x, y, z]

            if np.linalg.norm(q[1:]) < 1e-12:
                continue  # skip invalid quaternion

            axi = q[1:]/np.linalg.norm(q[1:])
            psi = 2 * np.arccos(np.clip(q[0], -1.0, 1.0))

            dotp = np.dot(axi, ax)
            dis = 2 * np.sqrt(abs(1 - dotp**2)) * np.sin(psi/2)

            if dis < dismax:
                thisindex += 1

                theta = 2 * np.arctan2(dotp * np.sin(psi/2), np.cos(psi/2))

                n1 = P[0, :]
                n2 = Qv[0, :]

                theta_quat = np.concatenate(([np.cos(theta/2)], np.sin(theta/2) * ax))
                RA = R.from_quat(np.roll(theta_quat, -1)).as_matrix()

                m1 = n1 + RA.T @ n2
                if np.linalg.norm(m1) < 1e-6:
                    continue
                m1 = m1 / np.linalg.norm(m1)
                m2 = RA @ m1

                phi = np.arccos(np.clip(abs(np.dot(m1, ax)), -1.0, 1.0))

                if abs(np.dot(ax, m1)) > 0.9999:
                    theta1 = -theta / 2
                    theta2 = theta / 2
                else:
                    theta1 = np.arctan2(np.dot(dir2, m1), np.dot(dir, m1))
                    theta2 = np.arctan2(np.dot(dir2, m2), np.dot(dir, m2))

                theta1 -= round(theta1 / period) * period
                theta2 -= round(theta2 / period) * period

                if abs(theta2 + period / 2) < 1e-6:
                    theta2 += period
                if abs(theta1 + period / 2) < 1e-6:
                    theta1 += period

                ksi = abs(theta2 - theta1)
                eta = abs(theta2 + theta1)

                distances[thisindex - 1] = dis
                ksis[thisindex - 1] = ksi
                etas[thisindex - 1] = eta
                phis[thisindex - 1] = phi

    # Slice arrays to only valid entries
    distances = distances[:thisindex]
    ksis = ksis[:thisindex]
    etas = etas[:thisindex]
    phis = phis[:thisindex]

    # Round to avoid numerical redundancy
    distances = np.round(distances * 1e6) * 1e-6
    ksis = np.round(ksis * 1e6) * 1e-6
    etas = np.round(etas * 1e6) * 1e-6
    phis = np.round(phis * 1e6) * 1e-6

    # Unique rows
    geom = np.unique(np.column_stack((distances, ksis, etas, phis)), axis=0).T

    return geom