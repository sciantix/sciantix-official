import numpy as np
from scipy.spatial.transform import Rotation as R
from scipy.spatial import ConvexHull
from scipy.integrate import simpson

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# ORIGINAL CODE translated to Python from Bulatov 2014
# UO2 parameters from Zhang 2021

def GB5DOF(P, Q, Metalparameter, eRGB=None):
    """
    Compute the energy of an arbitrary grain boundary in FCC metals.

    Parameters
    ----------
    P : np.ndarray
        3x3 rotation matrix of grain P.
    Q : np.ndarray
        3x3 rotation matrix of grain Q.
    Metalparameter : str or float
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
        parvec = makeparvec(Metalparameter)  # Option 2: real element
    else:
        parvec = makeparvec(Metalparameter, eRGB)  # Option 1: synthetic metal

    # Compute energy
    en = weightedmeanenergy(geom100, geom110, geom111, parvec[0])

    return en

def distances_to_set(P, Q, whichaxes, dismax=0.999999):
    """
    Calculates geometry parameters for a given grain boundary relative to a set of axes.
    The grain boundary normal is fixed at [1,0,0]
    
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

    % distance is 2*sin(delta/2) where delta is the angle of closest approach
    % between a misorientation axis and one of the axes.  Note there are 24
    % representations of the rotations and 3, 6, or 4 equivalent high-symmetry
    % axes, so it calculates as many as 144 distances.  But only ones below
    % the cutoff dismax are kept.
    %
    % Once it's picked the closest approximation to the boundary for a given
    % axis and coset element, it finds the parameters ksi, eta, phi defining
    % that idealized boundary (since the axis is defined, it's a 3-space).
    %
    % These are:
    % phi, the angle between the rotation axis and the boundary plane normal
    % (taken as the mean of the normals represented in the two actual grain
    % orientations, which works when dismax is less than 1)
    %
    % ksi, the misorientation angle
    %
    % eta, a parameter giving the second axis of the boundary plane normal in
    % terms of specified directions ('dirs') perpendicular to each
    % high-symmetry axis.

    """

    if whichaxes == '110':
        axes = np.array([
            [1, 1, 1, 1, 0, 0],
            [1,-1, 0, 0, 1, 1],
            [0, 0, 1,-1, 1,-1]
        ]) / np.sqrt(2)

        dirs = np.array([
            [0, 0, 0, 0, 1, 1],
            [0, 0, 1, 1, 0, 0],
            [1, 1, 0, 0, 0, 0]
        ])

    elif whichaxes == '111':
        axes = np.array([
            [1, 1,-1,-1],
            [1,-1, 1,-1],
            [1,-1,-1, 1]
        ]) / np.sqrt(3)

        dirs = np.array([
            [ 1, 1, 1, 1],
            [-1, 1, 1,-1],
            [ 0, 0, 0, 0]
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
        [0, 0,-1],
        [0, 1, 0]
    ])

    rotY90 = np.array([
        [ 0, 0, 1],
        [ 0, 1, 0],
        [-1, 0, 0]
    ])

    rotZ90 = np.array([
        [0,-1, 0],
        [1, 0, 0],
        [0, 0, 1]
    ])

    rotZ90m = np.array([
        [ 0, 1, 0],
        [-1, 0, 0],
        [ 0, 0, 1]
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
        ax = axes[:, i] # high symmetry axis
        dir = dirs[:, i] #corresponding direction
        dir2 = np.cross(ax, dir) # cross product to get second direction

        for j in range(24):
            Qv = V[j]
            Rmat = Qv.T @ P #relative rotation

            # r = R.from_matrix(Rmat)
            # q = r.as_quat()  # [x, y, z, w] quaternion
            # q = np.roll(q, 1)  # Convert to [w, x, y, z]

            q = mat2quat(Rmat)  # Convert rotation matrix to quaternion [w, x, y, z]

            if np.linalg.norm(q[1:]) < 1e-12:
                continue  # skip invalid quaternion: null rotation

            axi = q[1:]/np.linalg.norm(q[1:]) # normalize the axis of rotation
            psi = 2 * np.arccos(np.clip(q[0], -1.0, 1.0))

            dotp = np.dot(axi, ax)

            # compute rotational distance from the boundary to the rotation set
            dis = 2 * np.sqrt(abs(1 - dotp**2)) * np.sin(psi/2) 

            if dis < dismax:
                thisindex += 1

                theta = 2 * np.arctan2(dotp * np.sin(psi/2), np.cos(psi/2)) # angle of rotation about ax that closely approximates R

                # compute the normals of the grains
                n1 = P[0, :]
                n2 = Qv[0, :]

                # RA is the rotation about the axis ax that most closely approximates R
                # idealized roation RA from now on, not the original one
                theta_quat = np.concatenate(([np.cos(theta/2)], np.sin(theta/2) * ax))
                #RA = R.from_quat(np.roll(theta_quat, -1)).as_matrix()
                RA = quat2mat(theta_quat)  # Convert quaternion to rotation matrix

                m1 = n1 + RA.T @ n2
                if np.linalg.norm(m1) < 1e-6:
                    continue
                m1 = m1 / np.linalg.norm(m1)
                m2 = RA @ m1

                phi = np.arccos(np.clip(abs(np.dot(m1, ax)), -1.0, 1.0)) # inclination angle for the common rotation axis

                if abs(np.dot(ax, m1)) > 0.9999:  # if pure twist best approximation
                    theta1 = -theta / 2 #eta is meaningless in this case
                    theta2 = theta / 2
                else: # projection of m1 and m2 onto the plane perpendicular to ax + rotation angles relative to dir
                    theta1 = np.arctan2(np.dot(dir2, m1), np.dot(dir, m1))
                    theta2 = np.arctan2(np.dot(dir2, m2), np.dot(dir, m2))

                # Reduce both angles to the range [-period/2, period/2]
                theta1 -= round(theta1 / period) * period
                theta2 -= round(theta2 / period) * period

                # Adjust angles to avoid numerical issues
                if abs(theta2 + period / 2) < 1e-6:
                    theta2 += period
                if abs(theta1 + period / 2) < 1e-6:
                    theta1 += period

                # Since this is only being run on fcc elements, which are centrosymmetric, 
                # and all dir vectors are 2-fold axes, then
                # the operations of swapping theta1 and theta2, and of
                # multilying both by -1, are symmetries for the energy function. 
                # This lets us fold everything into a small right triangle in (ksi,eta) space:

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
    print(np.column_stack((distances, ksis, etas, phis)))
    geom = np.unique(np.column_stack((distances, ksis, etas, phis)), axis=0).T

    return geom

def quat2mat(q):
    e0, e1, e2, e3 = q

    den = e0**2 + e1**2 + e2**2 + e3**2
    m = np.array([
        [e0**2 + e1**2 - e2**2 - e3**2,     2 * (e1 * e2 - e0 * e3),       2 * (e1 * e3 + e0 * e2)],
        [2 * (e1 * e2 + e0 * e3),           e0**2 - e1**2 + e2**2 - e3**2, 2 * (e2 * e3 - e0 * e1)],
        [2 * (e1 * e3 - e0 * e2),           2 * (e2 * e3 + e0 * e1),       e0**2 - e1**2 - e2**2 + e3**2]
    ]) / den

    return m

def mat2quat(m):
    t = m[0, 0] + m[1, 1] + m[2, 2]
    if t > -0.999999999:
        e0 = np.sqrt(1 + t) / 2
        e = np.array([
            m[1, 2] - m[2, 1],
            m[2, 0] - m[0, 2],
            m[0, 1] - m[1, 0]
        ]) / (4 * e0)
    else:
        e0 = 0.0
        e3 = np.sqrt(-(m[0, 0] + m[1, 1]) / 2)
        if abs(e3) > 2e-8:
            e = np.array([
                m[0, 2] / (2 * e3),
                m[1, 2] / (2 * e3),
                e3
            ])
        else:
            e1 = np.sqrt((m[0, 0] + 1) / 2)
            if e1 != 0:
                e = np.array([e1, m[1, 0] / (2 * e1), 0])
            else:
                e = np.array([0, 1, 0])

    q = np.concatenate(([e0], -e))
    return q  # Returns [w, x, y, z]


def makeparvec(Metalparameter=None):
    """
    Creates a 43-parameter vector as used by weightedmeanenergy.
    
    Args:
        Metal parameter: Position on the Al-Cu axis (0=Al, 1=Cu), or a string 'Al', 'Ni', 'Au', or 'Cu', or fuel UO2
        eRGB: Energy of a "random" grain boundary in J/m^2.
    Returns:
        par43: numpy array of length 43 (first is eRGB, then 42 weighted params).
        Elementparameter: numeric value after processing.
    """

    par42Al = np.array([
        0.405204179289160, 0.738862004021890, 0.351631012630026, 2.40065811939667,
        1.34694439281655, 0.352260396651516, 0.602137375062785, 1.58082498976078,
        0.596442399566661, 1.30981422643602, 3.21443408257354, 0.893016409093743,
        0.835332505166333, 0.933176738717594, 0.896076948651935, 0.775053293192055,
        0.391719619979054, 0.782601780600192, 0.678572601273508, 1.14716256515278,
        0.529386201144101, 0.909044736601838, 0.664018011430602, 0.597206897283586,
        0.200371750006251, 0.826325891814124, 0.111228512469435, 0.664039563157148,
        0.241537262980083, 0.736315075146365, 0.514591177241156, 1.73804335876546,
        3.04687038671309, 1.48989831680317, 0.664965104218438, 0.495035051289975,
        0.495402996460658, 0.468878130180681, 0.836548944799803, 0.619285521065571,
        0.844685390948170, 1.02295427618256
    ])
    par42Cu = np.array([
        0.405204179289160, 0.738862004021890, 0.351631012630026, 2.40065811939667,
        1.34694439281655, 3.37892632736175, 0.602137375062785, 1.58082498976078,
        0.710489498577995, 0.737834049784765, 3.21443408257354, 0.893016409093743,
        0.835332505166333, 0.933176738717594, 0.896076948651935, 0.775053293192055,
        0.509781056492307, 0.782601780600192, 0.762160812499734, 1.10473084066580,
        0.529386201144101, 0.909044736601838, 0.664018011430602, 0.597206897283586,
        0.200371750006251, 0.826325891814124, 0.0226010533470218, 0.664039563157148,
        0.297920289861751, 0.666383447163744, 0.514591177241156, 1.73804335876546,
        2.69805148576400, 1.95956771207484, 0.948894352912787, 0.495035051289975,
        0.301975031994664, 0.574050577702240, 0.836548944799803, 0.619285521065571,
        0.844685390948170, 0.0491040633104212
    ])
    par42UO2 = np.array([    # Zhang 2021
        0.209, 0.016, 0.777, 
        3.588, 0.968, 5.000,
        0.306, 1.173, 
        0.651, 1.429, 
        36.362, 
        1.039, 0.868, 0.990, 0.949, 1.042, 0.346, 0.857,
        0.826, 1.006, 
        0.459, 1.205, 1.158, 1.186, 
        0.494, 
        1.049, 0.647, 1.129, 0.592, 1.100,
        0.983, 1.760, 2.723, 
        1.793, 
        1.281, 
        0.540, 0.826, 0.231, 1.073, 1.151, 1.416,
        0.100
    ])

    if isinstance(Metalparameter, str):
        if Metalparameter == 'Ni':
            eRGB = 1.44532834613925
            Metalparameter = 0.767911805073948
        elif Metalparameter == 'Al':
            eRGB = 0.547128733614891
            Metalparameter = 0.0
        elif Metalparameter == 'Au':
            eRGB = 0.529912885175204
            Metalparameter = 0.784289766313152
        elif Metalparameter == 'Cu':
            eRGB = 1.03669431227427
            Metalparameter = 1.0
        elif Metalparameter == 'UO2':
            eRGB = 1.545 # Zhang 2021
        else:
            raise ValueError("Undefined element")

    # Calculate weighted 43-parameter vector
    if Metalparameter == 'UO2':
        par43 = np.hstack([eRGB, par42UO2])
    else:   
        par43 = np.hstack([eRGB, par42Al + Metalparameter * (par42Cu - par42Al)])

    return par43, Metalparameter, eRGB

def weightedmeanenergy(geom100, geom110, geom111, pars):
    """
    Calculate the energy for a single grain boundary.

    Parameters:
        geom100, geom110, geom111 : 2D numpy arrays with shape (4, N)
            Each has 4 rows: distance; ksi; eta; phi.
        pars : array-like of length 43
            Parameters vector for the 5DOF energy function.

    Returns:
        en : float
            Energy in J/m^2.
    """

    eRGB = pars[0]    # Energy scale

    d0100 = pars[1]   # Max distance for 100 set
    d0110 = pars[2]   # Max distance for 110 set
    d0111 = pars[3]   # Max distance for 111 set

    weight100 = pars[4]
    weight110 = pars[5]
    weight111 = pars[6]

    offset = 1e-5

    e100 = set100(geom100, pars)
    e110 = set110(geom110, pars)
    e111 = set111(geom111, pars)

    d100 = geom100[0, :]
    d110 = geom110[0, :]
    d111 = geom111[0, :]

    # Weight calculations - rsw-like function of d
    s100 = np.sin(np.pi/2 * d100 / d0100)
    s100[d100 > d0100] = 1
    s100[d100 < offset * d0100] = offset * np.pi / 2
    w100 = (1 / (s100 * (1 - 0.5 * np.log(s100))) - 1) * weight100

    s110 = np.sin(np.pi/2 * d110 / d0110)
    s110[d110 > d0110] = 1
    s110[d110 < offset * d0110] = offset * np.pi / 2
    w110 = (1 / (s110 * (1 - 0.5 * np.log(s110))) - 1) * weight110

    s111 = np.sin(np.pi/2 * d111 / d0111)
    s111[d111 > d0111] = 1
    s111[d111 < offset * d0111] = offset * np.pi / 2
    w111 = (1 / (s111 * (1 - 0.5 * np.log(s111))) - 1) * weight111

    en = eRGB * (np.sum(e100 * w100) + np.sum(e110 * w110) + np.sum(e111 * w111) + 1) / (np.sum(w100) + np.sum(w110) + np.sum(w111) + 1)
    return en


def set100(geom100, pars):

    # Calculate the dimensionless contribution to the boundary based on the nearby <100> rotations.  
    # Meant to be called by weightedmeanenergy.m, but also can be
    # a stand-alone function for purposes of plotting cross sections through the function.
    #  Input variables geom100 and pars are as generated by distances_to_set.m
    #  and makeparvec.m.

    pwr1 = pars[7] # 100 twist mix power law
    pwr2 = pars[8] # 100 tilt mix power law

    ksi = geom100[1, :]
    eta = geom100[2, :]
    phi = geom100[3, :]

    entwist = twist100(ksi, pars)
    entilt = atgb100(eta, ksi, pars)

    x = phi / (np.pi / 2)
    en = entwist * (1 - x) ** pwr1 + entilt * x ** pwr2
    return en


def twist100(ksi, pars):
    a = pars[9] # 100 twist maximum energy
    b = a * pars[10] # 100 twist rsw shape factor

    perio = np.pi / 2 # twist period
    ksi = np.mod(np.abs(ksi), perio) #rotation symmetry
    ksi[ksi > perio/2] = perio - ksi[ksi > perio/2]

    sins = np.sin(2 * ksi)
    with np.errstate(divide='ignore', invalid='ignore'):
        xlogx = sins * np.log(sins)
    xlogx = np.nan_to_num(xlogx, nan=0.0)

    en = a * sins - b * xlogx
    return en


def atgb100(eta, ksi, pars):

    # fit to energy of all <100> tilt grain boundary
    pwr = pars[11] # 100 atgb interpolation law
    period = np.pi / 2

    en1 = stgb100(ksi, pars) # at eta = 0
    en2 = stgb100(period - ksi, pars) # at eta = pi/2

    en = np.zeros_like(ksi)
    select = en1 >= en2
    en[select] = en1[select] - (en1[select] - en2[select]) * (eta[select] / period) ** pwr
    en[~select] = en2[~select] - (en2[~select] - en1[~select]) * (1 - eta[~select] / period) ** pwr
    return en


def stgb100(ksi, pars):

    #dimenstionless 100 tilt energy
    # as piecewise-rsw function, specified by energy parameters en, angle breaks th, and shape factors a.
    
    en1 = 0 #sigma1 at left end
    en2 = pars[12] # peak before first sigma5
    en3 = pars[13] # first sigma5
    en4 = pars[14] # peak between sigma5's
    en5 = pars[15] # second sigma5
    en6 = pars[16] # sigma 17
    en7 = 0 #sigma 2 at right end

    a12 = a23 = a34 = a45 = a56 = a67 = 0.5 # rws shape factors, forcing the great majority to 0.5 helps to constrain the fit

    th1 = 0 #sigma 1 at left end
    th2 = pars[17] # peak position before first sigma5
    th3 = np.arccos(4 / 5) #first sigma5
    th4 = pars[18] # peak position between sigma5's
    th5 = np.arccos(3 / 5) # second sigma5
    th6 = 2 * np.arccos(5 / np.sqrt(34)) #sigma 17 rotation angle
    th7 = np.pi / 2 # sigma1 at right end

    en = np.zeros_like(ksi)

    select = ksi <= th2
    en[select] = en1 + (en2 - en1) * rsw(ksi[select], th1, th2, a12)

    select = (ksi >= th2) & (ksi <= th3)
    en[select] = en3 + (en2 - en3) * rsw(ksi[select], th3, th2, a23)

    select = (ksi >= th3) & (ksi <= th4)
    en[select] = en3 + (en4 - en3) * rsw(ksi[select], th3, th4, a34)

    select = (ksi >= th4) & (ksi <= th5)
    en[select] = en5 + (en4 - en5) * rsw(ksi[select], th5, th4, a45)

    select = (ksi >= th5) & (ksi <= th6)
    #en[select] = en6 + (en5 - en6) * rsw(ksi[select], th6, th5, a56) # original code
    en[select] = en5 + (en6 - en5) * rsw(ksi[select], th5, th6, a56) # changed, corrected from original code

    select = (ksi >= th6) & (ksi <= th7)
    en[select] = en7 + (en6 - en7) * rsw(ksi[select], th7, th6, a67)

    return en

def set110(geom110, pars):

    # dimensioonless contribution to the boundary based on the nearby <110> rotations.
    pwr1 = pars[19] # 110 twist mix power law
    pwr2 = pars[20] # 110 tilt mix power law

    ksi = geom110[1, :]
    eta = geom110[2, :]
    phi = geom110[3, :]

    entwist = twists110(ksi, pars)
    entilt = atgbs110(eta, ksi, pars)

    x = phi / (np.pi / 2)
    en = entwist * (1 - x) ** pwr1 + entilt * x ** pwr2
    return en


def atgbs110(eta, ksi, pars):
    a = pars[25] # 110 atgb interpolation law rsw shape factor
    period = np.pi

    en1 = stgbs110(ksi, pars)
    en2 = stgbs110(period - ksi, pars)

    en = np.zeros_like(eta)
    select = en1 >= en2
    en[select] = en2[select] + (en1[select] - en2[select]) * rsw(eta[select], np.pi, 0, a)
    en[~select] = en1[~select] + (en2[~select] - en1[~select]) * rsw(eta[~select], 0, np.pi, a)
    return en


def stgbs110(th, pars):
    
    en1 = 0
    en2 = pars[26] # peak between sigma 1 and 3
    en3 = pars[27] # coherent sigma 3 twin relative energy
    en4 = pars[28] #energy peak between sigma 3 and 11
    en5 = pars[29] # sigma 11 relative energy
    en6 = pars[30] #energy peak between sigma 11 and 1
    en7 = 0

    a12 = a23 = a34 = a45 = a56 = a67 = 0.5
    
    th1 = 0
    th2 = pars[31] # peak position between sigma 1 and 3
    th3 = np.arccos(1 / 3) #sigma 3
    th4 = pars[32] # peak position between sigma 3 and 11
    th5 = np.arccos(-7 / 11) #sigma11
    th6 = pars[33] # peak position between sigma 11 and higher sigma1
    th7 = np.pi

    th = np.pi - th  # legacy transform

    en = np.zeros_like(th)

    select = th <= th2
    en[select] = en1 + (en2 - en1) * rsw(th[select], th1, th2, a12)

    select = (th >= th2) & (th <= th3)
    en[select] = en3 + (en2 - en3) * rsw(th[select], th3, th2, a23)

    select = (th >= th3) & (th <= th4)
    en[select] = en3 + (en4 - en3) * rsw(th[select], th3, th4, a34)

    select = (th >= th4) & (th <= th5)
    en[select] = en5 + (en4 - en5) * rsw(th[select], th5, th4, a45)

    select = (th >= th5) & (th <= th6)
    en[select] = en5 + (en6 - en5) * rsw(th[select], th5, th6, a56)

    select = (th >= th6) & (th <= th7)
    en[select] = en7 + (en6 - en7) * rsw(th[select], th7, th6, a67)

    return en

def twists110(th, pars):

    th1 = pars[21] # 110 twist peak position
    th2 = np.arccos(1 / 3) # sigma 3
    th3 = np.pi / 2 # 110 90 degree boundary is semispecial, not csl

    en1 = pars[22] # 110 twist peak energy
    en2 = pars[23] # sigma3 energy, 110 twist so not a coherent twin
    en3 = pars[24] # energy at the symmetry point

    a01 = 0.5
    a12 = 0.5
    a23 = 0.5
    
    perio = np.pi # 110 twist period

    th = np.abs(th) % perio # rotation symmetry
    th[th > perio / 2] = perio - th[th > perio / 2]

    en = np.zeros_like(th)

    select = th <= th1
    en[select] = en1 * rsw(th[select], 0, th1, a01)

    select = (th > th1) & (th <= th2)
    en[select] = en2 + (en1 - en2) * rsw(th[select], th2, th1, a12)

    select = th > th2
    en[select] = en3 + (en2 - en3) * rsw(th[select], th3, th2, a23)

    return en


def twists111(theta, pars):
    thd = pars[36] # 111 twist peak position
    enm = pars[37] # 111 twist peak energy
    en2 = pars[27] # coherent sigma 3 energy
    a1 = pars[35]  # 111 twist rsw shape factor 
    a2 = a1

    theta = np.copy(theta)
    theta[theta > np.pi / 3] = 2 * np.pi / 3 - theta[theta > np.pi / 3]

    en = np.zeros_like(theta)
    select = theta <= thd
    en[select] = enm * rsw(theta[select], 0, thd, a1)
    en[~select] = en2 + (enm - en2) * rsw(theta[~select], np.pi / 3, thd, a2)

    return en


def atgbs111(eta, ksi, pars):

    # Calculate the energy of all <111> tilt grain boundary.
    # There's an additional symmetry in 111 atgbs that doesn't exist in 100 or  110 atgbs
    # This is because a rotation about [111] equal to half the period
    #  is equivalent to a mirror reflection in the (111) plane. 
    #  Both are Sigma3 operations.  The same is not true of the 45-degree [100] 
    # or the 90-degree [110] rotation. The following two lines account for this extra symmetry.
    ksi = np.copy(ksi)
    eta = np.copy(eta)

    ksi[ksi > np.pi / 3] = 2 * np.pi / 3 - ksi[ksi > np.pi / 3]
    eta[eta > np.pi / 3] = 2 * np.pi / 3 - eta[eta > np.pi / 3]

    # Below the following value of ksi, we ignore the eta dependence. 
    # This is because there's little evidence that it actually varies.  Above this 
    # value, we interpolate on an rsw function that follows the Sigma3 line, 
    # which is also a line of symmetry for the function.

    ksim = pars[38] # 111 atgb ksi
    enmax = pars[39] # 111 atgb maximum energy
    enmin = pars[40] # 111 atgb minimum energy
    encnt = pars[41] # 111 atgb energy at the symmetry point
    etascale = pars[42] # eta scale for 111 atgb

    a1 = 0.5
    a2 = 0.5

    en = np.zeros_like(ksi)

    select = ksi <= ksim
    en[select] = enmax * rsw(ksi[select], 0, ksim, a1)

    chi = enmin + (encnt - enmin) * rsw(eta[~select], 0, np.pi / (2 * etascale), 0.5)
    en[~select] = chi + (enmax - chi) * rsw(ksi[~select], np.pi / 3, ksim, a2)

    return en


def set111(geom111, pars):
    a = pars[34] # linear interpolation factor
    b = a - 1

    ksi = geom111[1, :]
    eta = geom111[2, :]
    phi = geom111[3, :]

    entwist = twists111(ksi, pars)
    entilt = atgbs111(eta, ksi, pars)

    x = phi / (np.pi / 2)
    # simple parabola is enough to fit the data
    en = entwist + (entilt - entwist) * (a * x - b * x ** 2)
    return en


# Read-Shockley-Wolf (RSW) function
def rsw(theta, theta1, theta2, a): 
    # This function computes the value of Read-Shockley-Wolf function at theta.
    # The RSW function is normalized to be 1.0 at theta2 and 0.0 at theta1.

    dtheta = theta2 - theta1 # interval of angles where defined
    theta = (theta - theta1) / dtheta * np.pi / 2 # normalized angle

    sins = np.sin(theta)
    xlogx = np.zeros_like(sins)

    select = sins >= 1e-6 # cut off for numerical stability
    xlogx[select] = sins[select] * np.log(sins[select])

    return sins - a * xlogx


################################### Mackenzie random distribution #############

# Define the function p(psi)
def p(psi_deg): 

    function = np.zeros_like(psi_deg, dtype=float)   
    sqrt2 = np.sqrt(2)
    sqrt2minus1 = sqrt2 - 1
    sqrt2plus1 = sqrt2 + 1


    for i in range(len(psi_deg)):
        psi = psi_deg[i]
        if psi < 45:
            psi = np.deg2rad(psi)
            function[i] = (2/15) * (1 - np.cos(psi))
        elif 45 <= psi < 60: 
            psi = np.deg2rad(psi)
            function[i] = (2/15) * (3*sqrt2minus1*np.sin(psi) -2*(1 - np.cos(psi)))
        elif 60 <= psi < 60.72:
            psi = np.deg2rad(psi)
            function[i] = (2/15) * ((3*sqrt2minus1 + (4/np.sqrt(3)))*np.sin(psi) - 6*(1 - np.cos(psi)))
        else: 
            psi = np.deg2rad(psi)
            X = sqrt2minus1/((1 - (sqrt2minus1/np.tan(psi/2))**2)**0.5)
            Y = sqrt2minus1**2 /((3 - (1/np.tan(psi/2))**2)**0.5)
            term1 = (2/15) * ((3*sqrt2minus1 + (4/np.sqrt(3)))*np.sin(psi) - 6*(1 - np.cos(psi)))
            term2 = - (8/(5*np.pi)) * (2*sqrt2minus1*np.arccos(X/np.tan(psi/2)) + (1/np.sqrt(3)*np.arccos(Y/np.tan(psi/2))))*np.sin(psi)
            term3 = (8/(5*np.pi)) * (2*np.arccos(sqrt2plus1*X/sqrt2) + np.arccos(sqrt2plus1*Y/sqrt2)) * (1 - np.cos(psi))
            function[i] = term1 + term2 + term3
    
    return function

# # Misorientation angle (in degrees)
# theta_deg = 40
# theta_rad = np.radians(theta_deg)
# print(f"Misorientation angle (degrees): {theta_rad}")

# P = np.array([
#     [1, 0, 0],  
#     [0, 1, 0],
#     [0, 0, 1]
# ])  # Reference grain (identity matrix)


# # Misorientation axis (e.g., [111])
# u = np.array([1, 1, 1])
# u = u / np.linalg.norm(u)  # Normalize the axis

# # Skew-symmetric matrix K from axis u
# K = np.array([
#     [0, -u[2], u[1]],
#     [u[2], 0, -u[0]],
#     [-u[1], u[0], 0]
# ])

# # Rodrigues' rotation formula
# I = np.eye(3)
# R = I + np.sin(theta_rad) * K + (1 - np.cos(theta_rad)) * (K @ K)

# # Define P and Q
# P = np.eye(3)  # reference grain
# Q = R          # misoriented grain

# # Optional: print results
# np.set_printoptions(precision=4, suppress=True)
# print("Rotation matrix Q:\n", Q)

# print("GB energy", GB5DOF(P, Q, 'UO2'))






# ############# Update from Morawiec 2025 ####################

# ksi = np.full(N, np.pi/2) # fixed misorientation angle
# eta = np.full(N, 0) # fixed symmetry

# phi = np.linspace(0, np.pi/2, N)  # from pure twist to pure tilt

# # Directions n = (sinφ, cosφ)
# n = np.vstack([np.sin(phi), np.cos(phi)]).T  # shape (N, 2)

# energyBulatov = eRGBvalue*set100(np.vstack([np.zeros(N), ksi, eta, phi]), pars)  # energy for Bulatov's set
# # Construct 1/γ₀-plot: points = (1/γ₀) * n
# eps = 1e-12
# inv_energy = 1 / (energyBulatov + eps)
# points = (inv_energy[:, None]) * n

# # Get convex hull
# hull = ConvexHull(points)
# hull_points = points[hull.vertices]

# # Function to compute new γ(n) from hull
# def gamma_convexified(n_vec):
#     # Normalize direction vector
#     n_vec = n_vec / np.linalg.norm(n_vec)
#     # Maximum dot product with hull points gives 1/γ
#     return 1 / np.max(hull_points @ n_vec)

# # Apply to all directions
# gamma_hconv = np.array([gamma_convexified(ni) for ni in n])

# plt.plot(np.rad2deg(phi), energyBulatov, label='Original γ₀')
# plt.plot(np.rad2deg(phi), gamma_hconv, label='H-convexified γ', linestyle='--')
# plt.xlabel("Inclination angle φ (degrees)")
# plt.ylabel("Surface energy")
# plt.legend()
# plt.show()

# plt.figure()
# plt.plot(points[:,0], points[:,1], 'o', label='1/γ₀ ⋅ n')
# for simplex in hull.simplices:
#     plt.plot(points[simplex, 0], points[simplex, 1], 'k-')
# plt.axis('equal')
# plt.title('1/γ₀-plot and its convex hull')
# plt.legend()
# plt.show()

# import numpy as np
# import matplotlib.pyplot as plt
# from scipy.spatial import ConvexHull
# from scipy.interpolate import griddata

# # Grid in φ and η
# N = 200
# phi = np.linspace(0, np.pi/2, N)
# eta = np.linspace(0, np.pi/2, N)
# Phi, Eta = np.meshgrid(phi, eta)
# Phi_flat = Phi.ravel()
# Eta_flat = Eta.ravel()

# # Fixed misorientation angle
# ksi = np.full_like(Phi_flat, np.pi/6)  # e.g., 30 degrees

# # Geometry input to energy model
# geom = np.vstack([np.zeros_like(ksi), ksi, Eta_flat, Phi_flat])
# gamma0 = eRGBvalue * set100(geom, pars)  # shape (N^2,)

# # Remove invalid or zero energies
# valid = (gamma0 > 1e-8) & np.isfinite(gamma0)
# gamma0 = gamma0[valid]
# Phi_valid = Phi_flat[valid]
# Eta_valid = Eta_flat[valid]

# # Map (phi, eta) to 2D direction vectors for plotting (e.g., polar map)
# x = np.sin(Phi_valid) * np.cos(Eta_valid)
# y = np.sin(Phi_valid) * np.sin(Eta_valid)

# # Construct 1/γ₀-plot points in 3D
# inv_gamma = 1 / gamma0
# points3D = np.vstack([x * inv_gamma, y * inv_gamma, inv_gamma]).T  # shape (Nvalid, 3)

# # Convex hull
# hull = ConvexHull(points3D)
# hull_pts = points3D[hull.vertices]

# # Function to compute convexified gamma via projection
# def gamma_hconv_xy(xi, yi):
#     norm = np.linalg.norm([xi, yi])
#     if norm < 1e-10:
#         return np.min(gamma0)  # axisymmetric center
#     direction = np.array([xi, yi, 1.0]) / np.linalg.norm([xi, yi, 1.0])
#     q = np.max(hull_pts @ direction)  # distance to hull in direction
#     return 1 / q

# # Evaluate h-convexified γ on grid
# x_grid = np.sin(Phi) * np.cos(Eta)
# y_grid = np.sin(Phi) * np.sin(Eta)
# gamma_hconv_grid = np.zeros_like(x_grid)

# for i in range(N):
#     for j in range(N):
#         gamma_hconv_grid[i, j] = gamma_hconv_xy(x_grid[i, j], y_grid[i, j])

# # Plot original and h-convexified γ₀
# plt.figure(figsize=(12, 5))
# plt.subplot(1, 2, 1)
# plt.contourf(Phi, Eta, griddata((Phi_valid, Eta_valid), gamma0, (Phi, Eta), method='linear'), levels=100, cmap='viridis')
# plt.colorbar(label='Original γ₀')
# plt.xlabel('φ')
# plt.ylabel('η')
# plt.title('Original γ₀')

# plt.subplot(1, 2, 2)
# plt.contourf(Phi, Eta, gamma_hconv_grid, levels=100, cmap='plasma')
# plt.colorbar(label='Convexified γ')
# plt.xlabel('φ')
# plt.ylabel('η')
# plt.title('h-Convexified γ')

# plt.tight_layout()
# plt.show()

# import numpy as np
# from scipy.spatial import ConvexHull

# def h_convexified_gamma(B, np_array, gamma0_np):
#     """
#     Computes h-convexified grain boundary energy Gamma(B) for a given boundary B.

#     Parameters:
#     - B: tuple (M, n) with rotation matrix M and boundary normal n (3D unit vector)
#     - np_array: NP x 3 array of auxiliary directions n_p
#     - gamma0_np: list or array of gamma0(n_p) values (length NP)

#     Returns:
#     - Gamma(B): h-convexified grain boundary energy at B
#     """
#     M, n = B

#     # Step 1: Compute x_p = n_p / gamma0(n_p)
#     x_p_array = np.array([n_p / gamma0 for n_p, gamma0 in zip(np_array, gamma0_np)])  # Shape (NP, 3)

#     # Step 2: Compute the convex hull of the set {x_p}
#     hull = ConvexHull(x_p_array)

#     # Step 3: Find intersection of ray λ * n with the convex polyhedron
#     # This involves solving for the smallest λ > 0 such that λ*n = convex combination of face
#     # Use ray-plane intersection for each hull facet
#     q_n = np.inf
#     for simplex in hull.simplices:
#         vertices = x_p_array[simplex]
#         # Compute plane of the facet using 3 points
#         v1, v2, v3 = vertices[:3]
#         normal = np.cross(v2 - v1, v3 - v1)
#         if np.linalg.norm(normal) == 0:
#             continue
#         normal = normal / np.linalg.norm(normal)
#         d = np.dot(normal, v1)
#         denom = np.dot(normal, n)
#         if denom <= 0:
#             continue
#         lam = d / denom
#         if lam > 0 and lam < q_n:
#             q_n = lam

#     # Step 4: Compute gamma(n) = 1 / q(n)
#     gamma_n = 1 / q_n
#     return gamma_n
