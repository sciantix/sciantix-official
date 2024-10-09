## FreeFEM.py
## --------------------
## Francesco A.B. Silva
## 09/03/2020


## Import external libraries:
import numpy as np
from scipy.sparse import coo_matrix


## =================================== ##
##   READ MATRIX IN FREEFEM++ FORMAT   ##
## =================================== ##

## Define function:
def FFmatrix_fread(file_name):

    ## Open file:
    with open(file_name,'r') as file:
        file_content = file.readlines()[2:]

    ## Extract matix size:
    ext_size = file_content[0].split()
    xx, mm, nn = int(ext_size[2]), int(ext_size[1]), int(ext_size[0])

    ## Define coordinates and content vectors:
    II, JJ, VV = [], [], []

    ## Process file content:
    for ii in range(xx):
        cur = file_content[ii+1].split()

        ## Store current coordinates and float value:
        if abs(float(cur[2])) >= 1e-32 :
            II.append( int(cur[0]) )
            JJ.append( int(cur[1]) )
            VV.append( float(cur[2]) )

    ## Build the coordinate sparse matrix and return it:
    MAT = coo_matrix((VV, (II,JJ) ), shape = (nn,mm) )
    return MAT.tocsr()


## =================================== ##
##   WRITE MATRIX IN FREEFEM++ FORMAT  ##
## =================================== ##

## Define function:
def FFmatrix_fwrite(matrix, file_name):

    ## Convert matrix to a coo matrix.
    try : matrix = matrix.tocoo()
    except : return False

    ## Extract matrix values and indices:
    data = matrix.data
    row, col = matrix.row, matrix.col

    ## Open file:
    with open(file_name, 'w') as file:

        ## Write constant upper lines:
        file.write("#  HashMatrix Matrix (COO) 0x26ea510\n")
        file.write("#    n    m    nnz    half    fortran    state\n")

        ## Write problem size line:
        file.write(str(matrix.shape[0]) + " " + str(matrix.shape[1]) + " ")
        file.write(str(matrix.nnz) + " 0 0 0 0\n")

        ## Write matrix content:
        for ii in range(matrix.nnz) :
            file.write("         " + str(row[ii]) )
            file.write("         " + str(col[ii]) )
            file.write(" " + str(data[ii]) + "\n")

    ## Return True state:
    return True


## =================================== ##
##   READ VECTOR IN FREEFEM++ FORMAT   ##
## =================================== ##

## Define function:
def FFvector_fread(file_name):

    ## Open file:
    with open(file_name,'r') as file:
        file_content = file.readlines()

        ## Extract matix size:
        ext_size = file_content[0].split()
        lin, col = int(ext_size[0]), int(ext_size[1])

        ## Prepare the array sturcture:
        VEC = np.empty((lin, col))

        ## Store array values:
        for ii in range(lin):
            cur = file_content[ii+1].split()
            VEC[ii, : ] = list(map(float, cur))

    ## Return numpy vector:
    return VEC


## =================================== ##
##   WRITE VECTOR IN FREEFEM++ FORMAT  ##
## =================================== ##

## Define function:
def FFvector_fwrite(vector, file_name):

    ## Open file:
    with open(file_name, 'w') as file:

        ## Write problem size line:
        file.write( str(vector.shape[0]) + " " + str(vector.shape[1]) + "\n")

        ## Write matrix content:
        for ii in range(vector.shape[0]):
            file.write("       " + "   ".join(map(str, vector[ii, :])) + "\n")

    ## Return True state:
    return True
