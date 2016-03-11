# function to create a special matrix object that can cache its inverse
# defines setters & getters for the matrix and its inverse
# Inverse computation is out of scope of this function
#
# No special checks are done if matrix is SQUARE or not - enhancement can be done
#
# Args:
#   x: Matrix object to be cached
#
# Returns:
#   list of setter and getter functions

makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverseMatrix to NULL
    inverseMatrix <- NULL
    
    # set the matrix object
    set <- function(newMatrixObj) {
        x <<- newMatrixObj
        inverseMatrix <<- NULL
    }
    
    # return the matrix object
    get <- function() {
        x
    }
    
    # set the inverse of a matrix
    setInverse <- function(newInverseMatrix) {
        inverseMatrix <<- newInverseMatrix
    }
    
    # get the inverse of a matrix
    getInverse <- function() {
        inverseMatrix
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# function to compute the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse has already been calculated 
# (and the matrix has not changed), then it retrieves the inverse from the cache
#
# No special checks are done if matrix is SQUARE or not - enhancement can be done
#
# Args:
#   makeCacheMatrix - list containing functions to set/get the matrix & inverse
#
# Returns:
#   inverse of the matrix

cacheSolve <- function(x, ...) {

    # check if Matrix inverse is already computed & cached
    inverserMatrix <- x$getInverse()
    if(!is.null(inverserMatrix)) {
        message("Returning the cached data")
        return(inverserMatrix)
    }
    
    # Compute the inverse and cache the results. Assume matrix is inversible
    matrixObj <- x$get()
    inverserMatrix <- solve(matrixObj)
    x$setInverse(inverserMatrix)
    inverserMatrix
}

