################################################################################
##
## cachematrix.R
##
## This file provides functions that allow a matrix inversion to be cached.
##
## Written for Programming Assignment 2 of Coursera's "R Programming" course,
## taught by Roger D. Peng of Johns Hopkins University.
##
## The original version of this file containing function prototypes, and the
## README.md file describing the assignment, were obtained by forking this repo:
##    https://github.com/rdpeng/ProgrammingAssignment2
##
## This version of cachematrix.R fleshes out those function prototypes, making
## them work, and adding a unit test.  It was initally created on 7-AUG-2015
## by Henry2016.
##
##


#-------------------------------------------------------------------------------
#
# makeCacheMatrix
#
# Creates and initializes a CacheMatrix.
#
# A cacheMatrix holds a copy of a square invertible matrix's data, and can also
# hold a copy of that matrix's inverse.  It's used by the cacheSolve function to
# save the inverse of a matrix, to avoid recomputing it.
#
# The cacheMatrix is implemented using a functional programming concept called
# a closure.
#
# Arguments:
# m.d -- the input square invertible matrix that gets stored in the cacheMatrix.
#
# Returns:
# funcList -- List of functions used to access cacheMatrix data, as follows:
#    1.  set -- sets the value of a square invertible matrix
#    2.  get -- gets the previously-set value of that matrix
#    3.  setInv -- sets the value of the matrix's inverse
#    4.  getInv -- gets the previously-set value of the matrix's inverse
#    (This list was based on the vector example shown in the README.md file.)
#
# See also:
#  For info about caches in computing:
#    https://en.wikipedia.org/wiki/Cache_%28computing%29
#  For info about closures in R:
#    http://adv-r.had.co.nz/Functional-programming.html
#    http://www.r-bloggers.com/closures-in-r-a-useful-abstraction/
#
#-------------------------------------------------------------------------------
makeCacheMatrix <- function(m.d = matrix()) {
    # Cached Data --------
    # m.d caches the matrix's data.
    # m.i caches the matrix's inverse.
    m.i <- NULL

    # Function to store matrix data  --------
    set <- function(new.m.d) {
        m.d <<- new.m.d  # Sets the matrix data
        m.i <<- NULL
    }

    # Function to get cached matrix data  --------
    get <- function(){
        m.d
    }

    # Function to cache the matrix inversion  --------
    setInv <- function(inv){
        m.i <<- inv
    }

    # Function to get the cached version of the matrix inversion  --------
    getInv <- function(){
        m.i
    }

    # A list of functions providing an interface to the CacheMatrix:
    funcList <- list(
        set = set,
        get = get,
        setInv = setInv,
        getInv = getInv
    )

    return(funcList)
}

#-------------------------------------------------------------------------------
#
# cacheSolve
#
# Returns the inverse of the CacheMatrix x
#
# We get and return x's cached matrix inverse, if it's available.
# Otherwise, we compute the matrix inverse, and save it in the cache.
#
#-------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    # Get the inverse matrix result from cache, if it's available.
    xInverseResult = x$getInv()

    # If it wasn't available ...
    if (!is.matrix(xInverseResult)) {
        # compute it,
        xInverseResult <- solve(x$get())
        # and save the result to cache.
        x$setInv(xInverseResult)
    }

    return(xInverseResult)
}

#-------------------------------------------------------------------------------
#
# Test data for unitTest()
#
# Consisting of a simple 3 x 3 matrix, its exact inverse, and an identity matrix.
#
# The data for the matrix and inverse came from:
#   http://www.purplemath.com/modules/mtrxinvr2.htm
#
#-------------------------------------------------------------------------------
matA <- matrix(
    c(1, 2, 3,
      0, 1, 4,
      5, 6, 0),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
)

inverseOfA <- matrix(
    c(-24, 18, 5,
      20, -15,-4,
      -5,   4, 1),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
)

identityMatrix3x3 = diag(3)

#-------------------------------------------------------------------------------
#
# unitTest
#
# A simple example showing how makeCacheMatrix and cacheSolve could be used.
# Also provides a quick test of software santity.
#
# Example:
#   > unitTest()
#   Unit testing passed.
#
#-------------------------------------------------------------------------------
unitTest <- function() {
    cm = makeCacheMatrix(matA)       # Creates a cachematrix from matrix matA
    matrixInverse1 = cacheSolve(cm)  # Computes and stores its inverse
    matrixInverse2 = cacheSolve(cm)  # Gets a cached copy of its inverse

    # check the results
    if (isTRUE(all.equal(matrixInverse1, inverseOfA)) &&
        isTRUE(all.equal(matrixInverse1, matrixInverse2)) &&
        isTRUE(all.equal((matA %*% matrixInverse1), identityMatrix3x3))) {
        message("Unit testing passed.")
    } else {
        message("Unit testing failed.")
    }
}
