###########################################################################################
##
## cachematrix.R
##
## This file provides functions that allow a matrix inversion to be cached.
##
## It was written for Programming Assignment 2 of Coursera's "R Programming" course,
## taught by Roger D. Peng, Jeff Leek, and Brian Caffo, of Johns Hopkins University.
##
## The original version of this file containing function prototypes, and the
## README.md file describing the assignment, weere obtained by forking the repo at:
##    https://github.com/rdpeng/ProgrammingAssignment2
##
## This version, fleshing out the function prototypes and making them work,
## was initally created on 7-AUG-2015 by Henry2016.
## 
##


#------------------------------------------------------------------------------------------
# makeCacheMatrix
#
# Creates and initializes a CacheMatrix.
#
# A cacheMatrix holds a copy of a square invertible matrix's data, and can also hold a copy
# of that matrix's inverse.  It's used by the cacheSolve function to save the inverse
# of a matrix, to avoid recomputing it.
#
# The cacheMatrix is implemented using a functional programming concept called a closure.
#
# Arguments:
# m.d -- the input square invertible matrix that gets stored in the cacheMatrix.
#
# Returns:
# funcList -- a list of functions used to access the cacheMatrix's data, as follows:
#    1.  set -- sets the value of a square invertible matrix
#    2.  get -- gets the previously-set value of that matrix
#    3.  setInv -- sets the value of the matrix's inverse
#    4.  getInv -- gets the previously-set value of the matrix's inverse
#    (This list was based on the vector example shown in the README.md file.)
#
# See also:
#  caches in computing:
#    https://en.wikipedia.org/wiki/Cache_%28computing%29
#  closures in R:
#    http://adv-r.had.co.nz/Functional-programming.html
#    http://www.r-bloggers.com/closures-in-r-a-useful-abstraction/
#
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
  get <- function() m.d
  
  # Function to cache the matrix inversion  --------
  setInv <- function(inv) m.i <<- inv
  
  # Function to get the cached the matrix inversion  --------
  getInv <- function() m.i
  
  # A list of functions providing an interface to the CacheMatrix:
  funcList <- list(
    set = set,
    get = get,
    setInv = setInv,
    getInv = getInv
  )
  
  return( funcList )
}

#-------------------------------------------------------------------------
# cacheSolve
#
# Returns the inverse of the CacheMatrix x
#
# We get and return x's cached matrix inverse, if it's available.
# Otherwise, we compute the matrix inverse, and save it in the cache.
#
cacheSolve <- function(x, ...) {

  # Get the inverse matrix result from cache, if it's available.
  xInverseResult = x$getInv()
  
  # If it wasn't available ...
  if( !is.matrix( xInverseResult ) ){
    # compute it,
    xInverseResult <- solve( x$get() )
    # and save the result to cache.
    x$setInv( xInverseResult )
  }
  
  return( xInverse )
}

#----------------------------------------------------------------------------------
# Test data
# 
# Consisting of a simple 3 x 3 matrix and its exact inverse.
#
# The test data came from matrices shown online at:
#   http://www.purplemath.com/modules/mtrxinvr2.htm
#
matrixA <- matrix(c(1,2,3, 0,1,4, 5,6,0), nrow = 3, ncol = 3, byrow = TRUE )
inverseOfA <- matrix(c(-24,18,5, 20,-15,-4, -5,4,1), nrow = 3, ncol = 3, byrow = TRUE )

#----------------------------------------------------------------------------------
# identityMatrix
#
# An original one-liner that creates an identity matrix with a given number of rows.
#
# Example:
#   > identityMatrix(3)
#        [,1] [,2] [,3]
#   [1,]    1    0    0
#   [2,]    0    1    0
#   [3,]    0    0    1
# 
identityMatrix <- function( x ) matrix( as.integer(((1:(x*x)) %% (x+1)) == 1), nrow = x, ncol = x, byrow = TRUE)

#-----------------------------------------------------------------------------
# unitTest
#
# A simple example showing how makeCacheMatrix and cacheSolve could be used.
# Also provides a quick test of software santity.
#
# Example:
#   > unitTest()
#   Unit testing passed.
#
unitTest <- function(){
  myCachedMatrix = makeCacheMatrix( matrixA )            # Creates a cachematrix containing matrixA
  matrixInverseInstance1 = cacheSolve( myCachedMatrix )  # Computes and stores the matrix inverse
  matrixInverseInstance2 = cacheSolve( myCachedMatrix )  # Gets a cached copy of the matrix inverse
  
  # check the results
  if( isTRUE( all.equal( matrixInverseInstance1, inverseOfA ) ) &&
      isTRUE( all.equal( matrixInverseInstance1, matrixInverseInstance2 ) ) &&
      isTRUE ( all.equal( (matrixA %*% matrixInverseInstance1), identityMatrix(3) ) )
    )
  {
    message( "Unit testing passed.")
  } 
  else
  {
    message( "Unit testing failed.")
  }
}