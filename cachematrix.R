## cachematrix.R
##
## This file provides functions for caching a matrix and
## it's inversion.
##
## It's Programming Assignment 2 in Coursera's R Programming Course.
##
## Created 7-AUG-2015 by Henry2016.
##


#-------------------------------------------------------------
# 
# makeCacheMatrix is a function that creates and initializes a
# CacheMatrix. It returns a list of functions that can be used to set and get
# the matrix's data and inverse.
#
# 1.  set the value of a square matrix
# 2.  get the previously-set value of the square matrix
# 3.  set the value of the matrix's inverse
# 4.  get the previously-set value of the matrix's inverse
#
# See also: http://www.r-bloggers.com/closures-in-r-a-useful-abstraction/
# and the vector example in the README.md file.

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
  
  # A list of functins providing an interface to the CacheMatrix:
  funcList <- list(
    set = set,
    get = get,
    setInv = setInv,
    getInv = getInv
  )
  
  return( funcList )
}

#-------------------------------------------------------------------------
# Returns the inverse of the CacheMatrix x, using it's cached value, if available.
cacheSolve <- function(x, ...) {
  # Get the inverse matrix from cache, if it's available
  cachedInv = x$getInv()
  if( is.matrix( cachedInv ) ){
    return( cachedInv )
  }
  
  # The matrix inverse wasn't cached, so compute it.
  xInverse <- solve( x$get() )
  # And save it to cache
  x$setInv( xInverse )
  return( xInverse )
}

#---------------------------------------------------------------------------
# Matrix example data from http://www.purplemath.com/modules/mtrxinvr2.htm
matrixA <- matrix(c(1,2,3, 0,1,4, 5,6,0), nrow = 3, ncol = 3, byrow = TRUE )
inverseA <- matrix(c(-24,18,5, 20,-15,-4, -5,4,1), nrow = 3, ncol = 3, byrow = TRUE )

# Function to form an identity matrix with a number of rows pssed as parameter x:
identityMatrix <- function( x ) matrix( as.integer(((1:(x*x)) %% (x+1)) == 1), nrow = x, ncol = x, byrow = TRUE)

#-----------------------------------------------------------------------------
# An example test function that shows how makeCacheMatrix and cacheSolve
# could be used.
unitTest <- function(){
  cm = makeCacheMatrix( matrixA )
  a1 = cacheSolve( cm )
  a2 = cacheSolve( cm )
  if( isTRUE( all.equal( a1, inverseA ) ) && isTRUE( all.equal( a1, a2 ) ) ){
    message( "Unit testing passed.")
  } else {
    message( "Unit testing failed.")
  }
}