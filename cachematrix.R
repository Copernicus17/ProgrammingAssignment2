## This program creates a function to calculate and cache the inverse of a square
## invertible matrix. This technique can save time; since the inverse is computed
## once; and the cached value is used each addtional time we need that inverse.
## (The following solution modifies the makeVector and cachemean template that
## was provided with this exercise).

## Note that the input matrix is assumed to square invertible, and that an error
## will occur if we input a non-square invertible matrix.

rm(list = ls())  ## for simplicity, start with a clean environment 


## makeCacheMatrix function creates a special "matrix" that can cache its inverse.
## The function returns a list with the following four functions:
## 1. sets the square invertible matrix
## 2. gets the square invertible matrix
## 3. sets the inverse matrix
## 4. gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## 1. sets the square invertible matrix
  m <- NULL
  set <- function(y) {
    x <<- y  ## assign value to object in another environment
    m <<- NULL
  }
  
  ## 2. gets the square invertible matrix
  get <- function() x
  
  ## 3. Set inverse matrix. 
  ## The solve function finds the inverse of square invertible matrices
  setsolve <- function(solve) m <<- solve  ##assigned in another environment
  
  ## 4. inverse matrix
  getsolve <- function() m
  
  ## The function returns a named list:
  return(list(set = set, 
              get = get,
              setsolve = setsolve,
              getsolve = getsolve))
}


## The cacheSolve function computes & caches the inverse of the special "matrix" 
## created by the makeCacheMatrix function above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve simply retrieves the inverse from the cache.
## The special matrix (from makeCacheMatrix) is input to the cacheSolve function. 

cacheSolve <- function(x, ...) {
  
  ## Returns the inverse of the matrix 'x' (and caches the result)
  
  ## check if we've already cached the matrix inverse
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)  ## returns previously cached inverse with message (if available)
  }
  
  ## computes the inverse and caches it
  data <- x$get()  ##get the matrix that we need to compute the inverse for
  m <- solve(data, ...)  ## solve is built-in R function to compute the inverse
  #cache the inverse 
  x$setsolve(m)
  message("The compued inverse matrix is below:")
  print(m)  ## print the inverse matrix
  return(m)  ## returns newly computed inverse
}


##Test using a simple 2x2 square invertible matrix:
testMatrix1 <- matrix(data = 1:4, nrow = 2)
message("The original input matrix is below:")
print(testMatrix1)

## run the functions
myTest <- makeCacheMatrix(testMatrix1)  ## create the special matrix
cacheSolve(myTest)  ## compute and cache the inverse
