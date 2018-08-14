## Assignment 2: 
## Author: Ralston Fonseca 
## Date: 14th Aug 2018
## Version: 1.0
## These two function : makeCacheMatrix() and cacheSolve()
## cache the inverse of a matrix.
################################################################################
  

## The makeCacheMatrix function does the following: 
## return: A list containing functions to
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse
## This list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) { 
  
          m_inv <- NULL
          
          set <- function(y) {
            x <<- y         # Assigning value to the object in the environment
            m_inv <<- NULL
          }
          
          get <- function() x
          setinverse <- function(inverse) m_inv <<- inverse
          getinverse <- function() m_inv
          
          list(set = set, 
               get = get, 
               setinverse = setinverse, 
               getinverse = getinverse)
} 
  

 
## The cacheSolve function does the following: 
## return: inverse of the original matrix input to makeCacheMatrix()
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache

cacheSolve <- function(x, ...) { 
  
        m_inv <- x$getinverse()
        
        # If the inverse has already been calculated
        
        if(!is.null(m_inv)) {
              message("getting cached data")
              return(m_inv) # Return Cached inverse
        }
        
        # Else calculate the inverse and return
        data <- x$get()
        m_inv <- solve(data, ...)
        
        x$setinverse(m_inv) 
        m_inv
  
} 
