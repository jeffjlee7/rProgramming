## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Makes cache of inverted matrix
makeCacheMatrix <- function(x = matrix()) {
      # x is a square inveritble matrix
      # return a list containing functions to provide input for cacheSolve()
      #    1. Set matrix
      #    2. Get matrix
      #    3. Set matrix inverse
      #    4. Get matrix inverse
      
      inv <- NULL
      set <- function(y){
            ## '<<-' operator that assigns a value to an object from another environment
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# Computes inverse of matrix frin makeCacheMatrix unless inverse has already been calculated. 
# Otherwise retrieves inverse from cache.
cacheSolve <- function(x, ...) {
      # Return a matrix that is the inverse of the original matrix input from makeCacheMatrix()
      
      inv <- x$getinv()
      
      # Message if the inverse has already been calculated
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      # If inverse has not been calculated, then calculate the inverse
      mat.data <- x$get()
      inv <- solve(mat.data, ...)
      
      # Sets the value of the inverse in the cache
      x$setinv(inv)
      
      return(inv)
}