## These two functions calcuate the inverse of a matrix and store the inverse in cache and retrieve them
## The functions are to save resources to not recalcuate inversed matrices if they are already solved

## The first function calcuate the inverse of the matrix ("mtx") and cache the output. The function itself does not produce an output.

makeCacheMatrix <- function(mtx = matrix()) {
  makeInv <- NULL
  set <- function(pp) {
    mtx <<- pp
    makeInv <<- NULL
  }
  get <- function() mtx
  setInv <- function(solve) makeInv <<- solve
  getInv <- function() makeInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The second function fetches the sovled inverse of the matrix ("mtx" from the previous function) if already calculated by the previous function
## If not already solved will calculate the inverse anew

cacheSolve <- function(mtx, ...) {
  thisInv <- mtx$getInv()
  if(!is.null(thisInv)) {
    message("getting cached inversed matrix")
    return(thisInv)
  }
  data <- mtx$get()
  thisInv <- solve(data, ...)
  mtx$setInv(thisInv)
  thisInv
}
