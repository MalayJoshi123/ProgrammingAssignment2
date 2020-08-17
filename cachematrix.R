## This function is used to create a cache matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
         x <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## This function is used to find the inverse of our above matrix

cacheSolve <- function(x, ...) {
  ## We are returning the value of inverse
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
        
}

