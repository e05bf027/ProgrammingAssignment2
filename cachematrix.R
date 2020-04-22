## makeCacheMatrix makes a matrix object that can cache
## the inverse of itself. cacheSolve then checks if the
## inverse has already been cached. If so, it retrives
## it. If not, it calculates it.

## makeCacheMatrix -  This function creates 
## a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y){
        x <<- y
        j <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)

}


## cacheSolve - this function computes the inverse of the special
## “matrix” returned by makeCacheMatrix above. If the
## inverse has already been calculated, then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    j <- x$getInverse()
    if(!is.null(j)){
        message("getting cached data")
        return(j)
    }
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(j)
    j
}
