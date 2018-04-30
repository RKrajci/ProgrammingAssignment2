## makeCacheMatrix() and cacheSolve() work together to first calculate the inverse of a square matrix (input into makeCacheMatrix()
## and inverted via cacheSolve()) and then store the resulting inverted matrix so that when the user calls cacheSolve() on the same
## matrix, the inversion will be pulled from the cache instead of being recalculated, saving processing time

## makeCacheMatrix() takes a user-inputted square matrix and stores it within the environment of makeCacheMatrix() as 'x'
## along with its inverse (once calculated with cacheSolve) as 'n' and creates 4 named functions, within the same environment, that 
## are available to be called by cacheSolve() and which take the result of cacheSolve and set it as the cache

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) n <<- solve
        getinverse <- function() n
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() takes a matrix created via makeCacheMatrix() and calculates the inverse of the matrix after checking to see if
## the inverse has already been calculated and the result cached (by calling named functions from the makeCacheMatrix() environment).
## If there is a cached inverse for the input matrix, the cached result will be returned instead of recalculating the inverse
## otherwise an inverse matrix will be calculated for the new input matrix

cacheSolve <- function(x, ...) {
        n <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n) ## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
        n ## Return a matrix that is the inverse of 'x'
}
