## This functions allow to cache potentially time-consuming computations related
## with square matrix inverse (solve) computations. However, for a very scalable 
## matrix, it may take too long to compute the solve, especially if it has to be
## computed repeatedly (e.g. in a loop). If the contents of a matrix are not 
## changing, it may make sense to cache the value of the solve so that when we
## need it again, it can be looked up in the cache rather than recomputed.



## This function creates a special "matrix", which is really containing
## a function to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the solve (inverse of matrix)
##  4. get the value of the solve (inverse of matrix)

makeCacheMatrix <- function(m = matrix()) {
    slv <- NULL
    set <- function(x) {
        m <<- x
        slv <<- NULL
    }
    get <- function() m
    setSolve <- function(solve) slv <<- solve
    getSolve <- function() slv
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## The following function calculates the solve (inverse of square matrix)
## of the special "matrix" created with the "makeCacheMatrix" function.
## It first checks to see if the solve has already been calculated. If so, 
## it gets the solve from the cache and skips the computation. Otherwise,
## it calculates the solve of the data and sets it's value in the cache 
## via the "setSolve" function.

cacheSolve <- function(m, ...) {
    slv <- m$getSolve()
    if(!is.null(slv)) {
        message("getting cached data")
        return(slv)
    }
    data <- m$get()
    slv <- solve(data)
    m$setSolve(slv)
    slv
}
