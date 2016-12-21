## Function porduces results of for the Programming assigment 2

## This function caches the inverse of a matrix, it produces a list of special values to 
## capture the cache in the other function

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## calculates the inverse of the matrix entered in the previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <-x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setsolve(s)
        s
}