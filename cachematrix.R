
## makeCacheMatrix is a function that creates a list of functions (set, get, setsolve and getsolve).
## makeCacheMatrix is called by cacheSolve.
## set changes the matrix stored.
## get returns the matrix x.
## setsolve stores the value of the input in a variable called m.
## getsolve  returns the value m.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
}

## cacheSolve calculates the inverse of a matrix.
## It searches if the inverse of the matrix we want to know has been previously calculated.
## If the inverse has not been calculated, the function calculates it and returns it.
## If the inverse has already been calculated, it says it takes the matrix previously calculated and returns it.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m   
}
