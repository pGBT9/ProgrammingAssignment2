## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following functions will cache the inverse of a matrix,
## and it can be looked up in the cache rather than recomputed
## if the contents of a matrix are not changing.

## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrx is really a list containing a function to
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the value of the inverse of the matrix (setinv)
## 4. get the value of the inverse of the matrix (getinv)

makeCacheMatrix <- function(x = matrix()) {             # 'x' (input) must be a matrix
        inv <- NULL                     # initializes inv to NULL
        set <- function(y) {            # changes the matrix x stored
                x <<- y                 # substitutes the matrix x with y (the input)
                inv <<- NULL            # restores to null the value of the inverse inv
        }
        get <- function() x             # returns the matrix x stored
        setinv <- function(inverse) inv <<- inverse     # store the inverse to inv
        getinv <- function() inv        # returns the inverse stored
        list(set = set, get = get,      # stores the 4 functions to the main function
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()               # get the inverse stored previously with getinv
        if(!is.null(inv)) {             # verify the value inv exists and is not NULL
                message("getting cached data")          # If it exists, returns a message
                return(inv)             # return the value of inv
        }
        data <- x$get()                 # get the matrix stored previously with get
        inv <- solve(data, ...)         # calculate the inverse of the matrix
        x$setinv(inv)                   # store the inverse with setinv
        inv                             # return the value of the inverse
}