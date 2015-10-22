## Computes the inverse of a a square invertible matrix
## and then caches the result for future recall

## This function sets and gets the matrix as well as the inverse matrix
## and caches thes result

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) { #Set function for initial matrix and save in cache
                x <<- y
                m <<- NULL
        }
        get <- function() x #get function from cache
        setinv <- function(inv) {m <<- inv} #set inverse matrix and save in cache
        getinv <- function() m #get inverse function
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## This function checks if the inverse matrix is already cached.
##If not it is calculating it and then setting it by calling above function. 

cacheSolve <- function(x, ...) {
        m <- x$getinv() # Retrieve any cached inverse matrix
        if(!is.null(m)) { # if there is a result display the cahed result
                message("getting cached data")
                return(m)
        }
        data <- x$get() # if not then calculate the invers matrix with solve
        m <- solve(data, ...)
        x$setinv(m) # and cache the result in m by caling setinv method from above
        m
}
