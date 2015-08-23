## These two functions compute and cache the inverse of a matrix

## The first function creates a special "matrix" 
## which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function computes the inverse of the special "matrix"
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) { ## If the inverse has been calculated, 
                      ## then it will be retrieved from the cache
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)## Otherwise, the inverse is calculated
    x$setinverse(m)      ## and put in the cache
    m               
}
