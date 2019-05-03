## assert: all matrices used here are square and invertible
## example:
## > M<-makeCacheMatrix(matrix(c(1,2,2,1), nrow=2, ncol=2))
## > cacheSolve(M) # initial call
##           [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## > cacheSolve(M) # next time data comes from cache
## getting cached data
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333

## Create a list of function in order to cache a matrix with its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv_) inv <<- inv_
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Retrieve the matrix inverse from cache if already cached - 
## otherwise compute inverse and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
