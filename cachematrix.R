## makeCache function creats a special matrix as a list. cacheSolve functoin computes matrix inverse

## makeCacheMatrix creates a list of functions to be used later. It also cahes the matrix inverse to be computed later

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y=matrix()) {
                x <<- y
                Inv<<- NULL
        }
        get <- function() x
        setInv <- function(Inv) Inv <<- Inv
        getInv <- function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## cacheSolve computes the matrix inverse by using solve(). If the matrix is repeated, it will return the cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInv()
        if(!is.null(Inv)) {
                print("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInv(Inv)
        Inv
}
