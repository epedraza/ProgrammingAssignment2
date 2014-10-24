## Use the rules of scope of R  and the operator <<-
## to create an object (matrix like)
## that can  store their inverse in cache

## Creates an object with associated functions to store and retrieve
## the object itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(calculated) inverse <<- calculated
    getinverse <- function() inverse
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}



## Use the object defined before to find if the inverse has been calculated
## returning this, if not then calculates the inverse, store in
## their cache and return its value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(is.null(inverse)) {
        message("calculating inverse")
        inverse <- solve(x$get())
        x$setinverse(inverse)
    } else {
        message("returning inverse from cache")
    }
    inverse
}
