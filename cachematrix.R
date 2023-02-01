##Some variables are in my native language, sorry in advance
#Here we create the function that returns the vector with all specifications for the cache

makeCacheMatrix <- function(x = matrix()) {
    inversaMatriz <- NULL
    set <- function(y) {
        x <<- y
        inversaMatriz <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inversaMatriz <<- solve
    conseguirInversa <- function() inversaMatriz
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = conseguirInversa)

##In simple, in this function we get the result of the inverse matrix (x)
cacheSolve <- function(x, ...) {
    inversaMatriz <- x$getinverse()
    if(!is.null(inversaMatriz)) {
        message("getting cached data")
        return(inversaMatriz)
    }
    data <- x$get()
    inversaMatriz <- solve(data, ...)
    x$setinverse(inversaMatriz)
    inversaMatriz
}
