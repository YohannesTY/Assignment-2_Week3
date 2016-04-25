# Date: April 24, 2016
# These pair of functions cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL #create a place holder 
    set <- function(y) {
        x <<- y
        I <<- NULL # resets I
    }
    get <- function() x
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    list(set=set, 
         get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
    I <- x$getinverse()
    if(!is.null(I)) {
        message("getting cached data")        
        return(I)
    }
    data <- x$get()
    I <- solve(data)
    x$setinverse(I)
    I
}
