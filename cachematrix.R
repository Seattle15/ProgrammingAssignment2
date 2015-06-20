## set the value of the matrix
## value of i (will hold the inverse matrix) is NULL
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i<<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Return a matrix that is the inverse of 'x'
## First check if the inverse matrix has been cached
## If it is cached, generate the message "getting cached data"
## If it is not cache, use solve function to inverse the matrix
## set the value of the inverse matric
## return the inverse matrix

cacheSolve <- function(x = matrix(x), ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix<- x$get()
    i <- solve(matrix, ...)
    x$setinverse(i)
    return(i)
}
