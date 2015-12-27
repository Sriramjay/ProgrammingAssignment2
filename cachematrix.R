## Function makeCacheMatrix "caches" the inverse of a matrix object
## Function cacheSolves calculates the inverse of a matrix object. If inverse of said matrix has already been cached, then it uses the 
## cached object returned by Function makeCacheMatrix, else it calculates the inverse of the matrix object
## 

## Setter & getter functions for the value of the matrix
## Setter & getter functions for inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Calculates the inverse of the matrix and returns a result. If cached inverse exists, returns it. If cached inverse does not exist,
## computes the inverse and caches the result for later use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse of matrix.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
