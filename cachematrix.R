## To utilize these functions you must build a matrix in which row and columns
## are equal. Assign this matrix to an object, call the makeCacheMatrix() on that
## object assigning it to another object. After completing these steps call
## cacheSolve() on the object to which makeCacheMatrix() was assigned.
## 

## This function will set and get the value of a matrix as well as set and get
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
        message("Ensure that matrix nrow:ncol = 1; solve function will be used")
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invert <<- inverse
        getinverse <- function() invert
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## This function will check to see if the matrix has been inverted, and solve
## the inverted matrix.

cacheSolve <- function(x, ...) {
        invert <- x$getinverse()
        if(!is.null(invert)) {
                message("retrieving chached data.")
                return(invert)
        }
        data <- x$get()
        invert <- solve(data)
        x$setinverse(invert)
        invert
}