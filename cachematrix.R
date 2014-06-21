## The first function creates a special matrix which involves a set of functions setting and
## getting the value of the matrix, and then setting and getting the value of the inverse.
## The second function calucates the inverse of the matrix created from the makeCacheMatrix,
## and checks to make sure the inverse is not already calcualted, and "get"s it if has been,
## if it hasn't been set it calcuates the new inverse and stores in "setinverse"

## The "makeCacheMatrix" function finds and sets the value of the matrix, and then sets and
## gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
          x <<- y
          m <<- NULL
        }
        get <-function() x
        setinverse <- function(solve)m <<- solve
        getinverse <- function() m
        list(set=set,get=get
             setinverse=setinverse,
             getinverse=getinverse)
}


## "cacheSolve" solves the inverse of the matrix, but first checks to make sure it hasn't
## already been solved, it it has it "get"s the inverse, otherwise it solves it and
## stores it in setinverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
