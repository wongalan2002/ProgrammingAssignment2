## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function cache the matrix
makeCacheMatrix <- function(x = matrix()) {
    # Clear the inverse veriable 
    inverse <- NULL
    # Store the matrix in veriable y and clear the stored inverse 
    # veriablewhen a new matrix was setted
    set <-function(y){
        x<<-y
        inverse<<-NULL
    }
    # Return the stored matrix
    get <- function(){
        x
    }
    # Set the inverse matrix
    setinverse <- function(inv){
        inverse <<-inv
    } 
    # Get the inverse matrix
    getinverse <- function() {
        inverse
    }
    list(set = set, get=get ,setinverse = setinverse, getinverse=getinverse)
}


## Return the matrix if there is on in the cache
## Otherwise, compute the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
