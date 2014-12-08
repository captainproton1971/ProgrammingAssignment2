## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix:  makes a "special" matrix, which is really a list of
## functions to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the matrix inverse
## - get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

        # on initial call, x is already stored.  Initialize inverse to NULL
        inverse<-NULL
        
        # set a new value of x, resetting the inverse b/c it's no longer valid
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        
        # return the data matrix x
        get <- function () x
        
        # set the cached inverse
        set_inverse <- function(inv=matrix()) {
                inverse <<- inv
        }

        # get the cached inverse
        get_inverse <- function () {
                inverse
        }
        
        # return a list of these functions.
        list (set=set, get=get, set_inverse=set_inverse, 
              get_inverse=get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Try and retrieve the cached inverse.
        inv <- x$get_inverse()
        
        ## If it's there (not NULL), then return the cached value
        if(!is.null(inv)){
                message ("getting cached inverse")
                return(inv)
        }
        
        ## If it's not there, compute the cached value.  First, get the
        ## elements of x
        data <- x$get()
        
        ## then compute the inverse...
        inv <- solve(data)
        
        ## finally store the inverse in the cache
        x$set_inverse(inv)
        
        ## return the computed inverse
        inv        
}
