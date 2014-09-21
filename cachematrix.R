## Pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   # initialize the inverted matrix
    
    # set the value of the matrix
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse of the matrix
    setInv <- function(inverse) inv <<- inverse
    
    #get the value of the inverse of the matrix
    getInv <- function() inv
    
    # return a list containing the functions above
    list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function above 
## If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getInv()    ## attemps to get the inverse
    
    ## check if the inverse has already been calculated
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)        ## return the cached inverse of matrix 'x'
    }
    
    ## Otherwise, we calculate the inverse of the matrix
    matrix <- x$get()
    inv <- solve(matrix,...)  ## calculate the inverse
    x$setInv(inv)             ## set the value of the inverse in the cache
    inv                       ## return the inverse of matrix 'x'
    
        
}
