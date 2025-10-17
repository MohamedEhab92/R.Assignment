## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                        # Initialize the inverse as NULL
    set <- function(y) {               # Method to set the matrix
        x <<- y                        # Assign y to x in parent env
        inv <<- NULL                   # Reset inverse cache
    }
    get <- function() x                # Method to get the matrix
    setinverse <- function(inverse) inv <<- inverse   # Store inverse
    getinverse <- function() inv                     # Retrieve inverse
    
    # Return a list of functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)                    # Return cached inverse
    }
    data <- x$get()                    # Get the matrix
    inv <- solve(data, ...)            # Compute inverse
    x$setinverse(inv)                  # Cache the inverse
    inv                                # Return inverse
}
