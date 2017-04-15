## The following two functions work together to compute the inverse of an invertible matrix and store it in the cache environment so it does not have to be computed for more than once.


## The first function, `makeCacheMatrix` creates a special "matrix", which is really a list containing a function to do the below:

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the inverse to NULL
    inv <- NULL
    
    # create the matrix and set the inverse to NULL in the working environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # Assign the value of solve to inv
    setinverse <- function(inverse) inv <<- inverse
    
    # get the value of inv, which is the inverse of the matrix
    getinverse <- function() inv
    
    # return a list of the functions that are created in this working environment
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
    
}


## The following function computes the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been computed. If so, it `get`s the inverse from the cache and skips the computation. Otherwise, it computes the inverse of the matrix and sets the value of the inverse in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
    
         # get the inv from the cache environment
         inv <- x$getinverse()
         
         # if inv already exsits in the cache environment, return the value of inv from the cache environment
         if(!is.null(inv)) {
                 message("getting cached data")
                 return(inv)
         }
         
         # create the matrix
         data <- x$get()
         
         # compute the inverse of the matrix
         inv <- solve(data, ...)
         
         # assign the computed inverse of the matrix to inv in the cache environment
         x$setinverse(inv)
         
         # return the inverse of the matrix 'x'
         inv
        
}
