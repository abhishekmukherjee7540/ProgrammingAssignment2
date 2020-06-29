## makeCacheMatrix and casheSolve functions together, help in calcuating the inverse of a matrix, in a more 
## efficient manner by utilising the caching ability of R to save processing power and increase computation 
## speed.

## makeCacheMatrix creates a special vector, which is really a list used to set the value of the matrix, 
## get the matrix's value, set the inverse of the  matrix and finally get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                
                set <- function(y) {
                    x <<- y
                    inv <<- NULL
                }
                
                get <- function(x) {
                    x
                }
                
                setinverse <- function(inverse) {
                            inv <<- inverse      
                }
                
                getinverse <- function() {
                            inv
                }
                
                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## casheSolve calculates the inverse of the special vector created in the above function. It first checks to see
## if the inverse has already been computed. If so, then it calls the inverse from the cache and skips the
## computation. Otherwise, it calcuates the inverse of the matrix and sets the value of the inverse in the cache
## using the setinverse function

cacheSolve <- function(x, ...) {
            inv <- x$getinverse()
            
            if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
            }
            
            data <- x$get()
            inv <- solve(data, ...)
            x$setinverse(inv)
            inv
}

