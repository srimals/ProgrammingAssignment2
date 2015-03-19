## Put comments here that give an overall description of what your
## functions do.
## Functions in this file has the feature to create a cacheable matrix class to 
##store inverse-matrix in cache storage (environment). CacheSolve function can 
##generate inverse matrix if not found in cache and store it before returning
## the inverse.

## Write a short comment describing this function
## this function creates a cachable matrix which is to store inverse of matrix 
##when called using cacheSolve function below.
makeCacheMatrix <- function(x = matrix()) {

        
        cm <- NULL
        
        ##set data into this cachable matrix and clear the environment variable 
        ##(cache storage) usin NULL value.
        set <- function(y)
        {
                x <<- y
                cm <<- NULL
        }
        
        ##this returns the matrix data.
        get <- function() 
        {
                x
        }
        
        ## store the inverted matrix in a cache storage for subsequent requests
        ##to eliminate repeated calculate.
        setInverse <- function(inverse)
        {
                cm <<- inverse
        }
        
        ## this return the data from the environment (cache storage)
        getInverse <- function()
        {
                cm
        }
        
        list(set = set, get = get, getInverse = getInverse, 
             setInverse = setInverse )
        
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## get the inverted matrix from the cache storage.
        cm <- x$getInverse()

        ## check if the cache storage has valid inverted matrix. if found 
        ##return without recalculating.
        if(!is.null(cm))
        {
                message("Getting inverse from cache")
                #return cached data.
                return(cm)
        }
        
        ## We are here bcos there's no data in cache. lets calculate 
        ##and store it for future.
        
        ##get the matrix
        data <- x$get()
        ## calculate the inverse of the matrix
        cm <- solve(data)
        ## store the inverse in the cache.
        x$setInverse(cm)
        ##return the inversed matrix.
        cm
}

## Sample data to generate invertible matrix
##sampledata = function()
##{
        ##data.frame(a = c(1,2,3,4,5), b=c(6,7,8,9,10), c=c(11:15), d=c(16:20), 
                ##e=c(21:25))
        ##a <- data.frame(w=c(2,2), a=c(3,2))
##        data.frame(a=c(2,-3,2), b=c(0,1,-1), c=c(-1,1,0))
##}

