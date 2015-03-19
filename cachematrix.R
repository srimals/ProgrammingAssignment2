## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        cm <- NULL
        
        set <- function(y)
        {
                x <<- y
                cm <<- NULL
        }
        
        get <- function() 
        {
                x
        }
        
        setInverse <- function(inverse)
        {
                cm <<- inverse
        }
        
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
        cm <- x$getInverse()
        
        if(!is.null(cm))
        {
                message("Getting inverse from cache")
                return(cm)
        }
        
        data <- x$get()
        cm <- solve(data)
        x$setInverse(cm)
        cm
}

sampledata = function()
{
        ##data.frame(a = c(1,2,3,4,5), b=c(6,7,8,9,10), c=c(11:15), d=c(16:20), e=c(21:25))
        ##a <- data.frame(w=c(2,2), a=c(3,2))
        data.frame(a=c(2,-3,2), b=c(0,1,-1), c=c(-1,1,0))
}



makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}



