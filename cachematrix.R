#PART 1:  EXAMPLE provided:

#The first function, makeVector creates a special "vector", which is really 
#a list containing a function to

#makeVector <- function(x = numeric()) {          
#        m <- NULL
#        set <- function(y) {                     1 - set the value of the vector
#               x <<- y
#                m <<- NULL
#        }
#        get <- function() x                      2 - get the value of the vector
#        setmean <- function(mean) m <<- mean     3 - set the value of the mean
#        getmean <- function() m                  4 - get the value of the mean
#        list(set = set, get = get,
#             setmean = setmean,
#             getmean = getmean)
#}
#-------------------------
## This function creates a special "matrix" object that can cache its inverse.  
## I'll use the same methodology as the example above.

makeCacheMatrix <- function(m = matrix()) {         
        i <- NULL                                #Here I'm copying but don't really understand
        set <- function(matrix) {                #1 - set the value of the matrix
                m <<- matrix
                i <<- NULL
        }
        
        get <- function() {m}                    #2 - get the matrix and return it
        setinverse <- function(inverse) {i <<- inverse}  #3 - set the inverse of the matrix
        getinverse <- function() {i}            #4 - get the inverse of the matrix
                                                #    and return inverse property
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)           # Return a list (copying format)
}

#PART 2:  EXAMPLE provided:

#The following function calculates the mean of the special "vector"
#created with the above function. However, it first checks to see if the
#mean has already been calculated. If so, it `get`s the mean from the
#cache and skips the computation. Otherwise, it calculates the mean of
#the data and sets the value of the mean in the cache via the `setmean`
#function.

#cachemean <- function(x, ...) {
#        m <- x$getmean()
#        if(!is.null(m)) {
#               message("getting cached data")
#                return(m)
#        }
#        data <- x$get()
#        m <- mean(data, ...)
#       x$setmean(m)
#        m
#}
#-------------------------
## This function computes the inverse of the special matrix returned by
## makeCacheMatrix above.  If the inverse has already been calculated (and
## the matrix has not changed), then the cacheSolve below should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse
        if (!is.null(m)) {         
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data      # actually calculate the inverse (instead
                                # of using the mean function in the example)
        x$setinverse(m)
        m                       # Return a matrix that is the inverse of 'x'
}
