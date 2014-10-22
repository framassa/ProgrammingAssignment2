## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {    
        inver<- NULL 			## set the value of the matrix
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
              
        get <- function() x  		## get the value of the matrix
        
      					## set the inverse of the matrix
        setinverse <- function(solve) inver <<- solve 
        getinverse <- function() inver
        
        list(set = set, get = get,             ## get the inverse of the matrix
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

        inver <- x$getinverse() ## get the inverse of the matrix

        
        if(!is.null(inver)) {   ## check if there is the matrix
                message("getting cached data")
                return(inver)
        }
        data <- x$get() ## if not get the inverse of the matrix 
        inver <- solve(data, ...)  ## set the inverse of the matrix 
        x$setinverse(inver)
        inver
}
