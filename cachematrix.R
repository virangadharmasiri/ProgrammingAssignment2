##This function creates a special "matrix" object that can cache its inverse for the input (which is an invertible square matrix)

##First, set the input 'x' as a matrix, set the solved value 'inver' as a null and set the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    
    set <- function(y) {         
       x <<- y
       inver <<- NULL
    }
    
    ##Here, get the value of the matrix, set the value of the inverse and get the value of the inverse
    
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" created by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),then it retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
  
    inver <- x$getinverse()     #Here, return a matrix that is the inverse of 'x'
    if(!is.null(inver)) {
       message("getting cached data")
       return(inver)
}
    ##Set the value of the inverse of the cache, compute the inverse of the matrix and set the value of the inverse of the cache
    
    mat <- x$get()
    inver <- solve(mat, ...)
    x$setinverse(inver)
    inver
}



