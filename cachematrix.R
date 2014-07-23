## 2 functions that cache the inverse of a matrix


## Create a matrix object that caches its inverse
makeCacheMatrix <- function( mtx = matrix() ) {

    ## initialize 
    i <- NULL

    ## set the matrix
    set <- function( matrix ) {
            mtx <<- matrix
            i <<- NULL
    }

    ## get the matrix
    get <- function() {
    	mtx
    }

    ## set the inverse of the matrix
    setinverse <- function(inverse) {
        i <<- inverse
    }

    ## get the inverse of the matrix
    getinverse <- function() {
        ## Return the inverse property
        i
    }

    ## return a list 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse of the matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    mtx <- x$getinverse()

    ## Just return the inverse if its already set
    if( !is.null(mtx) ) {
            message("getting cached data")
            return(mtx)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    mtx <- solve(data) %*% data

    ## Set the inverse to the object
    x$setinverse(mtx)

    ## Return the matrix
    mtx
}
