## makeCacheMatrix returns a list containing a function to set the value of the matrix, get the value of the matrix
## set the value of the inverse of matrix, get the value of the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    ## Critical is the next line. The operators <<- is normally only used in functions, and cause a search to made through PARENT 
    ## environments for an existing definition of the variable being assigned.  
    setmatr <- function(matr) m <<- matr
    getmatr <- function() m
    list(set = set, get = get,
         setmatr = setmatr,
         getmatr = getmatr)    
}


##  cacheSolve first checks to see if the inverse of matrix has already been calculated. If so, it gets the inverse of matrix from 
## the cache and skips the computation. Otherwise, it calculates the inverse of matrix and sets the value of the inverse of matrix
## in the cache via the setmatr function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatr()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## If m is null (i.e. not calculated yet) then:
    data <- x$get()
    m <- solve(data)
    x$setmatr(m)
    m
}

## Example how it works
## matrix v, 3x3
v<-matrix(rnorm(9),nrow=3,ncol=3)
b<-makeCacheMatrix(v)
## inverse is calculated the first time
cacheSolve(b)
## the second time cached data is used
cacheSolve(b)
## check to prove that it is inverse
cacheSolve(b) %*% v # is almost identity matrix

## to change the input matrix use:
b$set(matrix(rnorm(16),nrow=4,ncol=4))
## inverse itself
cacheSolve(b)
cacheSolve(b)
