## This functions will create first create a matrix and then calculate  
## the inverse of the matrix. It is assumed that the matrix is 
## always invertible.


## The function creates a list containing different functions than will then be used
## in cacheSolve function to create and calculating the inverse of the matrix.

makeCacheMatrix <- function(x = matrix(),rows,cols) {

m <- NULL

set <- function(y,nr,nc) {
         x <<- y
		 rows <<- nr
		 cols <<- nc
         m <<- NULL
     }

get <- function() {matrix(x,nrow=rows,ncol=cols)}

setinv <- function(inv) {m <<- inv}

getinv <- function() {m}

list(set = set, get = get, setinv = setinv,getinv = getinv)

}


## This functions takes list created in the makeCacheFunction and obtains the matrix
## to calculate its inverse. The inverse matrix will be returned.

cacheSolve <- function(x, ...) {
      m <- x$getinv()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m 
}
