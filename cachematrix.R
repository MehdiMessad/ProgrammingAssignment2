## Put comments here that give an overall description of what your
## functions do
## R Programming Coursera
#
## Programming Assignement 2
#
## This second assignement deals with matrix inversion, which is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
#
## We will use the following two functions to cache the inverse of a matrix:

## Write a short comment describing this function
#
## First, makeCacheMatrix will create a list containing a function to:
## # 1) Set the value of the matrix (via setmat)
## # 2) Get the value of the matrix (via getmat)
## # 3) Set the value of the inverse of the matrix (via setinv)
## # 4) Get the value of the inverse of the matrix (via getinv)
#

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        setmat <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmat <- function() {
                return(x)
        }
        setinv <- function(inverse) inv <<- inverse
        getinv <- function(inverse) {
                return(inv)
        }
        list(setmat=setmat, getmat=getmat, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
#
## Secondly, the function cacheSolve returns the inverse of the matrix assuming that the matrix is
## always invertible.
## cacheSolve checks first if the inverse has already been computed.
## If it is TRUE, cacheSolve gets the result and skips the computation.
## If it is FALSE, cacheSolve computes the inverse and sets the value in the cache using the setinv
## function.
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data.")
                return(inv)
        }
        data <- x$getmat()
        inv <- solve(data)
        x$setinv(inv)
        inv

}

## Example with a 4x4 matrix:
##
## > x=matrix(c(2,1,0,0,3,0,2,2,1,3,-3,3,5,1,2,1),4,4)
## > m= makeCacheMatrix(x)
## > m$getmat()
## [,1] [,2] [,3] [,4]
## [1,]    2    3    1    5
## [2,]    1    0    3    1
## [3,]    0    2   -3    2
## [4,]    0    2    3    1
##
## There will be no cache in the first run:
## > cacheSolve(m)
## [,1] [,2] [,3] [,4]
## [1,]   18  -35  -28    1
## [2,]    9  -18  -14    1
## [3,]   -2    4    3    0
## [4,]  -12   24   19   -1
##
## Retrieving from the cache in the second run:
## > cacheSolve(m)
## getting cached data.
## [,1] [,2] [,3] [,4]
## [1,]   18  -35  -28    1
## [2,]    9  -18  -14    1
## [3,]   -2    4    3    0
## [4,]  -12   24   19   -1
##
