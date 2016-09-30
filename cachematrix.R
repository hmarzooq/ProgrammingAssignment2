## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(a = matrix()) {
        invr <- NULL
        set <- function(n) {
                a <<- n
                invr <<- NULL
        }
        get <- function() a
        set_invr <- function(inverse) invr <<- inverse
        get_invr <- function() invr
        list(set=set, get=get, set_invr=set_invr, get_invr=get_invr)
}


## Write a short comment describing this function

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
        invr <- a$get_invr()
        if(!is.null(invr)) {
                message("Retreiving data from cache.")
                return(invr)
        }
        data <- a$get()
        invr <- solve(data)
        a$set_invr(invr)
        return(invr)
}


a = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(a)
m$get()


cacheSolve(m)  ## return value not from cache


cacheSolve(m) ## return value from cache