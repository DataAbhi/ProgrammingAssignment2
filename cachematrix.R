## Put comments here that give an overall description of what your
## functions do

##makeCachematrix makes a matrix, get gets the value; set changes it
##setsolve assigns the value of an invers; get solve prints the value of an inverse
##getsolve is none if the inverse has not been assigned

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve returns an inverse of matrix using the makeCacheMatrix list
##If inverse has already been calculated then it returns the cache value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
