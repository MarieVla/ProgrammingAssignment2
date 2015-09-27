makeCacheMatrix <- function(x = matrix()) { 
#   function that stores the cached value 
#   initialise cached value to NULL   
        m <- NULL
#  set matrix: create matrix in working environment      
        set <- function(y) {
        x <<- y
        m <<- NULL
    } 
#  get matrix: get value of matrix  
    get <- function() x
# set inverse: invert matrix and store in cache     
    setinv <- function(inverse)
        m <<- inverse
# get inverted matrix: get inverted matrix from cache
    getinv <- function() m
# return functions created         
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
# cacheSolve: function to create inverse of the matrix created in makeCacheMatrix,
#   if m exists function uses cached data
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
#if m does not exist, the function creates the inverse and sets the inverse
# in the cache: 
    else data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}