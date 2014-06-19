## Together, these two functions facilitate the creation of an inverted matrix (for a given one) and make sure
## that we can recall both matrices quickly from the cached memory.  

## The first function makes sure we can quickly set AND recall the value of a given matrix x and its inverted matrix. 

makeCacheMatrix<- function(x = matrix()) {
        i <- NULL  ##clears initially the inverted matrix in cache
        set <- function(y) {
                x <<- y
                i <<- NULL
        } ## "set" sets the value of the matrix to what is given by the user
        get <- function() x ##recalls the matrix
        setinvert <- function(invert) i <<- invert ##caches the inverted matrix calculated in the second function
        getinvert <- function() i ## recalls the inverted matrix
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## The second function facilitates the calculation of the inversed matrix given a matrix (already cached)

cacheSolve <- function(x, ...) {
        i <- x$getinvert()	## Here we check if the cache already contains an answer and we call it i
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }				## ...which we retrieve in this part (unless it is non-existant, "= NULL"
        data <- x$get()   	## ...here we recall the original matrix from the cache
        i <- solve(data, ...) ## ...here we calculate the inverted matrix...
        x$setinvert(i)  	## ...and here we file the inverted matrix to the cache
        i
}
