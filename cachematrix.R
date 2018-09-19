## These functions save time by retrieving a previously calculated value. Here we work on the inverse of a matrix.
## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL                              ##
        set <- function(y) {                     
                x <<- y                           
                inv <<- NULL                     
        }
        get <- function() x                    
        
        setinverse <- function(inverse) inv <<- inverse   ## assigns value of inv in parent environment
        getinverse <- function() inv                     ## gets the value of inv where called
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}
## The cacheSolve function gives the inverse of the special "matrix" returned by makeCacheMatrix above. If it is already calculated it will take the inverse from the cache

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {                   ## if the value of inv exists it gets the value from the cache
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                     ## if it didnt exist it will calculate it
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}