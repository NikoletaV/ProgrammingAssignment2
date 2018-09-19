## These functions save time by retrieving a previously calculated value. Here we work on the inverse of a matrix.
## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL                              
        set <- function(y) {                     
                x <<- y                           
                inv <<- NULL                     
        }
        get <- function() x                    
        
        setinverse <- function(inverse) inv <<- inverse  
        getinverse <- function() inv                     
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}
## The cacheSolve function gives the inverse of the special "matrix" returned by makeCacheMatrix above. If it is already calculated it will take the inverse from the cache

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}