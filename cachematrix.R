## The two functions in this script are used to create a special object that stores
## a numeric square matrix and caches its inverse


## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of the special "matrix" created with
## the function above. When the inverse has already been calculated it gets
## the inverse from the cache, otherwise it will calculate the invere of the 
## matrix and sets the inverse of the matrix in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
