## makeCacheMatrix creates a special "matrix" 

makeCacheMatrix <- function(x = matrix()) { 

##  set the value of the matrix
    	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
## get the value of the matrix
 
        get <- function() x

## set the value of the iversed matrix

        setInv <- function(solve) inv <<- solve

## get the value of the inversed matrix

        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## CasheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {

## sets inv equal to the inverse matrix

        inv <- x$getInv()

## Checked if inverse has already been calculated and retrieve the inverse from the cache.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

## If inverse has not been calculated then calculate it and print

        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}

