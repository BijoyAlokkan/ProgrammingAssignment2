## Below function helping in inversing matrix using caching method that minimize
## computational power needed 

## This function helps create a special matrix object that can be inversed easily

makeCacheMatrix <- function(x = matrix()) {
        inverse_m <- NULL
        
	  set <- function(a) {
                x <<- a
                inverse_m <<- NULL
        }
        get <- function() x
        
        getInverse <- function() inverse_m
		
	  setInverse <- function(inv_mat) inverse_m <<- inv_mat
        
	  list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function will return the inversed matrix if it is there in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  
	  inverse_m <- x$getInverse()
        if (!is.null(inverse_m)) {
                message("Inverse exists getting the matrix that is cached")
                return(inverse_m)
        }


        inverse_m <- solve(x$get())
        x$setInverse(inverse_m)
       
	  inverse_m
}
