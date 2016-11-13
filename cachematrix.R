## two functions to  cache the inverse of a matrix


makeCacheMatrix <- function( y = matrix() ) {
        
        ## inverse property
        i <- NULL
        
        ## set the matrix
        set <- function( matrix ) {
                y <<- matrix
                i <<- NULL
        }
        
        ## Method 
        get <- function() {
                
                y
        }
        
        ## set the inverse 
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## get the inverse of the matrix
        getInverse <- function() {
                
                i
        }
        
        ## list them
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Compute the inverse
cacheSolve <- function(x, ...) {
        
        ## Return a matrix inverx "x"
        y <- x$getInverse()
        
        ## return the inverse in cas of set
        if( !is.null(y) ) {
                message("getting cached data")
                return(y)
        }
        
        ## Get the matrix
        data <- x$get()
        
        ## Calculate the inverse 
        y <- solve(data) %*% data
        
        ## Set the inverse 
        x$setInverse(y)
        
        ## Return the matrix
        y
}