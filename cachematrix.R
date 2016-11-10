## Coursera - Week 3 - Programming Assignment : Lexical Scoping.

## makeCacheMatrix is a function which creates a special object matrix which cache the inverse of the input matrix. The input matrix should be invertible.

makeCacheMatrix <- function(x = matrix()){    
          m <- NULL
          set <- function(y){
                   x <<- y  
                   m <<- NULL 
                  }
          get <- function() x 
          setInverse <- function(solve) m<<- solve 
          getInverse <- function() m 
          list(set = set, get = get,
                       setInverse = setInverse,
                       getInverse = getInverse) 
}
          

## cacheSolve function computes the inverse of the special object matrix from the makeCacheMatrix function.
## If the inverse is already calculated then it will retrive the inverse from the above created cache.
                    
cacheSolve <- function(x, ...) {
        -       m <- x$getInverse()                 
                if(!is.null(m)){                    
                        message("getting cached data")    
                        return(m)                          
                }
                data <- x$get()                     
                m <- solve(data, ...)               
                x$setInverse(m)
                m
}
