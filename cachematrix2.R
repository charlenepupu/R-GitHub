## Create two functions to caching the inverse of a matrix rather than compute it repeatedly

##Step 1:
# "makeVector" function 
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of inverse of matrix
# 4.get the value of incerse of matrix
 


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    set <- function(y) { 
    x <<- y
    inv <<- NULL 

    } 

    get <- function() x 
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##Step 2 - Return a matrix that is the inverse of 'x'
# Using the following function to return the inverse of matrix. If we have had the inverse matrix 
# then output the result and skip the computation. If not, the setinverse function computes the inverse 
# and sets the value in cache.



cacheSolve <- function(x, ...) {
   inv <- x$getinverse() 
   if(!is.null(inv)) { 

   message("getting cached data.") 

   return(inv) 

  } 

   data <- x$get() 

   inv <- solve(data) 

   x$setinverse(inv) 

   inv 
}