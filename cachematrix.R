## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to: 
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
  ##Set value of m as null  
  m <- NULL
  
  ##1. Set the value of Vector
  set <- function(y) 
  {
      x <<- y
      m <<- NULL
  }
  
  ##2. Get the value of vector
  get <- function() 
  {  
      x
  }
  
  ##3. Set the value of the inverse in the cache
  setinverse <- function(inverse) 
  {
      m <<- inverse
  }
  
  ##4. Get the value of the inverse
  getinverse <- function() 
  {  
      m
  }
  
  ##Return the functions that can be used by the CacheSolve function to set and get the inverse of the square matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The following function calculates the inverse of the special "vector" created with the above function makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) 
{
    ## Try to get the inverse of matrix 'x' from the cache
    m <- x$getinverse()
    
    ## If matrix is available in cache, then return it and exit the function
    if(!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }
    
    ##Otherwise, get the matrix, calculate its inverse, and set it in cache
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
