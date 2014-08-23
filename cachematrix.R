## I have mainly used the discussion forums to complete my assignment.
## makeCacheMatrix takes a matrix as an input and 
    ##contains the list of functions that the cacheSolve function uses to cache the
    ##inverse of the matrix.  
## cacheSolve functions fetches the cached value of the inverse of the matrix,
    ##if it is the same matrix else calculates the inverse calling the functions
    ##mentioned in makeCachematrix.

##makeCacheMatrix function

makeCacheMatrix <- function(x = matrix())
{
  my_matrix <- NULL
  set <- function(y) 
  {
    x <<- y
    my_matrix <<- NULL
  }
  get <- function() {x}
  
  setinverse <- function(mean) {my_matrix <<- mean}
  
  getinverse <- function() {my_matrix}
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



##cacheSolve function. fetches the cached value of inverse of matrix or calculates if 
  ##it isnt cached.

cacheSolve <- function(x, ...) 
{
  my_matrix <- x$getinverse()
  if((!is.null(my_matrix) )) #& (my_matrix == x )))
  {
    message("getting cached data")
    return(my_matrix)
  }
  data <- x$get()
  my_matrix <- solve(data, ...)
  x$setinverse(my_matrix)
  my_matrix
}