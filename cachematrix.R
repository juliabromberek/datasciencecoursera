## Caching the inverse of a matrix
## makeCacheMatrix will compute and cache the inverse of the matrix
## cacheSolve will output the inverse of the matrix

## makeCacheMatrix will set the matrix, compute the inverse, and cache it
makeCacheMatrix <- function(x=matrix()){
  m <- NULL
  set <- function(y) {  
    x <<- y  #sets the value of the matrix
    m <<- NULL
  }
  get <- function() x  #gets the value of the matrix
  setinverse <- function(solve) m <<- solve  #sets the value of the inverse
  getinverse <- function() m  #gets the value of the inverse
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve will check if the inverse is already cached and return that value. 
## If not cached, it will calculate the inverse, cache it, and return it
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)  #checks to see if inverse is already cached and outputs it
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m) 
  m #if the inverse isn't cached, this calculates, caches, and outputs it
}
