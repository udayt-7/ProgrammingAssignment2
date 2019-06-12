#11-06-2019
#CACHE MATRIX

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   # variable for cached inverse
  set <- function(y) {
    x <<- y #matrix variable
    m <<- NULL
  }
  #return the matrix
  get <- function(){
    x
  }
  # assign actual inverse value calculated to 'm' variable
  setInverse <- function(inverse){
    m <<- inverse
  }
  # returns the cached Inverse value
  getInverse <- function(){
    m
  }
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  if(!is.null(m)) {    #if value not NULL, get old inverse value and exit
    message("getting cached data")
    return(m)
  }
  #calculate inverse since value is NULL and Retrieve the initial matrix
  #Calculate inverse using solve() and store to cache
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  
  m   # print the inverse matrix
}