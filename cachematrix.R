## returns a list of four functions to get/set the matrix, get/set inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #setting the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x # getting the matrix
  setInverse <- function(Inverse) m <<- Inverse # setting the inverse
  getInverse <- function() m # fetching the inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #list with 4 functions
}



## calculates (or retrieves/puts from/to cache) and returns the inverse of a matrix

cacheSolve <- function(x) {
  m <- x$getInverse() #getting the cached value for the inverse
  if(!is.null(m)) {   #in case there is the cache wee just return the cached value
    message("getting cached data")
    return(m)
  }
  data <- x$get() #if there is not cache we get the matrix
  m <- solve(data) #calculate the inverse
  x$setInverse(m) #cache it
  m
}

#sample
# x = matrix(c(1, -5, 3, -2),  nrow=2, ncol=2)   
# m = makeCacheMatrix(x)
# cacheSolve(m)
# 
# [,1]        [,2]
# [1,] -0.1538462 -0.23076923
# [2,]  0.3846154  0.07692308
# 
# cacheSolve(m)
# getting cached data
# [,1]        [,2]
# [1,] -0.1538462 -0.23076923
# [2,]  0.3846154  0.07692308