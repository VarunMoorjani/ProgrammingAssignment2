## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) 
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Evaluating the components and the environment of the function:
test <- makeCacheMatrix()
test


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv2 <- solve(data, ...)
  inv2
}


# Assingning a matrix to test the function
b <- matrix(c(2,2,3,5),2,2)
aux1 <- makeCacheMatrix(b)
aux1$get()
#      [,1] [,2]
#[1,]    2    3
#[2,]    2    5
aux1$setInverse()
aux1$getInverse()
#      [,1]  [,2]
#[1,]  1.25 -0.75
#[2,] -0.50  0.50


environment(makeCacheMatrix)
#<environment: R_GlobalEnv>
environment(aux1$set)
#<environment: 0x0000000004b5f5a8>
ls(environment(aux1$set))
#[1] "get"        "getInverse" "inv"        "set"        "setInverse" "x" 

aux2<- cacheSolve(aux1)
#getting cached data


# Testing if cachesolve returns the same value
aux2
#      [,1]  [,2]
#[1,]  1.25 -0.75
#[2,] -0.50  0.50


#Another test

c<-matrix(1:4,2,2)
aux3<-makeCacheMatrix(c)
aux4<- cacheSolve(aux3)


# There's no message this time because inv in cacheSolve cannot be found! It happens since it's stored, as shown before, in the
# specific environment of makeCacheMatrix and it has not been explicitly retrieved as in the test above.

aux4
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# Notice that makeCacheMatrix stored the new matrix
aux3$get()
#     ,1] [,2]
#[1,]    1    3
#[2,]    2    4

# But since the matrix inverse is solved only internally (through the <<- operator), it cannot be retrieved

aux3$getInverse()
#NULL

aux3$getInverse
#function() inv
#<environment: 0x0000000005155ad0>
