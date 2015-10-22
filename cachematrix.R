## This is Assignment Two from R Programming on Coursera
## Objective:
##   Write an R function that is able to
##   Calculate and cache the inverse of an invertible matrix
##   The solution should take advantage oflexical scoping 

## Which   Who            When          What
##  1.0    Sam Friedman   21-Oct-2015   Initial version

## Approach:
## Use two functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix acts as a matrix class with a constructor 
## and methods to store and interact with the matrix
## It does this be creating a list of functions 

## cacheSolve calculates the matrix inversion
## and stores the result in the "object" created by makeCacheMatrix

## constructor
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## property that stores the inverse is initially null
  
  set <- function(y) {  ## (re)set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x  ## return the value of the matrix
  
  ## set the value of the inverse matrix
  set_inverse <- function(inverse) inv <<- inverse
  
  ## get the value of the inverse matrix
  get_inverse <- function() inv
  
  ## return a list of functions (get/set methods)
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Return a matrix that is the inverse of 'x'
## use the functions created by the makeCacheMatrix function to interact with the matrix
cacheSolve <- function(x, ...) {
  
  m <- x$get_inverse() ## get the list object that stores the inverse
  
  ## if that object is not null that it has been cached
  ## return it 
  ## calculate nothing
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## implied else
  
  ## use the makeCacheMatrix function to get the matrix
  data <- x$get()
  
  ## solve for the inverse
  m <- solve(data, ...)
  
  ## use the makeCacheMatrix functions to store the inverse
  x$set_inverse(m)
  
  ## return the inverse
  m
}

testCacheMatrix <- function() {
  matrix_tmp <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
  
  matrix_object  <- makeCacheMatrix(matrix_tmp)
  
  ## first solve attempt performs a calculation
  matrix_inverse1 <- cacheSolve(matrix_object) 
  
  ## second solve attempt performs a calculation
  matrix_inverse2 <- cacheSolve(matrix_object) 
  
}