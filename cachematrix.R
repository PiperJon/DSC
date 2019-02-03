## Objective is to crete two functions
## makeCacheMatrix 
## cacheSolve 

## makeCacheMatrix creates a special matrix with additional information that can cache its own inverse
## to run, first create a matrix "mat", then matlist <- makeCacheMatrix(mat)

makeCacheMatrix <- function(x = matrix()) {
  ## x is initialised as argument, default empty matrix
  inv <-matrix()
  inv<- NULL                                
  ## initialise xinv  as an object in makeCacheVector environment
  set <- function(y){                     
    ## mutator 
    x <<- y                                 
    ## <<- assigns value y to object x in parent env, clear m
    inv <<- NULL                               
  }
  
  get<-function()x                       
  ## x undefined retrieve from parent environment
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## cacheSolve computes inverse of matrix returned by makeCacheMatrix, checking first to see if already calculated
## cachesolve(matlist)  where matlist is output of makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## call the getinv function from the list
  inv <- x$getinv()
  
  ## if inv is not null then it is a valid mean of the data, and inv is returned
  if(!is.null(inv)) {
    message("getting cached data")make
    return(inv)
  }
  ## if inv is null, calc the inverse, set it, return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
