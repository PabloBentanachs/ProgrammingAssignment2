## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##To catch the inverse of a matrix
## The function makeCacheMatrix() will create a matrix object which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinv<-function(solve) i<<-solve
  getinv<-function()i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## Write a short comment describing this function
##The function cacheSolve() will compute the inverse of the matrix object returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data, ...)
  x$setinv(i)
  i
  
}
