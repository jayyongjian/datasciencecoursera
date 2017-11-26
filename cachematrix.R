## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix and cacheSolve are functions for calculating large quantities of inverse matrixes
## by first checking whether an inverse matrix has been calculated before calculation.
##
## makeCacheMatrix creates a special vector to cache the inverse matrix
## cacheSolve calculates the inverse matrix by first checking whether the inverse matrix has already 
## been calculated. If so, it gets the inverse matrix from the cache.

## Write a short comment describing this function

#  makeCacheMatrix creates a special "vector", which is really a list containing a function to
#  1. set the values of the matrix
#  2. get the values of the matrix
#  3. get the values of the inverse matrix
#  4. get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   m<-NULL
   set<-function(y){
     x<<-y
     m<<-NULL
   }
   
   get<-function()x
   setinverse<-function(solve) m<<- solve
   getinverse<-function()m
   list(set=set,get=get,
        setinverse=setinverse,
        getinverse=getinverse)
}


## Write a short comment describing this function

#  cacheSolve calculates the inverse of the special "matrix" created with the makeCacheMatrix function.
#  However, it first checks to see if the inverse matrix has already been calculated. 
#  If so, it gets the inverse matrix from the cache and skips the computation. 
#  Otherwise, it calculates the inverse matrix and sets the inverse matrix in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
   m<-x$getinverse()
    if(!is.null(m)){
    message("getting cached data")
    return(m)
   }
   data<-x$get()
   m<-solve(data,...)
   x$setinverse(m)
   ## Return a matrix that is the inverse of 'x'
    m
}
