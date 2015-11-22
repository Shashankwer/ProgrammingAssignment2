## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The following function creates a  special matrix used for caching the results of the inverse of the squared matrix.
makeCacheMatrix <- function(x = matrix()) {
inv<-matrix() #creates an empty matrix
#function used for setting the inverse matrix 
set<- function(y){ 
  x<-y
  inv<-matrix() #initialized the inverse to null for each new matrix creation. 
}
get<-function() x #giving the matrix as output
setInverse<-function(in1) inv<<-in1 ##setting the inverse matrix 
getInverse<-function() inv
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
#the function will check whether the inverse exist in the cache or not if not then the new value is set
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  #if the above function call returns null matrix then the elements of the matrix that is mat[1,1] will be NA. Thus the function is.na(mat[1]) will return true
  if(!is.na(inv[1])) #considering first element.
   {
    message("Retrieving the inverse matrix from the cache")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInverse(inv)
  inv
}
