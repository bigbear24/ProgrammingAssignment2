## Concept of Parent/Local Function Enclosure has been put to use over here.
## Use of '<<-' super Assignment Operator and how it differs from '<-'
## We have 2 main function , one has the Cached  data (Inverse of Square matrix)
## If the Inverse(only for invertible) is already present , it will be 
## produced , else the second Function will compute and update the value 

## makeCacheMatrix comes with a list of functions to update the value of 
## Matrix , its inverse value 
## We have local function enclosures inside Parent function 
## Super assignment operator makes Changes to both parent environment and 
## Local environment.

makeCacheMatrix <- function(x=matrix()){
  m <<- NULL                #makes initial inverse as Null for the first time
  set <- function(y=matrix()){
    x <<- y                   #In case you have a new matrix to be computed
    m <<- NULL                # Set the value of x to the current matrix 
  }
  get <- function()x
  setinv <- function(inv)m<<-inv  # Assign the value of inv to the m 
  getinv <- function() m
  
  list('getinv'=getinv,'setinv'=setinv,'get'=get,'set'=set)
}


## It will retrieve the last Inverse if already computed for the invertible mat.

cacheSolve <- function(x,...){
  m<-x$getinv()
  if(!is.null(m)){
    print('Getting Cached Data')
    return(m)
    
  }
  data<-solve(x$get())
  x$setinv(data)
}


