#the following functions allow you to create a sprcial matrix, that can cache the inverse of a square invertible matrix

makeCacheMatrix <- function(x=matrix()){ #creates the special matrix to hold the cached inverse matrix
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function()x
  
  setmatrix<-function(solve) m<<-solve
  
  getmatrix<-function()m
  
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
    
}

cacheSolve <- function(x=matrix(), ...){ #caches the inverse matrix in the object created by makeCacheMatrix()
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached inverse matrix")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setmatrix(m)
}
