## Write a pair of functions that cache the inverse 
## of a matrix

## This function creates a special "matrix" object 
## that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function() x
  setinverse<-function(inverse) inv <<-inverse
  getinverse<-function() inv
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}

amatrix <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
amatrix$get() # returns original matrix
## [,1] [,2]
## [1,]    1   3
## [2,]    2   4

cacheSolve(amatrix) # computes, caches, and returns matrix inverse 
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

amatrix$getinverse() # returns matrix inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

cacheSolve(amatrix) # returns cached matrix inverse using previously computed matrix inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

amatrix$set(matrix(c(0,5,99,66),nrow=2,ncol=2)) # modify existing matrix
cacheSolve(amatrix) # computes, caches, and returns new matrix inverse
## [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0

amatrix$get() # returns matrix
## [,1] [,2]
## [1,]    0   99
## [2,]    5   66

amatrix$getinverse()
## [,1] [,2]
## [1,] -0.13333333  0.2
## [2,]  0.01010101  0.0