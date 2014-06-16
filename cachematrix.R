## Below functions helps caching inverse of matrix. 
## Inverse can be retrieved from cache instead of computing again when matrix data is not changed.
## This will be 2 step process.
## 1. create special matrix using makeCacheMatrix function.
## 2. calculate inverse using cacheSolve function if inverse is not available already

##The makeCacheMatrix function creates a special "matrix", 
##which is a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the Inverse of matrix
##get the value of the Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  
# Define inverse of matrix to NULL  
  invMatrix <- NULL

# set function to set the value of special matrix x using data from matrix y and 
#set inverse of matrix to NULL
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
  }

# set function to get value of special matrix x
  get <- function () {
    x
  }

# setInvMatrix function to set inverse of special matrix x
  setInvMatrix <- function (inverseM) {
    invMatrix <<- inverseM
  }

# getInvMatrix function to set inverse of special matrix x
  getInvMatrix <- function () {
  invMatrix
  }

  list (set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix) 

}


## This function checks if inverse of matrix is already calculated and if yes then return the same.
## If inverse of matrix is not available then it calculates the same.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get inverse of matrix using getInvMatrix function
  invMatrix <- x$getInvMatrix()
  
  # check if not null (already available) inverse is returned, if yes then return that value as inverse of matrix 
  
  if (!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  } 
  
  # if inverse of matrix is not available calculate the same and return 
  
  # get the matrix data from special matrix x
  data <- x$get()
  
  # calculate inverse of matrix
  invMatrix =  solve(data, ...)
  
  #set inverse of matrix in special matrix x
  x$setInvMatrix(invMatrix)
  
  #return inverse of matrix
  invMatrix
}
