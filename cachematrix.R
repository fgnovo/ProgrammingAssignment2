## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix, create a list containing a function to
## 1. set, that It sets the value of the matrix.
## 2. get, that It gets the value of the matrix.
## 3. setSolve, that It sets the value of the inverse matrix 
## 4. getSolve, that It gets the value of the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<-y
        inv <<-NULL
    }
    get <- function() x
    setSolve <- function(solve) inv <<- solve
    getSolve <- function() inv
    list(set = set,get =get,
         getSolve = getSolve,
         setSolve = setSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <-x$getSolve()
    if(!is.null(inv)){
        message("getting cached data")        
    }else{
        data <- x$get()
        inv <- solve(data)
        x$setSolve(inv)
    }
    inv
}
