## these two functions use cache to store calculations of matrix inverse
## for future reference


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## it is a list of four functions to Set and Get the value of a matrix 
## and Set and Get the Inverse (using "solve") of the same matrix 

## how to use:
## first, create an empty matrix "space" e.g. for matrix "mymatrix" 
## by: mymatrix <- makeCacheMatrix()
## then, use this to store a matrix: mymatrix$set(matrix(c(2, -1, 0, -1, 2, -1, 0, -1, 2),3,3))
## (...I checked, this 3X3 matrix above is solvable...)
## you can view it by typing: mymatrix$get

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) m <<- solve
    get_inverse <- function() m
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## This function computes the inverse of the special "matrix" returned by 
##     makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##     then cacheSolve should retrieve the inverse from the cache.
## so, from above steps (lines 12, 13), now you can apply this function below 
## to the defined matrix, "mymatrix" by using: cacheSolve(mymatrix)


cacheSolve <- function(x, ...) {
    
    m <- x$get_inverse() 
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
        
}



