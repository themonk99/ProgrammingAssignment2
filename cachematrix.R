## This function  
## 1) takes the vector X, and creates a Matrix.
## 2) determines if the inverse is NULL 
## 3) uses `<<-` to assign a value to Y, which is in an environment 
## different from the current environment. 

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
}

        get = function()x
                setinv = function(inverse) inv <<- inverse 
                getinv = function() inv
                list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Takes the inverse of makeCacheMatrix. If the inverse is has already 
## been calculated (ie: is not NULL), it calculates the inverse.

cacheSolve <- function(x, ...){
                inv = x$getinv()
                if (!is.null(inv)){
                        message("getting cached data")
                        
                        return(inv)
                }
        
        mat.data = x$get()
                inv = solve(mat.data, ...)
                        x$setinv(inv)
        
        return(inv)
}