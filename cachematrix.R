## The following functions cache the inverse of a matrix to save computational power were possible
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
          m <- NULL
          set <- function(y){
            x <<-y
            m <- NULL
          }
          
          get <-  function() x
          setinv <- function(inverse) m<<-inverse
          getinv <- function() m 
          list (set=set, get=get, setinv=setinv, getinv=getinv)


}

## Cachesolve calculates the inverse of the special matrix if it doesn't exsists. 
## If the inverse already exsist then retrieve it without computing

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinv()
          if(!is.null(m)){
            message("getting cached result")
            return(m)
          }
          data <- x$get()
          
          m <- solve(data, ...)
          x$setinv(m)
          m
}
