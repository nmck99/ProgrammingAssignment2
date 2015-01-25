
### These functions compute the inverse of the special "matrix" returned by makeCacheMatrix. 
### If the inverse has already been calculated (and the matrix has not changed), then cachesolve retrieves the inverse from the cache.

###This function creates a special "matrix" object and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Inverse<-NULL
        set<-function(y){
                x<<-y
                Inverse<<-NULL
        } 
        get<-function() x
        setInverse<-function(solve) Inverse<<- solve
        getInverse<-function() Inverse
        list(set=set, get=get,
             setInverse=setInverse,
             getInverse=getInverse)
        
}


### returns inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inverse<-x$getInverse()
        if(!is.null(Inverse)){
                message("getting cached data")
                return(Inverse)
        }
        message("newly calculating data")
        data<-x$get()
        Inverse<-solve(data, ...)
        x$setInverse(Inverse)
        Inverse        
}
