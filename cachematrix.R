## These functions are developed for the Week 3 Assignment for the 
## Coursera course R Programming

## They will use the caching features to help minimize the time used to compute
## the inverse of a matrix

## This function sets up a list that includes functions used to store and 
## retrieve a matrix, and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setMatrix<-function(solve) m<<-solve
        getMatrix<-function() m
        list(set=set,get=get,setMatrix=setMatrix,getMatrix=getMatrix)
}


## This function computes the inverse of a matrix, if it has not already been 
## solved once, and cached.  If it has been cached, it simply retrieves the 
## result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getMatrix()
        if(!is.null(m)){
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setMatrix(m)
        m
}
