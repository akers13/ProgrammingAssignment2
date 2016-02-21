## These two functions create lists of other functions that work in tandem to store
## the inverse of a matrix and to compute the inverse of another matrix after 
## checking to see if the inverse is already stored. If the inverse is stored, 
## this value is retrieved. Otherwise, the inverse is calculated as normal using 
## the "solve" function.

## makeCacheMatrix creates an empty matrix into which the inverse of a given matrix
## can be stored.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) m<<-solve
        getsolve<-function(solve) m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve checks to see if the inverse for which it is solving is stored.
## If it is stored, this function retrieves the stored value. If not, this
## function calculates the inverse as normal.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
