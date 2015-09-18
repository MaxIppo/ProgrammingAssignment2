## This 2 functions are aimed to calculate the inverse matrix of an input matrix, 
## reusing the inverse matrix already calculated previously, using 2 techniques:
## (1) specific environment variables (variables are recorderd in the 
## makeCacheMatrix environment); (2) function providing a list of funtion 
## to manage environment specific variables  


makeCacheMatrix <- function(x=matrix()) {
        is.matrix(i)
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inv) i <<- inv
        
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve got a function (and its environment) as input. It got variables
# from the argument function environemnt (as the matris or its inverse 
#if calculated). If no inverse matrix were calculated, it calculates the 
#inverse matrix. If inverse matrix were calculated it takes the exixting value.
# in this case it is not possible of discrepancy between matrix and its inverse 
# recorded in the argument function environment, in fact in case of setting up 
# the matrix with makeCacheMatrix$set, also the inverted matrix content is 
#initialized


cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m<-0
        m <- x$getinverse()
        if(!is.null(m)){
                print ("getting cached data")
                return(m)
        }
        data <- x$get()
        n <- solve(data)
        x$setinverse(n)
        n
}
