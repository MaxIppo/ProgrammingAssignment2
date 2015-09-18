## This 2 functions are aimed to calculate the inverse matrix of an input matrix, 
## reusing the inverse matrix already calculated previously, using 2 techniques:
## (1) specific environment variables (variables are recorderd in the 
## makeCacheMatrix environment); (2) function providing a list of funtion 
## to manage environment specific variables  


makeCacheMatrix <- function(x=matrix()) {
        i <- NULL #i as the inverse vector of the x input matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        } #function to set the input matrix and initialize inverse matrix to 
        #keep consistency between input matrix and previous inverse matrix 
        #calculation. Vectors saved in the "makeCacheMatrix" 
        #specific environment"
        
        get <- function() x
        #function to retrieve input matrix  
        
        setinverse <- function(inv) i <<- inv
        #function to set inverse matrix in the "makeCacheMatrix specific 
        #environment"
        
        getinverse <- function() i
        #function to retrieve inverse matrix  
        
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
} #function provide as output a list of functions able to work on variables 
# defined during run of the  makeCacheMatrix execution


## cacheSolve got a function (and its environment) as input. It got variables
# from the argument function environemnt (as the matris or its inverse 
#if calculated). If no inverse matrix were calculated, it calculates the 
#inverse matrix. If inverse matrix were calculated it takes the exixting value.
# in this case it is not possible of discrepancy between matrix and its inverse 
# recorded in the argument function environment, in fact in case of setting up 
# the matrix with makeCacheMatrix$set, also the inverted matrix content is 
#initialized


cacheSolve <- function(x) {
        
        m<-0 # "m" as the variable to store retrieved inverse matrix, initialized 
        m <- x$getinverse() # getinverse function ot the x instance of the 
        # argument funtion is invoked to return inverse matrix
        
        if(!is.null(m)){
                print ("getting cached data")
                return(m)
        # returns cahed inverse matrix if any, the string highlights the source 
        #of the inverse matrix 
        }
        data <- x$get()  # "data" to store input matrix
        n <- solve(data) # "n" to store inverse matrix of "data"
        x$setinverse(n) # store the (inverse) "n" matrix in the argument 
        #function specific environment
        n
        ## Return a matrix that is the inverse of 'x'
        }
