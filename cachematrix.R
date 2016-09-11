## There are two functions contained in this file, the first defines a 'special' 
## matrix with the ability to define the inverse of the matrix. For the purpose 
## of this, it is assumed that the matrix is invertible.The second function will
## cache the inverse of the matrix. 


## This function takes in a matrix, and allows for the cacheing of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ##Set the inverse matrix to NULL
        inv <- NULL 
        
        ##Initialize this 'special matrix' with a NULL inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        } 
        ##returns the 'special matrix'
        get <- function() x
        
        ##Allows the inverse matrix to be set by a user
        setInv <- function(solve) inv <<- solve 
        
        ##Returns the inverse                    
        getInv <- function() inv
        
        ##List the methods available
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## This function takes in an object from makeCacheMatrix, is checks if the
## inverse has already been calculated, if it is it doesn't copmute it.
## Otherwise is will calculate the inverse and set the inverse matrix in the 
## cache via the setInv function

cacheSolve <- function(x, ...) {
        
        ## Get the current inverse
        inv <- x$getInv()
        
        ##Check to see if inverse is already calculated
        if(!is.null(inv)){
                message("Getting cached matrix")
                return(inv)
        }
        
        ## If inverse is Null then get the matrix data and calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        ## Set the cached inverse
        x$setInv(inv)
        
        #Will return Inverse
        inv
        
}
