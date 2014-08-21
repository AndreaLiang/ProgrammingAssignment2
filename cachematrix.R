## running these two functions will cache the inverse of a matrix for future use

## the matrix is input into the 1st function, which then creates an object of type 'list'
## this object is input into the 2nd function,
## which returns the inverse of the matrix that was input to the first function.

## the second function also caches the inverse for future use. 

###########

## this 1st function takes a matrix input 
## and returns a list of functions that are related to the matrix input
## these functions can be 'called' externally 
## and most are 'called' in the 2nd function which will create the inverse

makeCacheMatrix <- function(x = matrix()) {  
        
        i <- NULL                                       ## i will be the inverse 
                                                        ## and is reset to NULL everytime makeCacheMatrix() is called 
        
                                                        ## These next 3 functions are not run 
                                                        ## when makeCacheMatrix() is called
        
        set <- function(y){                             ## can be used to input a new matrix 
                                                        ## without re-writing the function 
                x <<- y  
                i <<- NULL 
        }
        
        get <- function() {x}                           ## this function returns x, the value of the original vector 
        
        setinverse <- function(inverse) i <<- inverse   ## 'inverse' is the variable input argument for this function
                                                        ## this is called by cacheSolve() 
                                                        ## during the first cacheSolve() access
                                                        ## and it will store the value using superassignment
        
        getinverse <- function() {i}                    ## this will return the cached value to cacheSolve() 
                                                        ## on subsequent accesses 
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
} 

##########

## this 2nd function takes the output from the first function 
## if this is not the first time it 'sees' this output
## and it has already calculated the inverse of the matrix previously
## it will just return the inverse and stop the function. 
## however, if it is the first time that it 'sees' the output
## it will calculate the inverse of the matrix
## and store it within the first fn's output
## so that it can be retrieved in future without calculation


cacheSolve <- function(x, ...) {                        ## input the object here created by makeCacheMatrix
        
        i <- x$getinverse()                             ## accesses the object 'x' and gets the value of the inverse
        
        if(!is.null(i)) {                               ## if i is not NULL i.e. if inverse was already cached 
                                                        ## this is because makeVector-ing an object 
                                                        ## always sets the i back to NULL
                
                
                message ("getting cached data")  
                
                return (i)                              ## return() ends the function cacheSolve and returns 'i'
        }
        
        data <-x$get()  
        
        i <- solve(data, ...) 
        
        x$setinverse(i)                                 ## stores the calculated inverse in x (so i is no longer NULL)
        
        i                                               ## returns a matrix that is the inverse of x
}