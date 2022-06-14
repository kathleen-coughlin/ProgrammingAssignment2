## Assignment 2 Description:

#Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly (there 
# are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a 
# matrix. Write the following functions:
    #makeCacheMatrix
    #cacheSolve

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()){
    inv <- NULL # reset any previous inv value
    
    #clear any previously cached inverse or x
    setInput <- function(y){
        # assign input argument to the x object in parent environment
        x <<- y 
        #  assign NULL to the inv object in parent environment
        inv <<- NULL
    } 
    
    #retrieve x from makeCacheMatrix parent environment
    getInput <- function() x
    
    #set the value of the inverse and save to parent environment
    setInverse <- function(solve) inv<<- solve
    
    #find the right value for inv
    getInverse <- function() inv
    
    #define list of functions, name list elements & return to parent environment
    list(setInput=setInput, getInput=getInput, setInverse=setInverse, 
         getInverse=getInverse)
}



## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from
# the cache.

cacheSolve <- function(x, ...){
    # get inv from previous function
    inv <- x$getInverse()
    
    #evaluates whether inv retrieved above is NULL 
    #if cached value exists, then retrieve it
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    #if no cached inverse, assign matrix to variable data
    data <- x$getInput()
    
    #solve for data retrieved above and name to variable inv
    inv <- solve(data, ...)
    
    #setInverse based on previous step
    x$setInverse(inv)
    
    #return the value of inv
    inv
}


#Some test matrices recommended by Alan Berger
# https://www.coursera.org/learn/r-programming/discussions/forums/_ZerTCj2EeaZ8Apto8QB_w/threads/ePlO1eMdEeahzg7_4P4Vvg


#Test Matrix 1
#Define test matrix
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
solve(m1)
myMatrix_object <- makeCacheMatrix(m1)
myMatrix_object$getInput() # should return m1
myMatrix_object$getInverse() #should return inverse of m1 found in solve(m1)
cacheSolve(myMatrix_object) #should return same as above

#Test Matrix 2
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
n2
solve(n2)
myMatrix2_object <- makeCacheMatrix(n2)
cacheSolve(myMatrix2_object) #should return same as solve(n2)





