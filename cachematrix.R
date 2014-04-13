#############################################################################
# Assignment: R Programming assigment 2                                     #
# Function  : makeCacheMatrix & cacheSolve                                  #
# Author    : lsun                                                          #
# Detail    : Caching the Inverse of a Matrix                               #
#             Matrix inversion is usually a costly computation and their    #
#             may be some benefit to caching the inverse of a matrix rather #
#             than compute it repeatedly (there are also alternatives to    #
#             matrix inversion that we will not discuss here). Your         #
#             assignment is to write a pair of functions that cache the     #
#             inverse of a matrix.                                          #
#             makeCacheMatrix: This function creates a special "matrix"     #
#                              object that can cache its inverse.           #
#             cacheSolve: This function computes the inverse of the special #
#                         "matrix" returned by makeCacheMatrix above. If    #
#                         the inverse has already been calculated (and the  #
#                         matrix has not changed), then the cachesolve      #
#                         should retrieve the inverse from the cache.       #
#############################################################################

#############################################################################
# Function : makeCacheMatrix                                                #
# Description:  This function create a list containing a function to        #
#               set the value of the matrix                                 #
#               get the value of the matrix                                 #
#               set the value of the reverse matrix                         #
#               get the value of the reverse matrix                         #
#############################################################################

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setIMatrix <- function(IMatrix) m <<- IMatrix
    getIMatrix <- function() m
    list(set = set, get = get,
         setIMatrix = setIMatrix,
         getIMatrix = getIMatrix)     
}


#############################################################################
# Function : cacheSolve                                                     #
# Description: This function computes the inverse of the special "matrix"   #
#              returned by makeCacheMatrix above. If the inverse has        #
#              already been calculated (and the matrix has not changed),    #
#              then the cachesolve should retrieve the inverse from the     #
#              cache.                                                       #
#############################################################################

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getIMatrix()
    if(!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setIMatrix(m)
    m
}
