## The two functions in this file work together to provide a special "matrix"
## object that can cache its own inverse.  This is useful because computing an
## inverse can be time-consuming for large datasets.
##
## USAGE EXAMPLE:
## Create an empty "special" matrix.
## > mx = makeCacheMatrix()
##
## Set the value of the matrix (it must be a square matrix: 
## same nbr of rows and cols).
## > mx$set(matrix(c(89, 43, 56, 2), 2, 2))
##
## Get the value of the matrix.
## > mx$get()
##     [,1] [,2]
##[1,]   89   56
##[2,]   43    2
##
## Get the value of the inverse.
## > cacheSolve(mx)
##               [,1]         [,2]
##[1,] -0.00089686099  0.025112108
##[2,]  0.01928251121 -0.039910314


## This function creates a special "matrix" object that can cache its inverse.
## Arguments: 'm' is the value of the matrix you want to create 
##             (defaults to an empty matrix). 
## Returns: a list of 4 functions which can be used get/set the
##          value of the matrix and its inverse.
makeCacheMatrix <- function(m = matrix()) {
    ## This variable will store the inverse of matrix 'm'.
    inv <- NULL

    ## Since the 4 functions below are defined INSIDE makeCacheMatrix,
    ## the values of 'm' and 'inv' here are stored in their environment.
    ## These 4 functions share an envir (i.e. what exists inside
    ## makeCacheMatrix).

    ## This function sets the value of matrix 'm' 
    ## and resets its computed inverse 'inv' to NULL.
    set <- function(m_new_val) {
        ## Use the <<- operator to set the value of 'm' 
        ## in the PARENT environment (inside makeCacheMatrix).
        m <<- m_new_val
        inv <<- NULL
    }

    ## This function returns the matrix 'm'.
    get <- function() {
        ## 'm' is a free variable here; its value is stored in the parent envir 
        ## (in makeCacheMatrix).
        m
    }

    ## This function sets (it does not compute) the value of inv 
    ## (the inverse of matrix 'm').
    ## It should generally only be called by the cacheSolve() function below,
    ## which controls the caching and computation of inv.
    setinverse <- function(inv_new_val) {
        ## The <<- operator allows the function to set 'inv' inside the parent
        ## envir (in makeCacheMatrix).
        inv <<- inv_new_val
    }

    ## This function returns the value of inv (the inverse of matrix x). 
    ## inv is a free var here; its value is stored in the parent envir (in
    ## makeCacheMatrix). This function should generally only be called by the
    ## cacheSolve() function below, which controls the caching and computation
    ## of inv. The user should instead call cacheSolve() to get this value.
    getinverse <- function() inv

    ## Return a list containing the 4 functions created above.
    ## Their environment stores the values of x and inv.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" created by
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed since then), then the function should retrieve the
# inverse from the cache and return it. Otherwise, the function should compute
# the inverse, store it in the cache, and return it.
# 
# Arguments: 'z' is a special object (a list of functions) created by the
#                makeCacheMatrix function. 
#            '...' are optional additional arguments that will be
#                  passed along to the solve() function when computing the 
#                  inverse. 
# Returns a matrix that is the inverse of the matrix stored in 'z'.
cacheSolve <- function(z, ...) {
    ## Call the function stored in 'z' to get the cached value of the inverse.
    inverse <- z$getinverse()

    ## If the inverse has already been calculated (not NULL), just return it.
    if(!is.null(inverse)) {
        message("getting cached data")
        
        # Return the cached inverse
        return(inverse)
    }

    ## We need to calculate the inverse.
    ## Call the function stored in 'z' to get the value of the matrix.
    mat <- z$get()

    ## Call the built-in solve() function to compute the inverse, sending along 
    ## any extra args. This is the potentially slow step we are trying to avoid 
    ## calling unnecessarily.
    inverse <- solve(mat, ...)

    ## Call the function stored in 'z' to cache the computed inverse.
    z$setinverse(inverse)

    ## Return the computed inverse matrix.
    inverse
}
