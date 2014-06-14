## These functions provide an environment for a matrix 
# and its pre-computed inverse

## Create a matrix object that has a special environment containing its
## pre-calculated inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    set <- function(mat) {     ## set new or initial value of matrix
        x <<- mat
        inverse <<- NULL
    }
    get <- function() x   ##return matrix

    setinverse <- function(invmat) {
        inverse <<- invmat    ##cache the inverse provided as an argument
    }
    getinverse <- function() inverse
    list(set=set, get=get,
         setinverse=setinverse, getinverse=getinverse) 
                    ##vector of methods that 
                    ##operate on 'inverse' and 'x'
                    ## in this environment
}


## If the specified object does not already have a pre-computed inverse,
## this function computes, caches and returns this inverse
## If already computed, it simply returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv = x$getinverse()
    if (!is.null(xinv)) {
        message ("Getting cached inverse")
        return (xinv)
    }
    mat = x$get()
    xinv = solve(mat, ...)  ## calculate inverse of mat
    x$setinverse(xinv)   ## cache this inverse for next time
    xinv   ##return this inverse
}
