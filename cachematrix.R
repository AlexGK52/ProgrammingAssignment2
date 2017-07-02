## create structure (list) of 4 functions
makeCacheMatrix <- function(x = matrix()) {
    # there are "field" x by default
    invMat <- NULL
    set <- function(y) {
        # rewrite "fields" x  and invMat
        x <<- y
        invMat <<- NULL
    }
    # function get
    get <- function() x
    # function setInvMatrix(invMatrix)
    setInvMatrix <- function(invMatrix) invMat <<- invMatrix
    # function getInvMatrix()
    getInvMatrix <- function() invMat
    # return list -- names of the return "fields" coinside with with functon names
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    # check that x$get() is matrix and square matrix
    if(is.matrix(x$get()) && nrow(x$get()) == ncol(x$get()) ){
        # check what is inverse Matrix of x
        invMat <- x$getInvMatrix()
        if(!is.null(invMat)) {
        message("getting cached inverse Matrix")
        return(invMat)
        }
        data <- x$get()
        # check that there is inverse matrix
        if(det(data) == 0){
            message("There is no inverse matrix, determinant is zero")
        } else{
            invMat <- solve(data, ...)
            x$setInvMatrix(invMat)
            return(invMat)
        }
    } else{
        message("x is not matrix or is not square matrix")
    }
}

# Please don't send me other student works -- I am tired to check students works
# and I will not to review them 
