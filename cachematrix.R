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

## Tests:
## mat <- matrix(c(7,4,5,3), 2, 2)
## mat
#     [,1] [,2]
#[1,]    7    5
#[2,]    4    3
## cacheMat <- makeCacheMatrix(mat)
## cacheMat$get()
#     [,1] [,2]
#[1,]    7    5
#[2,]    4    3
## cacheMat$getInvMatrix()
## NULL
## invMat <- cacheSolve(cacheMat)
## invMat
#     [,1] [,2]
#[1,]    3   -5
#[2,]   -4    7
## round(invMat %*% mat)
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
## mat <- c(7,4,5,3)
## > mat
## [1] 7 4 5 3
## > cacheMat <- makeCacheMatrix(mat)
## > invMat <- cacheSolve(cacheMat)
## x is not matrix or is not square matrix
## > mat <- matrix(c(1,2,3,4,5,6,7,8,9), 3, 3)
## > cacheMat <- makeCacheMatrix(mat)
## > invMat <- cacheSolve(cacheMat)
## There is no inverse matrix, determinant is zero
## > mat <- matrix(c(1,2,3,4,5,6), 2, 3)
## > cacheMat <- makeCacheMatrix(mat)
## > invMat <- cacheSolve(cacheMat)
## x is not matrix or is not square matrix
## > mat <- matrix(c(7,4,5,3), 2, 2)
## > cacheMat <- makeCacheMatrix(mat)
## > invMat <- cacheSolve(cacheMat)
## > invMat
##      [,1] [,2]
## [1,]    3   -5
## [2,]   -4    7
## > invMat <- cacheSolve(cacheMat)
## getting cached inverse Matrix
## > invMat <- cacheSolve(cacheMat)
## getting cached inverse Matrix
## > cacheMat$set(matrix(c(7,4,5,3),2,2))
## > invMat <- cacheSolve(cacheMat)
## > invMat
##      [,1] [,2]
## [1,]    3   -5
## [2,]   -4    7

