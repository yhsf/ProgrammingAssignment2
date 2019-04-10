# These two functions cache a matrix's inverse

# This function is used to create an object, with matrix input,
# containing nested functions as named elements.
# The nested functions are for use with the cacheSolve function, and
# set or get a matrix, or set or gets the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL
    set_matrix <- function(y) {
        x <<- y
        matrix_inverse <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(inv) matrix_inverse <<- inv
    get_inverse <- function() matrix_inverse
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

# A function for use with list created by makeCacheMatrix. 
# It checks for a cached martix inverse and returns if input is identical,
# i.e. matrix_inverse has not been reset to NULL. 
# If NULL it computes/recomputes matrix inverse and caches result.

cacheSolve <- function(x, ...) {
    matrix_inverse <- x$get_inverse()
    if(!is.null(matrix_inverse)) {
        return(matrix_inverse)
    }
    new_matrix <- x$get_matrix()
    matrix_inverse <- solve(new_matrix)
    x$set_inverse(matrix_inverse)
    matrix_inverse
}
