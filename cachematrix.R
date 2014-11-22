
## Public function 
## Creates a Matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        # i will store the inverse
        inverse_Matrix <- NULL
        # set should be used to alter the matrix
        # it invalidates the cache
        set <- function(y) {
                x <<- y
                inverse_Matrix <<- NULL
        }
        # get the matrix
        get <- function() {
                x
        }
        # sets the inv variable
        
        setinv <- function(i) {
                inverse_Matrix <<- i
        }
        # getinv gets the cached inverse
        getinv <- function() {
                inverse_Matrix
        }
        # return the special matrix
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

## Intern function
## Returns the inverse matrix
## If it was calculated, and it has not changed, return the inverse from the cache

cacheSolve <- function(x, ...) {
        # caching the inverse
        inverse_Matrix <- x$getinv()
        if(!is.null(inverse_Matrix)) {
                # if the inverse is already cached,  return it
                message("getting cached inverse")
                return(inverse_Matrix)
        }
        # if not, calculates the inverse and cache it
        matr <- x$get()
        inverse_Matrix <- solve(matr, ...)
        x$setinv(inverse_Matrix)
        return(inverse_Matrix)
}
## Uses:
# matr <- makeCacheMatrix(matrix(1:10, 2, 5))
# cacheSolve(matr)
# cacheSolve(matr) ## "getting cached inverse"
