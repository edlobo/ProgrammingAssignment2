## This function computes the inverse matrix of a square matrix as input.
## It checks for the lenght

## The function takes any vector as argumet and builds a valid square matrix
## then the result gives a list of four functions as objects to see:
## 1. The original vector made as a matrix
## 2. Change the original matrix to other value by setting a new vector
## 3. Compute the inverse matrix of the given matrix
## 4. Show the inverse matrix just computed
##

makeCacheMatrix <- function(x = numeric()) {
                inv <- NULL
                vectorlength <- length(x)
                if (vectorlength %% sqrt(vectorlength) == 0) { #this condition only evaluates square matrix
                        message("You have input a square Matrix!")
                        mymatrix <- matrix(x, nrow = sqrt(vectorlength), ncol = sqrt(vectorlength))
                        x <- mymatrix
                }else {
                        message("No Sqr matrix -> no INV")
                        x <- 0
                        inv <- NULL
                }

        
        set <- function(y) {
                vectorlength <- length(y)
                if (vectorlength %% sqrt(vectorlength) == 0) {
                        message("You have input a square Matrix:")
                        mymatrix <- matrix(y, nrow = sqrt(vectorlength), ncol = sqrt(vectorlength))
                        print(mymatrix)
                        x <<- mymatrix
                        inv <<- NULL
                        }else {
                            message("No Sqr matrix -> no INV")
                            x <<- 0
                            inv <<- NULL
                }
        }
        get <- function() x
        setinv <- function(inv)  inv <<- solve(x)
        getinv <- function() inv
        list(set = set, 
             get = get,
             getinv = getinv,
             setinv = setinv)
}

## The next function allows to cache the Inv of the Matrix just computed with the funcion above
## In case the Inv has been calculated, it gets results from cached, otherwise compute the Inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
