## > source("cachematrix.R")
## > a <- makeCacheMatrix()
## > a$set(matrix(c(3,8,10,15,13,5,31,45,17), 3, 3))
## > a$get()
##      [,1] [,2] [,3]
## [1,]    3   15   31
## [2,]    8   13   45
## [3,]   10    5   17
## > cacheSolve(a)
##              [,1]        [,2]        [,3]
## [1,] -0.002096436 -0.05241090  0.14255765
## [2,]  0.164570231 -0.13574423  0.05922432
## [3,] -0.047169811  0.07075472 -0.04245283
## > cacheSolve(a)
## Getting cached data.
##              [,1]        [,2]        [,3]
## [1,] -0.002096436 -0.05241090  0.14255765
## [2,]  0.164570231 -0.13574423  0.05922432
## [3,] -0.047169811  0.07075472 -0.04245283

## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  
  # cache_data stores the cached value. It is initialised to NULL
  cache_data <- NULL
  
  # this function creates a matrix. The matrix is created in working environment.
  set <- function(y){
    x <<- y
    cache_data <<- NULL
  }
  
  # this function gets the matrix.
  get <- function() x
  
  # this function inverts given matrix and stores in array.
  set_inverse <- function(inverse) cache_data <<- inverse
  
  ## this function gets the inverted matrix from cache.
  get_inverse <- function() cache_data
  
  ## return the created functions to the working environment
  list(set = set, get = get , set_inverse = set_inverse, get_inverse = get_inverse)
  
  
}


## Write a short comment describing this function
## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cache_data <- x$get_inverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if(!is.null(cache_data)){
    message("Getting cached data.")
    
    # display matrix in console.
    return(cache_data)
  }
  
  # create matrix since it does not exist
  matrix <- x$get()
  
  # try catch is used to make sure the given matrix is invertible.
  # if not, then it will throw an error
  tryCatch( {
    # calculate inverse.
    cache_data <- solve(matrix, ...)
  },
  error = function(e) {
    message("Error:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    # set inverted matrix in cache
    x$set_inverse(cache_data)
  } )
  
  # display matrix in console.	
  return (cache_data)
  
}