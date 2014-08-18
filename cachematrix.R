#Assignment 2
#15 to 18 Aug 2014


## The following two functions work together to cache and retreive the inverse of a matrix.
## This is useful because, due to lexical scoping, all objects must stored in memory in R, making space in memory a limiting factor. 
## Matrix inversion is usually a (memory) costly computation, thus it is useful to cache the inverse of a matrix, rather than re-computing if it will be needed frequently.

# Refresher on matrices and their inverses http://www.mathsisfun.com/algebra/matrix-inverse.html
# Useful blog for understanding concepts http://adv-r.had.co.nz/Functions.html#lexical-scoping
# and http://tjadclark.com/blog/article/18-caching-and-lexical-scopes

# Reset R
rm(list=ls()) # ls() lists everything in R's brain and rm() removes all the objects in the list.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing functions to
#1 set the value of the matrix
#2 get the value of the matrix
#3 set the value of the inverse
#4 get the value of the inverse
# In other words the function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # # sets the value of m (the matrix inverse if used cacheSolve) to NULL
    setmatrix <- function(y) { #set the value of the matrix
        x <<- y
        m <<- NULL ## in the event that the matrix has changed, this changes the value of inverse of the matrix back to NULL
    }
    getmatrix <- function() x #get the value of the matrix
    setinverse <- function(solve) m <<- solve # compute the value of the inverse 
    getinverse <- function() m # get the value of the inverse
    list(setmatrix = setmatrix, getmatrix = getmatrix, # creates a list to house the four functions
         setinverse = setinverse,
         getinverse = getinverse)
}


########################

## The second function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix (above). 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
## If the input is new, it calculates the inverse of the data and sets the inverse in the cache via the setmatrix function.

cacheSolve <- function (x=matrix(), ...) {
    m <- x$getinverse() # runs the getinverse function to get the value of the cached inverse matrix
    if(!is.null(m)) { # if find something there (i.e. is not null) then sends a text message and returns the cached matrix
      message("getting cached data")
      return(m)
    }
    data <- x$getmatrix() # otherwise runs the get function to get the value of the input matrix
    m <- solve(data, ...) # compute the value of the inverse of the input matrix
    x$setinverse(m) # runs the setinverse function on the inverse to cache it
    m # returns the inverse
  }


#Test it out

mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
mat2 <- makeCacheMatrix(mat)
cacheSolve(mat2)
#Returns the inverse as expected

#If run again
cacheSolve(mat2)
#Returns the inverse after giving the "getting cached data" message


#Now try a different matrix with the same name
mat <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
mat2 <- makeCacheMatrix(mat)
cacheSolve(mat2)
#Returns the inverse of the new matrix as expected

####################################################################################

# My code modified from this code 

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# Helpful Note from Tim LeSaulnier on forum : The last thing evaluated is the list.  
# You are naming attributes of the list with what is on the left side of the equal signs, and assigning to those attributes  the objects which are on the right side of the equal signs.  
# When you execute "A = makeVector(B)", "A" is a list.  "Subsetting" A returns the corresponding values of the list which are functions defined inside the definition of makeVector
# You can then use these functions to retrieve or set B, or retrieve or set the mean of B inside the cachemean function.


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

# Testing the original vector and mean example

vec <- makeVector(c(1, 2, 3))
cachemean(vec)

#This won't work because makeVec returns a list
vec <- c(1, 2, 3)
vec <- makeVector(vec)
cachemean(vec)
class(vec) # The class of vec is a list not a numeric vector

#This will work
vec <- c(1, 2, 3)
vec2 <- makeVector(vec)
cachemean(vec2)
