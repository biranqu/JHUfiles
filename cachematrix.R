## Put comments here that give an overall description of what your
## functions do
#I always open my files with these packages, as they are very useful
install.packages('dplyr')
install.packages('tidyverse')
## Write a short comment describing this function
#In order to complete the assignment, I used the given makeVector and cachemean code that was given and applied it in order to solve matrices.
#I made some slight adjustments to the code in order to do this to solve it.
#This exercise seemed really easy, so please let me know if I missed anything. I explained the function of all code.
#At the bottom, I added 2 different matrices, which shows you that retrieving the cache works, and that changing the matrix required you to recalculate it.


makeCacheMatrix <- function(yourmatrix = matrix()) {
    cachedata <- NULL
    set <- function(y) {
          yourmatrix <<- y #yourmatrix is overwritten here to whatever matrix you want, if this command is used
          cachedata <<- NULL #whenever the set function is used, cachedata is set to NULL (cleared if you will)
}
    show <- function() yourmatrix #this functions calls yourmatrix
    setinverse <- function(solve) cachedata <<- solve #setinverse calculates the solve of what is in the cachedata(NULL initially)
    showinverse <- function() cachedata   #shows the result (which will be NULL). Having NULL here will cause the cacheSolve function to calculate cachedata
    list( set = set,
          show = show,
          setinverse = setinverse,
          showinverse = showinverse) # this creates a list of the functions, which allows calling with $ 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  cachedata <- x$showinverse() #the parent environment of makeCacheMatrixcontains a value for cachedata, if is was calculated before
  if(!is.null(cachedata)) {         #if the cachedata is NOT NULL, then the value(calculated in makeCacheMatrix, is returned here, with a message)
    message("getting cached data")
    return(cachedata)
  }
  data <- x$show() #calling data will call your matrix, using makeCacheMatrix to define yourmatrix. This part is only accessed when your cache = NULL
  cachedata <- solve(data) #calling cachedata will now inverse your matrix. Solve() is only done here. now cachedata != NULL, meaning that next time, the cache will be solved
  x$setinverse(cachedata) #here, cachedata is called and the matrix is solved
  cachedata #now, even if the original matrix was not stored, the 
        ## Return a matrix that is the inverse of 'x'
}
#the function of cacheSolve is to make the process of solving quicker. If the solve has already been done, than it does not need to be calculated again.


m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m2 <- matrix(c(1/3, -1/6, -3, 2/7), nrow = 2, ncol = 2)

inversematrix <- makeCacheMatrix(yourmatrix = m1)
inversematrix$set(m1) #instead of using the above line of code, you can simply use the set() command to change the matrix you want to invert

cacheSolve(x = inversematrix)
