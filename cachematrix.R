#ASSINGNMENT 2

#Building the functions

makeCacheMatrix <- function(x = matrix()) { #argument is a matrix
  storageinv <- NULL #creates null object storageinv
  set <- function(y) {
    x <<- y #sets y to x
    storageinv <<- NULL #sets null to storageinv
  }
  get <- function() {x} #get the value of the matrix
  setInv <- function(inverse) {storageinv <<- inverse} #sets the value of inverse
  getInv <- function() {storageinv} #gets the value of inverse
  list(set = set,get = get,setInv = setInv,getInv = getInv)
  #creates a list with the 4 functions we just built
}
  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv() #gets the inverse of x and assigns to inv
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  } #checks if the inverse has already been calculated
  mat <- x$get() #sets the function get using x as argument to mat
  inv <- solve(mat, ...) #solves x, therefore gets its inverse e assigns to inv
  x$setInv(inv)#uses the function setInv to storage inv to storgeinv
  inv #print inv
}

#Testing the functions, just run the lines one by one

#test 1
a<-makeCacheMatrix(matrix(c(1,2,3,0,1,4,0,0,1),nrow = 3,ncol = 3,byrow = TRUE))
a$get()
a$getInv()
cacheSolve(a)
cacheSolve(a)
a$getInv()

#test 2
b<-makeCacheMatrix(matrix(c(2, 1, 5, 3), nrow = 2, ncol = 2, byrow = TRUE))
b$get()
b$getInv()
cacheSolve(b)
cacheSolve(b)
b$getInv()
