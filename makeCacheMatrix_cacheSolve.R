makeCacheMatrix<- function(x= matrix()) {
    #Set the cached inverse varible "inv" to NULL
  inv<- NULL
    # This function initilizes the "Special" Matrix
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
    # This function return the matrix itself
  get <- function()x
   # This function store the calculated inverse
  setInverse <- function(inverse) inv<<- inverse
    # This function retriev the cached inverse
  getInverse <- function () inv
    # Return this "special" matrix
  list(set=set,
       get = get,
       setInverse=setInverse,
       getInverse = getInverse)
}

cacheSolve <- function (x, ...){
    # Get the stored value from the special matrix.
  inv <- x$getInverse()
    # The value is not NULL indicating that the value have already calculated. 
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  mat<- x$get()
  inv<- solve(mat, ...)
  
  x$setInverse(inv)
}

n<- makeCacheMatrix( matrix (4:7,2,2))
cacheSolve(n)
