makeCacheMatrix <- function(x = matrix(sample(1:100, 9), 3, 3)) {
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() {
    return(x)
  }
  
  setsolve <- function(inverse) {
    s <<- inverse
  }
  
  getsolve <- function() {
    return(s)
  }
  
  structure(list(set = set, get = get, setsolve = setsolve, getsolve = getsolve), class = "CacheMatrix")
}

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  
  if (!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  return(s)
}
