## makeCacheMatrix 
## this function creates a special matrix, which is an object that once its inverse can be computed by cacheSolve, it can cache it
## with the help of the scoping rules in R, it can cache its inverse that it is set to it by cacheSolve


makeCacheMatrix <- function(x = matrix()) {
		invMat <- NULL
		set <- function(y) {
			x <<- y 
			invMat <<- NULL 
		}
		get <- function() x 
		setInv <- function(inverseMat) invMat <<- inverseMat 
		getInv <- function () invMat
		list(set = set, get = get, setInv = setInv, getInv = getInv)
		
}

## cacheSolve
## cacheSolve is a function, that computes the inverse of the special matrix, that is found in makeCacheMatrix 
## if the inverse has been computed, then it gets the inverse from the cache, without re-computing

cacheSolve <- function(x, ...) { 
	invMat <- x$getInv() #get the inverse of the matrix by looking into the list x gives out
	if(!is.null(invMat)){
		message("getting cached inverse")
		return(invMat) 
	}
	# in the following, if the invMat is NULL, it will be computed 
	data <- x$get() #if there is nothing there, it gets the matrix
	invMat <- solve(data, ...) 
	x$setInv(invMat) #sets the invMat to the object x as to get cached
	invMat 
}
