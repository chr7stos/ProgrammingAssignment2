## makeCacheMatrix 
## function taking matrix x
## contains 4 other functions, like make makeVector
## in this case, we need the inverse of a matrix, so our functions will be:
## set, to set the matrix and get, to get the matri
## setInv to set the inverse of the matrix, getInv to get it
## the return values of these functions are finally stored in a list

makeCacheMatrix <- function(x = matrix()) {
		invMat <- NULL
		set <- function(y) {
			x <<- y # sets x to a new vector y
			invMat <<- NULL # initialises invMat to NULL
		}
		get <- function() x # returns the matrix x in input
		setInv <- function(inverseMat) invMat <- inverseMat # sets the inverse matrix
		getInv <- function () invMat
		list(set = set, get = get, setInv = setInv, getInv = getInv)
		#this special matrix, is a list  containing a function to set and get the matrix and then set and get the inverse of that matrix
}

#1 Create the makeCacheMatrix, that is a function that gets an object x that is a matrix()
#2 Initialise the invMat, that is the name of the inverse matrix that we will get afterwards
#3 Make the function set, this function takes y as an argument...

#4 Make the function get, that is a function that just returns the x matrix in the input
#5 Make setInv, that is a function that takes as argument the inverseMat and in its body, it assigns the inversMat to invMat
#6 The getInv function returns the invMat
#7 Create a list that has four values, that are equal to the function outputs



cacheSolve <- function(x, ...) {
	invMat <- x$getInv() #get the inverse of the matrix by looking into the list x gives out
	if(!is.null(invMat)){
		message("getting cached inverse")
		return(invMat) # if is not null, it will get what was calculated above
        ## Return a matrix that is the inverse of 'x'
	}
	data <- x$get() #if there is nothing there, it gets the matrix
	invMat <- solve(data, ...) #it calculates the inverse matrix and assings it to inv
	x$setInv(invMat) #sets the inv to the object x
	invMat #returns the result of the solved matrix
}

#1 After we give a matrix as input, the invMat will be NULL. In that case, the solve function will solve for the inverse of the matrix
#2 The second time we will call the function cacheSolve though, it will look in the x object to find if there is invMat in there
#3 If it finds it, it will just return it without recalculating it,




