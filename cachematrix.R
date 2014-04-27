## This file consists of two functions dedicated to find
## the inverse of a square matrix, and to cache the result of 
## this potentially time consuming operation so that it can
## be used readily in the future

## makeCacheMatrix takes in one argument (square matrix)
## and creates a list of four functions involving that matrix

## If no arguments are passed, makeCacheMatrix just returns a
## list containing four functions for an empty 0x0 matrix

## The four functions are:
##	set - stores a matrix into myMatrix and sets the cached
##		inverse matrix to NULL. 
##	get - returns the stored matrix
##	setinverse - stores a matrix into the variable used to store
## 		the inverse matrix
##	getinverse - returns the stored inverse matrix

makeCacheMatrix <- function(myMatrix = matrix()) {
	
	cachedInverseMatrix <- NULL

	set <- function(inputMatrix) {
		myMatrix <<- inputMatrix
		cachedInverseMatrix <<- NULL
	}

	get <- function() myMatrix

	setinverse <- function(myInverseMatrix) {
		cachedInverseMatrix <<- myInverseMatrix
	}

	getinverse <- function() cachedInverseMatrix

	list (set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## cacheSolve outputs the inverse of a square matrix that has
## been processed by makeCacheMatrix. If the inverse has never
## been calculated, the function calculates the inverse and
## stores it in the list. If the inverse has been calculated
## before, the function just returns the inverse matrix
## immediately.

## cacheSolve takes at least one argument (the list of four
## functions provided by makeCacheMatrix) and any additional
## arguments for the function solve()

## (For more information about the extra arguments for the
## function solve, please type in ?solve in the main terminal)

cacheSolve <- function(myMatrixList, ...) {

	cachedInvertedMatrix <- myMatrixList$getinverse()

	## Conditional that runs if there is already a cached
	## inverse matrix
	
	if(!is.null(cachedInvertedMatrix)) {
		message("Getting cached inverse matrix")
		return(cachedInvertedMatrix)
	}

	## Calculate the inverse for the first time if the inverse
	## is NULL (no cached inverse matrix)

	originalMatrix <- myMatrixList$get()
	cachedInvertedMatrix <- solve(originalMatrix,...)
	myMatrixList$setinverse(cachedInvertedMatrix)
	
	cachedInvertedMatrix
}
