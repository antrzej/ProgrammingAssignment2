## "makeCacheMatrix" and "cacheSolve" are functions allowing
## to store a matrix and calculate its inverse matrix in a way
## that result will be stored for consecutive uses (won't have to
## be calculated each time.

## Defines sets of functions to store the matrix and its inverse matrix
makeCacheMatrix <- function (x = matrix()){
	i <- NULL  ## "i" stores inverse matrix

	## set matrix stored in scope of given's "makeCacheMatrix"
	## instance and nullify inverse matrix stored in "i"
	set <- function (y) {  
		x <<- y
		i <<- NULL
	}
	## return matrix stored in scope of given's "makeCacheMatrix" instance
	get <- function() x

	## set inverse matrix stored in scope of given's "makeCacheMatrix" instance
	setInverse <- function(inverse) i<<-inverse
	## return inverse matrix stored in scope of given's "makeCacheMatrix" instance
	getInverse <- function() i

	## return list of functions managing given "makeCacheMatrix" instance
	list (set=set, get=get,
		setInverse=setInverse,
		getInverse=getInverse)
}

## Calculates (only if needed) and returns inverse matrix for the matrix currently 
## stored in given's "makeCacheMatrix" instance
cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if(!is.null(i)) return (i) ## if inverse matrix already available, return it

	## otherwise, calculate inverse matrix, set it in the instance of "makeCacheMatrix"
	## and return it
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}