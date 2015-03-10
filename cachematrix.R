## makeCacheMatrix function lets us handle matrix objects so that they can cache their inverses
## cacheSolve uses CacheMatrix objects created by makeCacheMatrix so that we only count the inverse 
## of a matrix if the matrix data has changed or the inverse hasn't been calculated yet

## This function creates a "matrix" object that can cache it's inverse for future usage
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	# When matrix data changes store it, but set inverse to NULL since we can't know if it's still correct one
	set <- function(y) {
			x <<- y
			inverse <<- NULL
	}

	# Get the matrix data
	get <- function() x

	# Cache the inverse data
	setInverse <- function(newInverse) inverse <<- newInverse

	# Get the inverse data
	getInverse <- function() inverse

	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)

}


## This function calculates (and stores) the inverse 
## of a CacheMatrix object x.
## Firstly it checks if the inverse has already been
## been previously calculated and returns that if true.
## If the CacheMatrix matrix has changed or the inverse
## hasn't been calculated yet the function calculates the inverse
## and stores it in the CacheMatrix object.
cacheSolve <- function(x, ...) {

	# Get the inverse from the CacheMatrix object 'x'
	m <- x$getInverse()

	# If the inverse isn't NULL return it as the inverse
	if(!is.null(m)) {
			message("getting cached data")
			return(m)
	}
	
	# Since an inverse hasn't been calculated yet, get the actual matrix data
	data <- x$get()
	# Count the inverse for the matrix with solve
	m <- solve(data, ...)

	# Store the inverse so we can use it later on
	x$setInverse(m)

	# Return the calculated inverse (works also with just m but I prefer return statements)
	return(m)

}
