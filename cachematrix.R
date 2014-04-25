## This Script for two functions to cache the inverse of a matrix  
## and retrive the inversed matrix if it has already been calculated 
## instade of do the calculation again, to save resourses in case of large set
## the inverse operation use the base package's function solve.

## This first function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
	m <- NULL
	#The Set function to store the variables in the cache using  <<- operator
	set <- function(y) 
	{
		x <<- y
		m <<- NULL
	}
	#get function to retreave the original variable from cache 
	get <- function() x

	#setsolve function to retreave the inversed variable from cache
	setsolve <- function(solve) m <<- solve

	#getsolve function to retreave the inversed variable from cache
	getsolve <- function() m
	list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)
}



## The following function computes the inverse of matrix returned by makeCacheMatrix function, 
## It retrieve the inverse from the cache If the inverse has already been calculated
cacheSolve <- function(x, ...) 
	{
	#get cached variables and check if the inverse of x is already cached 
	m <- x$getsolve() 
	if(!is.null(m))
	{
		message("getting cached data")
		return(m)
		#If Cached inverse found it would be returned
	}
	#If there is not Cached inverse for x found, then do the colculations again
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	return(m) 
	## 'm' is the matrix inverse for 'x'
}
