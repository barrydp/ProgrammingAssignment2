## [Put comments here that describe what your functions do]
## makeCacheMatrix <- function(x = matrix())  contains the routines necessary to make cacheSolve work
## cacheSolve <- function(x, ...)  contains the main function to get an inverse of a matrix.  Check to see if it
##   is in the cache; if it is return that.  If it isn't, create the inverse, put it in the cache, and return it.

makeCacheMatrix <- function(x = matrix()) 
{
	##  First make sure the cache is empty by setting it to NULL
	CachedInverseMatrix <- NULL
	
	## Now start the functions needed.
	##  Set sets x in the parent directory
    set <- function(y) 
	{
        x <<- y
        CachedInverseMatrix <<- NULL
    }
	
	# Get returns the matrix in x in parent directory
    get <- function() x
	
	## Set the cache with the given matrix
    setmatrix <- function(matrix) CachedInverseMatrix <<- matrix
	
	## get the cached matrix and return it to calling function
    getmatrix <- function() CachedInverseMatrix
	
    list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
	
	## First check to see if the inverse already exists in the cache;
	## if it is, return it and quit further execution
	CachedInverseMatrix <- x$getmatrix()
    if(!is.null(CachedInverseMatrix)) 
	{
        message("getting cached data")
        return(CachedInverseMatrix)
    }
	
	## If the data is not in the cache, compute it
    data <- x$get()
    CachedInverseMatrix <- solve(data, ...)
	
	## Once computed, put it in the cache
    x$setmatrix(CachedInverseMatrix)
	
	## Then return the inverse matrix
    CachedInverseMatrix
}
