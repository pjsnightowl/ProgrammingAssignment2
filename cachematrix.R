## The two functions implement a cached matrix inverse.
## On the first call to cacheSolve a given matrix is inverted by the
## R solve function. On subsequent calls the inverse is returned from
## a cache not recomputed. makeCacheMatrix makes a list of functions
## used in the cache including: to hold the inverse when first 
## calculated, "setinv" and "getinv" to retrieve the inverse during 
## subsequent calls.


##makeCacheMatrix makes the functions required for cache inverse.
makeCacheMatrix <- function(x = matrix()) {
 ##makeCacheMatrix makes the functions required for cache inverse
 ##
 ##input: matrix to be inverted
 ##
 ##output: list of four functions for handeling the input matrix
 ##        y placed in matrix x and the calculated inverse placed in m
 ##        function setinv places the calculated inverse in m for 
 ##        subsequent cache operation. getinv enables returns of 
 ##        calculated inverse in m from cache

        ##set inverse matrix to null
        m <- NULL 

        ##set function sets x to matrix to be inverted y and
        ## and inverse m to null
        set <- function(y) {
                x <<- y      ##set matrix x to input matrix y
                m <<- NULL   ##set inverse matrix m to null
        }

        ##get function returns the matrix to be inverted
        ##placed in x by set function
        get <- function() x

        ##setinv sets the computed inverse in matrix m
        setinv <- function(inv) m <<- inv
        
        ##getinv returns the computed inverse matrix placed 
        ## in m by setinv
        getinv <- function() m

        ## the four functions returned as a list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


 ##cacheSolve does cache inverse solution solving for inverse
 ##on first call and returning cached inverse matrix on subsequent calls
cacheSolve <- function(x, ...) {
 ##cacheSolve does cache inverse solution solving for inverse
 ##on first call and returning cached inverse matrix on subsequent calls
 ##
 ##input: the list of four cache functions made by makeCacheMatrix
 ##
 ##output: the inverse matrix either by calculation on first call
 ##        or from cache on subsequent calls

        ##get the inverse matrix using list x function getinv
        m <- x$getinv()

        ## If !is.null(m)) == TRUE then the inverse matrix
        ## has already been stored in m and can be returned
        ## without recalculating. This is the cache operation
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m) ##returns inverse matrix in cache
                          ##that was retrieved by the getinv()
        }

        ## If !is.null(m)) == FALSE then m = NULL and this is the
        ## first call to find the inverse matrix m and it must
        ##be calculated using the R function solve. Call this
        ## a "first time" call. In this case the following lines apply:

            ##gets the matrix called x in the makeCacheMatrix
            ##functions that is to be inverted
            data <- x$get() 

            ##R solve function inverts the matrix data and stores
            ##inverse in matrix m
            m <- solve(data, ...)

            ##Set the computed inverse m in the cache so on subsequent 
            ##calls to cacheSolve is will be returned directly by the
            ##"getting cached inverse" 
            x$setinv(m)

            ##return the inverse matrix m that has been calculated
            ##for the first time
            m
}
