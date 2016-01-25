## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix () creates a special matrix object that can cache its inverse   

makeCacheMatrix <- function(x = matrix()) {
## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        
        inv = NULL
        set = function(y) {
                # `<<-` used to assign a value to an object in an environment different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve() omputes the inverse of the matrix returned by makeCacheMatrix().If the inverse is already present and the matrix ahas not undergone any change, this function will retreive the inverse from the cache directly

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        
        # if the inverse has been calculated before, the following code will get the cached data
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # if not this code calcultes the inverse again usign the solve ()
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # settign the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
## now lets test the functions, the following code tests the above fucntions
test = function(mat){
        ## mat is an invertible matrix
        
        temp = makeCacheMatrix(mat)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
}
set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)
## the output got is as follows
## time difference of 1.614092 secs
## getting cached data time difference of 0 secs.
## clearly there is lot of time saving because of the cache functions
