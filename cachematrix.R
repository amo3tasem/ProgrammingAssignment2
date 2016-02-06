
## this function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    #Storce cache
    c <- NULL;
    
    # Intilize matrix in the environment
    set_matrix <- function(y)
    {
        x <<- y;
        c <<- NULL;
    }
    #Get matrix value
    get_matrix <- function() x;
    
    #Invert matrix
    set_inverse <- function(i) c <<- i;
    
    #Return inverse
    get_inverse <- function() c;
    
    list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse,
         get_inverse = get_inverse);
}

## this function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    #get cached value
    c <- x$get_inverse();
    
    #if cached value exists, return it
    if(!is.null(c))
    {
        return(c);
    }
    
    # if not get matrix then get its inverse and set its value to the cache in 
    ## the first function
    m <- x$get_matrix();
    
    c <- solve(m, ...);
    
    x$set_inverse(c);
    
    return(c);
}