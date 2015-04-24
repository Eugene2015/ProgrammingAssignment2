## makeCacheMatrix - creates "special matrix" object that can cache assigned matrix inverted value 
## cacheSolve - function that returns "special matrix" inversion.  stores once calculated inverted matrix
##              in cache and on sequntial calls returns stored /cached/ value

## Class factory pattern implementation that creates "special matrix" object  
##
## Creates object as list and adds to the list member variables and methods for their manipulation
## -- inverted - mvariable that holds inverted matrix
## -- set - method. setter for matrix. clears up inverted matrix variable
## -- get - method. getter for matrix
## -- set.inverted - method. setter for inverted variable
## -- get.inverted - method. getter for inverted variable
makeCacheMatrix <- function(direct = matrix()) {
    # object that holds inverted matrix (if calculated already)
    inverted <- NULL;
    
    # direct matrix operations :
    # -- setter
    set <- function(other){
        direct   <<- other;
        inverted <<- NULL
    }
    # -- getter
    get <- function(){
        direct;
    }
    
    # inverted matrix operations
    # -- setter
    set.inverted <- function(inverted)
    {
        inverted <<- inverted;
    }
    # -- getter    
    get.inverted <- function()
    {
        inverted;
    }

    list(set = set, get = get, set.inverted = set.inverted, get.inverted = get.inverted);    
}


## Matrix inversion function
##
## verifies if passed parameter is of "special matrix" type, after that calls object's
## inverted matrix getter. If method returns  value other than NULL it means that inversion 
## was already perfomed once. return saved value. If method returns NULL then performe inversion, 
## store it in object's internal variable for futire use and return newly calculated inverted matrix. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        
    # check parameter validity
    # -- parameter is not null
    if(is.null(x))
        stop("null reference. object is not set");
    
    # -- parameter is of special class created by makeCacheMatrix
    if((class(x) != "list") || (is.null("x$get.inverted") == T))
        stop("type mismatch. parameter is not of 'special matrix' type");
    
    # try to get cached value. 
    inverted.matrix <- x$get.inverted();
    
    
    # if cahced value exist then return it
    if(!is.null(inverted.matrix)) {
        message("getting cached data");
        return(inverted.matrix);
    }
            
    # being here means that there was no cached value : calculate and stored inverted matrix    
    message("compute inversion matrix");    
    # -- get original matrix
    matrix <- x$get();    
    # -- calculate inverted matrix
    inverted.matrix <- solve(matrix);
    
    # -- store value inside instance so next call will get it from object's cache 
    x$set.inverted(inverted.matrix);    
    
    # -- return - last evaluated object will be result of the function execution
    inverted.matrix;
}
