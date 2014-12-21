# Requirement for ginv generalized inverse used later
library('MASS')

cacheMatrix <- function(x =NULL){
    # [Matrix] => cacheMatrix    //EBNF signature
    # This function takes a matrix and returns a cacheMatrix
    # object. It acts as a closure to store matrix values.
    # Note that the requirements given in the assignment are not
    # necessary since it is possible to have a single object to
    # cache and store the matrix.
    
    #This sets up the outer environment variables. Notice that x is optional
    #I've also used .'s to identify the actual variables between
    #the function getters and setters for the variables.
    .matrix <- x
    .inverse <- NULL
    
    matrix <- function(y){
        # []
        
        if (!missing(y)){
            #This is the case where we're given the matrix to set
            .matrix <<- y 
            
            return(exports)     
            #This is cool for method chaining, if you're into it.
        } 
        
        #If we weren't given a matrix to set, the user wants the matrix
        #back, so we return that.
        return(.matrix)
    }
    
    inverse <- function(){
        if(is.null(.inverse)){
            #This is the case where the inverse wasn't computed yet, so
            # we compute it
            .inverse <- ginv(x)
            #and return it
            return(.inverse)
        }
        
        #Out here, .inverse wasn't null, so we return it's value since it's
        #already been cached.
        return(.inverse)
    }
    
    exports <- list(matrix=matrix, inverse=inverse)
    
    return(exports)
    
    
}
