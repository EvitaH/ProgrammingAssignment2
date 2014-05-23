
## These functions read in a matrix and calculate its inversion, but not only returns the inversion but also store it in the cache, 
##which can then be called and computing time for re-calculating inversion of matrix eliminated.

##An example of how to run the code:
##z <- makeCacheMatrix()
##This only reads in an empty matrix (since none given), sets the cache (i) to null. 
##z$set(matrix(1:4, 2, 2))    
##This is the input of the matrix z; the function set sets the matrix as x into the makeCacheMatrix and i as null 
##(input needs to be an inversible matrix!)
##z$get()
##this simply returns the matrix - not necessary to run this, but can be used as test to see that all read in correctly

##cacheSolve(z) 
##This first runs getsolve, which returns the contents of the cache; this is written into a local variable (i). 
##If this is not null, it has already been assigned in a previous run - setsolve - and will be returned here
##If getsolve is zero, the if clause is not true, and the last part of the second function occurs, 
##calculating the inverse matrix into the cache. Re-running cacheSolve pulls the inverse from the cache since it is now set.

makeCacheMatrix <- function(x = matrix()) {     #start a function with x as argument; has to be matrix
    i <- NULL    								#set i to be NULL - this only works within the function; this happens in the first round: 
    											#the cache will be set clear
    set <- function(y) {						#make a new function "set", which takes y as input
        x <<- y									#assign the value y to x in parent environment
        i <<- NULL								#i set as NULL in parent environment: this initiates an empty cache in the second run 
        										#of the scripts (the first time, when running once without giving it an x, nothing happens 
        										#within the functions but they will be set)
        										#as soon as cacheSolve runs, it feeds the chache as well, which will be used in the next run
    }
    get <- function() x							#running the function returns x (this grabs the matrix from the "set" input) 
    setsolve <- function(solve) {				#Assigns "solve" to variable i - the <<- points it to the parent environment
    i <<- solve	
    }
    getsolve <- function() i					#calling getsolve() returns i 
    list(set = set, get = get,					#make a list of the functions above and set the names - these functions can be called by name    
    setsolve = setsolve,
    getsolve = getsolve)
}


## This is the second half of the Matrix inversion. After setting the variables for the input matrix in makeCacheMatrix, 
##this function checks the cache for the inverse matrix; if the cache is empty the inverse of the matrix is calculated fresh; 
##if the cache is not empty, it retrieves it from the cache, indicated by message in Stdout.

cacheSolve <- function(x, ...) {    		#Make a function that takes x as an argument, and undefined number of other arguments (…)
    i <- x$getsolve()    					#assign value to i: use the getsolve function (which returns i from the cache) 
    										#- if this has been run before, this should still be in cache (i.e. not null), 
    										#if the cache is empty then i is null
    if(!is.null(i)) {						# if the solve (i) in the cache is not null, print message that this is from cache, 
    										#and return the inverse matrix
        message("getting cached data")		#notify user that this number actually comes from the cache 
        return(i)							
    }	
    data <- x$get()							#"else-part": if i is null, assign x to "data"
    i <- solve(data, ...)					#assign to i - here, the inverse matrix is actually being calculated
    x$setsolve(i)							#call setsolve: this is a function calling solve as function; argument = i 
    										#which is the inverse of the matrix; this function sets it into the cache
    i										#give i - gives the calculated inverse matrix
}

