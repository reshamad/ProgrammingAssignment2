## The following pair of functions work together to make repetitive time consuming 
## tasks easier and computationally better. 

## makeCacheMatrix function takes a matrix as its argument
## it then attempts to inverse it 
## and store the resultant inverse matrix in cache
## returns a list with functions as elements of the list

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

##get and set functions attempt to store the matrix in global variable
	setMat <-function (y){

		x <<- y	##assign value of y to global variable x
		m <<- NULL
	}	
	getMat <- function() x
				##returns the matrix

##get and set Inverse functions attempt to Inverse and return the new matrix
	setInverse <- function(my_inverse) {
		m<<- my_inverse	
	}			##inverse and store result in M
	getInverse <- function() m
				##returns the inverse matrix stored in M

##create the special matrix
	list(setMat=setMat, getMat=getMat, 
		setInverse= setInverse, 	
		getInverse = getInverse)

}


## Cachesolve takes the Special Matrix returned from the above function call as its argument
## internally, it calculates the inverse matrix and or/ retrieves it from the cache 
## and returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 m <- x$getInverse()	##calls the special vector's getmean()
					##stores the mean if found in variable m			
       if(!is.null(m)) {	##if value of mean is not null
                message("getting cached data")
                return(m)	##return the m value which is cached
       }
       mydata <- x$getMat()	
					##if value of mean IS null
					##then call the get of special vector to 
					##return the entire numeric vector

       m <- solve(mydata,...)	##calculate the inverse here
       x$setInverse(m)		##call setmean of special vector to 
       m				##store the mean in cache and return it

}

## TESTING data with following commands##
## source("CacheMatrix.R")
## A<- matrix (11:14,2,2) 		##creating a 2 x 2 matrix with numbers 11 through 14 
## A 						##print the matrix to see if its correctly populated
## myA <- makeCacheMatrix(A)
## myA 					##print the list to see if its correctly populated
## myA$getMat()				##should print the contents of the matrix
## myA$getInverse()			##should print NULL becuase we have NOT yet calc the inverse
## cacheSolve(myA)			##pass the "special" matrix to CacheSolve
						##should print the inverse matrix
## cacheSolve(myA)	##again	##see "getting cached data" message print
## myA$getInverse()			##should print inverse of the matrix

##testing the data again with fresh set of matrix##
## B <- matrix (c(10,9,8,7(,2,2)
## B
## myA$setMat(B)
## myA$getMat()
## myA$getInverse()
## cacheSolve(myA)
## cacheSolve(myA)
## myA$getInverse()

