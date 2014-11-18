## These functions cache the matrix inversion. 




## The makeCacheMatrix function performs the following functions:
## 1) set the matrix
## 2) get the matrix
## 3) set the inverted matrix
## 4) get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<-matrix()
	
	set<-function(y){
		x<<-y
		inv<<-matrix()
	}
	
	get<-function() x
	
	setinv<-function(inverse)inv<<-inverse
	
	getinv<-function() inv
	
	list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function calculates the inverse matrix of a matrix. If the inverted matrix is
## already calculated then it is retrieved from the cache memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        
        if (!is.na(inv)){
        	message("The inverted matrix is retrieved from cache")
        	return(inv)
        }
        
        matr<-x$get()
        inv<-solve(matr)
        
        x$setinv(inv)
        
        inv
               
        
}
