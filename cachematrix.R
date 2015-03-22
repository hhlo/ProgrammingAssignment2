makeCacheMatrix <- function(x=matrix()){
invr<- NULL
set<- function(y){
x<<-y
invr<<-NULL
}

get<-function()x
setinverse<- function(inverse) invr<<-inverse
getinverse<- function()invr

list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve<- function(x,...){
invr<- x$getinverse()
if(!is.null(invr)){
message("getting cached data")
return(invr)

}

data<- x$get()
invr<-solve(data,...)
x$setinverse(invr)
return(invr)
}

sample<-matrix(runif(4,25,50),2,2)
sampleCached <- makeCacheMatrix(test)
sample

sampleinvr <- cacheSolve(sampleCached)
sampleinvr
