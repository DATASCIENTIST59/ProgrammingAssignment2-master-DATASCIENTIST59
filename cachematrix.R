## This funtion can cache the inverse of a matrix

## makeCacheMatrix can cached a matrix


makeCacheMatrix <- function(Z = matrix()) {{
        J <- NULL
        
        set <- function(y) {
                Z <<- y
                J <<- NULL
        }
        
        get <- function() Z
        setinverseofmatrix <- function(inverse) J <<- inverse
        getinverseofmatrix <- function() J
        list(set = set,
             get = get,
             getinverseofmatrix = getinverseofmatrix,
             setinverseofmatrix = setinverseofmatrix)
}}



## cacheSolve can solve the matrix that was previously cached returning the inverse value of Z

cacheSolve <- function(Z, ...) {{
        J <- Z$getinverseofmatrix()
        if (!is.null(J)) {
                message("getting cached data")
                return(J)
        }
        data <- Z$get()
        J <- solve(data, ...)
        Z$setinverseofmatrix(J)
        J
}}

                                        ##Solution

##zfirst <- matrix(c(5, 7, 13, 4, 1, 7, 14, 3, 11), 3, 3) ## Here I am defining the matrix value for testing
##zfirst
        ##[,1] [,2] [,3]
        ##[1,]    5    4   14
        ##[2,]    7    1    3
        ##[3,]   13    7   11

##zsecond <- makeCacheMatrix(zfirst) ## I will use the funtion that was create before "makecacheMatrix" to store the matrix in order to solved
##> zsecond <- makeCacheMatrix(zfirst)
##> zsecond

        ##$set
        ##function (y) 
       ## {
        ##        Z <<- y
        ##        J <<- NULL
       ## }
        ##<environment: 0x0000000014e796a0>
        
        ##        $get
        ##function () 
        ##Z
        ##<environment: 0x0000000014e796a0>
        
        ##        $getinverseofmatrix
        ##function () 
        ##        J
        ##<environment: 0x0000000014e796a0>
        
         ##       $setinverseofmatrix
        ##function (inverse) 
        ##        J <<- inverse
        ##<environment: 0x0000000014e796a0>
        

##cacheSolve(zsecond) ## After used this function we will get the inverse of the matrix returned from cache
        ##          [,1]        [,2]         [,3]
         ##      [1,] -0.03311258  0.17880795 -0.006622517
         ##     [2,] -0.12582781 -0.42052980  0.274834437
          ##    [3,]  0.11920530  0.05629139 -0.076158940
      

                                                  ## Here we got
