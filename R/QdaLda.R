
## Computes and returns the value of the quadratic discrimant function.
##
## x - N(number of samples) x d(dimensions) matrix defining the sample data.
## muk - k(number of classes) x d(dimensions) matrix holding the means
##       for each class generated from the training data.
## vark - list of matrices of length k(number of classes) each element holds a
##        covariance matrix for each class generated from the training data.
## pc - list of length k(number of classes) that holds the probablity of
##      each class.
## nsamp - number of samples in the test set being analyzed.
## nclass - number of classes the function is to discriminate against.
deltQda <- function(x, nsamp, nclass, pc, muk, vark) {
    x <- as.matrix(x)
    #print(dim(x))
    d <- ncol(x)
    nsamp <- nrow(x)
    deltx <- matrix(0, nsamp, nclass)  # initialize output
    for(k in 1:nclass) {
        invSigma <- solve(vark[[k]])  # compute inverse of covar. matrix
        for(i in 1:nsamp) {
            #print(paste(k,i))
            xs <- matrix(x[i,1:d],1,d)  # an x sample
            #print(dim(xs))
            p1 <- -log(det(vark[[k]]))/2
            #print(paste(vark[[k]],p1))
            p2 <- 0.5*(xs-muk[k,])%*%invSigma%*%t(xs-muk[k,])
            #print(paste(vark[[k]],p1,p2))
            deltx[i,k] <- p1 - p2 + log(pc[k])
        }
    }
   
   return(deltx)
}

## Computes and returns the prior probability vector P(C) as a 1 x k matrix.
## 
## xtrain must be a |n x d+1| matrix where n is the number of training data
## samples and d is the number of dimesions.  The first column of xtrain must
## be the class labels/designations which must be an integer from 1 to k, 
## where k = number of classes.  Columns 2 through d+1 define each dimensional
## coordinate of a sample vector of length d.
##
## The function returns vector/list of size k corresponding to P(C). The
## first value will be P(C=1) followed by P(C=2), etc. up to P(C=K)
priorPofC <- function(xtrain) {
    nclass <- length(unique(xtrain[,1]))  # number of classes in training set
    pofc <- c()
    for(class in 1:nclass) {
        pofc <- c(pofc, length(which(xtrain[,1]==class)) / nrow(xtrain))
    }

    return(as.matrix(pofc))
}

## Computes and returns the mean vector of each class from the training data
## xtrn as a matrix.  The returned matrix is of size k(# of classes) x
## d(dimensions).  The first row is the mean vector for class 1, the second
## row for class 2 etc...
## 
## xtrain must be a |n x d+1| matrix where n is the number of training data
## samples and d is the number of dimesions.  The first column of xtrain must
## be the class labels/designations which must be an integer from 1 to k, 
## where k = number of classes.  Columns 2 through d+1 define each dimensional
## coordinate of a sample vector of length d.
classMeans <- function(xtrn) {
   ndim <- ncol(xtrn) - 1  # number of dimensions in each data sample
   nclass <- length(unique(xtrn[,1])) # calc the number of classes
   muk <- matrix(0,nclass,ndim)  # initialize matrix of means
   for(k in 1:nclass) {
      classrows <- which(xtrn[,1]==k) # find rows for particular class
      xclass <- xtrn[classrows,1:ncol(xtrn)]
      xclass <- as.matrix(xclass[,-1])

      for(idim in 1:ndim)
         muk[k,idim] <- mean(xclass[,idim])
   }
   
   return(as.matrix(muk))
}

## Computes and returns the covariance matrix for each class.  The function 
## returns a list of length k(number of classes) where each element contains 
## a covariance matrix for class = element number.
##
## xtrain must be a |n x d+1| matrix where n is the number of training data
## samples and d is the number of dimesions.  The first column of xtrain must
## be the class labels/designations which must be an integer from 1 to k, 
## where k = number of classes.  Columns 2 through d+1 define each dimensional
## coordinate of a sample vector of length d.
##
## If xtrn is 1-dimensional, the return list will contain values representing
## the variance = (standard deviation)^2 of the data within that class.
##
## muk - k(# of classes) x d(dimensions) matrix of class means where each row
##       is a vector containing the mean of the class = row number
classCovars <- function(xtrn, muk) {
    ndim <- ncol(xtrn) - 1  # number of dimensions in each data sample
    nclass <- length(unique(xtrn[,1])) # calc the number of classes
    sigkall <- c()
    for(class in 1:nclass) {
       classrows <- which(xtrn[,1]==class)
       xclass <- xtrn[classrows, 1:ncol(xtrn)]
       xclass <- as.matrix(xclass[,-1])
       Nk <- length(classrows)  # number of samples in class
       mukfill <- matrix(muk[class,], Nk,ndim,byrow=TRUE)

       sigk <- (t(xclass- mukfill) %*% (xclass - mukfill))/(Nk - 1)
       if(class == 1)
           sigkall <- list(sigk)  # create list if first class
       else
           sigkall = c(sigkall, list(sigk))  # append to list

    }

   return(sigkall)  # return list of covar matrices or vectors of std. dev's
}

## Computes and returns the value of the linear discrimant function.
##
## Function returns a N(number of samples) x k(number of classes) matrix,
## where the first column is the delta(x) for the first class and so on
## to the last column which is delta(x) for class k.
##
## x is a N(number of samples) x d(dimensions) matrix defining the sample data.
## muk is k(number of classes) x d(dimensions) matrix holding the means
## for each class generated from the training data.
## vark is list of length k(number of classes) that holds the covariant matrix
## for each class generated from the training data.
## pc is a list of length k(number of classes) that holds the probablity of
## each class.
## nsamp is the number of samples in the test set being analyzed.
## nclass is the number of classes the function is to discriminate against.
deltLda <- function(x, nsamp, nclass, pc, muk, var) {
    x <- as.matrix(x)
    #print(dim(var))
    d <- ncol(x)
    nsamp <- nrow(x)
    deltx <- matrix(0, nsamp,nclass)  # initialize output
    for(k in 1:nclass) {
        invSigma <- solve(var)
        for(i in 1:nsamp) {
            xs <- matrix(x[i,1:d], 1, d)  # an x sample
            mu <- matrix(muk[k,], 1, d, byrow=TRUE)
            p1 <- xs %*% invSigma %*% t(mu)
            p2 <- 0.5*(mu %*% invSigma %*% t(mu))
            deltx[i,k] <- p1 - p2 + log(pc[k])
        }
    }

    return(deltx)
}