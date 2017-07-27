source('../../R/QdaLda.R')

# Creates and plots the 2-D data table that does work well with
# LDA, QDA,and Logistic Regression. nc=number of classes, means=list
# of nc number of 2x2 mean matrices, stds= list of nc std. deviations
# - one per class.
plot2D_Nice <- function(nc=3, seed=711) {
    set.seed(711)
    means <- list(matrix(c(2, 2, 3, 3), 2, 2, byrow=TRUE),
                  matrix(c(2.5, 5.5, 4,6), 2, 2, byrow=TRUE),
                  matrix(c(5.5, 3.5, 7,3), 2, 2, byrow=TRUE))
    std <- c(0.4, 0.4, 0.6)
    classes <- c()
    data <- NULL
    for (k in 1:nc) {
        mus <- means[[k]]
        data <- rbind(data,
                cbind(rnorm(10,mus[1,1],std[k]), rnorm(10,mus[1,2],std[k])))
        data <- rbind(data,
                cbind(rnorm(10,mus[2,1],std[k]),rnorm(10,mus[2,2],std[k])))
        classes <-c(classes, rep(k,20))
    }

    plot(data[,1],data[,2],xlim=c(0,8),ylim=c(0,8),
         col=classes,pch=paste(classes),
         main="Nice 2-D Data",
         xlab="x", ylab="y")

    return(cbind(classes, data))
}

# Creates and plots the 2-D data table that does NOT work well with
# LDA, QDA,and Logistic Regression. nc=number of classes, means=list
# of nc number of 2x2 mean matrices, stds= list of nc std. deviations
# - one per class.
plot2D_Naughty <- function(nc,ms,stds) {
    means <- list(matrix(c(2,2, 5,5), 2,2,byrow=TRUE),
                  matrix(c(3,6, 6,3), 2,2,byrow=TRUE),
                  matrix(c(4,3, 8,5), 2,2,byrow=TRUE))

    std <- c(0.5, 0.7, 0.9)
    classes <- c()
    data <- NULL
    for (k in 1:nc) {
        mus <- means[[k]]
        data <- rbind(data,
                cbind(rnorm(10,mus[1,1],std[k]), rnorm(10,mus[1,2],std[k])))
        data <- rbind(data,
                cbind(rnorm(10,mus[2,1],std[k]),rnorm(10,mus[2,2],std[k])))
        classes <-c(classes, rep(k,20))
    }

    plot(data[,1],data[,2],xlim=c(0,8),ylim=c(0,8),
         col=classes,pch=paste(classes),
         main="Naughty 2-D Data",
         xlab="x", ylab="y")

    return(cbind(classes, data))
}

# plot the 2-D QDA results
plotQda_2d <- function(niceness=0) {
    # generate the test data
    nclass <- 3   # set the number of classes
    # code used to generate 3x2 plots for LDA and QDA evaluation
    par(mfrow=c(3,2))  # adjust as plots are added
    # plot the test data - set means and std. dev's inside the plot2D_...
    # functions
    if(niceness == 0) {
        xtrain <- plot2D_Nice(nclass)
        theta <- 30
    }
    else {
        xtrain <- plot2D_Naughty(nclass)
        theta <- 110
    }
    d <- ncol(xtrain)  # number of dimensions
    pc <- priorPofC(xtrain)
    muk <- classMeans(xtrain)
    var <- classCovars(xtrain, muk)
    #print(pc)
    #print(muk)
    #print(var)
    #print(dim(xtrain))

    # generate grid sample 2-D test data
    xs <- seq(0,8,len=25)
    ys <- xs
    xtest <- expand.grid(xs,ys)
    Ntest <- nrow(xtest)

    # calc & plot P(x|C=k) vs. x
    pxc <- xtest
    #mu <- matrix(muk[1,],1,ncol(xtest),byrow=TRUE)
    #pxc <- cbind(pxc, mydnormm(xtest, mu, var[[1]]))
    colnames(pxc)[] <- c("x1", "x2")

    for(class in 1:nclass){
        mu <- matrix(muk[class,],1,ncol(xtest),byrow=TRUE)
        pcalcs <- mydnormm(xtest,mu,var[[class]])
        pxc <- cbind(pxc, pcalcs)
        colnames(pxc)[ncol(pxc)] <- paste("pxc",class)
    }

    # now do the nifty 3-D plot...
    gaussiansInRows <- matrix(c(pxc[,3],pxc[,4],pxc[,5]),
                              nrow=3,byrow=TRUE)
    maxGaussianVal <- apply(gaussiansInRows,2,max)
    whichGaussianMax <- apply(gaussiansInRows,2,which.max)

    #persp(xs,ys,matrix(maxGaussianVal,length(xs),length(ys)),
    #      col=matrix(whichGaussianMax+1,length(xs),length(ys))
    #         [1:(length(xs)-1),1:(length(ys)-1)],
    #      xlab="x",ylab="y",zlab="P(x|Ck)",
    #      theta=theta, phi=30, ticktype="detailed", cex=0.5,
    #      cex.axis=0.7)

    contour(xs,ys,matrix(maxGaussianVal,length(xs),length(ys)),
            nlevels=5,xlab="x",ylab="y",main="P(x|C=k)")

    # calc & plot P(x) vs. x
    ptable <- cbind(pxc, rep(0, Ntest))  # init P(x) in last col
    pxcol <- ncol(ptable)                # set up P(x) column of values
    colnames(ptable)[pxcol] <- "pofx"
    for(class in 1:nclass)
        ptable[,pxcol] <- ptable[,pxcol]+(pxc[,class+2]*pc[class])

    # The P(x) plot is already a single surface, so we don't need to
    # find the max values like we did for P(x|C)
    #persp(xs,ys,matrix(ptable[,pxcol],length(xs),length(ys)),
    #      xlab="x",ylab="y",zlab="P(x)",
    #      theta=45, phi=30, ticktype="detailed", cex=0.5,
    #      cex.axis=0.7)

    contour(xs,ys,matrix(ptable[,pxcol],length(xs),length(ys)),
            nlevels=5,xlab="x",ylab="y",main="P(x)")

    # calc & plot P(C|x) vs. x
    for(class in 1:nclass) {
        pcx <- ptable[,class+2] * pc[class] / ptable[,nclass+3]
        ptable <- cbind(ptable, pcx)
        colnames(ptable)[ncol(ptable)] <- paste("pcx",class)
    }
    pcx1col <- nclass + 4  #x1,x2,pxc1,pxc2,pxc3,pofx,[this column]
    pcx2col <- pcx1col + 1
    pcx3col <- pcx2col + 1
    gaussiansInRows <- matrix(c(ptable[,7], ptable[,8],
                              ptable[,9]),nrow=3,byrow=TRUE)
    maxGaussianVal <- apply(gaussiansInRows,2,max)
    whichGaussianMax <- apply(gaussiansInRows,2,which.max)
    contour(xs,ys,matrix(maxGaussianVal,length(xs),length(ys)),
           nlevels=5,xlab="x",ylab="y",main="P(C=k|x)")


    # calc & plot little delta(x) QDA disciminant vs. x
    dx <- deltQda(xtest, Ntest, nclass, pc, muk, var)
    dx <- cbind(xtest,dx)
    #dx

    gaussiansInRows <- matrix(c(dx[,3], dx[,4], dx[,5]),
                             nrow=3, byrow=TRUE)
    maxGaussianVal <- apply(gaussiansInRows, 2, max)
    whichGaussianMax <- apply(gaussiansInRows, 2, which.max)
    contour(xs, ys, matrix(maxGaussianVal, length(xs), length(ys)),
           nlevels=10, xlab="x", ylab="y", main="QDA Discriminant")

    # do the QDA classification and plot the results
    classif <- dx
    classif <- cbind(classif, rep(0,nrow(classif)))
    colnames(classif) <- c("x", "y", paste("dx", 1:nclass),"class")
    for(rw in 1:nrow(classif))
      classif[rw,ncol(classif)] <- which.max(classif[rw,3:(ncol(classif)-1)])

    classes <-c(classif[,ncol(classif)])

    plot(classif[,1],classif[,2],xlim=c(0, 8),ylim=c(0, 8),
         col=classes,pch=paste(classes),main="QDA Classification",
         xlab="x", ylab="y")
}

# plot the 2-D LDA results.  niceness=0 is for nice data
# niceness=1 (or any non-zero value) for naughty data
plotLda_2d <- function(niceness=0) {
    # generate the test data
    nclass <- 3   # set the number of classes
    # code used to generate 3x2 plots for LDA and QDA evaluation
    par(mfrow=c(3,2))  # adjust as plots are added
    # plot the test data - set means and std. dev's inside the plot2D_...
    # functions
    if(niceness == 0) {
        xtrain <- plot2D_Nice(nclass)
        theta <- 30
    }
    else {
        xtrain <- plot2D_Naughty(nclass)
        theta <- 110
   }
    d <- ncol(xtrain)  # number of dimensions
    pc <- priorPofC(xtrain)
    muk <- classMeans(xtrain)
    var <- AvgLdaCovar(xtrain)
    #print(pc)
    #print(muk)
    #print(var)
    #print(dim(xtrain))

    # generate grid sample 2-D test data
    xs <- seq(0,8,len=25)
    ys <- xs
    xtest <- expand.grid(xs,ys)
    Ntest <- nrow(xtest)
   
    # calc & plot P(x|C=k) vs. x
    pxc <- xtest
    colnames(pxc)[] <- c("x1","x2")
   
    for(class in 1:nclass){
        mu <- matrix(muk[class,],1,ncol(xtest),byrow=TRUE)
        pcalcs <- mydnormm(xtest,mu,var)
        pxc <- cbind(pxc, pcalcs)
        colnames(pxc)[ncol(pxc)] <- paste("pxc",class)
    }

    # now do the contour plot
    gaussiansInRows <- matrix(c(pxc[,3], pxc[,4], pxc[,5]),
                              nrow=3, byrow=TRUE)
    maxGaussianVal <- apply(gaussiansInRows, 2, max)
    whichGaussianMax <- apply(gaussiansInRows, 2, which.max)

    contour(xs, ys, matrix(maxGaussianVal,length(xs),length(ys)),
            nlevels=5, xlab="x", ylab="y", main="P(x|C=k)")

    # calc & plot P(x) vs. x
    ptable <- cbind(pxc, rep(0,Ntest))  # init P(x) in last col
    pxcol <- ncol(ptable)               # set up P(x) column of values
    colnames(ptable)[pxcol] <- "pofx"
    for(class in 1:nclass)
        ptable[,pxcol] <- ptable[,pxcol] + (pxc[,class+2] * pc[class])

    contour(xs,ys,matrix(ptable[,pxcol], length(xs), length(ys)),
            nlevels=5, xlab="x", ylab="y", main="P(x)")

    # calc & plot P(C|x) vs. x
    #cols in ptable: x1,x2,pxc1,pxc2,pxc3,pofx
    for(class in 1:nclass) {
        pcx <- ptable[,class+2] * pc[class] / ptable[,nclass+3]
        ptable <- cbind(ptable, pcx)  # add cols for P(C|x) for each class
        colnames(ptable)[ncol(ptable)] <- paste("pcx",class)
    }
    pcx1col <- nclass + 4  #x1,x2,pxc1,pxc2,pxc3,pofx,[this column]
    pcx2col <- pcx1col + 1
    pcx3col <- pcx2col + 1
    gaussiansInRows <- matrix(c(ptable[,7], ptable[,8],
                                ptable[,9]), nrow=3, byrow=TRUE)
    maxGaussianVal <- apply(gaussiansInRows, 2, max)
    whichGaussianMax <- apply(gaussiansInRows, 2, which.max)
    contour(xs,ys,matrix(maxGaussianVal, length(xs), length(ys)),
            nlevels=5, xlab="x", ylab="y", main="P(C=k|x)")

    # calc & plot little delta(x) 2-D LDA disciminant vs. x
    dx <- deltLda(xtest, Ntest, nclass, pc, muk, var)  # returns Nxd matrix
    dx <- cbind(xtest, dx)

    gaussiansInRows <- matrix(c(dx[,3], dx[,4], dx[,5]),
                              nrow=3, byrow=TRUE)
    maxGaussianVal <- apply(gaussiansInRows, 2, max)
    whichGaussianMax <- apply(gaussiansInRows,2,which.max)
    contour(xs, ys, matrix(maxGaussianVal,length(xs),length(ys)),
            nlevels=10, xlab="x", ylab="y", main="LDA Discriminant")

    # do the QDA classification and plot the results
    classif <- dx
    classif <- cbind(classif, rep(0,nrow(classif)))
    colnames(classif) <- c("x","y",paste("dx",1:nclass),"class")
    for(rw in 1:nrow(classif))
        classif[rw,ncol(classif)] <- which.max(classif[rw,3:(ncol(classif)-1)])

    classes <-c(classif[,ncol(classif)])

    plot(classif[,1],classif[,2],xlim=c(0,8),ylim=c(0,8),
         col=classes,pch=paste(classes),xlab="x",ylab="y",
         main="Classification of Test Data")

}
