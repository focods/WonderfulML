## Returns a vector of integer indices that are a randomly selected
## subset of rows from orig input dataframe.
##
## orig - original (complete) dataset (dataframe), assumed that samples
##        are stored in rows of a 2-D table
## fraction - the fraction of the original dataset used for training
##            (e.g. 0.8 is 80%)
trainSetRows <- function(orig, fraction) {
    randorder <- sample(nrow(orig))      # shuffle the deck
    nTrain <- round(nrow(orig)*fraction) # calc # of rows in training set
    trainRows <- randorder[1:nTrain]     # 1st nTrain rows in rand order
    
    return(trainRows)
}

## Returns a vector of the row numbers of test set.
##
## allRows - vector of integers that are the indices of the 
##           entire dataset (training + test)
## trainRows - vector of integers that are the indices of
##             the rows in the entire dataset allocated to
##             being the test set
testSetRows <- function(allRows, trainRows) {
    return(setdiff(allRows, trainRows))
}