## Returns a vector of integer indices that are a randomly selected
## subset of rows from orig input dataframe.
##
## orig - original (complete) dataset (dataframe), assumed that samples
##        are stored in rows of a 2-D table
## fraction - the fraction of the original dataset used for training
##            (e.g. 0.8 is 80%)
## seed - integer, seed for pseudo-random number generator
trainSetRows <- function(orig, fraction, seed=711) {
    set.seed(seed)
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

## Groups df by df_sum_over_col, sums over it and adds
## new rows with f_name = 'all' and df_sum_over_col as the sum
## over the group.  TODO make the f_name a variable
addAllRows <- function(df, df_groupby_col=date, df_sum_over_col=x,
                       df_all_label_col=f_name, all_label = 'all') {
    # do our own quoting of the first two inputs so we can tell
    quo_df_groupby_col <- enquo(df_groupby_col)
    df_sum_over_col <- enquo(df_sum_over_col)
    # If df_all_label_col were a sting, we wouldn't need the next line.
    # Since we are passing parameters as we do dplyr functions, we need
    # the next line to 
    df_all_label_col <- quo_name(enquo(df_all_label_col))
    # build the records for the sums of df_groupby_col item
    ret_df <- df %>% group_by(!!quo_df_groupby_col) %>%
              summarize(!!quo_name(df_sum_over_col) := sum(!!df_sum_over_col)) %>% 
              mutate(!!df_all_label_col := all_label)
    # we now have all our, now just append them to the df and return
    ret_df <- bind_rows(df, ret_df)
    
    return(ret_df)
}

## test data
getTestData <- function() {
    d <- seq(as.Date('2017/01/01'), as.Date('2017/01/08'), "days")
    first_name <- rep("Jane", 8)
    first_name <- append(first_name, rep("Fred", 8))
    first_name <- append(first_name, rep("Sally", 8))
    dat <- data.frame(date=d, f_name=first_name, x=seq(1, 3*8, 1),
                      stringsAsFactors = FALSE)
    return(dat)
}









