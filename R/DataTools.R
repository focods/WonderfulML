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

## Groups df by df_group_by_col_name, sums over it and adds
## new rows with f_name = 'all' and df_group_by_col_name as the sum
## over the group.  TODO make the f_name a variable
appendAllCol <- function(df, df_group_by_col_name, all_name = 'all') {
    df_group_by_col_name <- enquo(df_group_by_col_name)
    
    ret_df <- df %>% group_by(date) %>%
              summarize(!!quo_name(df_group_by_col_name) :=
                            sum(!!df_group_by_col_name)) %>% 
              mutate(f_name = all_name)
    # ret_df <- ret_df[c(1,3,2)]  # no need to realign col's
    
    ret_df <- bind_rows(df, ret_df)
    
    return(ret_df)
}

## test data
d <- seq(as.Date('2017/01/01'), as.Date('2017/01/08'), "days")
first_name <- rep("Jane", 8)
first_name <- append(first_name, rep("Fred", 8))
first_name <- append(first_name, rep("Sally", 8))
dat <- data.frame(date=d, f_name=first_name, x=seq(1, 3*8, 1),
                  stringsAsFactors = FALSE)

# d2 <- group_by_(dat, 'date') %>% summarize_(daily_total = sum_('x'))
d3 <- mutate(df1, a = 3)

mutate_y <- function(df) {
    mutate(df, y = .data$a + .data$x)
}

df <- tibble(
    g1 = c(1, 1, 2, 2, 2),
    g2 = c(1, 2, 1, 2, 1),
    a = sample(5), 
    b = sample(5)
)






