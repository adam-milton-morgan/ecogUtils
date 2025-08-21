### Split mean
### Summer 2022
### adam.milton.morgan@gmail.com


## Takes a dataframe, splits it into many dataframes by value of the "word" column
## Averages columns in each word dataframe
## Puts them all back together into one dataframe with one row per word

splitmean <- function(df) {
  s <- split(df, df$word)
  s <- data.frame(t(sapply(s, function(x) colMeans(x[,(n.lx.cols + 1):ncol(x)]) )))
  s <- cbind(data.frame('word' = rownames(s)), s)
  return(s)
}
