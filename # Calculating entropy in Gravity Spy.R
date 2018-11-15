# Calculating entropy in Gravity Spy
# Corey Jackson

## An Example 
x = c(0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 0) # Entropy = 0 
y = c(1, 1, 3, 1, 0, 0, 0, 0, 1, 2, 0) # Entropy = 1.676988
z = c(1, 1, 3, 1, 0, 0, 20, 10, 1, 2, 0) # Entropy = 1.416828  (The above is has less entropy because efforts clustered in two bins)

# Large values of entropy mean the more "diverse" the dataset is. 

# Entropy can be measured using 
# https://cran.r-project.org/web/packages/entropy/entropy.pdf 
# http://www.cs.rochester.edu/u/james/CSC248/Lec6.pdf
entropy(x, method="ML")
entropy(y, method="ML")
entropy(z, method="ML")




library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(data.table)
library(tidytext)

# Code to import comment file and make comments bi-grams (see code from Amruta)
comments <- read_csv("~/Dropbox/INSPIRE/Papers & Presentations/Language Evolution (GROUP)/Data Analysis/Data Files/gravityspy_prep2.csv")
# Get promotion information "2018-02-10 01:51:13 UTC"
joindate <- read_csv("/Users/coreyjackson/Dropbox/INSPIRE/Papers & Presentations/Language Evolution (GROUP)/Data Analysis/Data Files/joindate.csv",
  col_types = cols(X1 = col_skip()))
joindate$first_class <- as.POSIXct(joindate$first_class, format="%Y-%m-%d %H:%M:%S")


# compute bigrams from comment dataframe
bigram_comments <- comments %>%
    unnest_tokens(bigram, filtered_words, token = "ngrams", n = 2)
#bigram_comments <- bigram_comments[which(is.na(bigram_comments$bigram)),]


bigram_commentssub <- bigram_comments[1:50000,]


cast<- dcast(bigram_commentssub, comment_user_login ~ bigram)
cast2 <- cast[,2:34187]

entfun <- function(x){
	entropy(x, method="LM")
}

entropyuse <- apply(cast2, MARGIN=1, FUN=function(x2) entfun(x2))
# Get user cast[1][354,]
# Get entropy entropyuse[1]
