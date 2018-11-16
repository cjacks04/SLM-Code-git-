# Calculating entropy in Gravity Spy
# Corey Jackson

##### An Example ###############
x = c(0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 0) 
entropy(x, method="ML") # Entropy = 0 
y = c(1, 1, 3, 1, 0, 0, 0, 0, 1, 2, 0) 
entropy(y, method="ML") # Entropy = 1.676988
z = c(1, 1, 3, 1, 0, 0, 20, 10, 1, 2, 0) 
entropy(z, method="ML") # Entropy = 1.416828 

##### Another Example ###############

library(entropy)
v = c(0,4,3,6,7,3,2,3,4,5)
entropy(discretize(v, numBins = 8, r = c(0,7))) # 1.834372

# In another way
p <- table(v)
p
#0 2 3 4 5 6 7 
#1 1 3 2 1 1 1 
v
#[1] 0 4 3 6 7 3 2 3 4 5
p <- p/sum(p); sum(-p*log(p))
#[1] 1.834372


# Large values of entropy mean the more "diverse" the dataset is. 
#################################

# https://cran.r-project.org/web/packages/entropy/entropy.pdf 
# http://www.cs.rochester.edu/u/james/CSC248/Lec6.pdf



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

board_summary_monthly_count <- comments %>% group_by(board_title,month=floor_date(comment_created_at, "month")) %>%
  summarize(
    total_comments=length(comment_user_login),
    total_users=length(unique(comment_user_login)))

## User information
comments_user_info <- comments %>% group_by(comment_user_id,comment_user_login) %>%
  summarize(comments_posted = length(comment_user_id),
            first_comment = min(comment_created_at),
            last_comment = max(comment_created_at)
  )
comments_user_info$first_comment <- as.POSIXct(comments_user_info$first_comment, format="%Y-%m-%d %H:%M:%S")

user_info <- merge(joindate,comments_user_info, by.x="user_id", by.y="comment_user_id")
user_info$days_to_post <- as.Date(as.character(user_info$first_comment), format="%Y-%m-%d")-
                  as.Date(as.character(user_info$first_class), format="%Y-%m-%d")
remove(joindate,comments_user_info) 

#### Bigram Specific
bigrams_appearance_monthcount <- bigram_comments %>% group_by(month=floor_date(comment_created_at, "month"),bigram) %>%
   summarize(total_bigrams=length(bigram)) 

bigrams_appearance_count <- bigram_comments %>% group_by(bigram) %>%
  summarize(total_bigrams=length(bigram))   

bigrams_monthly_count <- bigram_comments %>% group_by(month=floor_date(comment_created_at, "month")) %>%
  summarize(total_bigrams=length(bigram),unique_bigrams=length(unique(bigram)))
bigrams_monthly_count <- as.data.table(bigrams_monthly_count)[, monthnumber := .GRP, by = month]

#### Create names for Corpus
# https://cran.r-project.org/web/packages/magicfor/vignettes/magicfor.html

# Creates a list of named months (e.g., month1) for each month represented in the project. To be used in the next loop
new_names <- c()
for (i in 1:nrow(bigrams_monthly_count)){
  new_names[i] <- (paste("month",i,sep=""))
}

# Creates a list of named months (e.g., month1_single)  for each month represented in the project. To be used in the next loop
new_names2 <- c()
for (i in 1:nrow(bigrams_monthly_count)){
  new_names2[i] <- (paste("month",i,"_single",sep=""))
}

new_names_list <- as.list(new_names)

# Creates a list of named months (e.g., month1probability)  for each month represented in the project. To be used in the next loop
prob_names <- c()
for (i in 1:nrow(bigrams_monthly_count)){
  prob_names[i] <- (paste("month",i,"probability",sep=""))
}

### Creates a new dataframe (e.g., month1) containing each bigram and the user who created the bigram (aggregated over months so that final month has all bigrams includes dublicates)
Months <-unique(floor_date(bigram_comments$comment_created_at, "month"))
    for (i in 1:length(Months)){ 
    assign(new_names[i],bigram_comments[floor_date(bigram_comments$comment_created_at, "month") <= Months[i],])
    }  

### Creates a new dataframe (e.g., month1_single) containing each bigram used in that month
for (i in 1:length(Months)){ 
  assign(new_names2[i],bigram_comments[floor_date(bigram_comments$comment_created_at, "month") == Months[i],])
} 


# Need to cast all bi-grams in columns and count
cast<- dcast(bigram_commentssub, comment_user_login ~ bigram)

# remove first column
cast2 <- cast[,2:34187]


## Entropy function
entfun <- function(x){
 	entropy(x, method="ML")
 }

# compute entropy for row 
entropyuse <- apply(cast2, MARGIN=1, FUN=function(x) entfun(x))

