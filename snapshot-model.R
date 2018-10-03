# Corey Jackson
# Snapshot Language Model

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}
detach_package(plyr)

library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(data.table)
library(tidytext)

# Code to import comment file and make comments bi-grams (see code from Amruta)
comments <- read_csv("~/Dropbox/INSPIRE/Papers & Presentations/Language Evolution ()/Data Analysis/Data Files/gravityspy_prep2.csv")
# Get promotion information "2018-02-10 01:51:13 UTC"
joindate <- read_csv("/Users/coreyjackson/Dropbox/INSPIRE/Papers & Presentations/Language Evolution ()/Data Analysis/Data Files/joindate.csv",
  col_types = cols(X1 = col_skip()))
joindate$first_class <- as.POSIXct(joindate$first_class, format="%Y-%m-%d %H:%M:%S")

# compute bigrams from comment dataframe
bigram_comments <- comments %>%
    unnest_tokens(bigram, filtered_words, token = "ngrams", n = 2)
bigram_comments <- bigram_comments[which(is.na(bigram_comments$bigram)),]



## Board information
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

### Unigrams Month/Comments
#comments$comment_body2 <- gsub('http\\S+\\s*', "", comments$comment_body) #remove links
#comments$comment_body2 <- tolower(comments$comment_body2) #lower case
#comments$comment_body2 <- as.character(comments$comment_body2)
#stopWords = c(stopwords("en"))

## Removes stopwords in comment
# '%nin%' <- Negate('%in%')
# comments$comment_body2 <-lapply(comments$comment_body2, function(x) {
#   chk <- unlist(strsplit(x," "))
#   p <- chk[chk %nin% stopWords]
#   paste(p,collapse = " ")
# })

#comments$comment_body2 <- as.character(comments$comment_body2)



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


# Loop and create new dataframes
detach(package:plyr)
library(dplyr)
#dfList <- list(df1=month1, df2=month2)

# Compute_Month_Probability <- function(month,grp.var){
#   lenmon = nrow(month)
#   month %>% group_by_(grp.var) %>%
#       summarize(occurrence=length(grp.var),
#                 lenmon = lenmon,
#                 probability_occur = occurrence/lenmon,
#                 log_probability_occur = log(probability_occur)
#       )
# }

# for (i in new_names_list){ 
#   newname <- paste("probability",i,sep=".")
#   prod <- Compute_Month_Probability(i,"bigram")
#   assign(newname,prod)
# }

# monthN is all user bigrams terms for that month. non-unique
# monthN_probability is all unique bigrams and their probabilities
# monthN_p is for all user/bigram whats the probability from previous month
# month2_pc is all unique bigrams whats the probability from previous month

### Loops and creates a new dataframe for each month
detach(package:plyr)
library(dplyr)

# For each month compute the probability of the term occurring in a month. 
## Simply the number of times the bigram used/no. bigrams in corpus
month1_probability <- month1 %>% group_by(bigram) %>%
   summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month1),log_probability_occur = log(probability_occur) ) # Produces a dataframe with probabilities for each bigram of occurring in that month

# For each month compute the probability of the term occurring in a month. 
## Simply the number of times the bigram used/no. bigrams in corpus
month2_probability <- month2 %>% group_by(bigram) %>%
   summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month2),log_probability_occur = log(probability_occur) ) 
# Combines all the bigrams used including prior months with those in month 1
month2_p <- merge(month2,month1_probability, by=("bigram"),all.x=TRUE) 
month2_pc <- merge(month2_probability,month1_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month2_pc)[5]<-"occurrence.pm"
names(month2_pc)[6]<-"probability_occur.pm"
names(month2_pc)[7]<-"log_probability_occur.pm"
# replaces NA with 0
month2_p[ , 15:17][is.na(month2_p[ , 15:17] ) ] = 0 
month2_p$month <- "Month 2"

month3_probability <- month3 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month3),log_probability_occur = log(probability_occur) )
month3_p <- merge(month3,month2_probability, by=("bigram"),all.x=TRUE)
month3_pc <- merge(month3_probability,month2_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month3_pc)[5]<-"occurrence.pm"
names(month3_pc)[6]<-"probability_occur.pm"
names(month3_pc)[7]<-"log_probability_occur.pm"
month3_p[ , 15:17][is.na(month3_p[ , 15:17] ) ] = 0
month3_p$month <- "Month 3"

month4_probability <- month4 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month4),log_probability_occur = log(probability_occur) )
month4_p <- merge(month4,month3_probability, by=("bigram"),all.x=TRUE)
month4_pc <- merge(month4_probability,month3_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month4_pc)[5]<-"occurrence.pm"
names(month4_pc)[6]<-"probability_occur.pm"
names(month4_pc)[7]<-"log_probability_occur.pm"
month4_p[ , 15:17][is.na(month4_p[ , 15:17] ) ] = 0
month4_p$month <- "Month 4"

month5_probability <- month5 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month5),log_probability_occur = log(probability_occur) )
month5_p <- merge(month5,month4_probability, by=("bigram"),all.x=TRUE)
month5_pc <- merge(month5_probability,month4_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month5_pc)[5]<-"occurrence.pm"
names(month5_pc)[6]<-"probability_occur.pm"
names(month5_pc)[7]<-"log_probability_occur.pm"
month5_p[ , 15:17][is.na(month5_p[ , 15:17] ) ] = 0
month5_p$month <- "Month 5"

month6_probability <- month6 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month6),log_probability_occur = log(probability_occur) )
month6_p <- merge(month6,month5_probability, by=("bigram"),all.x=TRUE)
month6_pc <- merge(month6_probability,month5_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month6_pc)[5]<-"occurrence.pm"
names(month6_pc)[6]<-"probability_occur.pm"
names(month6_pc)[7]<-"log_probability_occur.pm"
month6_p[ , 15:17][is.na(month6_p[ , 15:17] ) ] = 0
month6_p$month <- "Month 6"

month7_probability <- month7 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month7),log_probability_occur = log(probability_occur) )
month7_p <- merge(month7,month6_probability, by=("bigram"),all.x=TRUE)
month7_pc <- merge(month7_probability,month6_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month7_pc)[5]<-"occurrence.pm"
names(month7_pc)[6]<-"probability_occur.pm"
names(month7_pc)[7]<-"log_probability_occur.pm"
month7_p[ , 15:17][is.na(month7_p[ , 15:17] ) ] = 0
month7_p$month <- "Month 7"

month8_probability <- month8 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month8),log_probability_occur = log(probability_occur) )
month8_p <- merge(month8,month7_probability, by=("bigram"),all.x=TRUE)
month8_pc <- merge(month8_probability,month7_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month8_pc)[5]<-"occurrence.pm"
names(month8_pc)[6]<-"probability_occur.pm"
names(month8_pc)[7]<-"log_probability_occur.pm"
month8_p[ , 15:17][is.na(month8_p[ , 15:17] ) ] = 0
month8_p$month <- "Month 8"

month9_probability <- month9 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month9),log_probability_occur = log(probability_occur) )
month9_p <- merge(month9,month8_probability, by=("bigram"),all.x=TRUE)
month9_pc <- merge(month9_probability,month8_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month9_pc)[5]<-"occurrence.pm"
names(month9_pc)[6]<-"probability_occur.pm"
names(month9_pc)[7]<-"log_probability_occur.pm"
month9_p[ , 15:17][is.na(month9_p[ , 15:17] ) ] = 0
month9_p$month <- "Month 9"

month10_probability <- month10 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month10),log_probability_occur = log(probability_occur) )
month10_p <- merge(month10,month9_probability, by=("bigram"),all.x=TRUE)
month10_pc <- merge(month10_probability,month9_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month10_pc)[5]<-"occurrence.pm"
names(month10_pc)[6]<-"probability_occur.pm"
names(month10_pc)[7]<-"log_probability_occur.pm"
month10_p[ , 15:17][is.na(month10_p[ , 15:17] ) ] = 0
month10_p$month <- "Month 10"

month11_probability <- month11 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month11),log_probability_occur = log(probability_occur) )
month11_p <- merge(month11,month10_probability, by=("bigram"),all.x=TRUE)
month11_pc <- merge(month11_probability,month10_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month11_pc)[5]<-"occurrence.pm"
names(month11_pc)[6]<-"probability_occur.pm"
names(month11_pc)[7]<-"log_probability_occur.pm"
month11_p[ , 15:17][is.na(month11_p[ , 15:17] ) ] = 0
month11_p$month <- "Month 11"

month12_probability <- month12 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month12),log_probability_occur = log(probability_occur) )
month12_p <- merge(month12,month11_probability, by=("bigram"),all.x=TRUE)
month12_pc <- merge(month12_probability,month11_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month12_pc)[5]<-"occurrence.pm"
names(month12_pc)[6]<-"probability_occur.pm"
names(month12_pc)[7]<-"log_probability_occur.pm"
month12_p[ , 15:17][is.na(month12_p[ , 15:17] ) ] = 0
month12_p$month <- "Month 12"

month13_probability <- month13 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month13),log_probability_occur = log(probability_occur) )
month13_p <- merge(month13,month12_probability, by=("bigram"),all.x=TRUE)
month13_pc <- merge(month13_probability,month12_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month13_pc)[5]<-"occurrence.pm"
names(month13_pc)[6]<-"probability_occur.pm"
names(month13_pc)[7]<-"log_probability_occur.pm"
month13_p[ , 15:17][is.na(month13_p[ , 15:17] ) ] = 0
month13_p$month <- "Month 13"


month14_probability <- month14 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month14),log_probability_occur = log(probability_occur) )
month14_p <- merge(month14,month13_probability, by=("bigram"),all.x=TRUE)
month14_pc <- merge(month14_probability,month13_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month14_pc)[5]<-"occurrence.pm"
names(month14_pc)[6]<-"probability_occur.pm"
names(month14_pc)[7]<-"log_probability_occur.pm"
month14_p[ , 15:17][is.na(month14_p[ , 15:17] ) ] = 0
month14_p$month <- "Month 14"

month15_probability <- month14 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month14),log_probability_occur = log(probability_occur) )
month15_p <- merge(month15,month14_probability, by=("bigram"),all.x=TRUE)
month15_pc <- merge(month15_probability,month14_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month15_pc)[5]<-"occurrence.pm"
names(month15_pc)[6]<-"probability_occur.pm"
names(month15_pc)[7]<-"log_probability_occur.pm"
month15_p[ , 15:17][is.na(month15_p[ , 15:17] ) ] = 0
month15_p$month <- "Month 15"


month16_probability <- month16 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month16),log_probability_occur = log(probability_occur) )
month16_p <- merge(month16,month15_probability, by=("bigram"),all.x=TRUE)
month16_pc <- merge(month16_probability,month15_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month16_pc)[5]<-"occurrence.pm"
names(month16_pc)[6]<-"probability_occur.pm"
names(month16_pc)[7]<-"log_probability_occur.pm"
month16_p[ , 15:17][is.na(month16_p[ , 15:17] ) ] = 0
month16_p$month <- "Month 16"

month17_probability <- month17 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month17),log_probability_occur = log(probability_occur) )
month17_p <- merge(month17,month16_probability, by=("bigram"),all.x=TRUE)
month17_pc <- merge(month17_probability,month16_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month17_pc)[5]<-"occurrence.pm"
names(month17_pc)[6]<-"probability_occur.pm"
names(month17_pc)[7]<-"log_probability_occur.pm"
month17_p[ , 15:17][is.na(month17_p[ , 15:17] ) ] = 0
month17_p$month <- "Month 17"

month18_probability <- month18 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month18),log_probability_occur = log(probability_occur) )
month18_p <- merge(month18,month17_probability, by=("bigram"),all.x=TRUE)
month18_pc <- merge(month18_probability,month17_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month18_pc)[5]<-"occurrence.pm"
names(month18_pc)[6]<-"probability_occur.pm"
names(month18_pc)[7]<-"log_probability_occur.pm"
month18_p[ , 15:17][is.na(month18_p[ , 15:17] ) ] = 0
month18_p$month <- "Month 18"

month19_probability <- month19 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month19),log_probability_occur = log(probability_occur) )
month19_p <- merge(month19,month18_probability, by=("bigram"),all.x=TRUE)
month19_pc <- merge(month19_probability,month18_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month19_pc)[5]<-"occurrence.pm"
names(month19_pc)[6]<-"probability_occur.pm"
names(month19_pc)[7]<-"log_probability_occur.pm"
month19_p[ , 15:17][is.na(month19_p[ , 15:17] ) ] = 0
month19_p$month <- "Month 19"

month20_probability <- month20 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month20),log_probability_occur = log(probability_occur) )
month20_p <- merge(month20,month19_probability, by=("bigram"),all.x=TRUE)
month20_pc <- merge(month20_probability,month19_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month20_pc)[5]<-"occurrence.pm"
names(month20_pc)[6]<-"probability_occur.pm"
names(month20_pc)[7]<-"log_probability_occur.pm"
month20_p[ , 15:17][is.na(month20_p[ , 15:17] ) ] = 0
month20_p$month <- "Month 20"

month21_probability <- month21 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month21),log_probability_occur = log(probability_occur) )
month21_p <- merge(month21,month20_probability, by=("bigram"),all.x=TRUE)
month21_pc <- merge(month21_probability,month20_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month21_pc)[5]<-"occurrence.pm"
names(month21_pc)[6]<-"probability_occur.pm"
names(month21_pc)[7]<-"log_probability_occur.pm"
month21_p[ , 15:17][is.na(month21_p[ , 15:17] ) ] = 0
month21_p$month <- "Month 21"

month22_probability <- month22 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month22),log_probability_occur = log(probability_occur) )
month22_p <- merge(month22,month21_probability, by=("bigram"),all.x=TRUE)
month22_pc <- merge(month22_probability,month21_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month22_pc)[5]<-"occurrence.pm"
names(month22_pc)[6]<-"probability_occur.pm"
names(month22_pc)[7]<-"log_probability_occur.pm"
month22_p[ , 15:17][is.na(month22_p[ , 15:17] ) ] = 0
month22_p$month <- "Month 22"

month23_probability <- month23 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month23),log_probability_occur = log(probability_occur) )
month23_p <- merge(month23,month22_probability, by=("bigram"),all.x=TRUE)
month23_pc <- merge(month23_probability,month22_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month23_pc)[5]<-"occurrence.pm"
names(month23_pc)[6]<-"probability_occur.pm"
names(month23_pc)[7]<-"log_probability_occur.pm"
month23_p[ , 15:17][is.na(month23_p[ , 15:17] ) ] = 0
month23_p$month <- "Month 23"

month24_probability <- month24 %>% group_by(bigram) %>%
  summarize(occurrence=length(bigram),probability_occur = length(bigram)/nrow(month24),log_probability_occur = log(probability_occur) )
month24_p <- merge(month24,month23_probability, by=("bigram"),all.x=TRUE)
month24_pc <- merge(month24_probability,month23_probability, by=("bigram"),all.x=TRUE) # a community probability 
names(month24_pc)[5]<-"occurrence.pm"
names(month24_pc)[6]<-"probability_occur.pm"
names(month24_pc)[7]<-"log_probability_occur.pm"
month24_p[ , 15:17][is.na(month24_p[ , 15:17] ) ] = 0
month24_p$month <- "Month 24"





##### Creating user level summaries #####
library(plyr)


userpost_m2 <- ddply(month2_p, c("comment_user_login","comment_id"), summarize,
				bigram_count = length(unique(bigram)),
				probability = mean(probability_occur)
				 )

user_m2 <- ddply(month2_p, c("comment_user_login"), summarize,
				bigram_count = length(unique(bigram)),
				probability = mean(probability_occur)
				 )
userpost_m2$month <- "Month 2"
user_m2$month <- "Month 2"

userpost_m3 <- ddply(month3_p, c("comment_user_login","comment_id"), summarize,
				bigram_count = length(unique(bigram)),
				probability = mean(probability_occur)
				 )

user_m3 <- ddply(month3_p, c("comment_user_login"), summarize,
				bigram_count = length(unique(bigram)),
				probability = mean(probability_occur)
				 )
userpost_m3$month <- "Month 3"
user_m3$month <- "Month 3"


userpost_m4 <- ddply(month4_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
        )

user_m4 <- ddply(month4_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
        )
userpost_m4$month <- "Month 4"
user_m4$month <- "Month 4"


userpost_m5 <- ddply(month5_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)

user_m5 <- ddply(month5_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
user_m5$month <- "Month 5"
userpost_m5$month <- "Month 5"

userpost_m6 <- ddply(month6_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)

user_m6 <- ddply(month6_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m6$month <- "Month 6"
user_m6$month <- "Month 6"

userpost_m7 <- ddply(month7_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)

user_m7 <- ddply(month7_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m7$month <- "Month 7"
user_m7$month <- "Month 7"


userpost_m8 <- ddply(month8_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m8 <- ddply(month8_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m8$month <- "Month 8"
user_m8$month <- "Month 8"

userpost_m9 <- ddply(month9_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m9 <- ddply(month9_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m9$month <- "Month 9"
user_m9$month <- "Month 9"

userpost_m10 <- ddply(month10_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m10 <- ddply(month10_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m10$month <- "Month 10"
user_m10$month <- "Month 10"

userpost_m11 <- ddply(month11_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m11 <- ddply(month11_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m11$month <- "Month 11"
user_m11$month <- "Month 11"

userpost_m12 <- ddply(month12_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m12 <- ddply(month12_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m12$month <- "Month 12"
user_m12$month <- "Month 12"


userpost_m13 <- ddply(month13_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m13 <- ddply(month13_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m13$month <- "Month 13"
user_m13$month <- "Month 13"

userpost_m14 <- ddply(month14_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m14 <- ddply(month14_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m14$month <- "Month 14"
user_m14$month <- "Month 14"

userpost_m15 <- ddply(month15_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m15 <- ddply(month15_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m15$month <- "Month 15"
user_m15$month <- "Month 15"

userpost_m16 <- ddply(month16_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m16 <- ddply(month16_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m16$month <- "Month 16"
user_m16$month <- "Month 16"


userpost_m17 <- ddply(month17_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m17 <- ddply(month17_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m17$month <- "Month 17"
user_m17$month <- "Month 17"

userpost_m18 <- ddply(month18_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m18 <- ddply(month18_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m18$month <- "Month 18"
user_m18$month <- "Month 18"


userpost_m19 <- ddply(month19_p, c("comment_user_login","comment_id"), summarize,
                     bigram_count = length(unique(bigram)),
                     probability = mean(probability_occur)
)
user_m19 <- ddply(month19_p, c("comment_user_login"), summarize,
                 bigram_count = length(unique(bigram)),
                 probability = mean(probability_occur)
)
userpost_m19$month <- "Month 19"
user_m19$month <- "Month 19"

userpost_m20 <- ddply(month20_p, c("comment_user_login","comment_id"), summarize,
                      bigram_count = length(unique(bigram)),
                      probability = mean(probability_occur)
)
user_m20 <- ddply(month20_p, c("comment_user_login"), summarize,
                  bigram_count = length(unique(bigram)),
                  probability = mean(probability_occur)
)
userpost_m20$month <- "Month 20"
user_m20$month <- "Month 20"

userpost_m21 <- ddply(month21_p, c("comment_user_login","comment_id"), summarize,
                      bigram_count = length(unique(bigram)),
                      probability = mean(probability_occur)
)
user_m21 <- ddply(month21_p, c("comment_user_login"), summarize,
                  bigram_count = length(unique(bigram)),
                  probability = mean(probability_occur)
)
userpost_m21$month <- "Month 21"
user_m21$month <- "Month 21"

userpost_m22 <- ddply(month22_p, c("comment_user_login","comment_id"), summarize,
                      bigram_count = length(unique(bigram)),
                      probability = mean(probability_occur)
)
user_m22 <- ddply(month22_p, c("comment_user_login"), summarize,
                  bigram_count = length(unique(bigram)),
                  probability = mean(probability_occur)
)
userpost_m22$month <- "Month 22"
user_m22$month <- "Month 22"

userpost_m23 <- ddply(month23_p, c("comment_user_login","comment_id"), summarize,
                      bigram_count = length(unique(bigram)),
                      probability = mean(probability_occur)
)
user_m23 <- ddply(month23_p, c("comment_user_login"), summarize,
                  bigram_count = length(unique(bigram)),
                  probability = mean(probability_occur)
)
userpost_m23$month <- "Month 23"
user_m23$month <- "Month 23"

userpost_m24 <- ddply(month24_p, c("comment_user_login","comment_id"), summarize,
                      bigram_count = length(unique(bigram)),
                      probability = mean(probability_occur)
)
user_m24 <- ddply(month24_p, c("comment_user_login"), summarize,
                  bigram_count = length(unique(bigram)),
                  probability = mean(probability_occur)
)
userpost_m24$month <- "Month 24"
user_m24$month <- "Month 24"

user_all <- rbind(user_m2,user_m3,user_m4,user_m5,user_m6,user_m7,user_m8,user_m9,user_m10,user_m11,
                  user_m12,user_m13,user_m14,user_m15,user_m16,user_m17,user_m18,user_m19,user_m20,
                  user_m21,user_m22,user_m23,user_m24)

#all_month <- rbind(month2_p,month3_p,month4_p,month5_p,month6_p,month7_p,month8_p,month9_p,month10_p,
#                     month11_p,month12_p,month13_p,month14_p,month15_p,month16_p,month17_p,month18_p,month19_p,month20_p,
#                     month21_p,month22_p,month23_p,month24_p)

userpost_all <- rbind(userpost_m2,userpost_m3,userpost_m4,userpost_m5,userpost_m6,userpost_m7,userpost_m8,
                  userpost_m9,userpost_m10,userpost_m11,userpost_m12,userpost_m13,userpost_m14,
                  userpost_m15,userpost_m16,userpost_m17,userpost_m18,userpost_m19,userpost_m20,
                  userpost_m21,userpost_m22,userpost_m23,userpost_m24)

# remove(userpost_m2,userpost_m3,userpost_m4,userpost_m5,userpost_m6,userpost_m7,userpost_m8,
#                       userpost_m9,userpost_m10,userpost_m11,userpost_m12,userpost_m13,userpost_m14,
#                       userpost_m15,userpost_m16,userpost_m17,userpost_m18,userpost_m19,userpost_m20,
#                       userpost_m21,userpost_m22,userpost_m23,userpost_m24,user_m2,user_m3,user_m4,user_m5,
#                       user_m6,user_m7,user_m8,user_m9,user_m10,user_m11,
#                       user_m12,user_m13,user_m14,user_m15,user_m16,user_m17,user_m18,user_m19,user_m20,
#                       user_m21,user_m22,user_m23,user_m24)
