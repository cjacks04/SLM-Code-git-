# Corey Jackson
# Newcomer Language Analysis
# Need to run snapshot-model.R prior to running this analysis

library(sqldf)

# Using newcomer status as 30 days from first classification compute 
user_info$newcomerend <- as.Date(as.character(user_info$first_class), format="%Y-%m-%d") + 30
user_info$newcomerends <- as.POSIXct(user_info$first_class, format="%Y-%m-%d %H:%M:%S") + 2591000 

user_info$newcomerende <- format(as.Date(user_info$first_class), "%Y-%m")
bigrams_monthly_count$monthe <- format(as.Date(bigrams_monthly_count$month), "%Y-%m")

user_info <- merge(user_info, bigrams_monthly_count[, c("monthe","monthnumber")],
	by.x = "newcomerende", by.y="monthe", 
	all.x = TRUE)

comments$createdate <- format(as.Date(comments$comment_created_at), "%Y-%m")
comments <- merge(comments, bigrams_monthly_count[, c("monthe","monthnumber")],
	by.x = "createdate", by.y="monthe", 
	all.x = TRUE)

# Gets all the comments that came within a newcomers first month of contributing 
seed.comments <- sqldf("SELECT 
 	c.comment_id, c.comment_user_login, c.monthnumber
    FROM comments c, user_info i 
    WHERE i.user_name == c.comment_user_login
    AND c.comment_created_at >= i.first_class AND c.comment_created_at <= i.newcomerends")

