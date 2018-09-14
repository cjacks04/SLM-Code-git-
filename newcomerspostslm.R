# Corey Jackson
# Newcomer Language Analysis
# Need to run snapshot-model.R prior to running this analysis


# Using newcomer status as 30 days from first classification compute 
user_info$newcomerend <- as.Date(as.character(user_info$first_class), format="%Y-%m-%d") + 30
user_info$newcomerende <- format(as.Date(user_info$first_class), "%Y-%m")
bigrams_monthly_count$monthe <- format(as.Date(bigrams_monthly_count$month), "%Y-%m")

user_info <- merge(user_info, bigrams_monthly_count[, c("monthe","monthnumber")],
	by.x = "newcomerende", by.y="monthe", 
	all.x = TRUE)