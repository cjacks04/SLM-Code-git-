# Calculate quasi-promotion for Gravity Spy

library(plyr)
library(dplyr)
library(readr)

#https://www.zooniverse.org/lab/1104/data-exports

class <- read_csv() # classification data taken from northwestern (available on dropbox)
class <- class[which(!is.na(class$user_id)),]
class<- class[which(!class$workflow_name %in% c(
	"Apprentice - No Training - Defunct",
	"Beginner 4",
	"workflow assignment split experiment workflow")),]

user_promotion <- ddply(class, c("user_name","user_id","workflow_name"), summarise,
					first_class = min(created_at)
					)