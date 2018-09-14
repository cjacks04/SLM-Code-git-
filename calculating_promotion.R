# Calculate joindate for Gravity Spy

library(plyr)
library(readr)

#https://www.zooniverse.org/lab/1104/data-exports
# computed using gravity-spy-classifications.csv on INSPIRE dropbox

class <- read_csv() # classification data taken from northwestern (available on dropbox)

user_join <- ddply(class, c("user_name","user_id"), summarise,
					first_class = min(created_at)
					)
write.csv(user_join,"~/Dropbox/INSPIRE/Papers & Presentations/Language Evolution ()/Data Analysis/Data Files/joindate.csv")

