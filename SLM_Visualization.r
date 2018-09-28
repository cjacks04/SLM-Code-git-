detach(package:plyr)

library(ggplot2)
library(dplyr)

# QUESTIONS
# In bigram_appearance_count, what are the weird bigrams starting with "<e3>..."?

# ISSUES
# There are a few null values popping up because the comment IDs are not found in month'x'_p. Examples: comment ID
# 95913 and 95917. This may be because the user may not be a newcomer.

# Visualizing the probabilities of newcomers' comments
newcomer_probability <- seed.comments
newcomer_probability$probability <- NA

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month2_p$probability_occur[month2_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month3_p$probability_occur[month3_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month4_p$probability_occur[month4_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month5_p$probability_occur[month5_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month6_p$probability_occur[month6_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month7_p$probability_occur[month7_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month8_p$probability_occur[month8_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month9_p$probability_occur[month9_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month10_p$probability_occur[month10_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month11_p$probability_occur[month11_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month12_p$probability_occur[month12_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month13_p$probability_occur[month13_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month14_p$probability_occur[month14_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month15_p$probability_occur[month15_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month16_p$probability_occur[month16_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month17_p$probability_occur[month17_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month18_p$probability_occur[month18_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month19_p$probability_occur[month19_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month20_p$probability_occur[month20_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month21_p$probability_occur[month21_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month22_p$probability_occur[month22_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month23_p$probability_occur[month23_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

for(i in 1:nrow(newcomer_probability))
{
  newcomer_probability$probability[i] <- 
    mean(month24_p$probability_occur[month24_p$comment_id == newcomer_probability$comment_id[i]], na.rm = TRUE)
}

newcomer_probability <- na.omit(newcomer_probability)
avg_monthly_prob <- newcomer_probability %>%
  group_by(monthnumber) %>%
  summarise(avg_prob = mean(probability))

ggplot(avg_monthly_prob) + 
  geom_bar(aes(monthnumber, avg_prob), stat = "identity", fill = "orange", color = "black", alpha = 0.75) + 
  geom_hline(yintercept = mean(avg_monthly_prob$avg_prob), linetype = "dashed", color = "red", size = 1.5) +
  geom_hline(yintercept = median(avg_monthly_prob$avg_prob), linetype = "dashed", color = "black", size = 1.5) + 
  labs(title = "Average Monthly Probabilities for Newcomers (Red - mean, black - median)", x = "Month Number", 
       y = "Average Probability")

summary(avg_monthly_prob$avg_prob)

# From the previous graph, month 6 had the lowest probabilities
month6_filtered <- month6_p[month6_p$probability_occur == 0, ]
length(unique(month6_filtered$bigram)) #2829 unique bigrams

ggplot(month6_filtered) + 
  geom_histogram(aes(comment_user_login), stat = "count", fill = "orange", color = "black", alpha = 0.75) + 
  labs(title = "Count of New Bigrams per User", x = "", y = "Bigram Count")

popular_bigrams_m6 <- data.frame(table(month6_filtered$bigram))
summary(popular_bigrams_m6$Freq)