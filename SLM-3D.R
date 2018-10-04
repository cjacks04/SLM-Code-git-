## Analysis for SLM
## Corey Jackson 
## July 23

# Comment to test Git


# Need to run snapshot-model.R before this analysis. 


# How many comments/users daily
board_summary <- ddply(comments, c("board_title"), summarize,
                       comments_posted = length(board_id),
                       unique_users = length(unique(comment_user_login)),
                       comment_length = mean(nchar(comment_body))
)

#### Create datafram of Bigram Month and the number of Comments

ggplot(bigrams_monthly_count, aes(month)) + 
  geom_line(aes(y = total_bigrams, colour = "total_bigrams")) + 
  geom_line(aes(y = unique_bigrams, colour = "unique_bigrams"))

ggplot(bigrams_appearance_count, aes(total_bigrams)) + 
  geom_bar()

#################
#### Analysis ###
#################

library(plotly)

plot_ly(user_all, x = ~month, y = ~probability, z = ~bigram_count,
        marker = list(color = ~probability, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = ''),
                      yaxis = list(title = 'Probability'),
                      zaxis = list(title = 'Bigrams')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Probability',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="scatter3d-colorscale")
chart_link

bigrams_monthly_count$month <- as.Date(bigrams_monthly_count$month)

setwd("~/Dropbox/INSPIRE/Papers & Presentations/Language Evolution (CSCW 2018)/Data Analysis/Figures")
pdf("bigram-month.pdf", height=5, width=12)
ggplot(data=bigrams_monthly_count, aes(x=month)) +
  geom_line(aes(y = total_bigrams, colour = "total_bigrams"),linetype="solid", color="black", size=.5) + 
  geom_line(aes(y = unique_bigrams, colour = "unique_bigrams"),linetype="dashed", color="black", size=.5) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-10-12"))), colour="red", linetype="dashed") + 
  labs(y="Bigrams", x="Project Month") + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + 
  theme_minimal() + 
  theme(
    legend.text=element_text(size=14),
    legend.title=element_text(size=14),
    axis.title=element_text(size=12),
    axis.text=element_text(size=14),
    axis.text.x=element_text(angle=60, hjust=1)
  ) 
dev.off()
