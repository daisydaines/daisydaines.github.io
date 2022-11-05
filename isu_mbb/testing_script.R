
# load libraries
library(emojifont)
library(googlesheets4)
library(tidyverse)

# https://bigskyconf.com/sports/mbball
# grab df from google sheet
gs4_deauth()
link22 <- "https://docs.google.com/spreadsheets/d/1YIMkvigzO-ermQiLu4DOTsW9U7ThaW4fXV9Dc0uRYZs/edit#gid=0"
df22 <- read_sheet(link22)

# extract columns for each season
df21 <- df22 %>% select(5:7)
df22 <- df22 %>% select(2:4)

df21$three_fga[df21$three_fga == -9] <- 9

#df21 <- df21 %>% mutate(running_pct = round(cumsum(three_fg) / cumsum(three_fga), 3) * 100)
df21$game <- 1:nrow(df21)

#df21 <- head(df21, - 6)
df21$three_fga <- as.numeric(df21$three_fga)

df22 <- df22 %>% mutate(running_pct = round(cumsum(three_makes) / cumsum(three_attempts), 3) * 100)
df22$game <- 1:nrow(df22)

# convert three_fga to num
df21 <- head(df21, - 6)
df21$three_fga <- as.numeric(df21$three_fga)

#model <- lm(running_pct ~ game, df21)
#predictDf <- data.frame(game = 25:30)
#predictDf$running_pct <- predict(model, predictDf)

model <- lm(three_fg ~ three_fga, df21)
pred_df <- data.frame(#three_fg = floor(runif(6, min= min(df21$three_fg), max= max(df21$three_fg))), 
  three_fga = floor(runif(6, min= min(df21$three_fga), max= max(df21$three_fga))), 
  #three_pct = three_fg / three_fga, 
  #running_pct = round(cumsum(three_fg) / cumsum(three_fga), 3) * 100, 
  game = 25:30)
pred_df$three_fg <- predict(model, pred_df)

# add original columns to later bind
str(pred_df)
pred_df$three_pct <- pred_df$three_fg / pred_df$three_fga
#pred_df$running_pct <- round(cumsum(pred_df$three_fg) / cumsum(pred_df$three_fga), 3) * 100

colnames(pred_df)
pred_df <- pred_df[,c(3,1,4,2)]


# rbind
df21 <- rbind(df21, pred_df)


df21 <- df21 %>% mutate(running_pct = round(cumsum(three_fg) / cumsum(three_fga), 3) * 100)

colnames(df22)
df22 <- df22[, c(1,2,3,5,4)]

df22 <- df22 %>% rename(
  three_fg = three_makes,
  three_fga = three_attempts,
  three_pct = pct
)


df21$season <- "2020-21"
df22$season <- "2021-22"

dfall <- rbind(df21, df22)

#subset(dfall, game>= 25 & season == "2020-21")

three <- ggplot(dfall, aes(x=game, y=running_pct, group=season), size = 1.5) +
  geom_line(aes(color=season), size= 1) + 
  theme_minimal() + 
  labs(y = "Running 3pt Percentage", 
       x= "Game")

three <- three + scale_color_manual(breaks = c("2020-21", "2021-22"),
                                    values=c("orange", "black"))


three <- three + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

print(three)
