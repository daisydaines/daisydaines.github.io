# load libraries
library(emojifont)
library(googlesheets4)
library(tidyverse)

# STATS LINK: https://bigskyconf.com/sports/mbball

# deauth --anyone with link can access
gs4_deauth()

# url
link22 <- "https://docs.google.com/spreadsheets/d/1YIMkvigzO-ermQiLu4DOTsW9U7ThaW4fXV9Dc0uRYZs/edit#gid=0"
df <- read_sheet(link22)

# extract columns for each season
# 2019-20
df20 <- df %>% select(8:10)
# 2020-21
df21 <- df %>% select(5:7)
# 2021-22
df22 <- df %>% select(2:4)

# rename columns
df20 <- df20 %>% rename(
  three_fg = three_fg...8,
  three_fga = three_fga...9,
  three_pct = three_pct...10
)

df21 <- df21 %>% rename(
  three_fg = three_fg...5,
  three_fga = three_fga...6,
  three_pct = three_pct...7
)

# print column names
colnames(df20)
colnames(df21)
# check structure
str(df20)
str(df21)
str(df22)
# change columns to numeric 
df20$three_fga <- as.numeric(df20$three_fga)
df21$three_fga <- as.numeric(df21$three_fga)
df22$three_attempts <- as.numeric(df22$three_attempts)
# add column for games
df20$game <- 1:nrow(df20)
df21$game <- 1:nrow(df21)
df22$game <- 1:nrow(df22)

# add running total for percentage
df20 <- df20 %>% mutate(running_pct = round(cumsum(three_fg) / cumsum(three_fga), 3) * 100)
#df21 <- df21 %>% mutate(running_pct = round(cumsum(three_fg) / cumsum(three_fga), 3) * 100)
df22 <- df22 %>% mutate(running_pct = round(cumsum(three_makes) / cumsum(three_attempts), 3) * 100)

# remove last 6 rows NA data
df21 <- head(df21, - 6)
# fix two numbers
df21[df21$three_fga== -9, "three_fga"] <- 9
df20[df20$three_fga == -7, "three_fga"] <- 7

# build simple linear model to predict final six games for COVID shortened season
model <- lm(three_fg ~ three_fga, df21)
# build df
pred_df <- data.frame(three_fga = floor(runif(6, min= min(df21$three_fga), max= max(df21$three_fga))), 
                      game = 25:30)
# add predicted numbers
pred_df$three_fg <- predict(model, pred_df)

# add original columns to later bind
str(pred_df)
pred_df$three_pct <- pred_df$three_fg / pred_df$three_fga

# reorder columns to match for rbind
colnames(pred_df)
pred_df <- pred_df[,c(3,1,4,2)]

# rbind predictions with df21
df21 <- rbind(df21, pred_df)

# add running percentage for entire season 
df21 <- df21 %>% mutate(running_pct = round(cumsum(three_fg) / cumsum(three_fga), 3) * 100)

# rename and reorder df22 columns
colnames(df22)
df22 <- df22[, c(1,2,3,5,4)]

df22 <- df22 %>% rename(
  three_fg = three_makes,
  three_fga = three_attempts,
  three_pct = pct
)

# add column for season to group by in ggplot
df20$season <- "2019-20"
df21$season <- "2020-21"
df22$season <- "2021-22"
# bind all dfs for dfall
dfall <- rbind(df20, df21, df22)

# ggplot: line chart colored by season
three <- ggplot(dfall, aes(x=game, y=running_pct, group=season), size = 1.5) +
  geom_line(aes(color=season), size= 1) + 
  theme_minimal() + 
  labs(y = "Running 3pt Percentage", 
       x= "Game")
# add relevant ISU colors
three <- three + scale_color_manual(breaks = c("2019-20", "2020-21", "2021-22"),
                                    values=c("gray", "orange", "black"))

# remove grid lines and make outer lines black for simple looks
three <- three + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# print plot
print(three)





