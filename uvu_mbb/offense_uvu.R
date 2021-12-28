# offense sides 
library(tidyverse)
library(modelr)
library(plyr)
library(readxl)
df1 <- read_excel(path = "OFFENSE.xlsx", sheet = "CBU")
df2 <- read_excel(path = "OFFENSE.xlsx", sheet = "CBU2")
df3 <- read_excel(path = "OFFENSE.xlsx", sheet = "Seattle")
df4 <- read_excel(path = "OFFENSE.xlsx", sheet = "Tarleton")
df5 <- read_excel(path = "OFFENSE.xlsx", sheet = "Tarleton 2")
df6 <- read_excel(path = "OFFENSE.xlsx", sheet = "Dixie St.")
df7 <- read_excel(path = "OFFENSE.xlsx", sheet = "Dixie St. 2")
df8 <- read_excel(path = "OFFENSE.xlsx", sheet = "NMSU")
df9 <- read_excel(path = "OFFENSE.xlsx", sheet = "NMSU 2")
df10 <- read_excel(path = "OFFENSE.xlsx", sheet = "UTRGV")
df11 <- read_excel(path = "OFFENSE.xlsx", sheet = "UTRGV 2")
df12 <- read_excel(path = "OFFENSE.xlsx", sheet = "GCU")
df13 <- read_excel(path = "OFFENSE.xlsx", sheet = "GCU 2")
df14 <- read_excel(path = "OFFENSE.xlsx", sheet = "NMSU3")

df1$Opponent <- "CBU"
df2$Opponent <- "CBU2"
df3$Opponent <- "SU"
df4$Opponent <- "TSU"
df5$Opponent <- "TSU2"
df6$Opponent <- "DSU"
df7$Opponent <- "DSU2"
df8$Opponent <- "NMSU"
df9$Opponent <- "NMSU2"
df10$Opponent <- "UTRGV"
df11$Opponent <- "UTRGV2"
df12$Opponent <- "GCU"
df13$Opponent <- "GCU2"
df14$Opponent <- "NMSU3"
df2$...8 <- NULL


conf <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9, df10, df11, df12, df13, df14)

names(conf)[7] <- "pts"
#warnings()
  #junk$nm[junk$nm == "B"] <- "b"

names(conf)[2] <- "passes"
names(conf)[3] <- "sides"

unique(conf$pts)

conf$pts[conf$pts == "TO"] <- "-0.5"
conf$pts[conf$pts == "Miss2pt"] <- "0"
conf$pts[conf$pts == "Miss3pt"] <- "0"
conf$pts[conf$pts == "Make2pt"] <- "2"
conf$pts[conf$pts == "Make3pt"] <- "3"
conf$pts[conf$pts == "miss2"] <- "0"
conf$pts[conf$pts == "miss3"] <- "0"
conf$pts[conf$pts == "make2"] <- "2"
conf$pts[conf$pts == "make3"] <- "3"

conf$pts[conf$pts == "miss2pt"] <- "0"
conf$pts[conf$pts == "miss3pt"] <- "0"
conf$pts[conf$pts == "make2pt"] <- "2"
conf$pts[conf$pts == "miss 2pt"] <- "0"
conf$pts[conf$pts == "miss23pt"] <- "0"
conf$pts[conf$pts == "made2"] <- "2"
conf$pts[conf$pts == "made3"] <- "3"

conf$pts[conf$pts == "Miss23pt"] <- "0"
conf$pts[conf$pts == "Miss 2pt"] <- "0"
conf$pts[conf$pts == "FOUL"] <- "1"
conf$pts[conf$pts == "Shooting foul"] <- "1"

new_conf <- conf
unique(conf$pts)
class(conf$pts)
class(conf$passes)
class(conf$sides)

conf$pts = as.numeric(conf$pts)
conf$passes = as.numeric(conf$passes)
conf$sides = as.numeric(conf$sides)


mod1 <- glm(data= conf, 
            formula = pts ~ passes * sides)
mod2 <- glm(data= conf, 
            formula = pts ~ passes + sides)
  
summary(mod1)
summary(mod2)

PvP <- add_predictions(conf, mod1, type = "response") %>% 
  ggplot(aes(x=passes)) + 
  geom_point(aes(y=pred), color= "Black", alpha=0.25) +
  geom_smooth(aes(y=pts), method="loess") +
  theme_minimal() +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) + 
                       labs(x= "# of passes",
                            y= "Points",
                            title= "Passes vs Points")

SvP <- add_predictions(conf, mod1, type = "response") %>% 
  ggplot(aes(x=sides)) + 
  geom_point(aes(y=pred), color= "Black", alpha=0.25) +
  geom_smooth(aes(y=pts), method="loess") +
  theme_minimal() +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) + 
  labs(x= "# of sides",
       y= "Points",
       title= "Sides vs Points")

print(SvP)
print(PvP)

ggplot(conf, aes(x=sides, y=passes,
                  color=pts, size=pts)) +
  geom_point(aes(), show.legend = TRUE) +
  theme_minimal() +
  scale_color_gradient(low = "red", high = "green") +
  xlab("sides") +
  ylab("passes")

ggplot(df_p2, aes(x=PLUS_MINUS, y=X2020.21/1000000,
                  color=MPG, size=MPG)) +
  geom_point(aes(), show.legend = TRUE) +
  theme_minimal() +
  scale_color_gradient(low = "yellow", high = "green") +
  xlab("Plus/Minus") +
  ylab("Salary ($M)")

conf2 <- conf[which(conf$play== "Thumb Up") %>% c(2:8)]

conf2 <- subset(conf, play == "Thumb Up", select("passes", "sides", "pts"))
  
  
  
  
  
