# The idea is to create a new measurement unit we can plot across time for each time that summarizes
# how efficient each team in their spending.
# We'll use the number of wins a team can get per dollar

library('Lahman')
data("Salaries")
data("Teams")

# Subset for years 2014 and 2015:
data_salaries <- subset(Salaries, yearID == 2014, select = c(yearID, teamID, lgID, playerID, salary))
data_salaries2 <- subset(Salaries, yearID == 2015, select = c(yearID, teamID, lgID, playerID, salary))
data_salaries <- rbind(data_salaries, data_salaries2)

data_teams1 <- subset(Teams, yearID == 2014, select = c(yearID, teamID, lgID, G, W, L))
data_teams2 <- subset(Teams, yearID == 2015, select = c(yearID, teamID, lgID, G, W, L))
data_teams <- rbind(data_teams1, data_teams2)

#filtering for 2014 and 2015
library(dplyr)

total_payroll <- data_salaries %>%
  group_by(yearID, teamID, lgID) %>%
  summarise(salary = sum(as.numeric(salary))) %>%
  group_by(yearID, teamID, lgID) %>%
  arrange(yearID)

total_payroll <- total_payroll[-c(9, 32), ]

# Creating a dataframe with the assembled data

moneyball <- data.frame(yearID = total_payroll$yearID, teamID = total_payroll$teamID,
                        lgID = total_payroll$lgID, salary = total_payroll$salary,
                        G = data_teams$G, W = data_teams$W, L = data_teams$L)

# Saving the data frame as a new data set

# write.csv(moneyball, file = "moneyball.csv")

# data <- read.csv("moneyball.csv")

# Calculate the number of wins per dollar spent on payroll for each team in each year:
# wpd_ij = wins_ij/payroll_ij

moneyball$WperDollar <- (moneyball$W/moneyball$salary)

moneyball$win_percentage <- (moneyball$W/moneyball$G)

moneyball2 <- moneyball %>%
  group_by(teamID)

avgStatsPerYear <- moneyball %>%
  group_by(teamID) %>%
  summarise(averagePayinYears = mean(salary),
            averageWininYears = mean(win_percentage))

library(ggplot2)
library(digest)

avgStatsPerYear %>%
  ggplot(aes(x=averagePayinYears, y=averageWininYears)) +
  geom_point(aes(colour=ifelse(teamID=="NYA", 'NYA as', "Other Teams"))) +
  xlab("Average Team Payroll") +
  ylab("Average Winning Percentage") +
  ggtitle("Your Team Spending Efficency 2014-2015") +
  geom_smooth(method = 'lm') +
  labs(colour="Team") +
  theme(text = element_text(),
        axis.text = element_text(angle = 90, vjust = 1))
  
#avgStatsPerYear2 <- moneyball %>%
 # group_by(yearID,teamID) %>%
#  summarise(averagePayinYears = mean(salary),
 #           averageWininYears = mean(win_percentage))
 
