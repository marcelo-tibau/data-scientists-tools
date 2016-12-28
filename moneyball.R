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

write.csv(moneyball, file = "moneyball.csv")

# Calculate the number of wins per dollar spent on payroll for each team in each year:
# wpd_ij = wins_ij/payroll_ij

data <- read.csv("moneyball.csv")




library('dplyr')

library(dplyr)
lahman_con <- src_sqlite("/home/ids_materials/lahman_sqlite/lahman2014.sqlite")

# let's calculate total payroll per year for the Americal League (AL)
# save the query as a string
salary_query <- 
  "SELECT yearID, sum(salary) as total_payroll 
   FROM Salaries 
   WHERE lgID == 'AL'
   GROUP BY yearID"

# send the query to the database
query_result <- lahman_con %>% tbl(sql(salary_query))

# at this point the query is not computed completely. To load the result
# of the query as a table in R use the collect function
result <- collect(query_result)