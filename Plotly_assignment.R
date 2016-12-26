library(devtools)
library(plotly)
library(ggplot2)

Sys.setenv("plotly_username" = "marcelotibau")
Sys.setenv("plotly_api_key" = "marcelot75")

download.file("https://www2.census.gov/programs-surveys/popest/datasets/2010-2016/national/totals/nst-est2016-alldata.csv", dest="nst-est2016-alldata.csv", mode="wb")

data <- read.csv("nst-est2016-alldata.csv")

data_us <- subset(data, select = c(NAME, CENSUS2010POP))

data_us <- data_us[c(2, 3, 4,5), ]

write.csv2(data_us, "data_us.csv")

data2 <- read.csv2("data_us.csv")

g <- ggplot(data2, aes(y = POPULATION, x = REGION), fill = REGION)
g <- g + geom_bar(stat = "identity") + ggtitle("U.S. Population (in Million) by Region") 

py <- ggplotly(g)
out <- py
out$response$url
#https://plot.ly/~marcelotibau/8/