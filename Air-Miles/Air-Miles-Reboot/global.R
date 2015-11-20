
#-------------------- Loading Libraries ------------------------#	

#Package(s) to extract data
#dtExtrPkg <- c("Rfacebook", "twitteR")
#sapply(dtExtrPkg, library, character.only = TRUE)
library(repmis)

#Packages to manipulate/analyse data
#dtManPkg <- c("plyr", "dplyr", "tidyr", "xts", "zoo", "lubridate", "topicmodels")
#sapply(dtManPkg, library, character.only = TRUE)
library(plyr)
library(dplyr)
library(tidyr)
library(xts)
library(zoo)
library(lubridate)
library(topicmodels)

#Packages for visualisations
#visPkg <- c("ggplot2", "wordcloud", "dygraphs", "scales", "RColorBrewer")
#sapply(visPkg, library, character.only = TRUE)
library(ggplot2)
library(scales)
library(wordcloud)
library(dygraphs)
library(RColorBrewer)


#Packages to develop an app
#appPkg <- c("shiny", "shinythemes", "shinydashboard", "shinyapps") 
#sapply(appPkg, library, character.only = TRUE)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyapps)
library(htmlwidgets)







#-------------------- Loading required data from GitHub Repo -------------------------#

baseUrl <- "https://github.com/aliarsalankazmi/AirMiles_FB_App_Reboot/raw/master/Data/"
source_data(paste0(baseUrl,'airmilesMEPageData.RData'))
source_data(paste0(baseUrl,'wordDataCommented.RData'))
source_data(paste0(baseUrl,'wordDataLiked.RData'))
source_data(paste0(baseUrl,'wordDataShared.RData'))
source_data(paste0(baseUrl, 'userComments.RData'))
source_data(paste0(baseUrl, 'likedDataDf.RData'))
source_data(paste0(baseUrl, 'twitterPage.RData'))
source_data(paste0(baseUrl, 'mostFavDf.RData'))
source_data(paste0(baseUrl, 'mostRetweetDf.RData'))
#source_data(paste0(baseUrl, 'postsData.RData'))
#load("C:\\Users\\kazami\\Desktop\\Aimia\\AirMiles\\Task2-FacebookApp\\postsData.RData")
#load("C:\\Users\\kazami\\Desktop\\Aimia\\AirMiles\\Task2-FacebookApp\\userComments.RData")
#load("C:\\Users\\kazami\\Desktop\\Aimia\\AirMiles\\Task2-FacebookApp\\likedDataDf.RData")
#load("C:\\Users\\kazami\\Desktop\\Aimia\\AirMiles\\Task2-FacebookApp\\twitterPage.RData")



#--------------------  Preliminary Manipulation of Data -------------------------#


airmilesMe <- tbl_df(airmilesMe)
airmilesMe$created_date <- ymd(gsub('T.*', '', airmilesMe$created_time))
airmilesMe <- arrange(airmilesMe, created_date)
airmilesMe$year_month <- as.yearmon(airmilesMe$created_date)
airmilesMe$month <- month(airmilesMe$created_date)
airmilesMe$monthName <- factor(month(airmilesMe$created_date, label = TRUE))
airmilesMe$quarter <- factor(quarter(airmilesMe$created_date))
airmilesMe$year <- factor(year(airmilesMe$created_date))
airmilesMe$day <- factor(wday(airmilesMe$created_date))
airmilesMe$dayName <- factor(wday(airmilesMe$created_date, label = TRUE), ordered = TRUE)
airmilesMe$week <- week(airmilesMe$created_date)
#airmilesMe <- ddply(airmilesMe, .(year_month), transform, month_week = 1+ week - min(week)) from: http://margintale.blogspot.com/2012/04/ggplot2-time-series-heatmaps.html


pageData <- airmilesMe






#--------------------  Preliminary theme function for GGplot2 -------------------------#


singleBarGeom <- function(myColour){geom_bar(stat = 'identity', fill = myColour)}
wordcloudColours <- brewer.pal(8, 'Dark2')






#--------------------  Preliminary function for capitalising strings -------------------------#


.simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}





#--------------------  Preliminary functions for formatting axis labels for Dyrgraph -------------------------#

#the axis label is passed as a date, this function outputs only the month of the date
#copied from: http://stackoverflow.com/questions/28915328/how-to-set-x-axis-in-dygraphs-for-r-to-show-just-month/28918684#28918684


getMonthYear <- 'function(d) {
                var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
                date = new Date(d);
                return monthNames[date.getMonth()] + " " +date.getFullYear(); }'

getYear <- 'function(d) {
                return d.getFullYear(); }'
