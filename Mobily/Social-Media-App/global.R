

#-------------------- Loading Libraries ------------------------#	

library(Rfacebook)
library(repmis)
library(lubridate)
library(dplyr)
library(xts)
library(dygraphs)
library(metricsgraphics)
library(tm)
library(wordcloud)
library(ggplot2)



#-------------------- Loading required data from GitHub Repo -------------------------#

baseUrl <- 'https://github.com/aliarsalankazmi/Mobily_FB_App/raw/master/Data/'        #replacing /blob/ with /raw/
source_data(paste0(baseUrl,'mobilyPageData.RData'))
source_data(paste0(baseUrl,'mobilyWordDataCommented.RData'))
source_data(paste0(baseUrl,'mobilyWordDataLiked.RData'))
source_data(paste0(baseUrl,'mobilyWordDataShared.RData'))




#-------------------- Manipulating data for further processing -------------------------#

mobily$created_date <- ymd(gsub('T.*', '', mobily$created_time))
mobily$created_month <- paste(month(mobily$created_date),year(mobily$created_date), sep = '-')

pageData <- tbl_df(mobily) %>%
		arrange(created_date)


byDay <- pageData %>%
		group_by(created_date) %>%
		select(created_date, comments_count, likes_count, shares_count) %>%
		summarise(totalPosts = n(),
			  totalLikes = sum(likes_count),
			  totalComments = sum(comments_count),
			  totalShares = sum(shares_count)) %>%
		arrange(created_date)





#-------------------- Manipulating data for an Overall View  -------------------------#

byDayxts <- as.xts(x = as.matrix(as.data.frame(byDay[,colnames(byDay) != 'created_date'])), order.by = byDay$created_date)







#-------------------- Manipulating data for Monthly View  -------------------------#

pageData$created_month <- factor(pageData$created_month, levels = pageData$created_month, ordered = TRUE)
#pageData$created_month <- as.yearmon(pageData$created_month, '%m-%Y')



byMonth <- pageData %>%
		group_by(created_month) %>%
		select(created_month, comments_count, likes_count, shares_count) %>%
		summarise(totalPosts = n(),
			  totalLikes = sum(likes_count),
			  avgLikes = round(mean(likes_count)),
			  medLikes = round(median(likes_count)),
			  totalComments = sum(comments_count),
			  avgComments = round(mean(comments_count)),
			  medComments = round(median(comments_count)),
			  totalShares = sum(shares_count),
			  avgShares = round(mean(shares_count)),
			  medShares = round(median(shares_count))) %>%
		arrange(created_month)



byMonthxts <- as.xts(x = as.matrix(as.data.frame(byMonth[,colnames(byMonth) != 'created_month'])), 
order.by = as.yearmon(byMonth$created_month, '%m-%Y'))





#-------------------- Manipulating data for WeekDay View  -------------------------#

pageData$created_day <- wday(pageData$created_date, label = TRUE)

byWeekDay <- pageData %>%
		group_by(created_day) %>%
		select(created_day, comments_count, likes_count, shares_count) %>%
		summarise(totalPosts = n(),
			  totalLikes = sum(likes_count),
			  avgLikes = round(mean(likes_count)),
			  medLikes = round(median(likes_count)), 
			  totalComments = sum(comments_count),
			  avgComments = round(mean(comments_count)),
			  medComments = round(median(comments_count)),
			  totalShares = sum(shares_count),
			  avgShares = round(mean(shares_count)),
			  medShares = round(median(shares_count))) %>%
		arrange(created_day)





#-------------------- Setting up an aesthetic  -------------------------#

singleBarGeom <- function(myColour){geom_bar(stat = 'identity', fill = myColour, colour = 'black', size = 1)}









