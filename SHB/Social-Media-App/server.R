library(shiny)
library(dygraphs)
library(ggplot2)
library(wordcloud)

shinyServer(function(input, output, session) {


#---------------------- Plotting for a General Overview --------------------#

	output$totalOverview <- renderDygraph({
				byDayxts[, colnames(byDayxts) == 'totalPosts'] %>%
				dygraph(main = 'Total Posts per Day Since Beginning', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('totalPosts', label = 'Total Posts') %>%
				dyEvent(date = "2015-1-01", "New Tedallal Launch", labelLoc = "bottom") %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#d8b365') %>% 
				dyRangeSelector()
			})

	output$likedOverview <- renderDygraph({
				byDayxts[, colnames(byDayxts) == 'totalLikes'] %>%
				dygraph(main = 'Total Likes per Posts Since Beginning', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('totalLikes', label = 'Total Likes') %>%
				dyEvent(date = "2015-1-01", "New Tedallal Launch", labelLoc = "bottom") %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#FFCC00') %>% 
				dyRangeSelector()
			})

	output$commentedOverview <- renderDygraph({
				byDayxts[, colnames(byDayxts) == 'totalComments'] %>%
				dygraph(main = 'Total Comments per Posts Since Beginning', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('totalComments', label = 'Total Comments') %>%
				dyEvent(date = "2015-1-01", "New Tedallal Launch", labelLoc = "bottom") %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#31a354') %>% 
				dyRangeSelector()
			})

	output$sharedOverview <- renderDygraph({	
				byDayxts[, colnames(byDayxts) == 'totalShares'] %>%
				dygraph(main = 'Total Shares per Posts Since Beginning', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('totalShares', label = 'Total Shares') %>%
				dyEvent(date = "2015-1-01", "New Tedallal Launch", labelLoc = "bottom") %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#2c7fb8') %>% 
				dyRangeSelector()
			})



#---------------------- Plotting for a Monthly Overview --------------------#
	

	output$totalMonthly <- renderDygraph({
				byMonthxts[, colnames(byMonthxts) == c('totalPosts')] %>%
				dygraph(main = 'Total Posts in Each Month', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('totalPosts', label = 'avg Likes') %>%
				dyEvent(date = "2015-1-01", "New Tedallal Launch", labelLoc = "bottom") %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#d8b365') %>% 
				dyRangeSelector()
			})

	output$likedMonthly <- renderDygraph({
				byMonthxts[, colnames(byMonthxts) == c('avgLikes')] %>%
				dygraph(main = 'Average Likes per Post Received in Each Month', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgLikes', label = 'avg Likes') %>%
				dyEvent(date = "2015-1-01", "New Tedallal Launch", labelLoc = "bottom") %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#FFCC00') %>% 
				dyRangeSelector()
			})

	output$commentedMonthly <- renderDygraph({
				byMonthxts[, colnames(byMonthxts) == c('avgComments')] %>%
				dygraph(main = 'Average Comments per Post in Each Month', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgComments', label = 'avg Comments') %>%
				dyEvent(date = "2015-1-01", "New Tedallal Launch", labelLoc = "bottom") %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#31a354') %>% 
				dyRangeSelector()
			})

	output$sharedMonthly <- renderDygraph({
				byMonthxts[, colnames(byMonthxts) == c('avgShares')] %>%
				dygraph(main = 'Average Shares per Posts in Each Month', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgShares', label = 'avg Shares') %>%
				dyEvent(date = "2015-1-01", "New Tedallal Launch", labelLoc = "bottom") %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#2c7fb8') %>% 
				dyRangeSelector()
			})



#---------------------- Plotting for a WeekDay Overview --------------------#

	output$totalWeekday <- renderPlot({
				byWeekDay %>% 
					ggplot(aes(x = created_day, y = totalPosts)) + singleBarGeom(myColour = '#d8b365') + theme_bw() + 
					xlab('') + ylab('') + ggtitle('Total Posts')
			})

	output$likedWeekday <- renderPlot({
				byWeekDay %>% 
					ggplot(aes(x = created_day, y = avgLikes)) + singleBarGeom(myColour =  '#FFCC00') + theme_bw() + 
					xlab('') + ylab('') + ggtitle('Average Likes')
			})

	output$commentedWeekday <- renderPlot({
				byWeekDay %>% 
					ggplot(aes(x = created_day, y = avgComments)) + singleBarGeom(myColour =  '#31a354')  + theme_bw() + 
					xlab('') + ylab('') + ggtitle('Average Comments')
			})

	output$sharedWeekday <- renderPlot({
				byWeekDay %>% 
					ggplot(aes(x = created_day, y = avgShares)) + singleBarGeom(myColour =  '#2c7fb8')  + theme_bw() + 
					xlab('') + ylab('') + ggtitle('Average Shares')
			})




#---------------------- Plotting for Correlations --------------------#

	output$corrPlot1 <- renderPlot({
				pageData %>% 
					ggplot(aes(x = likes_count, y = comments_count)) + geom_point(colour = '#d8b365', size = 4, alpha = .5) + 
					theme_bw() + xlab('Total Likes') + ylab('Total Comments')
			})

	output$corrPlot2 <- renderPlot({
				pageData %>% 
					ggplot(aes(x = likes_count, y = shares_count)) + geom_point(colour = '#31a354', size = 4, alpha = .5) + 
					theme_bw() + xlab('Total Likes') + ylab('Total Shares')
			})

	output$corrPlot3 <- renderPlot({
				pageData %>% 
					ggplot(aes(x = shares_count, y = comments_count)) + geom_point(colour = '#2c7fb8', size = 4, alpha = .5) + 
					theme_bw() + xlab('Total Shares') + ylab('Total Comments')
			})



#---------------------- Plotting for Wordclouds --------------------#

	colours <- brewer.pal(8, 'Dark2')

	output$likedWords <- renderPlot({
				wordcloud(words = likedDf$words, freq = likedDf$freq, random.order = FALSE, random.color = FALSE, colors = colours)
			})

	output$commentedWords <- renderPlot({
				wordcloud(words = commentedDf$words, freq = commentedDf$freq, random.order = FALSE, random.color = FALSE, colors = colours)		})

	output$sharedWords <- renderPlot({
				wordcloud(words = sharedDf$words, freq = sharedDf$freq, random.order = FALSE, random.color = FALSE, colors = colours)
			})




})
