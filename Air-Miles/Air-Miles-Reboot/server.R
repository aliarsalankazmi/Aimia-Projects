

shinyServer(function(input, output, session) {








#--------------------  Generating and Plotting a Calendar Heatmap of Data -------------------------#


withProgress(message = "Calculating a Calendar Heatmap for Facebook Data", value = 0, max = 10,
expr = {
calendarDataFb <- pageData %>%
			mutate(yearNum = year(created_date)) %>%
			filter(yearNum >= max(yearNum) - 5) %>%
			select(year, monthName, dayName, id, likes_count, comments_count, shares_count, message, link) %>%
			group_by(year, monthName, dayName) %>%
			summarise(totalLikes = sum(likes_count),
				  totalComments = sum(comments_count),
				  totalShares = sum(shares_count),
				  Data = paste("Post:", message, "\nLikes:", likes_count, "Shares:", shares_count, "Comments:", comments_count, 
				  "Link:", link, collapse = "\n\n")) %>%
			arrange(year, monthName, dayName) %>%
			ungroup() %>%
			mutate(#logTenLikes = log10(totalLikes),
				#logTenComments = log10(totalComments),
				#logTenShares = log10(totalShares),
				totalLikesComments = totalLikes + totalComments,
				logTenLikesComments = log10(totalLikesComments)) %>%
			as.data.frame()
		incProgress(amount = 5)
		Sys.sleep(2)
		incProgress(amount = 5)

})




output$fbHeatmap <- renderPlot({

			withProgress(message = "Generating a Calendar Heatmap for Facebook Data", value = 0, max = 10,
			  expr = {
				incProgress(amount = 2)
				fbhmap <- calendarDataFb %>%
				    	ggplot(aes(x = monthName, y = dayName, fill = logTenLikesComments)) +
					geom_tile()
				fbhmap <- fbhmap + geom_point(aes(size = totalShares), colour = "black") + 
				    		scale_fill_gradient(low = "white", high = "#0099FF",
								    #high = "#009900", 
								    name = "Likes & Comments",
								    breaks = c(1, 2, 3, 4),
								    labels = expression(10^1, 10^2, 10^3, 10^4)) +
				    		facet_wrap(~ year, ncol = 1) +
				    		theme_bw() +
				    		theme(legend.position = "right", strip.text = element_text(size = rel(1.2), face = "bold"), 
							strip.background = element_rect(fill = "white")) +
				    		labs(x = "", y = "") + 
						ggtitle("Facebook Calendar Heatmap") +
						theme(plot.title = element_text(size = rel(1.4))) +
				    		scale_size_continuous(name = "Shares")
				incProgress(amount = 6)
				Sys.sleep(2.5)
				incProgress(amount = 2)

				fbhmapDownload <<- function(){fbhmap}
				fbhmap
					
				})
			})




output$fbMapLabels <- renderTable({
			
			nearPoints(calendarDataFb, input$fbCMapDClick, threshold = 5, maxpoints = 1)

			})


output$fbCmap_Btn <- downloadHandler(
				filename = 'Facebook Calendar Heatmap.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = fbhmapDownload(), device = device, width = 10, height = 8)
      })











withProgress(message = "Calculating a Calendar Heatmap for Twitter Data", value = 0, max = 10,
expr = {
calendarDataTw <- twitterData %>%
			mutate(yearNum = year(created)) %>%
			filter(yearNum >= max(yearNum) - 5) %>%
			select(year, monthName, dayName, id, favoriteCount, retweetCount, text) %>%
			group_by(year, monthName, dayName) %>%
			summarise(totalFavourites = sum(favoriteCount),
				  totalRetweets = sum(retweetCount),
				  Data = paste("Post:", text, "\nFavourites:", favoriteCount, "Retweets:", retweetCount, collapse = "\n\n")) %>%
			arrange(year, monthName, dayName) %>%
			#ungroup() %>%
			#mutate(totalFavRetweets = totalFavourites + totalRetweets,
			#	logTenFavRetweets = log10(totalFavRetweets),
			#	logTenFavourites = log10(totalFavourites),
			#	logTenRetweets = log10(totalRetweets)) %>%
			as.data.frame()
		incProgress(amount = 5)
		Sys.sleep(2)
		incProgress(amount = 5)

})



output$twHeatmap <- renderPlot({

			withProgress(message = "Generating a Calendar Heatmap for Twitter Data", value = 0, max = 10,
			  expr = {
				incProgress(amount = 2)
				twhmap <- calendarDataTw %>%
				    	ggplot(aes(x = monthName, y = dayName, fill = totalFavourites)) +
					geom_tile()
				twhmap <- twhmap + geom_point(aes(size = totalRetweets), colour = "black") + 
				    		scale_fill_gradient(low = "white", high = "#0099FF",
								    #high = "#009900", 
								    name = "Favourites") +
				    		facet_wrap(~ year, ncol = 1) +
				    		theme_bw() +
				    		theme(legend.position = "right", strip.text = element_text(size = rel(1.2), face = "bold"), 
							strip.background = element_rect(fill = "white")) +
				    		labs(x = "", y = "") + 
						ggtitle("Twitter Calendar Heatmap") +
						theme(plot.title = element_text(size = rel(1.4))) +
				    		scale_size_continuous(name = "Retweets")
				incProgress(amount = 6)
				Sys.sleep(2.5)
				incProgress(amount = 2)

				twhmapDownload <<- function(){twhmap}
				twhmap
					
				})
			})



output$twMapLabels <- renderTable({
			
			nearPoints(calendarDataTw, input$twCMapDClick, threshold = 5, maxpoints = 1)

			})


output$twCmap_Btn <- downloadHandler(
				filename = 'Twitter Calendar Heatmap.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = twhmapDownload(), device = device, width = 10, height = 8)
      })











#---------------------- Plotting Bar graphs/Violin diagrams/Boxplots for Overall activity--------------------#



output$fbActivityRatio <- renderPlot({

				withProgress(message = "Generating Activity Bar graphs", value = 0, max = 10,
				expr = {
					incProgress(amount = 5)
					if(input$fbActRatioRBtn == "overall"){

					fbActPlot <- pageData %>% 
							select(likes_count, shares_count, comments_count) %>% 
							rename_("Likes" = "likes_count", "Shares" = "shares_count", "Comments" = "comments_count") %>%
							summarise_each(funs(sum)) %>% 
							gather() %>% 
							mutate(overallProp = value/sum(value)) %>%
							ggplot(aes(x = reorder(key, overallProp), y = overallProp)) +
							singleBarGeom(myColour = '#0099FF') + 
							scale_y_continuous(labels = percent) + 
							theme_bw() + xlab('Activity') + ylab('Proportion') + ggtitle('Facebook Proportion of Activities')

					} else if(input$fbActRatioRBtn == "yearly"){

					fbActPlot <- pageData %>% 
							mutate(yearNum = year(created_date)) %>%
							filter(yearNum >= max(yearNum) - 5) %>%
							select(year, likes_count, shares_count, comments_count) %>% 
							group_by(year) %>%
							summarise(Likes = sum(likes_count),
								  Shares = sum(shares_count),
								  Comments = sum(comments_count)) %>% 
							gather(Activity, count, -year) %>% 
							arrange(year) %>%
							group_by(year) %>%
							mutate(overallProp = count/sum(count)) %>%
							ggplot(aes(x = year, y = overallProp)) +
							geom_bar(aes(fill = Activity), position = "fill", stat = "identity") + 
							scale_y_continuous(labels = percent) + 
							scale_fill_brewer(palette="Paired") +
							#scale_fill_manual(values = c("#006BB2", "#0099FF", "#80CCFF")) +
							theme_bw() + xlab('Year') + ylab('Proportion') + ggtitle('Facebook Proportion of Activities')

					} else{

					fbActPlot <- pageData %>% 
							select(created_time, quarter, likes_count, shares_count, comments_count) %>%
							mutate(year = year(created_time)) %>%
							filter(year >= max(year) - 5) %>%
							group_by(year, quarter) %>%
							summarise(Likes = sum(likes_count),
								  Shares = sum(shares_count),
								  Comments = sum(comments_count)) %>% 
							gather(Activity, count, -year, -quarter) %>% 
							arrange(year, quarter) %>%
							group_by(year, quarter) %>%
							mutate(overallProp = count/sum(count)) %>%
							filter(!is.nan(overallProp)) %>%
							ggplot(aes(x = quarter, y = overallProp)) +
							geom_bar(aes(fill = Activity), position = "fill", stat = "identity") + 
							scale_y_continuous(labels = percent) + 
							scale_fill_brewer(palette="Paired") +
							#scale_fill_manual(values = c("#006BB2", "#0099FF", "#80CCFF")) +
							theme_bw() + xlab('Quarter') + ylab('Proportion') + ggtitle('Facebook Proportion of Activities') +				
							facet_wrap(~year, ncol = 2)
					}
				Sys.sleep(1.5)
				incProgress(amount = 5)

				fbActPlotDownload <<- function(){fbActPlot}
				fbActPlot				

				})
			})



output$fbActRatio_Btn <- downloadHandler(
				filename = 'Facebook Activity Proportions.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = fbActPlotDownload(), device = device, width = 10, height = 8)
      })



output$fbLikes_violin <- renderPlot({

				withProgress(message = "Generating Boxplots", value = 0, max = 10,
				expr = {
					incProgress(amount = 5)
					if(input$fblikeRBtn == "yearly"){
					   likesBPlot <- pageData %>% 
						mutate(yearNum = year(created_date)) %>%
						filter(yearNum >= max(yearNum) - 5) %>%
						select(year, likes_count) %>%
     						ggplot(aes(x = year, y = likes_count)) +
    						#geom_violin(fill = "#0099FF") + 
						geom_boxplot(width = .2, colour = "#0099FF", outlier.colour = "#0099FF") +
     						theme_minimal() +
     						labs(y = "", x = "Year") + 
						ggtitle(expression(paste(Log[10],"(Likes)", " on Facebook Posts")))  +
						theme(plot.title = element_text(size = rel(1.4))) +
						scale_y_log10(labels = trans_format("log10", math_format(10^.x)), breaks = trans_breaks("log10", function(x) 10^x))
					} else{
					    likesBPlot <- pageData %>% 
						mutate(yearNum = year(created_date)) %>%
						filter(yearNum >= max(yearNum) - 5) %>% 
						select(year, quarter, likes_count) %>%
     						ggplot(aes(x = quarter, y = likes_count)) +
     						#geom_violin(fill = "#0099FF") +
     						geom_boxplot(width = .2, colour = "#0099FF", outlier.colour = "#0099FF") +
     						theme_bw() +
     						facet_wrap(~ year, ncol = 3) +
     						labs(y = "", x = "Quarter") + 
						ggtitle(expression(paste(Log[10],"(Likes)", " on Facebook Posts")))  +
						theme(plot.title = element_text(size = rel(1.4)), strip.text.x = element_text(size = rel(1.3))) +
						scale_y_log10(labels = trans_format("log10", math_format(10^.x)), breaks = trans_breaks("log10", function(x) 10^x))
					}
				Sys.sleep(1.5)

				likesBPlotDownload <<- function(){likesBPlot}
				likesBPlot

				})
			})



output$fbLikesV_Btn <- downloadHandler(
				filename = 'Facebook Likes Boxplots.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = likesBPlotDownload(), device = device, width = 10, height = 8)
      })




output$fbComments_violin <- renderPlot({
				if(input$fbcommentRBtn == "yearly"){
					commentsBPlot <- pageData %>%  
					mutate(yearNum = year(created_date)) %>%
					filter(yearNum >= max(yearNum) - 5) %>%
					select(year, comments_count) %>%
     					ggplot(aes(x = year, y = comments_count)) +
     					#geom_violin(fill = "#0099FF") + 
					geom_boxplot(width = .2, colour = "#0099FF", outlier.colour = "#0099FF") +
     					theme_minimal() +
     					labs(y = "", x = "Year") +
					ggtitle(expression(paste(Log[10],"(Comments)", " on Facebook Posts"))) +
					theme(plot.title = element_text(size = rel(1.3))) +
					scale_y_log10(labels = trans_format("log10", math_format(10^.x)), breaks = trans_breaks("log10", function(x) 10^x))
				} else{
					commentsBPlot <- pageData %>% 
					mutate(yearNum = year(created_date)) %>%
					filter(yearNum >= max(yearNum) - 5) %>% 
					select(year, quarter, comments_count) %>%
     					ggplot(aes(x = quarter, y = comments_count)) +
     					#geom_violin(fill = "#0099FF") +
     					geom_boxplot(width = .2, colour = "#0099FF", outlier.colour = "#0099FF") +
     					theme_bw() +
     					facet_wrap(~ year, ncol = 3) +
     					labs(y = "", x = "Quarter") + 
					ggtitle(expression(paste(Log[10],"(Comments)", " on Facebook Posts")))  +
					theme(plot.title = element_text(size = rel(1.4)), strip.text.x = element_text(size = rel(1.3))) +
					scale_y_log10(labels = trans_format("log10", math_format(10^.x)), breaks = trans_breaks("log10", function(x) 10^x))
				}

				commentsBPlotDownload <<- function(){commentsBPlot}
				commentsBPlot
				
			})




output$fbCommentsV_Btn <- downloadHandler(
				filename = 'Facebook Comments Boxplots.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = commentsBPlotDownload(), device = device, width = 10, height = 8)
      })




output$fbShares_violin <- renderPlot({

				if(input$fbshareRBtn == "yearly"){
					sharesBPlot <- pageData %>% 
					mutate(yearNum = year(created_date)) %>%
					filter(yearNum >= max(yearNum) - 5) %>% 
					select(year, shares_count) %>%
     					ggplot(aes(x = year, y = shares_count)) +
     					#geom_violin(fill = "#0099FF") +
     					geom_boxplot(width = .2, colour = "#0099FF", outlier.colour = "#0099FF") +
     					theme_minimal() +
     					labs(y = "", x = "Year") +
					ggtitle(expression(paste(Log[10],"(Shares)", " on Facebook Posts"))) +
					theme(plot.title = element_text(size = rel(1.3))) +
					scale_y_log10(labels = trans_format("log10", math_format(10^.x)), breaks = trans_breaks("log10", function(x) 10^x))
				} else{
					sharesBPlot <- pageData %>% 
					mutate(yearNum = year(created_date)) %>%
					filter(yearNum >= max(yearNum) - 5) %>% 
					select(year, quarter, shares_count) %>%
     					ggplot(aes(x = quarter, y = shares_count)) +
     					#geom_violin(fill = "#0099FF") +
     					geom_boxplot(width = .2, colour = "#0099FF", outlier.colour = "#0099FF") +
     					theme_bw() +
     					facet_wrap(~ year, ncol = 3) +
     					labs(y = "", x = "Quarter") + 
					ggtitle(expression(paste(Log[10],"(Shares)", " on Facebook Posts")))  +
					theme(plot.title = element_text(size = rel(1.4)), strip.text.x = element_text(size = rel(1.3))) +
					scale_y_log10(labels = trans_format("log10", math_format(10^.x)), breaks = trans_breaks("log10", function(x) 10^x))
				}

				sharesBPlotDownload <<- function(){sharesBPlot}
				sharesBPlot

			})




output$fbSharesV_Btn <- downloadHandler(
				filename = 'Facebook Shares Boxplots.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = sharesBPlotDownload(), device = device, width = 10, height = 8)
      })






output$twActivityRatio <- renderPlot({

				withProgress(message = "Generating Activity Bar graphs", value = 0, max = 10,
				expr = {
					incProgress(amount = 5)
					if(input$twActRatioRBtn == "overall"){

					twActRatioPlot <- twitterData %>%
								select(retweetCount, favoriteCount) %>% 
								rename_("Retweets" = "retweetCount", "Favourites" = "favoriteCount") %>%
								summarise_each(funs(sum)) %>% 
								gather() %>% 
								mutate(overallProp = value/sum(value)) %>%
								ggplot(aes(x = reorder(key, overallProp), y = overallProp)) +
								singleBarGeom(myColour = '#0099FF') + 
								scale_y_continuous(labels = percent) + 
								theme_bw() + xlab('Activity') + ylab('Proportion') + ggtitle('Twitter Proportion of Activities')
								

					} else if(input$twActRatioRBtn == "yearly"){

					twActRatioPlot <- twitterData %>%
								mutate(year = year(created)) %>%
								filter(year >= max(year) - 5) %>%
								select(year, retweetCount, favoriteCount) %>% 
								group_by(year) %>%
								summarise(Retweets = sum(retweetCount),
								 	  Favourites = sum(favoriteCount)) %>% 
								gather(Activity, count, -year) %>% 
								arrange(year) %>%
								group_by(year) %>%
								mutate(overallProp = count/sum(count)) %>%
								ggplot(aes(x = year, y = overallProp)) +
								geom_bar(aes(fill = Activity), position = "fill", stat = "identity") + 
								scale_y_continuous(labels = percent) + 
								scale_fill_brewer(palette="Paired") +
								#scale_fill_manual(values = c("#006BB2", "#0099FF", "#80CCFF")) +
								theme_bw() + xlab('Year') + ylab('Proportion') + ggtitle('Twitter Proportion of Activities')


					} else{

					twActRatioPlot <- twitterData %>%
								select(created, quarter, retweetCount, favoriteCount) %>%
								mutate(year = year(created)) %>%
								filter(year >= max(year) - 5) %>%
								group_by(year, quarter) %>%
								summarise(Retweets = sum(retweetCount),
									  Favourites = sum(favoriteCount)) %>% 
								gather(Activity, count, -year, -quarter) %>% 
								arrange(year, quarter) %>%
								group_by(year, quarter) %>%
								mutate(overallProp = count/sum(count)) %>%
								filter(!is.nan(overallProp)) %>%
								ggplot(aes(x = factor(quarter), y = overallProp)) +
								geom_bar(aes(fill = Activity), position = "fill", stat = "identity") + 
								scale_y_continuous(labels = percent) + 
								scale_fill_brewer(palette="Paired") +
								#scale_fill_manual(values = c("#006BB2", "#0099FF", "#80CCFF")) +
								theme_bw() + xlab('Quarter') + ylab('Proportion') + ggtitle('Twitter Proportion of Activities') +				
								facet_wrap(~year, ncol = 2)

					}
					Sys.sleep(1.5)
					incProgress(amount = 5)
					twActRatioPlotDownload <<- function(){twActRatioPlot}
					twActRatioPlot
				})
			})				





output$twActRatio_Btn <- downloadHandler(
				filename = 'Twitter Activity Proportions.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = twActRatioPlotDownload(), device = device, width = 10, height = 8)
      })







output$twFavourites_violin <- renderPlot({

				withProgress(message = "Generating Histograms", value = 0, max = 10,
				expr = {
					incProgress(amount = 5)
					if(input$twFavRBtn == "yearly"){
					   favsBPlot <- twitterData %>%
						mutate(year = year(created)) %>%
						filter(year >= max(year) - 5) %>% 
						select(year, favoriteCount) %>%
     						ggplot(aes(x = favoriteCount)) +
    						geom_histogram(fill = "#0099FF") +
     						theme_minimal() +
						facet_wrap(~ year, ncol = 3) + 
     						labs(y = "Counts", x = "Number of Favourites") + 
						ggtitle("Count of the number of favourites made on each Twitter Post")  +
						theme(plot.title = element_text(size = rel(1.4)))
					} else{
					    favsBPlot <- twitterData %>%
						mutate(year = year(created)) %>%
						filter(year >= max(year) - 5) %>% 
						select(year, quarter, favoriteCount) %>%
     						ggplot(aes(x = favoriteCount)) +
    						geom_histogram(fill = "#0099FF") +
     						theme_bw() +
						facet_grid(quarter ~ year) +
     						labs(y = "Counts", x = "Number of Favourites") + 
						ggtitle("Count of the number of favourites made on each Twitter Post")  +
						theme(plot.title = element_text(size = rel(1.4)))
					}
				Sys.sleep(1.5)

				favsBPlotDownload <<- function(){favsBPlot}
				favsBPlot

				})
			})



output$twFav_Btn <- downloadHandler(
				filename = 'Twitter Favourites Histogram.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = favsBPlotDownload(), device = device, width = 10, height = 8)
      })




output$twRetweets_violin <- renderPlot({
				if(input$twRetweetRBtn == "yearly"){
					   retweetBPlot <- twitterData %>% 
						mutate(year = year(created)) %>%
						filter(year >= max(year) - 5) %>%
						select(year, retweetCount) %>%
     						ggplot(aes(x = retweetCount)) +
    						geom_histogram(fill = "#0099FF") +
     						theme_minimal() +
						facet_wrap(~ year, ncol = 3) + 
     						labs(y = "Counts", x = "Number of Retweets") + 
						ggtitle("Count of the number of Retweets on each Twitter Post")  +
						theme(plot.title = element_text(size = rel(1.4)))
					} else{
					    retweetBPlot <- twitterData %>% 
						mutate(year = year(created)) %>%
						filter(year >= max(year) - 5) %>%
						select(year, quarter, retweetCount) %>%
     						ggplot(aes(x = retweetCount)) +
    						geom_histogram(fill = "#0099FF") +
     						theme_bw() +
						facet_grid(quarter ~ year) + 
     						labs(y = "Counts", x = "Number of Retweets") + 
						ggtitle("Count of the number of Retweets on each Twitter Post") +
						theme(plot.title = element_text(size = rel(1.4)))
					}

				retweetBPlotDownload <<- function(){retweetBPlot}
				retweetBPlot
				
			})




output$twRetweet_Btn <- downloadHandler(
				filename = 'Twitter Retweets Histogram.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = retweetBPlotDownload(), device = device, width = 10, height = 8)
      })









#---------------------- Plotting Dygraphs for Monthly/daily data --------------------#



######Calculations for each year
byYear <- pageData %>%
		group_by(year) %>%
		select(year, comments_count, likes_count, shares_count) %>%
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
		arrange(year)

#Generating an xts object for plotting yearly dygraphs
byYearxts <- as.xts(x = as.matrix(as.data.frame(byYear[,colnames(byYear) != 'year'])), 
order.by = parse_date_time(byYear$year, "%y")) 



######Calculations for each quarter per year
byYearQuarter <- pageData %>%
		group_by(year, quarter) %>%
		select(year, quarter, comments_count, likes_count, shares_count) %>%
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
		ungroup() %>%
		mutate(yearQuarter = as.Date(as.yearqtr(paste(year, quarter, sep = "-")))) %>%
		arrange(year, quarter)

#Generating an xts object for plotting yearly dygraphs
byYearQuarterxts <- as.xts(x = as.matrix(as.data.frame(byYearQuarter[,colnames(byYearQuarter) != "yearQuarter"])), 
order.by = byYearQuarter$yearQuarter) 


######Calculations for monthly view
byMonth <- pageData %>%
		group_by(year_month) %>%
		select(year_month, comments_count, likes_count, shares_count) %>%
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
		arrange(year_month)


#Generating an xts object for plotting monthly dygraphs
byMonthxts <- as.xts(x = as.matrix(as.data.frame(byMonth[,colnames(byMonth) != 'year_month'])), 
order.by = byMonth$year_month) 
monthMaxDate <- as.character(byMonth$year_month[nrow(byMonth)])
monthMaxDate <- parse_date_time(paste('01', monthMaxDate, sep = '-'), "dmY")
monthMinDate <- as.character(byMonth$year_month[nrow(byMonth) - 4])
monthMinDate <- parse_date_time(paste('01', monthMinDate, sep = '-'), "dmY")





#######Calculations for daily view
byDay <- pageData %>%
		group_by(created_date) %>%
		select(created_date, comments_count, likes_count, shares_count) %>%
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
		arrange(created_date)


#Generating an xts object for plotting daily dygraphs
byDayxts <- as.xts(x = as.matrix(as.data.frame(byDay[,colnames(byDay) != 'created_date'])), order.by = byDay$created_date)
dailyMaxDate <- max(byDay$created_date)
dailyMinDate <- dailyMaxDate %m-% months(4)







output$fbDygraph_totalPosts <- renderDygraph({

		#withProgress(message = "Generating Dygraphs", value = 0, max = 10,
		#expr = {
		#incProgress(amount = 2)
		if(input$fbDygTpRBtn == "monthly"){
			byMonthxts[, colnames(byMonthxts) == c('totalPosts')] %>%
				dygraph(main = 'Total Posts in Each Month', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('totalPosts', label = 'Total Posts') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>% 
				dyAxis("x",valueFormatter=JS(getMonthYear), axisLabelFormatter=JS(getMonthYear)) %>%
				dyRangeSelector(dateWindow = c(monthMinDate, monthMaxDate))
		}else if(input$fbDygTpRBtn == "daily"){
			byDayxts[, colnames(byDayxts) == 'totalPosts'] %>%
				dygraph(main = 'Total Posts per Day Since Beginning', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('totalPosts', label = 'Total Posts') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>% 
				dyRangeSelector(dateWindow = c(dailyMinDate, dailyMaxDate))
		}else if(input$fbDygTpRBtn == "quarterly"){
			byYearQuarterxts[, colnames(byYearQuarterxts) == 'totalPosts'] %>%
				dygraph(main = 'Total Posts per Quarter per Year', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('totalPosts', label = 'Total Posts') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF')
		}
		else {
			byYearxts[, colnames(byYearxts) == c('totalPosts')] %>%
				dygraph(main = 'Total Posts per Year', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('totalPosts', label = 'avg Likes') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>% 
				dyAxis("x", axisLabelFormatter=JS(getYear))
		}
		#incProgress(amount = 3)
		#Sys.sleep(1.5)
		#incProgress(amount = 5)
	#})

})


output$fbDygraph_avgLikes <- renderDygraph({

		#withProgress(message = "Generating Dygraphs", value = 0, max = 10,
		#expr = {
		#incProgress(amount = 2)
		if(input$fbDygAlRBtn == "monthly"){
			byMonthxts[, colnames(byMonthxts) == c('avgLikes')] %>%
				dygraph(main = 'Average Likes per Post Received in Each Month', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgLikes', label = 'Average Likes') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>% 
				dyAxis("x",valueFormatter=JS(getMonthYear), axisLabelFormatter=JS(getMonthYear)) %>%
				dyRangeSelector(dateWindow = c(monthMinDate, monthMaxDate))
		}else if(input$fbDygAlRBtn == "daily"){
			byDayxts[, colnames(byDayxts) == 'avgLikes'] %>%
				dygraph(main = 'Average Likes per Post Daily', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgLikes', label = 'Average Likes') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>% 
				dyRangeSelector(dateWindow = c(dailyMinDate, dailyMaxDate))
		}else if(input$fbDygAlRBtn == "quarterly"){
			byYearQuarterxts[, colnames(byYearQuarterxts) == 'avgLikes'] %>%
				dygraph(main = 'Average Likes per Post Quarterly', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgLikes', label = 'Average Likes') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF')
		}
		else {
			byYearxts[, colnames(byYearxts) == c('avgLikes')] %>%
				dygraph(main = 'Average Likes per Post Yearly', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgLikes', label = 'Average Likes') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>% 
				dyAxis("x", axisLabelFormatter=JS(getYear))

		}
		#incProgress(amount = 3)
		#Sys.sleep(1.5)
		#incProgress(amount = 5)
	#})

})





output$fbDygraph_avgComments <- renderDygraph({

		#withProgress(message = "Generating Dygraphs", value = 0, max = 10,
		#expr = {
		#incProgress(amount = 2)
		if(input$fbDygAcRBtn == "monthly"){
			byMonthxts[, colnames(byMonthxts) == c('avgComments')] %>%
				dygraph(main = 'Average Comments per Post Received in Each Month', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgComments', label = 'Average Comments') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>% 
				dyAxis("x",valueFormatter=JS(getMonthYear), axisLabelFormatter=JS(getMonthYear)) %>% 
				dyRangeSelector(dateWindow = c(monthMinDate, monthMaxDate))
		}else if(input$fbDygAcRBtn == "daily"){
			byDayxts[, colnames(byDayxts) == 'avgComments'] %>%
				dygraph(main = 'Average Comments per Post Daily', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgComments', label = 'Average Likes') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>% 
				dyRangeSelector(dateWindow = c(dailyMinDate, dailyMaxDate))
		}else if(input$fbDygAcRBtn == "quarterly"){
			byYearQuarterxts[, colnames(byYearQuarterxts) == 'avgComments'] %>%
				dygraph(main = 'Average Comments per Post Quarterly', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgComments', label = 'Average Comments') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF')
		}
		else {
			byYearxts[, colnames(byYearxts) == c('avgComments')] %>%
				dygraph(main = 'Average Comments per Post Yearly', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgComments', label = 'Average Comments') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>% 
				dyAxis("x", axisLabelFormatter=JS(getYear))

		}
		#incProgress(amount = 3)
		#Sys.sleep(1.5)
		#incProgress(amount = 5)
	#})

})




output$fbDygraph_avgShares <- renderDygraph({

		#withProgress(message = "Generating Dygraphs", value = 0, max = 10,
		#expr = {
		#incProgress(amount = 2)
		if(input$fbDygAsRBtn == "monthly"){
			byMonthxts[, colnames(byMonthxts) == c('avgShares')] %>%
				dygraph(main = 'Average Shares per Post Received in Each Month', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgShares', label = 'Average Shares') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>%  
				dyAxis("x",valueFormatter=JS(getMonthYear), axisLabelFormatter=JS(getMonthYear)) %>%
				dyRangeSelector(dateWindow = c(monthMinDate, monthMaxDate))
		}else if(input$fbDygAsRBtn == "daily"){
			byDayxts[, colnames(byDayxts) == 'avgShares'] %>%
				dygraph(main = 'Average Shares per Post Daily', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgShares', label = 'Average Shares') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>% 
				dyRangeSelector(dateWindow = c(dailyMinDate, dailyMaxDate))
		}else if(input$fbDygAsRBtn == "quarterly"){
			byYearQuarterxts[, colnames(byYearQuarterxts) == 'avgShares'] %>%
				dygraph(main = 'Average Shares per Post Quarterly', group = 'overall') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgShares', label = 'Average Shares') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF')
		}
		else {
			byYearxts[, colnames(byYearxts) == c('avgShares')] %>%
				dygraph(main = 'Average Shares per Post Yearly', group = 'averaged') %>%
				dyAxis('x', drawGrid = FALSE) %>%
				dySeries('avgShares', label = 'Average Shares') %>%
				dyOptions(includeZero = TRUE, gridLineColor = "lightblue", colors = '#0099FF') %>% 
				dyAxis("x", axisLabelFormatter=JS(getYear))

		}
		#incProgress(amount = 3)
		#Sys.sleep(1.5)
		#incProgress(amount = 5)
	#})

})








#---------------------- Plotting Weekday Activities --------------------#



byWeekDay_overall <- pageData %>%
			group_by(dayName) %>%
			select(day, comments_count, likes_count, shares_count) %>%
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
			arrange(dayName)


byWeekDay_yearly <- pageData %>%
			mutate(yearNum = year(created_date)) %>%
			filter(yearNum >= max(yearNum) - 5) %>%
			group_by(year, dayName) %>%
			select(year, day, comments_count, likes_count, shares_count) %>%
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
			arrange(year, dayName)


byWeekDay_quarterly <- pageData %>%
			mutate(yearNum = year(created_date)) %>%
			filter(yearNum >= max(yearNum) - 5) %>%
			group_by(year, quarter, dayName) %>%
			select(year, day, comments_count, likes_count, shares_count) %>%
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
			arrange(year, quarter, dayName)


byWeekDay_monthly <- pageData %>%
			mutate(yearNum = year(created_date)) %>%
			filter(yearNum >= max(yearNum) - 2) %>%
			group_by(year, monthName, dayName) %>%
			select(year, day, comments_count, likes_count, shares_count) %>%
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
			arrange(year, monthName, dayName)





output$weekAct_totalPosts <- renderPlot({

			withProgress(message = "Generating Weekly Activity Graphs", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)

				if(input$fbweekActTpRBtn == "overall"){
				
				weekAct_graphTp <- byWeekDay_overall %>% 
					ggplot(aes(x = dayName, y = totalPosts)) + 
					singleBarGeom(myColour = '#0099FF') + 
					theme_bw() + xlab('') + ylab('') + ggtitle('Total Posts')	

				}else if(input$fbweekActTpRBtn == "yearly"){

				weekAct_graphTp <- byWeekDay_yearly %>% 
					ggplot(aes(x = dayName, y = totalPosts)) + 
					singleBarGeom(myColour = '#0099FF') + 
					theme_bw() + xlab('') + ylab('') + ggtitle('Total Posts') +
					facet_wrap(~year, ncol = 3)

				}else if(input$fbweekActTpRBtn == "quarterly"){

				weekAct_graphTp <- byWeekDay_quarterly %>% 
					ggplot(aes(x = dayName, y = totalPosts)) + 
					singleBarGeom(myColour = '#0099FF') + 
					theme_bw() + xlab('') + ylab('') + ggtitle('Total Posts') + 
					facet_grid(year ~ quarter) + 
					theme(axis.text.x = element_text(angle = 55, vjust = .5))

				}else {

				weekAct_graphTp <- byWeekDay_monthly %>% 
					ggplot(aes(x = dayName, y = totalPosts)) + 
					singleBarGeom(myColour = '#0099FF') + 
					theme_bw() + xlab('') + ylab('') + ggtitle('Total Posts') + 
					facet_grid(year ~ monthName) + 
					theme(axis.text.x = element_text(angle = 90, vjust = .5))

				}
			incProgress(amount = 3)
			Sys.sleep(1)
			incProgress(amount = 5)

			weekActTpDownload <<- function(){weekAct_graphTp}
			weekAct_graphTp
		
		})
})



output$fbWeekTp_Btn <- downloadHandler(
				filename = 'Total Facebook Posts.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = weekActTpDownload(), device = device, width = 10, height = 8)
      })




output$weekAct_avgLikes <- renderPlot({

			withProgress(message = "Generating Weekly Activity Graphs", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)

				if(input$fbweekActALRBtn == "overall"){

					weekAct_graphAL <- byWeekDay_overall %>% 
						ggplot(aes(x = dayName, y = avgLikes)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Likes')					

				}else if(input$fbweekActALRBtn == "yearly"){

					weekAct_graphAL <- byWeekDay_yearly %>% 
						ggplot(aes(x = dayName, y = avgLikes)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Likes') +
						facet_wrap(~year, ncol = 3)

				}else if(input$fbweekActALRBtn == "quarterly"){

					weekAct_graphAL <- byWeekDay_quarterly %>% 
						ggplot(aes(x = dayName, y = avgLikes)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Likes') +
						facet_grid(year ~ quarter) + 
						theme(axis.text.x = element_text(angle = 55, vjust = .5))

				}else {

					weekAct_graphAL <- byWeekDay_monthly %>% 
						ggplot(aes(x = dayName, y = avgLikes)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Likes') +
						facet_grid(year ~ monthName) + 
						theme(axis.text.x = element_text(angle = 90, vjust = .5))

				}
			incProgress(amount = 3)
			Sys.sleep(1)
			incProgress(amount = 5)

			weekActALDownload <<- function(){weekAct_graphAL}
			weekAct_graphAL
		
		})

})



output$fbWeekAl_Btn <- downloadHandler(
				filename = 'Average Facebook Likes.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = weekActALDownload(), device = device, width = 10, height = 8)
      })







output$weekAct_avgComments <- renderPlot({

			withProgress(message = "Generating Weekly Activity Graphs", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)

				if(input$fbweekActACRBtn == "overall"){

					weekAct_graphAC <- byWeekDay_overall %>% 
						ggplot(aes(x = dayName, y = avgComments)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Comments')

				}else if(input$fbweekActACRBtn == "yearly"){

					weekAct_graphAC <- byWeekDay_yearly %>% 
						ggplot(aes(x = dayName, y = avgComments)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Comments') +
						facet_wrap(~year, ncol = 3)

				}else if(input$fbweekActACRBtn == "quarterly"){

					weekAct_graphAC <- byWeekDay_quarterly %>% 
						ggplot(aes(x = dayName, y = avgComments)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Comments') +
						facet_grid(year ~ quarter) + 
						theme(axis.text.x = element_text(angle = 55, vjust = .5))

				}else {

					weekAct_graphAC <- byWeekDay_monthly %>% 
						ggplot(aes(x = dayName, y = avgComments)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Comments') +
						facet_grid(year ~ monthName) + 
						theme(axis.text.x = element_text(angle = 90, vjust = .5))

				}
			incProgress(amount = 3)
			Sys.sleep(1)
			incProgress(amount = 5)


			weekActACDownload <<- function(){weekAct_graphAC}
			weekAct_graphAC
		
		})

})



output$fbWeekAc_Btn <- downloadHandler(
				filename = 'Average Facebook Comments.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = weekActACDownload(), device = device, width = 10, height = 8)
      })






output$weekAct_avgShares <- renderPlot({

			withProgress(message = "Generating Weekly Activity Graphs", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)

				if(input$fbweekActASRBtn == "overall"){

					weekAct_graphAS <- byWeekDay_overall %>% 
						ggplot(aes(x = dayName, y = avgShares)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Shares')

				}else if(input$fbweekActASRBtn == "yearly"){

					weekAct_graphAS <- byWeekDay_yearly %>% 
						ggplot(aes(x = dayName, y = avgShares)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Shares') +
						facet_wrap(~year, ncol = 3)

				}else if(input$fbweekActASRBtn == "quarterly"){

					weekAct_graphAS <- byWeekDay_quarterly %>% 
						ggplot(aes(x = dayName, y = avgShares)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Shares') +
						facet_grid(year ~ quarter) + 
						theme(axis.text.x = element_text(angle = 55, vjust = .5))

				}else {

					weekAct_graphAS <- byWeekDay_monthly %>% 
						ggplot(aes(x = dayName, y = avgShares)) + 
						singleBarGeom(myColour = '#0099FF') + 
						theme_bw() + xlab('') + ylab('') + ggtitle('Average Shares') +
						facet_grid(year ~ monthName) + 
						theme(axis.text.x = element_text(angle = 90, vjust = .5))

				}
			incProgress(amount = 3)
			Sys.sleep(1)
			incProgress(amount = 5)


			weekActASDownload <<- function(){weekAct_graphAS}
			weekAct_graphAS
		
		})

})



output$fbWeekAs_Btn <- downloadHandler(
				filename = 'Average Facebook Shares.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = weekActASDownload(), device = device, width = 10, height = 8)
      })






output$fbCorrPlot <- renderPlot({

			withProgress(message = "Generating Weekly Activity Graphs", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)	
			xAxisLabel <- input$fbCorrX
			yAxisLabel <- input$fbCorrY
			xAxisLabel <- .simpleCap(gsub("_", " ", xAxisLabel))
			yAxisLabel <- .simpleCap(gsub("_", " ", yAxisLabel))
			scattPlot <- pageData %>% 
					ggplot(aes_string(x = input$fbCorrX, y = input$fbCorrY)) + 
					geom_point(colour = '#0099FF', size = 4, alpha = .5) + 
					geom_smooth(fullrange = TRUE) +
					theme_bw() +
					xlab(xAxisLabel) + ylab(yAxisLabel)

			if(input$fbCorrRBtn == "overall"){

				scattPlot

			}else if(input$fbCorrRBtn == "yearly"){

				scattPlot <- scattPlot + facet_wrap(~ year, ncol = 3)

			}else {

				scattPlot <- scattPlot + facet_grid(year ~ quarter)

			}
			incProgress(amount = 5)
			Sys.sleep(1)

			scattPlotDownload <<- function(){scattPlot}
			scattPlot

		})
})




output$fbCorr_Btn <- downloadHandler(
				filename = 'Facebook Activity Correlation.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = scattPlotDownload(), device = device, width = 10, height = 8)
      })





#---------------------- Plotting User Likes, Comments, Retweets, Favourites --------------------#








output$likingActivityFb <- renderPlot({

			withProgress(message = "Generating Liking Activity Graph", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)

			#Add quarters too and numeric columns for year
			likedDataDf$quarter <- quarter(likedDataDf$created_date)
			likedDataDf$yearNum <- as.numeric(as.character(likedDataDf$year))

			if(input$fbLikesCntBtn == "overall"){

	gLikingActivity <- likedDataDf %>%
				group_by(from_name) %>%
				summarise(total_likes = n()) %>%
				arrange(desc(total_likes)) %>%
				filter(cume_dist(total_likes) > .99) %>%
					ggplot(aes(x = reorder(from_name, total_likes), y = total_likes)) +
					geom_segment(aes(xend = from_name), yend = 0, colour = "#0099FF") + 
					geom_point(size = 3, colour = "#0099FF") +
					theme_minimal() +
					coord_flip() +
					ylab("Number of likes") +
					xlab("Customers") +
					ggtitle("Overall Customer Likes Ranking")

			}else if(input$fbLikesCntBtn == "yearly"){

	gLikingActivity <- likedDataDf %>%
				group_by(year, from_name) %>%
    				summarise(total_likes = n()) %>%
    				arrange(desc(total_likes)) %>%
    				filter(cume_dist(total_likes) > .99) %>%
    					ggplot(aes(x = reorder(from_name, total_likes), y = total_likes)) +
    					geom_segment(aes(xend = from_name), yend = 0, colour = "#0099FF") + 
    					geom_point(size = 3, colour = "#0099FF") +
    					theme_bw() +
    					coord_flip() +
    					ylab("Number of Likes") +
    					xlab("Customers") +
    					ggtitle("Yearly Customer Likes Ranking") + facet_grid(. ~ year)

	
			}else if(input$fbLikesCntBtn == "quarterly"){

	gLikingActivity <- likedDataDf %>%
				filter(yearNum >= max(yearNum) - 2, yearNum <= max(yearNum)) %>%
    				group_by(year, quarter, from_name) %>%
    				summarise(total_likes = n()) %>%
    				arrange(desc(total_likes)) %>%
    				filter(cume_dist(total_likes) > .995) %>%
    					ggplot(aes(x = reorder(from_name, total_likes), y = total_likes)) +
    					geom_segment(aes(xend = from_name), yend = 0, colour = "#0099FF") + 
    					geom_point(size = 3, colour = "#0099FF") +
    					theme_bw() +
    					coord_flip() +
    					theme(axis.text.y = element_text(size = 10)) + 
    					ylab("Number of Likes") + 
    					xlab("Customers") +
    					ggtitle("Quarterly Customer Likes Ranking") + facet_grid(year ~ quarter)


			}else {

	gLikingActivity <- likedDataDf %>%
				filter(yearNum == max(yearNum)) %>%
    				group_by(month, from_name) %>%
    				summarise(total_likes = n()) %>%
    				arrange(desc(total_likes)) %>%
    				filter(cume_dist(total_likes) > .9) %>%
    					ggplot(aes(x = reorder(from_name, total_likes), y = total_likes)) +
    					geom_segment(aes(xend = from_name), yend = 0, colour = "#0099FF") + 
    					geom_point(size = 3, colour = "#0099FF") +
    					theme_bw() +
    					coord_flip() +
    					theme(axis.text.y = element_text(size = 10)) + 
    					ylab("Number of Likes") + 
    					xlab("Customers") +
    					ggtitle("Monthly Customer Likes Ranking for Current Year") + facet_grid(.~ month)
			

			}

			gLikingActivityDownload <<- function(){gLikingActivity}
			gLikingActivity

		})


	})



output$likingAct_Btn <- downloadHandler(
				filename = 'Facebook Liking Activity.png', content = function(file){
        				device <- function(...){
          				grDevices::png(..., res = 300, units = "in")
        				}
        			ggsave(file, plot = gLikingActivityDownload(), device = device, width = 10, height = 8)
      })







#---------------------- Plotting Word clouds for Aimia's FB + Twitter Posts --------------------#


maxWordFreq_likedC <- max(likedDf$freq)
maxWordFreq_commC <- max(sharedDf$freq)
maxWordFreq_sharedC <- max(commentedDf$freq)
maxWordFreq_fbUsers <- max(userCommentsDf$freq)
maxWordFreq_twFavs <- max(mostFavDf$freq)
maxWordFreq_twRetweet <- max(mostRetweetDf$freq)



output$likedMinSlider <- renderUI({
				sliderInput("likedSlider", label = "Minimum Word Frequency", min = 0, 
        				   max = maxWordFreq_likedC, value = maxWordFreq_likedC/5)
	})


output$commMinSlider <- renderUI({
				sliderInput("commSlider", label = "Minimum Word Frequency", min = 0, 
        				   max = maxWordFreq_commC, value = maxWordFreq_commC/5)
	})


output$sharedMinSlider <- renderUI({
				sliderInput("sharedSlider", label = "Minimum Word Frequency", min = 0, 
        				   max = maxWordFreq_sharedC, value = maxWordFreq_sharedC/5)
	})


output$usersFBMinSlider <- renderUI({
				sliderInput("FBuserCommSlider", label = "Minimum Word Frequency", min = 0, 
        				   max = maxWordFreq_fbUsers, value = maxWordFreq_fbUsers/5)
	})


output$favMinSlider <- renderUI({
				sliderInput("TWFavSlider", label = "Minimum Word Frequency", min = 0, 
        				   max = maxWordFreq_twFavs, value = maxWordFreq_twFavs/5)
	})


output$retweetMinSlider <- renderUI({
				sliderInput("TWRetweetSlider", label = "Minimum Word Frequency", min = 0, 
        				   max = maxWordFreq_twRetweet, value = maxWordFreq_twRetweet/5)
	})





output$likedWordCloud <- renderPlot({

			withProgress(message = "Generating Word Cloud", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)
			LwordCloudDownload <<- function(){
						wordcloud(words = likedDf$words, freq = likedDf$freq, random.order = FALSE, 
						random.color = FALSE, colors = wordcloudColours, min.freq = input$likedSlider)}
			Sys.sleep(1)
			incProgress(amount = 5)
			LwordCloudDownload()
		})
	})


output$likedC_Btn <- downloadHandler(
				filename = "Liked Posts' WordCloud.png", 
				content = function(x){
					png(x)
					LwordCloudDownload()
					dev.off()
			})



output$commWordCloud <- renderPlot({

			withProgress(message = "Generating Word Cloud", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)
			CwordCloudDownload <<- function(){
					wordcloud(words = commentedDf$words, freq = commentedDf$freq, random.order = FALSE,
					random.color = FALSE, colors = wordcloudColours, min.freq = input$commSlider)}
			Sys.sleep(1)
			incProgress(amount = 5)
			CwordCloudDownload()
		})
	})



output$commC_Btn <- downloadHandler(
				filename = "Commented Posts' WordCloud.png", 
				content = function(x){
					png(x)
					CwordCloudDownload()
					dev.off()
			})




output$sharedWordCloud <- renderPlot({

			withProgress(message = "Generating Word Cloud", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)
			SwordCloudDownload <<- function(){
					wordcloud(words = sharedDf$words, freq = sharedDf$freq, random.order = FALSE, 
					random.color = FALSE, colors = wordcloudColours, min.freq = input$sharedSlider)}
			Sys.sleep(1)
			incProgress(amount = 5)
			SwordCloudDownload()
		})
	})



output$sharedC_Btn <- downloadHandler(
				filename = "Shared Posts' WordCloud.png", 
				content = function(x){
					png(x)
					SwordCloudDownload()
					dev.off()
			})



output$userCommentsCloud <- renderPlot({

			withProgress(message = "Generating Word Cloud", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)
			userCommCloudDownload <<- function(){
					wordcloud(words = userCommentsDf$words, freq = userCommentsDf$freq, random.order = FALSE, 
					random.color = FALSE, colors = wordcloudColours, min.freq = input$FBuserCommSlider)}
			Sys.sleep(1)
			incProgress(amount = 5)
			userCommCloudDownload()
		})
	})



output$usersFb_Btn <- downloadHandler(
				filename = "User Posts' WordCloud.png", 
				content = function(x){
					png(x)
					userCommCloudDownload()
					dev.off()
			})





output$twFavCloud <- renderPlot({

			withProgress(message = "Generating Word Cloud", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)
			twFavCDownload <<- function(){
					wordcloud(words = mostFavDf$words, freq = mostFavDf$freq, random.order = FALSE, 
					random.color = FALSE, colors = wordcloudColours, min.freq = input$TWFavSlider)}
			Sys.sleep(1)
			incProgress(amount = 5)
			twFavCDownload()
		})
	})



output$favC_Btn <- downloadHandler(
				filename = "Favourited Tweets' WordCloud.png", 
				content = function(x){
					png(x)
					twFavCDownload()
					dev.off()
			})


output$twRetweetCloud <- renderPlot({

			withProgress(message = "Generating Word Cloud", value = 0, max = 10,
			expr = {
			incProgress(amount = 2)
			twRetweetCDownload <<- function(){
					wordcloud(words = mostRetweetDf$words, freq = mostRetweetDf$freq, random.order = FALSE, 
					random.color = FALSE, colors = wordcloudColours, min.freq = input$TWRetweetSlider)}
			Sys.sleep(1)
			incProgress(amount = 5)
			twRetweetCDownload()
		})
	})



output$retweetC_Btn <- downloadHandler(
				filename = "Retweeted Tweets' WordCloud.png", 
				content = function(x){
					png(x)
					twRetweetCDownload()
					dev.off()
			})








})
