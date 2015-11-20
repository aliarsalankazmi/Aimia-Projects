myPkgs <- c("shiny", "shinythemes", "shinydashboard", "repmis")
sapply(myPkgs, library, character.only = TRUE)



shinyUI(navbarPage(title = div(img(src="amLogo_small.png"), "Air Miles ME Social Media Analysis App"),
		   theme = shinytheme("cerulean"),
		   inverse = FALSE,


		#1. Will include calendar heatmaps for facebook and twitter
		tabPanel(br(), title = "Overall View",
		  tabsetPanel(
			tabPanel(title = "Facebook Calendar Heatmap",
					fluidRow(column(width = 10,
							plotOutput("fbHeatmap", width = "1200px", height = "700px", dblclick = "fbCMapDClick")),
						 column(width = 2, downloadButton(outputId = "fbCmap_Btn", label = "Download Image", class = "btn btn-primary"))
						),
					fluidRow(column(width = 12, tableOutput("fbMapLabels"))
						)
				),
			tabPanel(title = "Facebook Activity Distribution",
					fluidRow(column(width = 8, 
							plotOutput("fbActivityRatio")),
						 column(width = 4, 
							wellPanel(radioButtons("fbActRatioRBtn", label = h4("Roll up:"), 
									choices = list("Overall" = "overall", "Yearly" = "yearly", "Quarterly" = "quarterly")),
								downloadButton(outputId = "fbActRatio_Btn", label = "Download Image", class = "btn btn-primary")))
						),
					hr(),
					fluidRow(column(width = 8, 
							plotOutput("fbLikes_violin")),
						 column(width = 4, 
							wellPanel(radioButtons("fblikeRBtn", label = h4("Roll up:"), 
									choices = list("Yearly" = "yearly", "Quarterly" = "quarterly")),
								downloadButton(outputId = "fbLikesV_Btn", label = "Download Image", class = "btn btn-primary")))
						),
					hr(),
					fluidRow(column(width = 8, 
							plotOutput("fbComments_violin")),
						 column(width = 4,
							wellPanel(radioButtons("fbcommentRBtn", label = h4("Roll up:"), 
									choices = list("Yearly" = "yearly", "Quarterly" = "quarterly")),
								downloadButton(outputId = "fbCommentsV_Btn", label = "Download Image", class = "btn btn-primary")))
						),
					hr(),
					fluidRow(column(width = 8, 
							plotOutput("fbShares_violin")),
						 column(width = 4, 
							wellPanel(radioButtons("fbshareRBtn", label = h4("Roll up:"), 
									choices = list("Yearly" = "yearly", "Quarterly" = "quarterly")),
								downloadButton(outputId = "fbSharesV_Btn", label = "Download Image", class = "btn btn-primary")))
						)
				),
			tabPanel(title = "Twitter Calendar Heatmap",
					fluidRow(column(width = 10,
							plotOutput("twHeatmap", width = "1200px", height = "700px", dblclick = "twCMapDClick")),
						 column(width = 2, downloadButton(outputId = "twCmap_Btn", label = "Download Image", class = "btn btn-primary"))
						),
					fluidRow(column(width = 12, tableOutput("twMapLabels"))
						)
				),
			tabPanel(title = "Twitter Activity Distribution",
					fluidRow(column(width = 8, 
							plotOutput("twActivityRatio")),
						 column(width = 4,
							wellPanel(radioButtons("twActRatioRBtn", label = h4("Roll up:"), 
									choices = list("Overall" = "overall", "Yearly" = "yearly", "Quarterly" = "quarterly")),
								downloadButton(outputId = "twActRatio_Btn", label = "Download Image", class = "btn btn-primary")))
						),
					hr(),
					fluidRow(column(width = 8, 
							plotOutput("twFavourites_violin")),
						 column(width = 4,
							wellPanel(radioButtons("twFavRBtn", label = h4("Roll up:"), 
									choices = list("Yearly" = "yearly", "Quarterly" = "quarterly")),
								downloadButton(outputId = "twFav_Btn", label = "Download Image", class = "btn btn-primary")))
						),
					hr(),
					fluidRow(column(width = 8, 
							plotOutput("twRetweets_violin")),
						 column(width = 4, 
							wellPanel(radioButtons("twRetweetRBtn", label = h4("Roll up:"), 
									choices = list("Yearly" = "yearly", "Quarterly" = "quarterly")),
								downloadButton(outputId = "twRetweet_Btn", label = "Download Image", class = "btn btn-primary")))
						)
				)
			)
		   ),

		#2. Will include dygraphs with optional monthly + daily views + weekday graphs for correlation with selectable date component
		tabPanel(br(), title = "Time-Series View",
		  tabsetPanel(
			tabPanel(title = "Facebook Time-series View",
				fluidRow(column(width = 8,
						dygraphOutput("fbDygraph_totalPosts")),
					 column(width = 4, 
						wellPanel(radioButtons("fbDygTpRBtn", label = h4("Roll up:"), 
							choices = list("Yearly" = "yearly", "Monthly" = "monthly", "Daily" = "daily"))))#Removing: "Quarterly" = "quarterly"
					),
				fluidRow(column(width = 8,
						dygraphOutput("fbDygraph_avgLikes")),
					 column(width = 4, 
						wellPanel(radioButtons("fbDygAlRBtn", label = h4("Roll up:"), 
							choices = list("Yearly" = "yearly", "Monthly" = "monthly", "Daily" = "daily"))))#Removing: "Quarterly" = "quarterly"
					),
				fluidRow(column(width = 8,
						dygraphOutput("fbDygraph_avgComments")),
					 column(width = 4, 
						wellPanel(radioButtons("fbDygAcRBtn", label = h4("Roll up:"), 
							choices = list("Yearly" = "yearly", "Monthly" = "monthly", "Daily" = "daily"))))#Removing: "Quarterly" = "quarterly"
					),
				fluidRow(column(width = 8,
						dygraphOutput("fbDygraph_avgShares")),
					 column(width = 4, 
						wellPanel(radioButtons("fbDygAsRBtn", label = h4("Roll up:"), 
							choices = list("Yearly" = "yearly", "Monthly" = "monthly", "Daily" = "daily"))))#Removing: "Quarterly" = "quarterly"
					)
				),
			tabPanel(title = "Facebook Weekday View",
				fluidRow(column(width = 8,
						plotOutput("weekAct_totalPosts")),
					 column(width = 4,
						wellPanel(radioButtons("fbweekActTpRBtn", label = h4("Roll up:"),
							  choices = list("Overall" = "overall", "Yearly" = "yearly", "Quarterly" = "quarterly", "Monthly" = "monthly")),
						downloadButton(outputId = "fbWeekTp_Btn", label = "Download Image", class = "btn btn-primary"))) 
					),
				fluidRow(column(width = 8,
						plotOutput("weekAct_avgLikes")),
					 column(width = 4,
						wellPanel(radioButtons("fbweekActALRBtn", label = h4("Roll up:"),
							  choices = list("Overall" = "overall", "Yearly" = "yearly", "Quarterly" = "quarterly", "Monthly" = "monthly")),
						downloadButton(outputId = "fbWeekAl_Btn", label = "Download Image", class = "btn btn-primary"))) 
					),
				fluidRow(column(width = 8,
						plotOutput("weekAct_avgComments")),
					 column(width = 4,
						wellPanel(radioButtons("fbweekActACRBtn", label = h4("Roll up:"),
							  choices = list("Overall" = "overall", "Yearly" = "yearly", "Quarterly" = "quarterly", "Monthly" = "monthly")),
						downloadButton(outputId = "fbWeekAc_Btn", label = "Download Image", class = "btn btn-primary"))) 
					),
				fluidRow(column(width = 8,
						plotOutput("weekAct_avgShares")),
					 column(width = 4,
						wellPanel(radioButtons("fbweekActASRBtn", label = h4("Roll up:"),
							  choices = list("Overall" = "overall", "Yearly" = "yearly", "Quarterly" = "quarterly", "Monthly" = "monthly")),
						downloadButton(outputId = "fbWeekAs_Btn", label = "Download Image", class = "btn btn-primary")))  
					)				
				),
			tabPanel(title = "Facebook Activity Correlation",
				fluidRow(column(width = 8, 
						plotOutput("fbCorrPlot")),
					 column(width = 4,
						wellPanel(selectInput(inputId = "fbCorrX", label = "Select variable for X-axis",
								      choices = list("Likes" = "likes_count", "Comments" = "comments_count", "Shares" = "shares_count")),
							  selectInput(inputId = "fbCorrY", label = "Select variable for Y-axis",
								      choices = list("Comments" = "comments_count", "Likes" = "likes_count", "Shares" = "shares_count")),
							  radioButtons("fbCorrRBtn", label = h4("Roll up:"),
								choices = list("Overall" = "overall", "Yearly" = "yearly", "Quarterly" = "quarterly")),
							  downloadButton(outputId = "fbCorr_Btn", label = "Download Image", class = "btn btn-primary")))
					)
				)
			)
		    ),

		#3. Will include customer names ordered by likes on FB posts
		tabPanel(br(), title = "Customer Activity",
			tabsetPanel(
				tabPanel(title = "Facebook Likes",
					fluidRow(column(width = 9,
							plotOutput("likingActivityFb", width = "900px", height = "700px")),
						 column(width = 3,
							wellPanel(radioButtons("fbLikesCntBtn", label = h4("Roll up:"),
								  choices = list("Overall" = "overall", "Yearly" = "yearly", 
										 "Quarterly" = "quarterly", "Monthly" = "monthly")),
								  downloadButton(outputId = "likingAct_Btn", label = "Download Image", class = "btn btn-primary")))
						)
					)
				)
			),

		#4. Will include word clouds for Facebook's most liked, commented, shared posts, and Twitter's most re-tweeted, favourited posts
		navbarMenu(br(), title = "Word Clouds",
			tabPanel(title = "Air Miles ME's Posts",
				tabsetPanel(
					tabPanel(title = "Most Liked FB Posts",
						fluidRow(column(width = 8,
								plotOutput("likedWordCloud", width = "1200px", height = "700px")),
							 column(width = 4,
								wellPanel(uiOutput("likedMinSlider"),
									  downloadButton(outputId = "likedC_Btn", label = "Download Image", class = "btn btn-primary")))
							)
						),
					tabPanel(title = "Most Commented FB Posts",
						fluidRow(column(width = 8,
								plotOutput("commWordCloud", width = "1200px", height = "700px")),
							 column(width = 4,
								wellPanel(uiOutput("commMinSlider"),
									  downloadButton(outputId = "commC_Btn", label = "Download Image", class = "btn btn-primary")))
							)
						),
					tabPanel(title = "Most Shares FB Posts",
						fluidRow(column(width = 8,
								plotOutput("sharedWordCloud", width = "1200px", height = "700px")),
							 column(width = 4,
								wellPanel(uiOutput("sharedMinSlider"),
									  downloadButton(outputId = "sharedC_Btn", label = "Download Image", class = "btn btn-primary")))
							)
						),
					tabPanel(title = "Most Favourited TW Posts",
						fluidRow(column(width = 8,
								plotOutput("twFavCloud", width = "1200px", height = "700px")),
							 column(width = 4,
								wellPanel(uiOutput("favMinSlider"),
									  downloadButton(outputId = "favC_Btn", label = "Download Image", class = "btn btn-primary")))
							)
						),
					tabPanel(title = "Most Retweeted W Posts",
						fluidRow(column(width = 8,
								plotOutput("twRetweetCloud", width = "1200px", height = "700px")),
							 column(width = 4,
								wellPanel(uiOutput("retweetMinSlider"),
									  downloadButton(outputId = "retweetC_Btn", label = "Download Image", class = "btn btn-primary")))
							)
						)
					)
				),
			tabPanel(title = "Customers' Comments",
				tabsetPanel(
					tabPanel(title = "Facebook Users' Comments",
						fluidRow(column(width = 8,
							plotOutput("userCommentsCloud", width = "1200px", height = "700px")),
							column(width = 4,
							wellPanel(uiOutput("usersFBMinSlider"),
							downloadButton(outputId = "usersFb_Btn", label = "Download Image", class = "btn btn-primary")))
							)
						)
					  )
				),
			tabPanel(title = "Sentiment Analysis"))




))


