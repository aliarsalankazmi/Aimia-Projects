library(shiny)
library(dygraphs)

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("
      @import url(http://fonts.googleapis.com/css?family=Poiret+One);
      
      h1 {
        font-family: 'Poiret One', cursive;
        font-weight: 500;
        line-height: 1.1;
      }

    "))
  ),
  
	titlePanel(h1("Saudi Hollandi Bank's Facebook Data Analysis")),
	tabsetPanel(
		tabPanel("Overall View",
				fluidRow(
					column(width = 6, dygraphOutput("totalOverview")),
					column(width = 6, dygraphOutput("likedOverview"))
					),
        			br(),
				br(),
				fluidRow(
					column(width = 6, dygraphOutput("commentedOverview")),
					column(width = 6, dygraphOutput("sharedOverview"))
					)
			),
		tabPanel("Monthly View",
				fluidRow(
					column(width = 6, dygraphOutput("totalMonthly")),
					column(width = 6, dygraphOutput("likedMonthly"))
					),
        			br(),
				br(),
				fluidRow(
					column(width = 6, dygraphOutput("commentedMonthly")),
					column(width = 6, dygraphOutput("sharedMonthly"))
					)
			),
		tabPanel("Week Day View",
				fluidRow(
					column(width = 6, plotOutput("totalWeekday")),
					column(width = 6, plotOutput("likedWeekday"))
					),
        			br(),
				br(),
				fluidRow(
					column(width = 6, plotOutput("commentedWeekday")),
					column(width = 6, plotOutput("sharedWeekday"))
					)
			),
		tabPanel("Correlation",
				fluidRow(
					column(width = 4, plotOutput("corrPlot1")),
					column(width = 4, plotOutput("corrPlot2")),
					column(width = 4, plotOutput("corrPlot3"))
					)
			),
		tabPanel("Word Clouds",
				fluidRow(
					column(h3("Words Appearing in Most Liked Posts"),
                 width = 12, plotOutput("likedWords"))
					),
        			br(),
				br(),
				fluidRow(
					column(h3("Words Appearing in Most Commented on Posts"),
					       width = 12, plotOutput("commentedWords"))
					),
        			br(),
				br(),
				fluidRow(
					column(h3("Words Appearing in Most Shared Posts"),
					       width = 12, plotOutput("sharedWords"))
					)
			),
		tabPanel("About")
		)
	)
)
