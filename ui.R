#--------------------------------------------------------------------------------------
#ui.R
#--------------------------------------------------------------------------------------

if(!require(shiny)) 
  install.packages("shiny")

tweets <- read.csv("Tweets.csv", header=TRUE)
tweets_text_list <- tweets$text[1:1000]
 
shinyUI( fluidPage(
  titlePanel("Sentiment Text Mining Engine"),
  fluidRow(
    column(12, selectInput("input_text", label=h3("Choose a Tweet Message:"),
                           choices = as.character(tweets_text_list)),
           submitButton("Submit") )
  ),
  fluidRow(
    column(12, h3("Your Tweet Message is:"), tableOutput("table") )
  )
))