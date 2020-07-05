#--------------------------------------------------------------------------------------
#server.R
#--------------------------------------------------------------------------------------

if(!require(shiny))
    install.packages("shiny")
if(!require(proxy))
    install.packages("proxy")

source("helpercode.R")
 
shinyServer( function(input, output)
{
    output$table <- renderTable(
    {
        sentiment_prediction(input$input_text)
    })
})