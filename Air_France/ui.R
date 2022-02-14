#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(ggplot2)
library(imputeTS)
library(plotly)
library(stringr)
library(reshape2)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Air France Marketing Business Case"),
  tabsetPanel(
    tabPanel("Publishers", 
             h1("What publisher should we choose?"), 
             fluidRow(
               column(4, plotlyOutput("publisher1")), 
               column(8, plotlyOutput("publisher2"))
             ),
             fluidRow(
               column(4, HTML(
                 "<br><br><br><ul style='font-size:20px'><li>Google - Global is the one that has the highest amount of booking per ad, Overture has the lowest.</li>
                 <br><li>Nevertheless, the one that gets a higher ratio of clicks per ad is Google Global, with a significant ratio of roa.</li>
                 <br><li>Looking into the search engine click thru rate most of the booking are made between the first quartile, with a couple of outsiders reaching over 400 bookings.</li></ul>"
               )
               ), 
               column(8, plotOutput("publisher3"))   
             )
    ),
    tabPanel("Match Types",
             h1("Which match type is best?"), 
             fluidRow(
               column(4,fluidRow(
                 column(12, plotOutput("match_type2", height = 800)))
               ),
               column(8, 
                      fluidRow(
                        column(12, plotlyOutput("match_type1", height = 300))),
                      fluidRow(
                        column(12, plotlyOutput("match_type3", height = 300))),
                      fluidRow(
                        column(12, HTML(
                          "<br><br><br><ul style='font-size:20px'><li>Broad is the most used match type, and Exact is least used.</li>
                          <br><li>Exact matches generate the highes amount of revenue per ad, and then Advanced</li>
                          <br><li>On average, exact and advanced matches have more or less the same cost, while broad matches have a higher cost but generate little revenue and impressions</li></ul>"
                        )))
               ) 
             )), 
    tabPanel("Bidding Strategy", 
             h1("Is bidding to be on top worth it?"), 
             plotOutput("mixed_us"),
             plotOutput("mixed_global")
    ),
    tabPanel("Summary", 
             h1("So what's the strategy?"), 
             fluidRow(
               column(5, plotlyOutput("campaign1")), 
               column(5, plotlyOutput("campaign3")), 
               
               column(2, 
                      fluidRow(
                        column(6, radioButtons(
                          "Topic",
                          "Topic:",
                          c(
                            "Topic 1" = "Topic 1",
                            "Topic 2" = "Topic 2",
                            "Topic 3" = "Topic 3",
                            "Topic 4" = "Topic 4",
                            "Topic 5" = "Topic 5"
                          ),
                          selected = "Topic 1"
                        )),
                        column(6, tableOutput("tables"))
              
             ))),
             fluidRow(
               column(8, 
                      plotlyOutput("campaign2")),
               column(4, HTML(
                 "<br><br><br><ul style='font-size:20px'><li>Of the ads that have been posted, 20% of the ones with an exact match type have had a positive return, higher than all other types</li>
                                <br><li>Key words that include the words air france have had a 45% success rate vs a 10% success rate for the other types of keywords</li>
                                <br><li>20% of the ads placed on Google Global have had positive returns, double that of any other publisher</li></ul>"
               )))
             
             
             
    ),
    tabPanel("Prediction", 
             h1("Are our insights correct?"), 
             fluidRow( 
               column(3, 
                      fluidRow(
                        column(12, align = "center", h2("Train Score"))),
                      fluidRow(
                        column(12, align = "center", verbatimTextOutput("conf_train"),
                               tags$head(tags$style(HTML("
                            #conf_train {
                              font-size: 20px;
                            }
                            ")))
                               ))),
               column(3, 
                      fluidRow(
                        column(12, align = "center", h2("Test Score"))),
                      fluidRow(
                        column(12, align = "center", verbatimTextOutput("conf_test"),
                               tags$head(tags$style(HTML("
                            #conf_test {
                              font-size: 20px;
                            }
                            ")))
                               ))),
               column(6, 
                      fluidRow(
                        column(12, plotlyOutput("impacct"))))
               ),
             fluidRow(
               column(12, 
                      fluidRow(
                        column(6, align = "center", h4("ROC AUC"))),
                      fluidRow(
                        column(8, align = "center", plotOutput("auc")), 
                        column(4, HTML(
                               "<br><br><br><ul style='font-size:20px'><li>Our model provides business insight</li>
                               <br><li>Our model predicts with about 93% accuracy, and since the train and test scores are similar we can assume that our model will work with new data.</li>
                               <br><li>Publishing on Google - Global has the biggest impact on business success (increasing about 800%), having an exact match type increases the odds of business success by 147%, having an air france branded campaign increases the odds of business success by more than 600%, and including the word air france in the key work increases the odds of business success by almost 300%</li>
                               </ul>"
                        ))))
             )
    )
  
  # Sidebar with a slider input for number of bins
  
)))


