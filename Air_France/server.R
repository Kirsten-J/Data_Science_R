#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggthemes)
library(fastDummies)
library(glue)
library(dplyr)

library(ROCR)
library(stringr)
library(splitstackshape)
library(Hmisc)
library(caret)
library(tibble)

# Air france case
#setwd("~/Desktop/Hult Classes/R")
setwd("C:/Users/kirst/OneDrive/Desktop/HULT/Data Science R")


library(readxl)
library(ggplot2)
library(imputeTS)
library(plotly)
library(topicmodels)
library(tm)
library(SnowballC)

air_france <- read_excel("Air France Case Spreadsheet Supplement.xls", sheet=2, na="N/A") #Desktop/Hult Classes/R/
air_france <- read_excel("Air France Case Spreadsheet.xlsx", sheet=3, na="N/A") #Desktop/Hult Classes/R/

#install.packages("dplyr")


# for (var in names(air_france)) {
#   air_france %>% count(.data[[var]]) %>% print()
# }#closing loop
# colnames(air_france)

#campaign is unassigned
#key word type the entire column unassigned
set.seed(123)

modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
match_mode <- modes(x=air_france$`Match Type`)

air_france <- air_france %>% mutate(`Match Type` = if_else(is.na(`Match Type`), match_mode, `Match Type`))
#air_france$`Publisher Name` <- gsub("Overture - US", "Yahoo - US", air_france$`Publisher Name`)
#air_france$`Publisher Name` <- gsub("Overture - Global", "Yahoo - US", air_france$`Publisher Name`)

air_france <- air_france %>% mutate(roa = `Amount`/`Total Cost`)
air_france <- air_france %>% mutate(ticket_price = Amount / `Total Volume of Bookings` )
air_france <- air_france %>% mutate(lead_cost = `Total Cost` / `Clicks`)
air_france <- air_france %>% mutate(amount_clicks = `Amount` / `Clicks`)
air_france <- air_france %>% mutate(prob_booking = `Total Volume of Bookings` / `Clicks`)
air_france <- air_france %>% mutate(revenue = `Amount`-`Total Cost`)
is.na(air_france) <- sapply(air_france, is.infinite)

air_france$ticket_price <- na_replace(air_france$ticket_price, fill = 0)
air_france$lead_cost <- na_replace(air_france$lead_cost, fill = 0)
air_france$amount_clicks <- na_replace(air_france$amount_clicks, fill = 0)
air_france$roa <- na_replace(air_france$roa, fill = 0)

#air_france <- air_france %>% mutate(change = `Total Volume of Bookings` / `Impressions`*10000  /`Engine Click Thru %`)

#### TABLES 
pub_imp <- air_france %>% group_by(`Publisher Name`) %>% summarise(sum_imp = sum(Impressions), ratio_imp= sum(Impressions)/n(), n_imp = n())
#overture was bought by yahoo in 2003 -> overture had veeery high impressions

pub_clicks <- air_france %>% group_by(`Publisher Name`) %>% summarise(sum_clicks = sum(Clicks), ratio_clicks= sum(Clicks)/n(), n_clicks = n())
# Google global doing well, google us not probably bc not a lot of americans cross the ocean
pub_roa <- air_france %>% group_by(`Publisher Name`) %>% summarise(sum_roa = sum(roa), ratio_roa = sum(roa)/n(), n_roa = n())
# general overview, it is better to assume to focus on google global bc they get the most return per ad, the amount of clicks is also higher

pub_amtclicks <- air_france %>% group_by(`Publisher Name`) %>% summarise(sum_amtclicks = sum(amount_clicks), ratio_amtclicks = sum(amount_clicks)/n(), n_amtclicks = n())
# yahoo US spends most amount/click therefore they are paying higher ticket prices. might be better to advertise expensive trips here. perhaps the audience is older and has more savings/ they are business trips

pub_costclicks <- air_france %>% group_by(`Publisher Name`) %>% summarise(sum_costclicks  = sum(`Avg. Cost per Click`), ratio_costclicks = sum(`Avg. Cost per Click`)/n(), n_costclicks = n())
# yahoo is cheaper than google 
pub_costs <- air_france %>% group_by(`Publisher Name`) %>% summarise(sum_costs  = sum(`Total Cost`), ratio_costs  = sum(`Total Cost`)/n(), n_costs  = n())
# yahoo is cheaper than google 

pub_revenue <- air_france %>% group_by(`Publisher Name`) %>% summarise(sum_revenue  = sum(revenue), ratio_revenue  = sum(revenue)/n(), n_revenue  = n())
# yahoo is cheaper than google 

pub_book <- air_france %>% group_by(`Publisher Name`) %>% summarise(sum_book  = sum(`Total Volume of Bookings`), ratio_book  = sum(`Total Volume of Bookings`)/n(), n_book  = n())
# yahoo is cheaper than google 


pub_data <- merge(pub_imp, pub_clicks, by = 1)
pub_data <- merge(pub_data, pub_roa, by = 1)
pub_data <- merge(pub_data, pub_amtclicks, by = 1)
pub_data <- merge(pub_data, pub_costclicks, by = 1)
pub_data <- merge(pub_data, pub_costs, by = 1)
pub_data <- merge(pub_data, pub_revenue, by = 1)
pub_data <- merge(pub_data, pub_book, by = 1)


mat_imp <- air_france %>% group_by(`Match Type`) %>% summarise(sum_imp = sum(Impressions), ratio_imp= sum(Impressions)/n(), n_imp = n())
#Standard has more impressions by ad, and most total impressions even though there arent as many ads. Broad does not give a lot of impressions 

mat_clicks <- air_france %>% group_by(`Match Type`) %>% summarise(sum_clicks = sum(Clicks), ratio_clicks= sum(Clicks)/n(), n_clicks = n())
# Exact has the most clicks per ad, probably because they find exactly what they are looking for. for this same reason broad has very little clicks, because the ad might have very little to do with what they are looking for

mat_roa <- air_france %>% group_by(`Match Type`) %>% summarise(sum_roa = sum(roa), ratio_roa= sum(roa)/n(), n_roa = n())
# Exact has the most roa, probably for same reasons as clicks

mat_amtclicks <- air_france %>% group_by(`Match Type`) %>% summarise(sum_amtclicks = sum(amount_clicks), ratio_amtclicks = sum(amount_clicks)/n(), n_amtclicks  = n())
# advanced has a higher amount euro per click <- analyze why 

mat_costclicks <- air_france %>% group_by(`Match Type`) %>% summarise(sum_costclicks  = sum(`Avg. Cost per Click`), ratio_costclicks = sum(`Avg. Cost per Click`)/n(), n_costclicks = n())
# yahoo is cheaper than google 
mat_costs <- air_france %>% group_by(`Match Type`) %>% summarise(sum_costs  = sum(`Total Cost`), ratio_costs  = sum(`Total Cost`)/n(), n_costs  = n())
# yahoo is cheaper than google 

mat_revenue <- air_france %>% group_by(`Match Type`) %>% summarise(sum_revenue  = sum(revenue), ratio_revenue  = sum(revenue)/n(), n_revenue  = n())
# yahoo is cheaper than google 


mat_data <- merge(mat_imp, mat_clicks, by = 1)
mat_data <- merge(mat_data, mat_roa, by = 1)
mat_data <- merge(mat_data, mat_amtclicks, by = 1)
mat_data <- merge(mat_data, mat_costclicks, by = 1)
mat_data <- merge(mat_data, mat_costs, by = 1)
mat_data <- merge(mat_data, mat_revenue, by = 1)



camp_imp <- air_france %>% group_by(`Campaign`) %>% summarise(sum_imp = sum(Impressions), ratio_imp= sum(Impressions)/n(), n_imp = n())
#Standard has more impressions by ad, and most total impressions even though there arent as many ads. Broad does not give a lot of impressions 

camp_clicks <- air_france %>% group_by(`Campaign`) %>% summarise(sum_clicks = sum(Clicks), ratio_clicks= sum(Clicks)/n(), n_clicks = n())
# Exact has the most clicks per ad, probably because they find exactly what they are looking for. for this same reason broad has very little clicks, because the ad might have very little to do with what they are looking for

camp_roa <- air_france %>% group_by(`Campaign`) %>% summarise(sum_roa = sum(roa), ratio_roa= sum(roa)/n(), n_roa = n())
# Exact has the most roa, probably for same reasons as clicks

camp_amtclicks <- air_france %>% group_by(`Campaign`) %>% summarise(sum_amtclicks = sum(amount_clicks), ratio_amtclicks = sum(amount_clicks)/n(), n_amtclicks  = n())
# advanced has a higher amount euro per click <- analyze why 

camp_costclicks <- air_france %>% group_by(`Campaign`) %>% summarise(sum_costclicks  = sum(`Avg. Cost per Click`), ratio_costclicks = sum(`Avg. Cost per Click`)/n(), n_costclicks = n())
# yahoo is cheaper than google 
camp_costs <- air_france %>% group_by(`Campaign`) %>% summarise(sum_costs  = sum(`Total Cost`), ratio_costs  = sum(`Total Cost`)/n(), n_costs  = n())
# yahoo is cheaper than google 

camp_revenue <- air_france %>% group_by(`Campaign`) %>% summarise(sum_revenue  = sum(revenue), ratio_revenue  = sum(revenue)/n(), n_revenue  = n())
# yahoo is cheaper than google 

camp_data <- merge(camp_imp, camp_clicks, by = 1)
camp_data <- merge(camp_data, camp_roa, by = 1)
camp_data <- merge(camp_data, camp_amtclicks, by = 1)
camp_data <- merge(camp_data, camp_costclicks, by = 1)
camp_data <- merge(camp_data, camp_costs, by = 1)
camp_data <- merge(camp_data, camp_revenue, by = 1)

air_france_us <- air_france[which(str_detect(air_france$`Publisher Name`, "US", negate = FALSE) == TRUE),]
air_france_global <- air_france[which(str_detect(air_france$`Publisher Name`, "Global", negate = FALSE) == TRUE),]
air_france_us <- air_france_us[which(str_detect(air_france_us$`Publisher Name`, "MSN", negate = FALSE) == FALSE),]
air_france_global <- air_france_global[which(str_detect(air_france_global$`Publisher Name`, "MSN", negate = FALSE) == FALSE),]

# air_france$`Publisher Name` <- as.factor(air_france$`Publisher Name`)
# air_france$pubname <- as.numeric(air_france$`Publisher Name`)
# air_france$`Match Type` <- as.factor(air_france$`Match Type`)
# air_france$matchtype <- as.numeric(air_france$`Match Type`)
# air_france$Campaign <- as.factor(air_france$Campaign )
# air_france$camp <- as.numeric(air_france$Campaign )

air_france_pred <- air_france[,c("Publisher Name", "Match Type", "Campaign", "Search Engine Bid", "revenue", "Sub_key_ group")]
air_france_pred$bid <- air_france_pred$`Search Engine Bid`
for (i in 1:nrow(air_france_pred)){ 
  if(air_france_pred$revenue[i] > 0){
    air_france_pred$binary[i] <- 1 
  } else { air_france_pred$binary[i] <-  0 
  }}
#dummy_cols(fastDummies_example, select_columns = "numbers")
air_france_pred <- dummy_cols(air_france_pred, select_columns = c("Publisher Name", "Match Type", "Campaign", "Sub_key_ group"))

people_money <- air_france_pred %>% group_by(`Publisher Name`) %>% summarise(sum1 = sum(binary)/n() * 100, sum0 = sum(binary == 0)/n() * 100)
people_money2 <- air_france_pred %>% group_by(`Match Type`) %>% summarise(sum1 = sum(binary)/n() * 100, sum0 = sum(binary == 0)/n() * 100)
people_money3 <- air_france_pred %>% group_by(`Sub_key_ group`) %>% summarise(sum1 = sum(binary)/n() * 100, sum0 = sum(binary == 0)/n() * 100)


# hey <- colnames(air_france_pred)
# for(i in 1:length(hey)){
#   print(glue('`{hey[i]}` + '))
# }

air_france$doc_id <- 1:nrow(air_france)
air_france_pred$idx <- 1:nrow(air_france_pred)
air_france_pred_train <- stratified(air_france_pred, group = air_france_pred$binary, size = .8)
air_france_pred_test <- air_france_pred[-air_france_pred_train$idx, ]

logreg <- glm("binary ~ 
`Publisher Name_Google - Global` + 
`Publisher Name_MSN - Global` + 
`Publisher Name_MSN - US` + 
`Publisher Name_Overture - Global` + 
`Publisher Name_Overture - US` + 
`Publisher Name_Yahoo - US` + 
`Campaign_Air France Branded` + 
`Campaign_Air France Global Campaign` + 
`Sub_key_ group_Air France` + 
`Sub_key_ group_Ticket type` +  
`Match Type_Exact`", data = air_france_pred_train, family = "binomial")

regression_values<- logreg$coefficients
regression <- as.data.frame(as.matrix(regression_values,  ncol = 1, nrow = 10))
regression <- rownames_to_column(regression)
colnames(regression) <- c("variable", "coeff")
regression$impact <- (exp(regression$coeff) - 1) * 100

predicted_values_test <- predict(logreg, air_france_pred_test, type = 'response')
predicted_values_train <- predict(logreg, air_france_pred_train, type = 'response')

conf_test <- confusionMatrix(data = as.factor(as.numeric(predicted_values_test > 0.5)), reference = as.factor(air_france_pred_test$binary), positive = "1")
conf_train <- confusionMatrix(data = as.factor(as.numeric(predicted_values_train > 0.5)), reference = as.factor(air_france_pred_train$binary), positive = "1")


pred_val_logit <- prediction(predicted_values_test, air_france_pred_test$binary)
perf_logit <- performance(pred_val_logit, "tpr", "fpr")




### Sub_key_group###
subkey_imp <- air_france %>% group_by(`Sub_key_ group`) %>% summarise(sum_imp = sum(Impressions), ratio_imp= sum(Impressions)/n(), n_imp = n())
#Standard has more impressions by ad, and most total impressions even though there arent as many ads. Broad does not give a lot of impressions 

subkey_clicks <- air_france %>% group_by(`Sub_key_ group`) %>% summarise(sum_clicks = sum(Clicks), ratio_clicks= sum(Clicks)/n(), n_clicks = n())
# Exact has the most clicks per ad, probably because they find exactly what they are looking for. for this same reason broad has very little clicks, because the ad might have very little to do with what they are looking for

subkey_roa <- air_france %>% group_by(`Sub_key_ group`) %>% summarise(sum_roa = sum(roa), ratio_roa= sum(roa)/n(), n_roa = n())
# Exact has the most roa, probably for same reasons as clicks

subkey_amtclicks <- air_france %>% group_by(`Sub_key_ group`) %>% summarise(sum_amtclicks = sum(amount_clicks), ratio_amtclicks = sum(amount_clicks)/n(), n_amtclicks  = n())
# advanced has a higher amount euro per click <- analyze why 

subkey_costclicks <- air_france %>% group_by(`Sub_key_ group`) %>% summarise(sum_costclicks  = sum(`Avg. Cost per Click`), ratio_costclicks = sum(`Avg. Cost per Click`)/n(), n_costclicks = n())
# yahoo is cheaper than google 
subkey_costs <- air_france %>% group_by(`Sub_key_ group`) %>% summarise(sum_costs  = sum(`Total Cost`), ratio_costs  = sum(`Total Cost`)/n(), n_costs  = n())
# yahoo is cheaper than google 

subkey_revenue <- air_france %>% group_by(`Sub_key_ group`) %>% summarise(sum_revenue  = sum(revenue), ratio_revenue  = sum(revenue)/n(), n_revenue  = n())
# yahoo is cheaper than google 

subkey_data <- merge(subkey_imp, subkey_clicks, by = 1)
subkey_data <- merge(subkey_data, subkey_roa, by = 1)
subkey_data <- merge(subkey_data, subkey_amtclicks, by = 1)
subkey_data <- merge(subkey_data, subkey_costclicks, by = 1)
subkey_data <- merge(subkey_data, subkey_costs, by = 1)
subkey_data <- merge(subkey_data, subkey_revenue, by = 1)


forlda<-air_france %>% 
  dplyr::select(doc_id,Keyword)
colnames(forlda)[2] <- 'text'

########Ctreating corpus

myCorpus <- Corpus(VectorSource(forlda$text))
docs <- forlda$doc_id
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)

##creating document term matrix
dtm <- DocumentTermMatrix(myCorpus)

#########Number of Topics

k<-5
lda <- LDA(dtm, control = list(alpha = 0.1,seed=0), k)

ldaOut.topics <- as.data.frame(topics(lda))


###Top Terms per Topic
ldaOut.terms <- as.data.frame(terms(lda,5))

ldaOut.topics <- cbind(doc_id = rownames(ldaOut.topics), ldaOut.topics)

airfrance<-merge(air_france,ldaOut.topics,by = "doc_id",all.x = TRUE)

colnames(airfrance)[ncol(airfrance)] <- "Topic"

######Amount and cost for topics

topicanalysis<-airfrance %>% 
  group_by(Topic) %>% 
  summarise(Amount = sum(Amount),Cost=sum(`Total Cost`),Totalclicks=sum(Clicks))

fig4a <- plot_ly(topicanalysis, x = ~`Topic`, y = ~Amount, type = 'bar', name = 'Amount')
fig4a <- fig4a %>% add_trace(y = ~Cost, name = 'Cost')
fig4a <- fig4a %>% layout(yaxis = list(title = '$'), barmode = 'group')
fig4a.layout.template = 'theme_bw()'
fig4a



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$publisher1 <- renderPlotly({
    #air_france %>% ggplot(aes(x = ratio_roa, y = ratio_clicks, color = `Publisher Name`)) + geom_point()
    ggplotly(ggplot(data = pub_data, aes(x = ratio_roa, y = ratio_clicks, color = `Publisher Name`)) + 
               geom_point(aes(size = ratio_revenue)) + 
               theme_bw() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylab("Average clicks") + xlab("Average ROA")
    )
  })
  
  output$publisher2 <- renderPlotly({
    ggplotly(ggplot(data = pub_data, aes(x = `Publisher Name`, y = ratio_book, color = `Publisher Name`)) + 
               geom_col(aes(fill = `Publisher Name`)) + 
               theme_bw() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")  + ylab("Average bookings")
    )
  })
  
  output$publisher3 <- renderPlot({
    ggplot(data = air_france, aes(x = `Engine Click Thru %`, y = `Total Volume of Bookings`,  color = `Publisher Name`, alpha = 0)) + 
      geom_point(size = 3, aes(fill = `Publisher Name`))  + 
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") 
    
  })
  
  output$match_type1 <- renderPlotly({
    ggplotly(ggplot(data = mat_data, aes(y = ratio_revenue, x = ratio_imp, color = `Match Type`)) + 
               geom_point(size = 5) + 
               theme_bw() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") + xlab("Average impressions") + ylab("Average revenue")
    )
    
  })
  
  output$match_type3 <- renderPlotly({
    ggplotly(ggplot(data = mat_data, aes(y = ratio_costclicks, x = ratio_amtclicks, color = `Match Type`)) + 
               geom_point(size = 5) + 
               theme_bw() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") + xlab("Average amount per click") + ylab("Average cost per click")
    )
    
  })
  
  output$mixed_us <- renderPlot({
    ggplot(data = air_france_us, aes(x = `Search Engine Bid`, y = Impressions, color = `Publisher Name`)) + 
      geom_point(size = 3) +
      facet_grid( .~`Publisher Name`)+ 
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) 
    
    
  })
  output$mixed_global <- renderPlot({
    ggplot(data = air_france_global, aes(x = `Search Engine Bid`, y = Impressions, color = `Publisher Name`)) + 
      geom_point(size = 3) +
      facet_grid( .~`Publisher Name`)+ 
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) 
    
    
  })
  
  output$match_type2 <- renderPlot({
    ggplot(data = mat_data, aes(x = `Match Type`, y = n_imp, fill = `Match Type`))+
      geom_col() + 
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ylab("Number of ads")
    
    
  })
  
  output$campaign1 <- renderPlotly({
    ggplotly(ggplot(data = people_money2, aes(y = sum1, x = `Match Type`, fill = `Match Type`)) + 
               geom_col() + 
               theme_bw() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
               scale_fill_manual(values = c("#CA6702", "#BB3E03", "#AE2012", "#9B2226")) + ylab("Percentage of success")
             )
    
  })
  
  
  output$campaign2 <- renderPlotly({
      ggplotly(ggplot(data = people_money, aes(y = sum1, x = `Publisher Name`, fill = `Publisher Name`)) + 
               geom_col() + 
               theme_bw() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylab("Percentage of success") 
    )
    
  })
  
  output$campaign3 <- renderPlotly({
    ggplotly(fig4a 
    )
    
  })
  
  
  output$conf_test <- renderPrint({
    print(conf_test$table)
    print(round(conf_test$overall[1] * 100, 2))
  })
  output$conf_train <- renderPrint({
    print(conf_train$table)
    print(round(conf_train$overall[1] * 100, 2))
  })
  
  output$auc <- renderPlot({
    plot(perf_logit)
    abline(coef = c(0,1), col="red")
  })
  
  output$impacct <- renderPlotly({
    ggplotly(ggplot(data = regression, aes(y = impact, x = variable, fill = variable)) + 
               geom_col() + 
               theme_bw() +
               theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                                                                                  axis.title.x=element_blank(), axis.text.x=element_blank(),
                                                                                          axis.ticks.x=element_blank())
    )
  })
  output$tables <- renderTable({ 
    ldaOut.terms[,input$Topic]
  })
  
  topics <- reactive({
    col <- ldaOut.terms %>% select(y = input$Topic)
    
  })
})
