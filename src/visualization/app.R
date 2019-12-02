
# DATE: 27.09.2019
# This app is for exploring the NRW2019 dataset visually
 



library(shiny)
library(plyr)
library(tidyverse)
library(stringi)
library(stringr)
# library(shinyjs)
library(lubridate)
library(shinyWidgets)
library(RcppRoll)


# jscode <- "shinyjs.closeWindow = function() { window.close(); }"

#### Define Global Variables, non-reactive #### -----------------------------------------

polChoices <- list("Party Leadership" = "Leader",
                   "Party - aggregated Politicians" = "PartyPol",
                   "National Party" = "PartyMain",
                   "Party - Regional" = "Party",
                   "Journalists" = "Journalist",
                   "Media" = "Media")

polChoices3 <- list("Party Leadership" = "Leader",
                   "Party - aggregated Politicians" = "PartyPol",
                   "National Party" = "PartyMain",
                   "Party - Regional" = "Party",
                   "Media" = "Media")


polChoicesAds <- list("Party Leadership" = "Leader",
                   "Party - aggregated Politicians" = "PartyPol",
                   "Party - Regional" = "Party",
                   "National Party" = "PartyMain")


yChoices <- list("Number of Posts" = "numPosts",
                 "Number of Shares" = "shareCount",
                 "Number of Likes" = "favoriteCount",
                 "Number of Comments" = "commentCount",
                 "Maximum Reply to Share Ratio" = "repShareRatio",
                 "Total Interactions" = "interactionCount",
                 "Number of Mentions" = "mentionCount")

yChoices2 <- yChoicesAds <- list("Average Shares per Post" = "shareCount",
                 "Average Likes per Post" = "favoriteCount",
                 "Average Comments per Post" = "commentCount",
                 "Average Reply to Share Ratio" = "repShareRatio",
                 "Average Interactions per Post" = "interactionCount")

yChoices3 <- list("Laugh" = "haha_count",
                                 "Love" = "love_count",
                                 "Wow" = "wow_count",
                                 "Sad" = "sad_count",
                                 "Angry" = "angry_count")

xChoices <- xChoicesAds <- list("By Day" = "Date",
                 "By Week" = "Week",
                 "Overall" = "All")

xChoicesAds2 <- list("Total Spending" = "Total", "Gender (incomplete data)" = "Gender", "Age (incomplete data)" = "Age")

gChoices2 <- list("Mean" = "yAvg", "Median" = "yMed", "Boxplot" = "bPlot")
gChoices3 <- list("Totals" = "yTot", "Mean" = "yAvg", "Median" = "yMed", "Boxplot" = "bPlot")

partyColors <- c("JOY PAMELA RENDI-WAGNER" = "red1", "SEBASTIAN KURZ" = "turquoise1", "NORBERT HOFER" = "blue", 
                 "BEATE MEINL-REISINGER" = "deeppink", 
                 "WERNER KOGLER" = "green3", "PETER PILZ" = "grey", "IVO HAJNAL" = "red4", "FAYAD MULLA" = "purple",
                 "SPÖ" = "red1", "ÖVP" = "turquoise1", "FPÖ" = "blue", "NEOS" = "deeppink", 
                 "GRÜNE" = "green3", "LISTE JETZT" = "grey", "KPÖ" = "red4", "WANDEL" = "purple",
                 "DIE PRESSE" = "darkslategrey", "DER STANDARD" = "firebrick", "DIE KRONE" = "darkturquoise", "FALTER" = "seagreen",
                 "HEUTE" = "saddlebrown", "KURIER" = "yellow4", "ÖSTERREICH" = "chocolate", "ORF" = "dimgrey", 
                 "PROFIL" = "orange3", "UNZENSURIERT.AT" = "royalblue", "ZEIT IM BILD" = "thistle",
                 "ARMIN WOLF" = "plum", "FLORIAN KLENK" = "mediumseagreen", "INGRID THURNHER" = "slategrey",
                 "LOU LORENZ" = "black", "CORINNA MILBORN" = "paleturquoise", "MARTIN THÜR" = "lightslategrey", 
                 "HANNO SETTELE" = "darkgrey", "FELIX BAUMGARTNER" = "lightblue", "ANDREAS GABALIER" = "darkblue",
                 "JAN BÖHMERMANN" = "red2")


minDate <- as.Date("2019-09-08")
maxDate <- as.Date("2019-10-05")

#### Utility Functions: #### ----------------------------------

get_yLabel <- function(yVal, countType = "", Site = "All") {
  yLab <- switch(yVal,
                 numPosts = "Number of Posts",
                 favoriteCount = "Number of Likes",
                 shareCount = if (Site == "All") "Number of Shares/Retweets" else "Number of Shares",
                 commentCount = "Number of Comments Received",
                 repShareRatio = "Maximum Reply to Share Ratio",
                 interactionCount = "Number of Interactions",
                 mentionCount = "Number of Mentions",
                 haha_count = "Number of Laugh Reactions",
                 love_count = "Number of Love Reactions",
                 wow_count = "Number of Wow Reactions",
                 sad_count = "Number of Sad Reactions",
                 angry_count = "Number of Angry Reactions")
  
  yLab <- paste(countType, yLab, sep = " ") %>%
    str_trim()
  return(yLab)
}

# compute the new std. dev of sum of random vars
calc_sdev <- function(V) {
  newV <- V*V 
  
  sdev <- sum(newV) %>%
    sqrt() 
}


#### Load the required data: #### -----------------------------
postsLoaded <- reactiveVal(FALSE)


# names(PolPosts)
# [1] "Name"             "user"             "userID"           "text"             "textID"           "replyToUser"     
# [7] "replyToID"        "dateCreated"      "Level"            "favoriteCount"    "shareCount"       "Site"            
# [13] "Type"             "Party"            "Funktion"         "Politician"       "origPost"         "replyCount"      
# [19] "commentCount"     "Date"             "mentionCount"     "interactionCount"

# All the Politician Posts, and Ad Data
load("Data/AppData.RData")


if (exists("PolPosts", inherits = FALSE) && exists("FBPosts", inherits = FALSE)) postsLoaded(TRUE) else postsLoaded(FALSE)


#########################################################################################
############################ Define UI ##################################################
#########################################################################################

ui <- fluidPage(
  
  # useShinyjs(),
  # extendShinyjs(text = jscode, functions = c("closeWindow")),
  
  tabsetPanel(type = "tabs",
              
              ############# Tab Panel TOTAL AGGREGATES ##################################
              tabPanel("Total Aggregates",
                       
                       sidebarLayout(
                         
                         #### SIDE BAR PANEL --------------------------------------------    
                         sidebarPanel(
                           br(),
                           br(),
                           checkboxGroupInput("plotCategory", 
                                              "Choose a category (more than one selection possible)", 
                                              choices = polChoices, 
                                              selected = "Leader"),
                           br(),
                           br(),
                           radioButtons("xAxis",
                                        "How should the time measurements be aggregated?  One selection only.",
                                        choices = xChoices,
                                        selected = "Date"),
                           br(),
                           br(),
                           conditionalPanel(
                             condition = "input.xAxis != 'Week'",
                             column(width = 12,
                             sliderInput("timeInterval",
                                         "Which time frame should be displayed?",
                                         min = minDate,
                                         max = maxDate,
                                         value = c(minDate, maxDate)),
                             br(),
                             fluidRow(
                               column(width = 4, 
                                      helpText("Use a rolling mean?"),
                                      switchInput(
                                        inputId = "RMean",
                                        value = FALSE,
                                        onStatus = "success",
                                        offStatus = "danger"
                                      )# end rMean button
                               ), # end rMean column
                               column(width = 7, offset = 1,
                                      helpText("Determine the size of the window (activated only if Rolling Mean is switched on):"),
                                      knobInput(
                                        inputId = "RMeanWindow",
                                        label = "",
                                        value = 3,
                                        step = 1,
                                        min = 1,
                                        max = 15,
                                        displayInput = TRUE,
                                        displayPrevious = TRUE,
                                        lineCap = "round",
                                        fgColor = "#428BCA",
                                        inputColor = "#428BCA",
                                        width = 60
                                      ) # end knobInput
                                      ) # end rMeanWindow column
                               ) # end RMean Window row
                             ) # end Date column
                           ),  # end conditional Panel
                           radioButtons("yAxis",
                                        "What should be measured?  Please make only one choice.",
                                        choices = yChoices,
                                        selected = "numPosts"),
                           br(),
                           br(),
                           fluidRow(column(width = 3,
                                           downloadLink('downloadData', 'Download Data')),
                                    column(width = 3, offset = 3,
                                           downloadLink('downloadPlot', 'Download Plot'))
                           ),
                           br(),
                           actionBttn(
                             inputId = "Info1",
                             label = "Info",
                             style = "pill", 
                             color = "success"
                           )
                         ),
                         
                         #### MAIN PANEL ------------------------------------------------      
                         mainPanel(
                           br(),
                           titlePanel("Total Numbers of Posts and Interactions"),
                           fluidRow(column(width = 11,
                                           plotOutput('interactionsPlot')),
                                    column(width = 1,
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomIn1",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-plus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomOut1",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-minus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomReset1",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("undo-alt"))
                                    ) # end Zoom Column
                           ),
                           br(),
                           fluidRow(column(width = 11,
                                           plotOutput('interactionsPlot2')),
                                    column(width = 1,
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomIn2",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-plus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomOut2",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-minus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomReset2",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("undo-alt"))
                                    ) # end Zoom Column
                           )
                         ) # end mainPanel
                         
                       ) # end sidebarLayout
              ), # end tabPanel Total Aggregates
              
              
              ############# Tab Panel AVERAGE AGGREGATES ################################
              tabPanel("Average Aggregates",
                       
                       sidebarLayout(
                         
                         #### SIDE BAR PANEL --------------------------------------------    
                         sidebarPanel(
                           br(),
                           br(),
                           checkboxGroupInput("plotCategory2", 
                                              "Choose a Category (you can make more than one selection)", 
                                              choices = polChoices, 
                                              selected = "Leader"),
                           br(),
                           br(),
                           radioButtons("xAxis2",
                                        "How should the time measurements be aggregated?  Please choose one",
                                        choices = xChoices,
                                        selected = "Date"),
                           br(),
                           br(),
                           conditionalPanel(
                             condition = "input.xAxis2 != 'Week'",
                             column(width = 12, 
                             sliderInput("timeInterval2",
                                         "Which time frame should be displayed?",
                                         min = minDate,
                                         max = maxDate,
                                         value = c(minDate, maxDate)),
                             br(),
                             fluidRow(
                               column(width = 4, 
                                      helpText("Should we use a rolling mean?"),
                                      switchInput(
                                        inputId = "RMean2",
                                        value = FALSE,
                                        onStatus = "success",
                                        offStatus = "danger"
                                      )# end rMean button
                               ), # end rMean column
                               column(width = 7, offset = 1,
                                      helpText("Determine the size of the window (activated only if Rolling Mean is switched on):"),
                                      knobInput(
                                        inputId = "RMeanWindow2",
                                        label = "",
                                        value = 3,
                                        step = 1,
                                        min = 1,
                                        max = 15,
                                        displayInput = TRUE,
                                        displayPrevious = TRUE,
                                        lineCap = "round",
                                        fgColor = "#428BCA",
                                        inputColor = "#428BCA",
                                        width = 60
                                      ) # end knobInput
                               ) # end rMeanWindow column
                             ) # end RMean Window row
                           ) # end Date column
                         ), # end conditional Panel
                           br(),
                           br(),
                           fluidRow(column(width = 5,
                                           radioButtons("yAxis2",
                                        "What should be measured?  Please select one",
                                        choices = yChoices2,
                                        selected = "shareCount")),
                                    column(width = 5, offset = 2,
                                           radioButtons("gType",
                                                        "Which kind of average?",
                                                        choices = gChoices2,
                                                        selected = "yAvg"))
                           ),
                          br(),
                           br(),
                           fluidRow(column(width = 3,
                                           downloadLink('downloadData2', 'Download Data')),
                                    column(width = 3, offset = 3,
                                           downloadLink('downloadPlot2', 'Download Plot'))
                           ),
                           br(),
                         actionBttn(
                           inputId = "Info2",
                           label = "Info",
                           style = "pill", 
                           color = "success"
                         )
                         ),
                         
                         #### MAIN PANEL ------------------------------------------------      
                         mainPanel(
                           br(),
                           titlePanel("Average Numbers of Posts and Interactions"),
                           fluidRow(column(width = 11,
                                           plotOutput('interactionsPlot3')),
                                    column(width = 1,
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomIn3",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-plus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomOut3",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-minus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomReset3",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("undo-alt"))
                                    ) # end Zoom Column
                           ),
                           br(),
                           fluidRow(column(width = 11,
                                           plotOutput('interactionsPlot4')),
                                    column(width = 1,
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomIn4",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-plus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomOut4",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-minus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomReset4",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("undo-alt"))
                                    ) # end Zoom Column
                           )
                         ) # end mainPanel
                         
                         
                       ) # end sidebarLayout
              ), # end tabPanel Average Aggregates
              
              
              
              ############# Tab Panel FACEBOOK REACTIONS ################################
              tabPanel("Facebook Reactions",
                       
                       sidebarLayout(
                         
                         #### SIDE BAR PANEL --------------------------------------------    
                         sidebarPanel(
                           br(),
                           br(),
                           checkboxGroupInput("plotCategory3", 
                                              "Choose a Category (you can make more than one selection)", 
                                              choices = polChoices3, 
                                              selected = "Leader"),
                           br(),
                           br(),
                           radioButtons("xAxis3",
                                        "How should time measurements be aggregated? Please select only one:",
                                        choices = xChoices,
                                        selected = "Date"),
                           br(),
                           br(),
                           conditionalPanel(
                             condition = "input.xAxis3 != 'Week'",
                             column(width = 12,
                             sliderInput("timeInterval3",
                                         "Which time frame should be displayed?",
                                         min = minDate,
                                         max = maxDate,
                                         value = c(minDate, maxDate)),
                             br(),
                             fluidRow(
                               column(width = 4, 
                                      helpText("Use a rolling mean?"),
                                      switchInput(
                                        inputId = "RMean3",
                                        value = FALSE,
                                        onStatus = "success",
                                        offStatus = "danger"
                                      )# end rMean button
                               ), # end rMean column
                               column(width = 7, offset = 1,
                                      helpText("Determine the size of the window (activated only if Rolling Mean is switched on):"),
                                      knobInput(
                                        inputId = "RMeanWindow3",
                                        label = "",
                                        value = 3,
                                        step = 1,
                                        min = 1,
                                        max = 15,
                                        displayInput = TRUE,
                                        displayPrevious = TRUE,
                                        lineCap = "round",
                                        fgColor = "#428BCA",
                                        inputColor = "#428BCA",
                                        width = 60
                                      ) # end knobInput
                               ) # end rMeanWindow column
                             ) # end RMean Window row
                           ) # end Date column
                           ), # end conditional Panel
                           br(),
                           br(),
                           fluidRow(column(width = 5,
                                           radioButtons("yAxis3",
                                                        "What should be measured? Please select only one",
                                                        choices = yChoices3,
                                                        selected = "haha_count")),
                                    column(width = 5, offset = 2,
                                           radioButtons("gType3",
                                                        "Display totals, or averages?",
                                                        choices = gChoices3,
                                                        selected = "yTot"))
                           ),
                           br(),
                           br(),
                           fluidRow(column(width = 3,
                                           downloadLink('downloadData3', 'Download Data')),
                                    column(width = 3, offset = 3,
                                           downloadLink('downloadPlot3', 'Download Plot'))
                           ),
                           br(),
                           actionBttn(
                             inputId = "Info3",
                             label = "Info",
                             style = "pill", 
                             color = "success"
                           )
                         ),
                         
                         #### MAIN PANEL ------------------------------------------------      
                         mainPanel(
                           br(),
                           titlePanel("Facebook Reactions"),
                           fluidRow(column(width = 11,
                                           plotOutput('interactionsPlot5')),
                                    column(width = 1,
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomIn5",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-plus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomOut5",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-minus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomReset5",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("undo-alt"))
                                    ) # end Zoom Column
                           ),
                           br(),
                           fluidRow(column(width = 11,
                                           plotOutput('interactionsPlot6')),
                                    column(width = 1,
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomIn6",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-plus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomOut6",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-minus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomReset6",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("undo-alt"))
                                    ) # end Zoom Column
                           )
                         ) # end mainPanel
                         
                         
                       ) # end sidebarLayout
              ), # end tabPanel Average Aggregates
              
              
              
              ############# Tab Panel PROMOTED POSTS ####################################
              tabPanel("Promoted Posts",
                       
                       sidebarLayout(
                         
                         #### SIDE BAR PANEL --------------------------------------------    
                         sidebarPanel(
                           br(),
                           br(),
                           checkboxGroupInput("plotCategoryAds", 
                                              "Select a category (more than one selection possible).", 
                                              choices = polChoicesAds, 
                                              selected = "Leader"),
                           br(),
                           br(),
                           radioButtons("xAxisAds",
                                        "How should the time measurements be aggregated? Please select only one.",
                                        choices = xChoicesAds,
                                        selected = "Date"),
                           br(),
                           br(),
                           conditionalPanel(
                             condition = "input.xAxisAds != 'Week'",
                             sliderInput("timeIntervalAds",
                                         "Which time interval should be displayed?",
                                         min = minDate,
                                         max = maxDate,
                                         value = c(minDate, maxDate)),
                             br(),
                             fluidRow(
                               column(width = 4, 
                                      helpText("Should we use a rolling mean?"),
                                      switchInput(
                                        inputId = "RMeanAds",
                                        value = FALSE,
                                        onStatus = "success",
                                        offStatus = "danger"
                                      )# end rMean button
                               ), # end rMean column
                               column(width = 7, offset = 1,
                                      helpText("Determine the size of the window (activated only if Rolling Mean is switched on):"),
                                      knobInput(
                                        inputId = "RMeanWindowAds",
                                        label = "",
                                        value = 3,
                                        step = 1,
                                        min = 1,
                                        max = 15,
                                        displayInput = TRUE,
                                        displayPrevious = TRUE,
                                        lineCap = "round",
                                        fgColor = "#428BCA",
                                        inputColor = "#428BCA",
                                        width = 60
                                      ) # end knobInput
                               ) # end rMeanWindow column
                             ) # end RMean Window row
                           ), # end Conditional Panel
                           br(),
                           br(),
                           fluidRow(column(width = 5,
                                           radioButtons("yAxisAds",
                                                        "What should be measured? Please choose only one.",
                                                        choices = yChoicesAds,
                                                        selected = "shareCount")),
                                    column(width = 5, offset = 2,
                                           radioButtons("gTypeAds",
                                                        "Which kind of average?",
                                                        choices = c("yAvg", "yMed", "bPlot"),
                                                        selected = "yAvg"))
                           ),
                           br(),
                           br(),
                           fluidRow(column(width = 3,
                                           downloadLink('downloadDataAds', 'Download Data')),
                                    column(width = 3, offset = 3,
                                           downloadLink('downloadPlotAds', 'Download Plot'))
                           ),
                           br(),
                           actionBttn(
                             inputId = "Info4",
                             label = "Info",
                             style = "pill", 
                             color = "success"
                           )
                         ),
                         
                         #### MAIN PANEL ------------------------------------------------      
                         mainPanel(
                           titlePanel(h3("Facebook Ads Library Data: A Comparison of Promoted vs. Non-Promoted Posts")),
                           br(),
                           fluidRow(column(width = 11,
                                           plotOutput('interactionsPlotAds', height = 800)),
                                    column(width = 1,
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomInAds",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-plus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomOutAds",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-minus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomResetAds",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("undo-alt"))
                                    )# end Zoom Column
                           ), # end upper plot
                           conditionalPanel(
                             condition = "input.gTypeAds != 'bPlot'",
                             br(),
                             h4("Ratio of Promoted Posts to Not Promoted Posts"),
                           br(),
                           fluidRow(column(width = 11,
                                           plotOutput('interactionsPlotAdsRatio', height = 800)),
                                    column(width = 1,
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomInAdsRatio",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-plus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomOutAdsRatio",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("search-minus")),
                                           br(),
                                           br(),
                                           actionBttn(
                                             inputId = "ZoomResetAdsRatio",
                                             label = "", 
                                             style = "material-circle",
                                             color = "danger",
                                             size = "xs",
                                             icon = icon("undo-alt"))
                                    )# end Zoom Column
                           ) # end Lower Plot  
                           ) # end conditionalPanel 
                         ) # end mainPanel
                         
                       ) # end sidebarLayout
              ), # end tabPanel Promoted Posts
              
              
              
              
              
              ############# Tab Panel ADVERTISING BUDGETS ###############################
              tabPanel("Advertising Budgets",
                       
                       fluidPage(
                         
                         #### Top PANEL --------------------------------------------    
                         titlePanel("Explore advertising budgets on Facebook"),
                         br(),
                         
                         fluidRow(
                           column(width = 3, style = "background-color:#c0c0c0;",
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                           checkboxGroupInput("plotCategoryAds2", 
                                              "Select a category (more than one selection possible).", 
                                              choices = polChoicesAds, 
                                              selected = "Leader"),
                           br(),
                           br(),
                           br(),
                           # radioButtons("xAxisAds2",
                           #              "How is the budget divided among the following demographics/regions? Please select only one.",
                           #              choices = xChoicesAds2,
                           #              selected = "Total"),
                           # br(),
                           # br(),
                           br(),
                           br(),
                           fluidRow(column(width = 3, offset = 1,
                                           downloadLink('downloadDataAds2', 'Download Data')),
                                    column(width = 3, offset = 4,
                                           downloadLink('downloadPlotAds2', 'Download Plot'))
                           ),
                           br(),
                           br(),
                           br(),
                           actionBttn(
                             inputId = "Info5",
                             label = "Info",
                             style = "pill", 
                             color = "success"
                           ),
                           br(),
                           br(),
                           br()
                         ),
                         
                         column(width = 8, offset = 1,
                           titlePanel(h3("Facebook Ads Library Data: Advertising Budgets")),
                           br(),
                           fluidRow(plotOutput('interactionsPlotAds2')),
                           br()
                           )
                         ),  # end Top Panel
 
                         #### Lower PANEL ------------------------------------------------      
                         fluidRow(
                           br(),
                           br(),
                           h4("Breakdown of advertising budgets for each Party"),
                           fluidRow(plotOutput('interactionsPlotAds3')),
                           br()
                         ) # end Lower Panel
                         
                       ) # end fluid Page
              ), # end tabPanel Promoted Posts
             
              ############# Tab Panel TOPICS HTML #######################################
              tabPanel("Explore Topics",
                       
                       fluidPage(
                         
                         titlePanel("Explore topics generated by LDA"),
                         
                         fluidRow(
                           includeHTML("topics_final_LDAn20.html")
                           ),
                         br(),
                         fluidRow(column(width = 2,
                                         actionBttn(
                                           inputId = "Info6",
                                           label = "Info",
                                           style = "pill", 
                                           color = "success"
                                         ))),
                         br()
                         )
                       ) # end Topics1 Tab Panel
              
              
  ) # end tabsetPanel
) # end fluidPage











#########################################################################################
############### Define the server function ##############################################
#########################################################################################

server <- function(input, output, session) {

  
###### GLOBAL REACTIVE VALUES ######-----------------------------------------------------
  
  yLimits <- reactiveValues(Plot1 = NULL, Plot2 = NULL, Plot3 = NULL, Plot4 = NULL, 
                            Plot5 = NULL, Plot6 = NULL, PlotAds = NULL, PlotAdsRatio = NULL)


###### Utility Functions ######----------------------------------------------------------
  
  get_rollMeanDF <- function(df, tInt, gVars, cVar, rWin, useVar = "yCount", pivotVar = "Site") {
    
    uVar <- sym(useVar)
    
    D <- as.Date(tInt[1]:tInt[2], origin = "1970-01-01")
    
    D <- tibble(Date = D)
    
    selectVars <- setdiff(gVars, "Date")
    
    df <- ungroup(df)
    
    polDF <- df %>%
      select(one_of(selectVars)) %>%
      unique()
    
    newDF <- adply(polDF, .margins = 1, function(p) {
      N <- nrow(D)
      newP <- slice(p, rep(1:n(), each = N)) %>%
        bind_rows()
      
      df <- bind_cols(newP, D)
    })
    
    newCurrDF <- select(df, one_of(c(cVar, "Date", pivotVar, useVar))) 
    
    
    newCurrDF <- right_join(newCurrDF, newDF, by = c(cVar, "Date", pivotVar)) %>%
      mutate(!!uVar := replace_na(!!uVar, 0L))
    
    newCurrDF <- newCurrDF %>%
      group_by_at(selectVars) %>%
      mutate(!!uVar := roll_mean(!!uVar, n = rWin, align = "center", fill = 0))
    
    #     newCurrDF[[useVar]] <- roll_mean(newCurrDF[[useVar]], n = rWin, align = "center", fill = 0L)

    return(newCurrDF)
  }
  
  
###### PREPROCESS DF for GRAPHING, Total Aggregates Tab ######---------------------------
  
  currentDF <- reactive({
    req(postsLoaded())
    req(input$plotCategory)
    
    currDF <- NULL
    
    plotCat <- input$plotCategory
    timeInt <- input$timeInterval
    yAxis <- input$yAxis
    xAxis <- input$xAxis
    useRMean <- input$RMean
    if (useRMean == TRUE) RWindow <- input$RMeanWindow else RWindow <- 1
    
    if (yAxis == "mentionCount") {
      usePosts <- PolPosts
    } else {
      usePosts <- filter(PolPosts, !is.na(origPost))
    }
    
    if (xAxis == "All") {
      groupVars <- c("Type", "Party", "Site", "lType")
    } else {
      if (xAxis == "Week") timeInt <- c(minDate, maxDate)
      groupVars <- c("Type", "Party", xAxis, "Site", "lType")
    }
    
    colorVar <- "Party"
    nameVar <- NULL
    
    
    if (any(plotCat %in% c("Leader", "Influencer", "Journalist", "Media"))) {
      groupVars <- union(groupVars, "Name")
      colorVar <- "Name"
      
      if (all(plotCat == "Media")) nameVar <- "Media"
      
      if ("Leader" %in% plotCat) {
        lDF <- filter(usePosts, Funktion == "Leader") %>%
          mutate(Type = "Leader")
        
        if (!is_empty(lDF)) {
          currDF <- filter(lDF, !(textID %in% currDF$textID)) %>%
            bind_rows(currDF)
        }
      }
      
      if (any(plotCat %in% c("Leader", "Influencer", "Journalist", "Media"))) {
        lDF <- filter(usePosts, Type %in% plotCat, Funktion != "Leader")
        
        if (!is_empty(currDF)) {
          currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
            bind_rows(lDF)
        } else currDF <- lDF
      }
    }
    
    if ("PartyPol" %in% plotCat) {
      lDF <- filter(usePosts, Type == "Politician", Funktion != "Leader")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF)  %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    
    if ("PartyMain" %in% plotCat) {
      lDF <- filter(usePosts, Funktion == "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    if ("Party" %in% plotCat) {
      lDF <- filter(usePosts, Type == "Party", Funktion != "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    currDF <- currDF %>%
      filter(Date >= timeInt[1], Date <= timeInt[2])
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    if (yAxis == "numPosts") {
      currDF <- group_by_at(currDF, groupVars) %>%
        summarize(yCount = n())
    } else if (yAxis == "repShareRatio") {
      currDF <- group_by_at(currDF, groupVars) %>%
        summarize(yCount = max(.data[[yAxis]], 0, na.rm = TRUE))
    } else {
      currDF <- group_by_at(currDF, groupVars) %>%
        summarize(yCount = sum(.data[[yAxis]], na.rm = TRUE))
    }
    
    
    ### if useRMean is true:
    if (useRMean == TRUE && xAxis == "Date") {
      currDF <- get_rollMeanDF(currDF, tInt = timeInt, gVars = groupVars, cVar = colorVar, rWin = RWindow)
    }
  
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    currDF$graphVar <- currDF[[colorVar]]
    if (is_empty(nameVar)) currDF$guideVar <- colorVar else currDF$guideVar <- nameVar

    return(ungroup(currDF))
  })
  

###### PREPROCESS DF for GRAPHING, Average Aggregates Tab ######------------------------- 
  
  averageDF <- reactive({
    req(input$plotCategory2)
    currDF <- NULL
    
    plotCat <- input$plotCategory2
    timeInt <- input$timeInterval2
    yAxis <- input$yAxis2
    xAxis <- input$xAxis2
    yVar <- input$gType
    BoxPlot <- yVar == "bPlot"
    useRMean <- input$RMean2
    if (useRMean == TRUE) RWindow <- input$RMeanWindow2 else RWindow <- 1
    
    usePosts <- filter(PolPosts, !is.na(origPost))
    
    if (xAxis == "All") {
      groupVars <- c("Type", "Party", "Site", "lType")
    } else {
      if (xAxis == "Week") timeInt <- c(minDate, maxDate)
      groupVars <- c("Type", "Party", xAxis, "Site", "lType")
    }
    
    colorVar <- "Party"
    nameVar <- NULL
    
    if (any(plotCat %in% c("Leader", "Influencer", "Journalist", "Media"))) {
      groupVars <- union(groupVars, "Name")
      colorVar <- "Name"
      
      if (all(plotCat == "Media")) nameVar <- "Media"
      
      if ("Leader" %in% plotCat) {
        lDF <- filter(usePosts, Funktion == "Leader") %>%
          mutate(Type = "Leader")
        
        if (!is_empty(lDF)) {
          currDF <- filter(lDF, !(textID %in% currDF$textID)) %>%
            bind_rows(currDF)
        }
      }
      
      if (any(plotCat %in% c("Leader", "Influencer", "Journalist", "Media"))) {
        lDF <- filter(usePosts, Type %in% plotCat, Funktion != "Leader")
        
        if (!is_empty(currDF)) {
          currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
            bind_rows(lDF)
        } else currDF <- lDF
      }
    }
    
    
    if ("PartyPol" %in% plotCat) {
      lDF <- filter(usePosts, Type == "Politician", Funktion != "Leader")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF)  %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    
    if ("PartyMain" %in% plotCat) {
      lDF <- filter(usePosts, Funktion == "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    if ("Party" %in% plotCat) {
      lDF <- filter(usePosts, Type == "Party", Funktion != "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    

    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    currDF <- currDF %>%
      filter(Date >= timeInt[1], Date <= timeInt[2])
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    

    if (BoxPlot == FALSE) {
    currDF <- group_by_at(currDF, groupVars) %>%
      summarize(yAvg = mean(.data[[yAxis]], na.rm = TRUE), yMed = median(.data[[yAxis]], na.rm = TRUE)) %>%
      drop_na(yAvg, yMed)
    } else {
      currDF$yCount <- currDF[[yAxis]]
      currDF <- drop_na(currDF, yCount)
    }
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    
    ### if useRMean is true:
    if (useRMean == TRUE && xAxis == "Date" && BoxPlot == FALSE) {
      currDF <- get_rollMeanDF(currDF, tInt = timeInt, gVars = groupVars, cVar = colorVar, rWin = RWindow, useVar = yVar)
    }
    
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL) 

    currDF$graphVar <- currDF[[colorVar]]
    if (is_empty(nameVar)) currDF$guideVar <- colorVar else currDF$guideVar <- nameVar
    
    return(ungroup(currDF))
  })


###### PREPROCESS DF for GRAPHING, Facebook Reactions Tab ######---------------------------
  
  totalFB <- reactive({
    req(postsLoaded())
    req(input$plotCategory3)
    currDF <- NULL
    
    plotCat <- input$plotCategory3
    timeInt <- input$timeInterval3
    yAxis <- input$yAxis3
    xAxis <- input$xAxis3
    useRMean <- input$RMean3
    if (useRMean == TRUE) RWindow <- input$RMeanWindow3 else RWindow <- 1
    
    
      usePosts <- FBPosts

    if (xAxis == "All") {
      groupVars <- c("Type", "Party", "isAd")
    } else {
      if (xAxis == "Week") timeInt <- c(minDate, maxDate)
      groupVars <- c("Type", "Party", xAxis, "isAd")
    }
    
    colorVar <- "Party"
    nameVar <- NULL
    
    if (any(plotCat %in% c("Leader", "Influencer", "Journalist", "Media"))) {
      groupVars <- union(groupVars, "Name")
      colorVar <- "Name"
      
      if (all(plotCat == "Media")) nameVar <- "Media"
      
      if ("Leader" %in% plotCat) {
        lDF <- filter(usePosts, Funktion == "Leader") %>%
          mutate(Type = "Leader")
        
        if (!is_empty(lDF)) {
          currDF <- filter(lDF, !(textID %in% currDF$textID)) %>%
            bind_rows(currDF)
          
        }
      }
      
      if (any(plotCat %in% c("Leader", "Influencer", "Journalist", "Media"))) {
        lDF <- filter(usePosts, Type %in% plotCat, Funktion != "Leader")
        
        if (!is_empty(currDF)) {
          currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
            bind_rows(lDF)
        } else currDF <- lDF
      }
    }
    
    if ("PartyPol" %in% plotCat) {
      lDF <- filter(usePosts, Type == "Politician", Funktion != "Leader")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF)  %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    
    if ("PartyMain" %in% plotCat) {
      lDF <- filter(usePosts, Funktion == "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    if ("Party" %in% plotCat) {
      lDF <- filter(usePosts, Type == "Party", Funktion != "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    currDF <- currDF %>%
      filter(Date >= timeInt[1], Date <= timeInt[2])
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
      currDF <- group_by_at(currDF, groupVars) %>%
        summarize(yCount = sum(.data[[yAxis]], na.rm = TRUE))

      ### if useRMean is true:
      if (useRMean == TRUE && xAxis == "Date") {
        currDF <- get_rollMeanDF(currDF, tInt = timeInt, gVars = groupVars, cVar = colorVar, rWin = RWindow, pivotVar = "isAd")
      }
      
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    currDF$graphVar <- currDF[[colorVar]]
    if (is_empty(nameVar)) currDF$guideVar <- colorVar else currDF$guideVar <- nameVar
    
    return(ungroup(currDF))
  })
  
  averageFB <- reactive({
    req(postsLoaded())
    req(input$plotCategory3)
    currDF <- NULL
    
    plotCat <- input$plotCategory3
    timeInt <- input$timeInterval3
    yAxis <- input$yAxis3
    xAxis <- input$xAxis3
    yVar <- input$gType3
    useRMean <- input$RMean3
    if (useRMean == TRUE) RWindow <- input$RMeanWindow3 else RWindow <- 1
    
    if (xAxis == "All") {
      groupVars <- c("Type", "Party", "isAd")
    } else {
      if (xAxis == "Week") timeInt <- c(minDate, maxDate)
      groupVars <- c("Type", "Party", xAxis, "isAd")
    }
    
    colorVar <- "Party"
    nameVar <- NULL
    BoxPlot <- yVar == "bPlot"

    usePosts <- FBPosts
    
    if (any(plotCat %in% c("Leader", "Influencer", "Journalist", "Media"))) {
      groupVars <- union(groupVars, "Name")
      colorVar <- "Name"
      
      if (all(plotCat == "Media")) nameVar <- "Media"
      
      if ("Leader" %in% plotCat) {
        lDF <- filter(usePosts, Funktion == "Leader") %>%
          mutate(Type = "Leader")
        
        if (!is_empty(lDF)) {
          currDF <- filter(lDF, !(textID %in% currDF$textID)) %>%
            bind_rows(currDF)
        }
      }
      
      if (any(plotCat %in% c("Leader", "Influencer", "Journalist", "Media"))) {
        lDF <- filter(usePosts, Type %in% plotCat, Funktion != "Leader")
        
        if (!is_empty(currDF)) {
          currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
            bind_rows(lDF)
        } else currDF <- lDF
      }
    }
    
    
    if ("PartyPol" %in% plotCat) {
      lDF <- filter(usePosts, Type == "Politician", Funktion != "Leader")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF)  %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    
    if ("PartyMain" %in% plotCat) {
      lDF <- filter(usePosts, Funktion == "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    if ("Party" %in% plotCat) {
      lDF <- filter(usePosts, Type == "Party", Funktion != "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    currDF <- currDF %>%
      filter(Date >= timeInt[1], Date <= timeInt[2])
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    
    
    if (BoxPlot == FALSE) {
      currDF <- group_by_at(currDF, groupVars) %>%
        summarize(yAvg = mean(.data[[yAxis]], na.rm = TRUE), yMed = median(.data[[yAxis]], na.rm = TRUE))
    } else {
      currDF$yCount <- currDF[[yAxis]]
    }
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    ### if useRMean is true:
    if (useRMean == TRUE && xAxis == "Date" && BoxPlot == FALSE) {
      currDF <- get_rollMeanDF(currDF, tInt = timeInt, gVars = groupVars, cVar = colorVar, 
                               rWin = RWindow, useVar = yVar, pivotVar = "isAd")
    }
    
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL) 
    
    currDF$graphVar <- currDF[[colorVar]]
    if (is_empty(nameVar)) currDF$guideVar <- colorVar else currDF$guideVar <- nameVar
    
    return(ungroup(currDF))
  })
    
###### PREPROCESS DF for GRAPHING, Promoted Posts Tab ######----------------------------- 
  adsDF <- reactive({
    req(postsLoaded())
    req(input$plotCategoryAds)
    
    currDF <- NULL
    
    plotCat <- input$plotCategoryAds
    timeInt <- input$timeIntervalAds
    yAxis <- input$yAxisAds
    xAxis <- input$xAxisAds
    yVar <- input$gTypeAds
    BoxPlot <- yVar == "bPlot"
    useRMean <- input$RMeanAds
    if (useRMean == TRUE) RWindow <- input$RMeanWindowAds else RWindow <- 1
    
    usePosts <- FBPosts
    
    if (xAxis == "All") {
      groupVars <- c("Type", "Party", "isAd")
    } else {
      if (xAxis == "Week") timeInt <- c(minDate, maxDate)
      groupVars <- c("Type", "Party", xAxis, "isAd")
    }
    
    colorVar <- "Party"
    
    
    if ("Leader" %in% plotCat) {
      groupVars <- union(groupVars, "Name")
      colorVar <- "Name"
      
      lDF <- filter(usePosts, Funktion == "Leader") %>%
        mutate(Type = "Leader")
      
      if (!is_empty(lDF)) {
        currDF <- filter(lDF, !(textID %in% currDF$textID)) %>%
          bind_rows(currDF)
      }
    }
    
    
    if ("PartyPol" %in% plotCat) {
      lDF <- filter(usePosts, Type == "Politician", Funktion != "Leader")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF)  %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    
    if ("PartyMain" %in% plotCat) {
      lDF <- filter(usePosts, Funktion == "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    if ("Party" %in% plotCat) {
      lDF <- filter(usePosts, Type == "Party", Funktion != "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    currDF <- currDF %>%
      filter(Date >= timeInt[1], Date <= timeInt[2])
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    
    
    if (BoxPlot == FALSE) {
      currDF <- group_by_at(currDF, groupVars) %>%
        summarize(yAvg = mean(.data[[yAxis]], na.rm = TRUE), yMed = median(.data[[yAxis]], na.rm = TRUE))
    } else {
      currDF$yCount <- currDF[[yAxis]]
    }
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    
    ### if useRMean is true:
    if (useRMean == TRUE && xAxis == "Date" && BoxPlot == FALSE) {
      currDF <- get_rollMeanDF(currDF, tInt = timeInt, gVars = groupVars, cVar = colorVar, rWin = RWindow, useVar = yVar, pivotVar = "isAd")
    }
    
    currDF$graphVar <- currDF[[colorVar]]
    currDF$guideVar <- colorVar
    
    return(ungroup(currDF))
  })

  
  ## compute the ratio promoted to not promoted posts  
  ratioDF <- reactive({
    req(postsLoaded())
    req(adsDF())
    
    yVal <- input$gTypeAds
    
    
    if (yVal == "bPlot") return(NULL)
    
    currDF <- adsDF()
    
    tempDF <- currDF %>%
      mutate(isAd = str_replace(isAd, " ", "")) 
    
    if (yVal == "yAvg") {
      keepVars <- setdiff(names(tempDF), "yMed")
      tempDF <- tempDF %>%
        select(one_of(keepVars)) %>%
        pivot_wider(names_from = isAd, values_from = yAvg, names_prefix = "yAvg_", names_sep = "_", values_fill = list(yAvg = 0)) %>%
        mutate(yAvgRatio = ifelse((yAvg_notPromoted < 0.01 | yAvg_Promoted < 0.01), NA, round((yAvg_Promoted/yAvg_notPromoted), 2)))
    } else {
      keepVars <- setdiff(names(tempDF), "yAvg")
      tempDF <- tempDF %>%
        select(one_of(keepVars)) %>%
        pivot_wider(names_from = isAd, values_from = yMed, names_prefix = "yMed_", names_sep = "_", values_fill = list(yMed = 0)) %>%
        mutate(yMedRatio = ifelse((yMed_notPromoted < 0.01 | yMed_Promoted < 0.01), NA, round((yMed_Promoted/yMed_notPromoted), 2))) 
    }
    
    return(tempDF)
  })
  
###### PREPROCESS DF for GRAPHING, Advertising Budget Tab ######-------------------------
  
  adsDF2 <- reactive({
    req(postsLoaded())
    req(input$plotCategoryAds2)
    
    plotCat <- input$plotCategoryAds2
    xAxis <- "total"    #  str_to_lower(input$xAxisAds2)
    
    switch(xAxis,
           "gender" = {
             useDF <- GenderAds %>%
               rename(Spending = GSpending, StDev = GStDev)
           },
           "age" = {
             useDF <- AgeAds %>%
               rename(Spending = GSpending, StDev = GStDev)
           },
           "region" = {
             useDF <- RegionAds %>%
               rename(Spending = RSpending, StDev = RStDev)
           },
           useDF <- TotalAds %>%
             rename(Spending = TotSpent) %>%
             mutate(StDev = 0))
    
    
    currDF <- NULL
    groupVars <- "Party"
    xVar <- xAxis
    
    if (xAxis != "total") groupVars <- c(groupVars, xAxis)
    
    if ("Leader" %in% plotCat) {
      groupVars <- union("Name", groupVars)
      colorVar <- "Name"
      
      lDF <- filter(useDF, Funktion == "Leader") %>%
        mutate(Type = "Leader")
      
      
      if (!is_empty(lDF)) {
        currDF <- filter(lDF, !(Name %in% currDF$Name)) %>%
          bind_rows(currDF)
      }
      
    }
    
    
    if ("PartyPol" %in% plotCat) {
      lDF <- filter(useDF, Type == "Politician", Funktion != "Leader")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(Name %in% lDF$Name)) %>%
          bind_rows(lDF)  %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    
    if ("PartyMain" %in% plotCat) {
      lDF <- filter(useDF, Funktion == "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(Name %in% lDF$Name)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    if ("Party" %in% plotCat) {
      lDF <- filter(useDF, Type == "Party", Funktion != "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(Name %in% lDF$Name)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      colorVar <- "Party"
    }
    
    currDF <- group_by_at(currDF, groupVars) %>%
      summarize(TotAmount = sum(Spending),
                TotSD = calc_sdev(StDev))
    
    currDF <- filter(currDF, !is.na(Party)) %>%
      mutate(TotSD = ifelse(is.na(TotSD), 0.75*TotAmount, TotSD)) %>%
      mutate(TotPlus = round(TotAmount + TotSD),
             TotMinus = round(TotAmount - TotSD))
    
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    currDF$xVar <- xVar
    currDF$graphVar <- currDF[[colorVar]]
    currDF$guideVar <- colorVar
    
    return(currDF)
    
  })
  
  
  ### within party variation
  
  adsDF3 <- reactive({
    req(postsLoaded())
    xAxis <- "total"    #  str_to_lower(input$xAxisAds2)
    
    switch(xAxis,
           "gender" = {
             useDF <- GenderAds %>%
               rename(Spending = GSpending, StDev = GStDev)
           },
           "age" = {
             useDF <- AgeAds %>%
               rename(Spending = GSpending, StDev = GStDev)
           },
           "region" = {
             useDF <- RegionAds %>%
               rename(Spending = RSpending, StDev = RStDev)
           },
           useDF <- TotalAds %>%
             rename(Spending = TotSpent) %>%
             mutate(StDev = 0))
    
    
    currDF <- NULL
    groupVars <- c("Type", "Party")
    xVar <- xAxis
    
    if (xAxis != "total") groupVars <- c(groupVars, xAxis)
    
    useDF = mutate(useDF, Type = ifelse(Funktion == "Leader", "Leader", 
                                        ifelse(Funktion == "Bundespartei", "National Party",
                                               ifelse(Type == "Politician", "Candidate", "Regional or Local Party"))))
    
    currDF <- group_by_at(useDF, groupVars) %>%
      summarize(TotAmount = sum(Spending),
                TotSD = calc_sdev(StDev))
    
    currDF <- filter(currDF, !is.na(Party)) %>%
      mutate(TotSD = ifelse(is.na(TotSD), 0.75*TotAmount, TotSD)) %>%
      mutate(TotPlus = round(TotAmount + TotSD),
             TotMinus = round(TotAmount - TotSD))
    
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    currDF$xVar <- xVar
    
    return(ungroup(currDF))
    
  }) 
  
###### CREATE PLOTS  for TOTAL AGGREGATES ######-----------------------------------------

### interactions divided by Site ###
facetPlot <- reactive({
  req(postsLoaded())
  req(currentDF())
  
  yLim <- yLimits$Plot1
  yAxis <- input$yAxis
  xAxis <- input$xAxis
  yLabel <- get_yLabel(yAxis)
  gDF <- currentDF() %>%
    mutate(Party = factor(Party)) %>%
    arrange(Type, Party)
  
  nameVar <- unique(gDF$guideVar)

  if (xAxis == "Date") {
    g <- ggplot(gDF, aes(x = Date, y = yCount)) +
      geom_line(aes(color = graphVar, group = graphVar, linetype = lType)) +
      facet_wrap(~Site) +
      scale_color_manual(values = partyColors) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      labs(color = nameVar, linetype = "Function")
  } else if (xAxis == "Week") {
    g <- ggplot(gDF, aes(x = Week, y = yCount)) +
      geom_col(aes_string(fill = "Party", color = "lType"), position = "dodge", size = 1.5) +
      facet_wrap(~Site) +
      scale_fill_manual(values = partyColors) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      labs(fill = "Name", color = "Function") +
      guides(linetype = FALSE)
  } else {
    g <- ggplot(gDF, aes(x = graphVar, weight = yCount)) +
      geom_bar(aes_string(fill = "Party", color = "lType"), size = 2) +
      facet_wrap(~Site) +
      scale_fill_manual(values = partyColors) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      labs(fill = nameVar, color = "Function") +
      guides(linetype = FALSE)
  }
  
  g <- g + 
    ylab(yLabel)
  
  if (!is_empty(yLim)) g <- g + coord_cartesian(ylim = yLim)
  
  return(g)
  
}) 

 
  output$interactionsPlot = renderPlot({
    req(facetPlot())
    facetPlot()
  })
  
  
  
### interactions combining both sites  ###
  
  allPlot <- reactive({
    req(postsLoaded())
    req(currentDF())
    
    yLim <- yLimits$Plot2
    yAxis <- input$yAxis
    xAxis <- input$xAxis
    yLabel <- get_yLabel(yAxis)
    gDF <- currentDF() %>%
      mutate(Party = factor(Party))
    
    nameVar <- unique(gDF$guideVar)
    groupVars <- setdiff(names(gDF), c("Site", "yCount"))
    gDF <- group_by_at(gDF, groupVars) %>%
      summarize(allCount = sum(yCount))  %>%
      arrange(Type, Party)
    
    if (xAxis == "Date") {
      g <- ggplot(gDF, aes(x = Date, y = allCount)) +
        geom_line(aes(color = graphVar, group = graphVar, linetype = lType)) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = nameVar, linetype = "Function")
    } else if (xAxis == "Week") {
      g <- ggplot(gDF, aes(x = Week, y = allCount)) +
        geom_col(aes_string(fill = "Party", color = "lType", linetype = "lType"), position = "dodge", size = 1.5) +
        scale_fill_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(fill = nameVar, color = "Function") +
        guides(linetype = FALSE)
    } else {
      g <- ggplot(gDF, aes(x = graphVar, weight = allCount)) +
        geom_bar(aes_string(fill = "Party", color = "lType", linetype = "lType"), size = 2) +
        scale_fill_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(fill = nameVar, color = "Function") +
        guides(linetype = FALSE)
    }
    
    g <- g + 
      ggtitle("Facebook and Twitter Combined") +
      ylab(yLabel)
    
    if (!is_empty(yLim)) g <- g + coord_cartesian(ylim = yLim)
    
    return(g)
    
  })
  
  output$interactionsPlot2 = renderPlot({
    req(allPlot())
    allPlot()
  })


###### CREATE PLOTS  for AVERAGE AGGREGATES ######---------------------------------------
 
  boxGraph <- function(DF, X, Y, Facet = TRUE) {
    nameVar = unique(DF$guideVar)

    if (X %in% c("Date", "Week")) {
      g <- ggplot(DF, aes_string(x = "graphVar", y = Y)) +
        geom_boxplot(aes_string(fill = "Party", color = "Party")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = nameVar) +
        guides(fill = FALSE)
      if (Facet == TRUE) {
        g <- g + facet_grid(rows = vars(Site), cols = vars(Week))
      } else {
        g <- g + facet_wrap(~Week)
      }
      
    } else {
      g <- ggplot(DF, aes_string(x = "graphVar", y = Y)) +
        geom_boxplot(aes_string(fill = "Party", color = "Party")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = nameVar) +
        guides(fill = FALSE)
      if (Facet == TRUE) g <- g + facet_wrap(~Site)
      
    }
    
    return(g)
  }
  
  
  ### interactions divided by Site ###
  facetBoxPlot <- reactive({
    req(postsLoaded())
    req(averageDF())
    req(input$plotCategory2)
    
    yLim <- yLimits$Plot3
    yAxis <- input$yAxis2
    xAxis <- input$xAxis2
    yVal <- input$gType
    
    if (yVal == "yAvg") {
      cType <- "Mean"
      bPlot <- FALSE
    } else if (yVal == "yMed") {
      cType = "Median"
      bPlot <- FALSE
    } else {
      cType <- ""
      bPlot <- TRUE
    }
    
    yLabel <- get_yLabel(yAxis, countType = cType)
    
    gDF <- averageDF() %>%
      mutate(Party = factor(Party)) %>%
      arrange(Type, Party)
    
    nameVar <- unique(gDF$guideVar)
    
    if (bPlot == TRUE) {
      g <- boxGraph(gDF, xAxis, yAxis, Facet = TRUE)
    } else {
      if (xAxis == "Date") {
        g <- ggplot(gDF, aes_string(x = "Date", y = yVal)) +
          geom_line(aes(color = graphVar, group = graphVar, linetype = lType)) +
          facet_wrap(~Site) +
          scale_color_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(color = nameVar, linetype = "Function")
      } else if (xAxis == "Week") {
        g <- ggplot(gDF, aes_string(x = "Week", y = yVal)) +
          geom_col(aes_string(fill = "Party", color = "lType"), position = "dodge", size = 1.5) +
          facet_wrap(~Site) +
          scale_fill_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = nameVar, color = "Function") +
          guides(linetype = FALSE)
      } else {
        g <- ggplot(gDF, aes_string(x = "graphVar", weight = yVal)) +
          geom_bar(aes_string(fill = "Party", color = "lType"), size = 2) +
          facet_wrap(~Site) +
          scale_fill_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = nameVar, color = "Function") +
          guides(linetype = FALSE)
      }
    }
    
    g <- g + 
      ylab(yLabel)
    
    if (!is_empty(yLim)) g <- g + coord_cartesian(ylim = yLim)
    
    return(g)
    
  }) 
  
  
  output$interactionsPlot3 = renderPlot({
    req(facetBoxPlot())
    facetBoxPlot()
  })
  
  
  ### interactions combining both sites  ###
  
  allBoxPlot <- reactive({
    req(postsLoaded())
    req(averageDF())
    req(input$plotCategory2)
    
    yLim <- yLimits$Plot4
    yAxis <- input$yAxis2
    xAxis <- input$xAxis2
    yVal <- input$gType
    
    if (yVal == "yAvg") {
      cType <- "Mean"
      bPlot <- FALSE
    } else if (yVal == "yMed") {
      cType = "Median"
      bPlot <- FALSE
    } else {
      cType <- ""
      bPlot <- TRUE
    }
    
    yLabel <- get_yLabel(yAxis, countType = cType)
    
    gDF <- averageDF() %>%
      mutate(Party = factor(Party)) %>%
      arrange(Type, Party)
    
    nameVar <- unique(gDF$guideVar)
    

    groupVars <- setdiff(names(gDF), c("Site", "yCount", "yMed", "yAvg"))

    if (bPlot == TRUE) {
      gDF <- group_by_at(gDF, groupVars) %>%
        summarize(allCount = sum(.data[[yAxis]], na.rm = TRUE))  %>%
        arrange(Type, Party)
      g <- boxGraph(gDF, xAxis, "allCount", Facet = FALSE)
    } else {
      gDF <- group_by_at(gDF, groupVars) %>%
        summarize(allCount = sum(.data[[yVal]]))  %>%
        arrange(Type, Party)
      
    if (xAxis == "Date") {
      g <- ggplot(gDF, aes(x = Date, y = allCount)) +
        geom_line(aes(color = graphVar, group = graphVar, linetype = lType)) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = nameVar, linetype = "Function")
    } else if (xAxis == "Week") {
      g <- ggplot(gDF, aes(x = Week, y = allCount)) +
        geom_col(aes_string(fill = "Party", color = "lType", linetype = "lType"), position = "dodge", size = 1.5) +
        scale_fill_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(fill = nameVar, color = "Function") +
        guides(linetype = FALSE)
    } else {
      g <- ggplot(gDF, aes(x = graphVar, weight = allCount)) +
        geom_bar(aes_string(fill = "Party", color = "lType", linetype = "lType"), size = 2) +
        scale_fill_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(fill = nameVar, color = "Function") +
        guides(linetype = FALSE)
    }
    }
    
    g <- g + 
      ggtitle("Facebook and Twitter Combined") +
      ylab(yLabel)
    
    if (!is_empty(yLim)) g <- g + coord_cartesian(ylim = yLim)
    
    return(g)
    
  })
  
  output$interactionsPlot4 = renderPlot({
    req(allBoxPlot())
    allBoxPlot()
  })
  
  
  
  
###### CREATE PLOTS for FB REACTIONS ######---------------------------------------------
  
  boxFBGraph <- function(DF, X, Y, Facet = TRUE) {
    nameVar = unique(DF$guideVar)
    
    if (Facet == TRUE) xVar <- "isAd" else xVar <- "graphVar"
    
    if (X %in% c("Date", "Week")) {
      g <- ggplot(DF, aes_string(x = xVar, y = Y)) +
        geom_boxplot(aes_string(fill = "Party", color = "Party")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = "Name") +
        guides(fill = FALSE)
      if (Facet == TRUE) {
        g <- g + facet_grid(cols = vars(graphVar), rows = vars(Week))
      } else {
        g <- g + facet_wrap(~Week)
      }
      
    } else {
      g <- ggplot(DF, aes_string(x = xVar, y = Y)) +
        geom_boxplot(aes_string(fill = "Party", color = "Party")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = nameVar) +
        guides(fill = FALSE)
      if (Facet == TRUE) g <- g + facet_wrap(vars(graphVar))
      
    }
    
    return(g)
  }
  
  
  ### interactions divided by Promoted vs. non-Promoted ###
  FBPlot <- reactive({
    req(postsLoaded())
    req(totalFB())
    
    yLim <- yLimits$Plot5
    yAxis <- input$yAxis3
    xAxis <- input$xAxis3
    yVal <- input$gType3
    useRMean <- input$RMean3
    
    if (yVal == "yTot") {
      cType <- "Total"
      bPlot <- FALSE
    } else if (yVal == "yAvg") {
      cType <- "Mean"
      bPlot <- FALSE
    } else if (yVal == "yMed") {
      cType = "Median"
      bPlot <- FALSE
    } else {
      cType <- ""
      bPlot <- TRUE
    }
    
    yLabel <- get_yLabel(yAxis, countType = cType, Site = "FB")
    
    
    if (yVal == "yTot") {
      gDF <- req(totalFB())  %>%
        mutate(Party = factor(Party)) %>%
        arrange(Type, Party)
      
      nameVar <- unique(gDF$guideVar)
      
      if (xAxis == "Date") {
        g <- ggplot(gDF, aes(x = Date, y = yCount)) +
          geom_line(aes(color = graphVar, linetype = isAd)) +
          scale_color_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(color = nameVar, linetype = "Promoted?")
      } else if (xAxis == "Week") {
        g <- ggplot(gDF, aes(x = isAd, y = yCount)) +
          geom_col(aes(fill = graphVar, color = isAd), position = "dodge", size = 1.5) +
          facet_grid(rows = vars(Week), cols = vars(graphVar)) +
          scale_fill_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = nameVar, color = "Promoted?") 
      } else {
        g <- ggplot(gDF, aes_string(x = "isAd", weight = "yCount")) +
          geom_bar(aes(fill = graphVar, color = isAd), size = 2) +
          scale_fill_manual(values = partyColors) +
          facet_wrap(vars(!!gVar)) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = nameVar, color = "Promoted?")
      }
      
    } else {
      gDF <- req(averageFB())  %>%
        mutate(Party = factor(Party)) %>%
        arrange(Type, Party)
      
      nameVar <- unique(gDF$guideVar)
      
      if (bPlot == TRUE) {
        g <- boxFBGraph(gDF, xAxis, yAxis, Facet = TRUE)
      } else {
        
        if (xAxis == "Date") {
          g <- ggplot(gDF, aes_string(x = "Date", y = yVal)) +
            geom_line(aes(color = graphVar, linetype = isAd)) +
            scale_color_manual(values = partyColors) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            labs(color = nameVar, linetype = "Function")
        } else if (xAxis == "Week") {
          g <- ggplot(gDF, aes_string(x = "isAd", y = yVal)) +
            geom_col(aes_string(fill = "Party", color = "Party"), position = "dodge", size = 1.5) +
            facet_grid(cols = vars(graphVar), rows = vars(Week)) +
            scale_fill_manual(values = partyColors) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            labs(fill = nameVar) +
            guides(color = FALSE)
        } else {
          g <- ggplot(gDF, aes_string(x = "isAd", weight = yVal)) +
            geom_bar(aes_string(fill = "Party", color = "Party"), size = 2) +
            facet_wrap(vars(graphVar)) +
            scale_fill_manual(values = partyColors) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            labs(fill = nameVar) +
            guides(color = FALSE)
        }
      }
    }
    
    g <- g + 
      ggtitle("Facebook Reactions: Promoted vs. Non-Promoted Combined") +
      ylab(yLabel)
    
    if (!is_empty(yLim)) g <- g + coord_cartesian(ylim = yLim)
    
    return(g)
    
  }) 
  
  
  output$interactionsPlot5 = renderPlot({
    req(FBPlot())
    FBPlot()
  })
  
  
  
  ### interactions combining Promoted & non-Promoted Posts  ###
  
  allFBPlot <- reactive({
    req(postsLoaded())

    yLim <- yLimits$Plot6
    yAxis <- input$yAxis3
    xAxis <- input$xAxis3
    yLabel <- get_yLabel(yAxis, Site = "FB")
    
    yVal <- input$gType3
    useRMean <- input$RMean3
    
    if (yVal == "yTot") {
      cType <- "Total"
      bPlot <- FALSE
    } else if (yVal == "yAvg") {
      cType <- "Mean"
      bPlot <- FALSE
    } else if (yVal == "yMed") {
      cType = "Median"
      bPlot <- FALSE
    } else {
      cType <- ""
      bPlot <- TRUE
    }
    

    if (yVal == "yTot") {
    gDF <- req(totalFB()) %>%
      mutate(Party = factor(Party))
    
    nameVar <- unique(gDF$guideVar)
    groupVars <- setdiff(names(gDF), c("isAd", "yCount"))
    gDF <- group_by_at(gDF, groupVars) %>%
      summarize(allCount = sum(yCount))  %>%
      arrange(Type, Party)
    
    if (xAxis == "Date") {
      g <- ggplot(gDF, aes(x = Date, y = allCount)) +
        geom_line(aes(color = graphVar, group = graphVar)) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = nameVar)
    } else if (xAxis == "Week") {
      g <- ggplot(gDF, aes(x = Week, y = allCount)) +
        geom_col(aes_string(fill = "Party"), position = "dodge", size = 1.5) +
        scale_fill_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(fill = nameVar) +
        guides(linetype = FALSE)
    } else {
      g <- ggplot(gDF, aes(x = graphVar, weight = allCount)) +
        geom_bar(aes_string(fill = "Party"), size = 2, position = "dodge") +
        scale_fill_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(fill = nameVar) +
        guides(linetype = FALSE)
    }
    } else {
      gDF <- req(averageFB()) %>%
        mutate(Party = factor(Party))
      
      nameVar <- unique(gDF$guideVar)
      groupVars <- setdiff(names(gDF), c("isAd", "yCount", "yMed", "yAvg"))
      

      if (bPlot == TRUE) {
        gDF <- group_by_at(gDF, groupVars) %>%
          summarize(allCount = sum(.data[[yAxis]], na.rm = TRUE))  %>%
          arrange(Type, Party)
        g <- boxFBGraph(gDF, xAxis, "allCount", Facet = FALSE)
      } else {
        gDF <- group_by_at(gDF, groupVars) %>%
          summarize(allCount = sum(.data[[yVal]]))  %>%
          arrange(Type, Party)
        
        if (xAxis == "Date") {
          g <- ggplot(gDF, aes_string(x = "Date", y = "allCount")) +
            geom_line(aes(color = graphVar)) +
            scale_color_manual(values = partyColors) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            labs(color = nameVar)
        } else if (xAxis == "Week") {
          g <- ggplot(gDF, aes(x = graphVar, y = allCount)) +
            geom_col(aes_string(fill = "Party", color = "Party"), position = "dodge", size = 1.5) +
            facet_wrap(~Week) +
            scale_fill_manual(values = partyColors) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            labs(fill = nameVar) +
            guides(color = FALSE)
        } else {
          g <- ggplot(gDF, aes(x = graphVar, weight = allCount)) +
            geom_bar(aes_string(fill = "Party", color = "Party"), size = 2) +
            scale_fill_manual(values = partyColors) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
            labs(fill = nameVar) +
            guides(color = FALSE)
        }
      }
      
    }
    
    g <- g + 
      ggtitle("Facebook Reactions: Promoted and Non-Promoted Combined") +
      ylab(yLabel)
    
    if (!is_empty(yLim)) g <- g + coord_cartesian(ylim = yLim)
    
    return(g)
    
  })
  
  output$interactionsPlot6 = renderPlot({
    req(allFBPlot())
    allFBPlot()
  })
  
  
###### CREATE PLOTS  for PROMOTED POSTS ######-------------------------------------------
  
  boxGraphAds <- function(DF, X, Y) {
    nameVar = unique(DF$guideVar)
    
    if (X %in% c("Date", "Week")) {
      g <- ggplot(DF, aes_string(x = "isAd", y = Y)) +
        geom_boxplot(aes_string(fill = "Party", color = "Party")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = nameVar) +
        guides(fill = FALSE)
      g <- g + facet_grid(cols = vars(graphVar), rows = vars(Week))
    } else {
      g <- ggplot(DF, aes_string(x = "isAd", y = Y)) +
        geom_boxplot(aes_string(fill = "Party", color = "Party")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = nameVar) +
        guides(fill = FALSE) 
      g <- g + facet_wrap(~graphVar, ncol = 1)
    }
    
    return(g)
  }
  
  
  ### interactions divided by isAd ###
  facetBoxPlotAds <- reactive({
    req(postsLoaded())
    req(adsDF())
    req(input$plotCategoryAds)
    
    yLim <- yLimits$PlotAds
    yAxis <- input$yAxisAds
    xAxis <- input$xAxisAds
    yVal <- input$gTypeAds
    
    if (yVal == "yAvg") {
      cType <- "Mean"
      bPlot <- FALSE
    } else if (yVal == "yMed") {
      cType = "Median"
      bPlot <- FALSE
    } else {
      cType <- ""
      bPlot <- TRUE
    }
    
    yLabel <- get_yLabel(yAxis, countType = cType, Site = "FB")
    
    gDF <- adsDF() %>%
      mutate(Party = factor(Party)) %>%
      arrange(Type, Party)
    
    nameVar <- unique(gDF$guideVar)
    
    gDF <- mutate(gDF, groupVar = paste0(graphVar, isAd))
    
    if (bPlot == TRUE) {
      g <- boxGraphAds(gDF, xAxis, yAxis)
    } else {
      if (xAxis == "Date") {
        g <- ggplot(gDF, aes_string(x = "Date", y = yVal)) +
          geom_line(aes(color = graphVar, group = groupVar, linetype = isAd)) +
          scale_color_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(color = nameVar, linetype = "Advertised?")
      } else if (xAxis == "Week") {
        g <- ggplot(gDF, aes_string(x = "isAd", y = yVal)) +
          geom_col(aes(fill = graphVar, color = isAd), position = "dodge", size = 1.5) +
          facet_grid(row = vars(Party), col = vars(Week)) +
          scale_fill_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = nameVar) +
          guides(color = FALSE)
      } else {
        g <- ggplot(gDF, aes_string(x = "isAd", weight = yVal)) +
          geom_bar(aes(fill = graphVar, color = isAd), size = 2) +
          facet_wrap(~Party, ncol = 1) +
          scale_fill_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = nameVar) +
          guides(color = FALSE)
      }
    }
    
    g <- g + 
      ylab(yLabel)
    
    if (!is_empty(yLim)) g <- g + coord_cartesian(ylim = yLim)
    
    return(g)
    
  }) 
  
  
  output$interactionsPlotAds = renderPlot({
    req(facetBoxPlotAds())
    facetBoxPlotAds()
  })

  
  ### interactions ratio: Promoted divided by not Promoted ###
  facetBoxPlotAdsRatio <- reactive({
    req(postsLoaded())
    req(ratioDF())
    
    
    yLim <- yLimits$PlotAdsRatio
    yAxis <- input$yAxisAds
    xAxis <- input$xAxisAds
    yVal <- input$gTypeAds
    
    if (yVal == "yAvg") {
      cType <- "Mean"
      bPlot <- FALSE
      yVal <- "yAvgRatio"
    } else if (yVal == "yMed") {
      cType = "Median"
      bPlot <- FALSE
      yVal <- "yMedRatio"
    } else {
      cType <- ""
      bPlot <- TRUE
    }
    
    yLabel1 <- get_yLabel(yAxis, countType = cType, Site = "FB")
    
    yLabel <- paste0(yLabel1, " Promoted Posts / ", yLabel1, " not Promoted Posts")
    
    gDF <- ratioDF() %>%
      mutate(Party = factor(Party)) %>%
      arrange(Type, Party) 
    
    nameVar <- unique(gDF$guideVar)
    
      if (xAxis == "Date") {
        g <- ggplot(gDF, aes_string(x = "Date", y = yVal)) +
          geom_line(aes(color = graphVar)) +
          scale_color_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(color = nameVar)
      } else if (xAxis == "Week") {
        g <- ggplot(gDF, aes_string(x = "graphVar", y = yVal)) +
          geom_col(aes(fill = graphVar), size = 1.5) +
          facet_wrap(~Week) +
          scale_fill_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = nameVar)
      } else {
        g <- ggplot(gDF, aes_string(x = "graphVar", weight = yVal)) +
          geom_bar(aes(fill = graphVar), size = 2, position = "dodge") +
          scale_fill_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = nameVar)
      }
    
    
    g <- g + 
      ylab(yLabel)
    
    if (!is_empty(yLim)) g <- g + coord_cartesian(ylim = yLim)
    
    return(g)
    
  }) 
  
  
  output$interactionsPlotAdsRatio = renderPlot({
    req(facetBoxPlotAdsRatio())
    facetBoxPlotAdsRatio()
  })
  
  
  
###### CREATE PLOTS  for ADVERTISING BUDGET ######---------------------------------------  
  
  AdsPlot2 <- reactive({
    req(postsLoaded())
    req(adsDF2())
    
    yLim <- yLimits$PlotAds2
    
    xAxis <- "Total"  # input$xAxisAds2
    gDF <- adsDF2()
    
    cVar <- unique(gDF$xVar) 
    colorVar <- sym(cVar)
    
    nameVar <- unique(gDF$guideVar) 
    
    if (xAxis == "Total") {
      g <- ggplot(gDF, aes(x = reorder(graphVar, TotAmount), y = TotAmount, fill = factor(graphVar))) +
        geom_col() +
        labs(x = nameVar, y = "Spending in Euro", color = nameVar, fill = nameVar) +
        scale_fill_manual(values = partyColors) +
        scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})
      
    } else {
    g <- ggplot(gDF, aes(x = reorder(graphVar, TotAmount), y = TotPlus, fill = factor(!!colorVar))) +
      geom_col(position = "dodge", alpha = 0.5, size = 0.2, color = "black") +
      geom_col(aes(x = reorder(graphVar, TotAmount), y = TotAmount, fill = factor(!!colorVar)), alpha = 0, color = "black", size = 0.25, position = "dodge") +
      geom_col(aes(x = reorder(graphVar, TotAmount), y = TotMinus, fill = factor(!!colorVar)), alpha = 1, size = 0.2, position = "dodge", color = "black") +
      labs(x = nameVar, y = "Spending in Euro", color = cVar, fill = nameVar) +
      scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})
    }
    
    if (!is_empty(yLim)) g <- g + coord_flip(ylim = yLim) else g <- g + coord_flip()

    return(g)
  })
  
  output$interactionsPlotAds2 = renderPlot({
    req(AdsPlot2())
    AdsPlot2()
  })
  
  
  
### Within party variation
  AdsPlot3 <- reactive({
    req(postsLoaded())
    req(adsDF3())
    
    yLim <- yLimits$PlotAds3
    
    xAxis <- "Total"  # input$xAxisAds2
    gDF <- adsDF3()
    
    cVar <- unique(gDF$xVar) 
    colorVar <- sym(cVar)
    
    if (xAxis == "Total") {
      g <- ggplot(gDF, aes(x = Type, y = TotAmount, fill = factor(Party), color = factor(Type))) +
        geom_col() +
        labs(x = "Party Function", y = "Spending in Euro", color = "Type") +
        facet_wrap(~Party) +
        scale_fill_manual(values = partyColors)  +
        guides(fill = FALSE) +
        scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})
      
    } else {
      g <- ggplot(gDF, aes(x = Type, y = TotPlus, fill = factor(Party), color = factor(Type))) +
        geom_col(position = "dodge", alpha = 0.5, size = 0.2) +
        geom_col(aes(x = Type, y = TotAmount, fill = factor(Party), color = factor(Type)), alpha = 0, size = 0.25, position = "dodge") +
        geom_col(aes(x = Type, y = TotMinus, fill = factor(Party), color = factor(Type)), alpha = 1, size = 0.2, position = "dodge") +
        facet_wrap(~Party) +
        labs(x = "Party Function", y = "Spending in Euro", color = "Type") +
        guides(fill = FALSE) +
        scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})
    }
    
    if (!is_empty(yLim)) g <- g + coord_flip(ylim = yLim) else g <- g + coord_flip()
    
    return(g)
  })
  
  output$interactionsPlotAds3 = renderPlot({
    req(AdsPlot3())
    AdsPlot3()
  })
  
#########################################################################################
###################### DOWNLOAD DATA  ###################################################
#########################################################################################

output$downloadData <- downloadHandler(
  filename = function() {
    req(postsLoaded())
    isolate({
      plotCat <- input$plotCategory
      yAxis <- input$yAxis
      xAxis <- input$xAxis
    })
    paste("NRW2019", plotCat[1], xAxis, yAxis, Sys.Date(), "Data.csv", sep = "_", collapse = "")
  },
  content = function(con) {
    req(postsLoaded())
    req(currentDF())
    isolate({
      yAxis <- quo_name(input$yAxis)
    })
    
    currDF <- currentDF() %>%
    rename(!!yAxis := yCount) 
    
    keepVars <- names(currDF) %>%
      setdiff(c("user", "userID", "textID", "Type", "Funktion", "colorVar", "graphVar", "lType", "guideVar"))
    
    currDF <- select(currDF, one_of(keepVars))
    
    write_csv(currDF, con)
  }
)

  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      isolate({
        plotCat <- input$plotCategory
        yAxis <- input$yAxis
        xAxis <- input$xAxis
      })
      paste("NRW2019", plotCat[1], xAxis, yAxis, Sys.Date(), "Plot.pdf", sep = "_", collapse = "")
    },
    content = function(file) {
      req(facetPlot())
        ggsave(file, plot = facetPlot(), device = 'pdf')
      }
  )
  
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      isolate({
        plotCat <- input$plotCategory2
        yAxis <- input$yAxis2
        xAxis <- input$xAxis2
      })
      paste("NRW2019", plotCat[1], xAxis, yAxis, Sys.Date(), "AvgData.csv", sep = "_", collapse = "")
    },
    content = function(con) {
      req(postsLoaded())
      req(averageDF())

        currDF <- averageDF() 
        
        keepVars <- names(currDF) %>%
          setdiff(c("user", "userID", "textID", "Type", "Funktion", "colorVar", "graphVar", "lType", "guideVar"))
        
        currDF <- select(currDF, one_of(keepVars))
      
      write_csv(currDF, con)
    }
  )
  
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      isolate({
        plotCat <- input$plotCategory2
        yAxis <- input$yAxis2
        xAxis <- input$xAxis2
        if (xAxis == "Date") xAxis <- "Week"
      })
      paste("NRW2019", plotCat[1], xAxis, yAxis, Sys.Date(), "AvgPlot.pdf", sep = "_", collapse = "")
    },
    content = function(file) {
      req(facetBoxPlot())
      ggsave(file, plot = facetBoxPlot(), device = 'pdf')
    }
  )

  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      isolate({
        plotCat <- input$plotCategory3
        yAxis <- input$yAxis3
        xAxis <- input$xAxis3
      })
      paste("NRW2019", plotCat[1], xAxis, yAxis, Sys.Date(), "FBReactions.csv", sep = "_", collapse = "")
    },
    content = function(con) {
      req(postsLoaded())
      req(totalFB())
      
      currDF <- totalFB() 
      
      keepVars <- names(currDF) %>%
        setdiff(c("user", "userID", "textID", "Type", "Funktion", "colorVar", "graphVar", "lType", "guideVar"))
      
      currDF <- select(currDF, one_of(keepVars))
      
      write_csv(currDF, con)
    }
  )
  
  
  output$downloadPlot3 <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      isolate({
        plotCat <- input$plotCategory3
        yAxis <- input$yAxis3
        xAxis <- input$xAxis3
        if (xAxis == "Date") xAxis <- "Week"
      })
      paste("NRW2019", plotCat[1], xAxis, yAxis, Sys.Date(), "FBReactionsPlot.pdf", sep = "_", collapse = "")
    },
    content = function(file) {
      req(FBPlot())
      ggsave(file, plot = FBPlot(), device = 'pdf')
    }
  )
  

  output$downloadDataAds <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      isolate({
        plotCat <- input$plotCategoryAds
        yAxis <- input$yAxisAds
        xAxis <- input$xAxisAds
      })
      paste("NRW2019", plotCat[1], xAxis, yAxis, Sys.Date(), "AdsData.csv", sep = "_", collapse = "")
    },
    content = function(con) {
      req(postsLoaded())

      currDF <- adsDF() 
      
      keepVars <- names(currDF) %>%
        setdiff(c("user", "userID", "textID", "Type", "Funktion", "colorVar", "graphVar", "lType", "nameVar", "guideVar"))
      
      currDF <- select(currDF, one_of(keepVars))
      
      write_csv(currDF, con)
    }
  )
  
  
  output$downloadPlotAds <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      isolate({
        plotCat <- input$plotCategoryAds
        yAxis <- input$yAxisAds
        xAxis <- input$xAxisAds
        if (xAxis == "Date") xAxis <- "Week"
      })
      paste("NRW2019", plotCat[1], xAxis, yAxis, Sys.Date(), "AdsPlot.pdf", sep = "_", collapse = "")
    },
    content = function(file) {
      req(facetBoxPlotAds())
      ggsave(file, plot = facetBoxPlotAds(), device = 'pdf')
    }
  )

 
  output$downloadDataAds2 <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      isolate({
        plotCat <- input$plotCategoryAds
        xAxis <- input$xAxisAds2
      })
      paste("NRW2019", plotCat[1], xAxis, Sys.Date(), "AdsData2.csv", sep = "_", collapse = "")
    },
    content = function(con) {
      req(postsLoaded())
      
      currDF <- adsDF2() 
      
      keepVars <- names(currDF) %>%
        setdiff(c("user", "userID", "textID", "Type", "Funktion", "colorVar", "graphVar", "lType", "nameVar", "guideVar"))
      
      currDF <- select(currDF, one_of(keepVars))
      
      write_csv(currDF, con)
    }
  )
  
  
  output$downloadPlotAds2 <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      isolate({
        plotCat <- input$plotCategoryAds
        xAxis <- input$xAxisAds2
      })
      paste("NRW2019", plotCat[1], xAxis, Sys.Date(), "AdsPlot2.pdf", sep = "_", collapse = "")
    },
    content = function(file) {
      req(AdsPlot2())
      ggsave(file, plot = AdsPlot2(), device = 'pdf')
    }
  )
  
  
  
    
#########################################################################################
###################### ZOOM IN/OUT ######################################################
#########################################################################################
  
  #### Zoom In/Out Plot1: ####-----------------------------------------------------------
  
  observeEvent(input$ZoomIn1, {
    req(facetPlot())
    yLims <- yLimits$Plot1
    if (is_empty(yLims)) {
      g <- facetPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 0.75
    
    if (yLims[2] < 100) {
      yLims[2] <- floor(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- floor(yLims[2]/100) * 100
    } else {
      yLims[2] <- floor(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot1 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomOut1, {
    req(facetPlot())
    yLims <- yLimits$Plot1
    if (is_empty(yLims)) {
      g <- facetPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 1.25
    
    if (yLims[2] < 100) {
      yLims[2] <- ceiling(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- ceiling(yLims[2]/100) * 100
    } else {
      yLims[2] <- ceiling(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot1 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomReset1, {
    req(facetPlot())
    yLimits$Plot1 <- NULL
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  #### Zoom In/Out Plot2: ####-----------------------------------------------------------
  
  observeEvent(input$ZoomIn2, {
    req(allPlot())
    yLims <- yLimits$Plot2
    if (is_empty(yLims)) {
      g <- allPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 0.75
    
    if (yLims[2] < 100) {
      yLims[2] <- floor(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- floor(yLims[2]/100) * 100
    } else {
      yLims[2] <- floor(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot2 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomOut2, {
    req(allPlot())
    yLims <- yLimits$Plot2
    if (is_empty(yLims)) {
      g <- allPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 1.25
    
    if (yLims[2] < 100) {
      yLims[2] <- ceiling(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- ceiling(yLims[2]/100) * 100
    } else {
      yLims[2] <- ceiling(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot2 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomReset2, {
    req(allPlot())
    yLimits$Plot2 <- NULL
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  
  
  #### Zoom In/Out Plot3: ####-----------------------------------------------------------
  
  observeEvent(input$ZoomIn3, {
    req(facetBoxPlot())
    yLims <- yLimits$Plot3
    if (is_empty(yLims)) {
      g <- facetBoxPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 0.75
    
    if (yLims[2] < 100) {
      yLims[2] <- floor(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- floor(yLims[2]/100) * 100
    } else {
      yLims[2] <- floor(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot3 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomOut3, {
    req(facetBoxPlot())
    yLims <- yLimits$Plot3
    if (is_empty(yLims)) {
      g <- facetBoxPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 1.25
    
    if (yLims[2] < 100) {
      yLims[2] <- ceiling(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- ceiling(yLims[2]/100) * 100
    } else {
      yLims[2] <- ceiling(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot3 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomReset3, {
    req(facetBoxPlot())
    yLimits$Plot3 <- NULL
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  #### Zoom In/Out Plot4: ####-----------------------------------------------------------
  
  observeEvent(input$ZoomIn4, {
    req(allBoxPlot())
    yLims <- yLimits$Plot4
    if (is_empty(yLims)) {
      g <- allBoxPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 0.75
    
    if (yLims[2] < 100) {
      yLims[2] <- floor(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- floor(yLims[2]/100) * 100
    } else {
      yLims[2] <- floor(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot4 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomOut4, {
    req(allBoxPlot())
    yLims <- yLimits$Plot4
    if (is_empty(yLims)) {
      g <- allBoxPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 1.25
    
    if (yLims[2] < 100) {
      yLims[2] <- ceiling(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- ceiling(yLims[2]/100) * 100
    } else {
      yLims[2] <- ceiling(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot4 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomReset4, {
    req(allBoxPlot())
    yLimits$Plot4 <- NULL
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  
  
  
  
  #### Zoom In/Out Plot5: ####-----------------------------------------------------------
  
  observeEvent(input$ZoomIn5, {
    req(FBPlot())
    yLims <- yLimits$Plot5
    if (is_empty(yLims)) {
      g <- FBPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 0.75
    
    if (yLims[2] < 100) {
      yLims[2] <- floor(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- floor(yLims[2]/100) * 100
    } else {
      yLims[2] <- floor(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot5 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomOut5, {
    req(FBPlot())
    yLims <- yLimits$Plot5
    if (is_empty(yLims)) {
      g <- FBPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 1.25
    
    if (yLims[2] < 100) {
      yLims[2] <- ceiling(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- ceiling(yLims[2]/100) * 100
    } else {
      yLims[2] <- ceiling(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot5 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomReset5, {
    req(FBPlot())
    yLimits$Plot5 <- NULL
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  #### Zoom In/Out Plot6: ####-----------------------------------------------------------
  
  observeEvent(input$ZoomIn6, {
    req(allFBPlot())
    yLims <- yLimits$Plot6
    if (is_empty(yLims)) {
      g <- allFBPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 0.75
    
    if (yLims[2] < 100) {
      yLims[2] <- floor(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- floor(yLims[2]/100) * 100
    } else {
      yLims[2] <- floor(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot6 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomOut6, {
    req(allFBPlot())
    yLims <- yLimits$Plot6
    if (is_empty(yLims)) {
      g <- allFBPlot()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 1.25
    
    if (yLims[2] < 100) {
      yLims[2] <- ceiling(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- ceiling(yLims[2]/100) * 100
    } else {
      yLims[2] <- ceiling(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$Plot6 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomReset6, {
    req(allFBPlot())
    yLimits$Plot6 <- NULL
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  
  
  #### Zoom In/Out AdsPlot: ####--------------------------------------------------------
  
  observeEvent(input$ZoomInAds, {
    req(facetBoxPlotAds())
    yLims <- yLimits$PlotAds
    if (is_empty(yLims)) {
      g <- facetBoxPlotAds()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 0.75
    
    if (yLims[2] < 100) {
      yLims[2] <- floor(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- floor(yLims[2]/100) * 100
    } else {
      yLims[2] <- floor(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$PlotAds <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomOutAds, {
    req(facetBoxPlotAds())
    yLims <- yLimits$PlotAds
    if (is_empty(yLims)) {
      g <- facetBoxPlotAds()
      yLims <- layer_scales(g)$x$range$range 
    } 
    
    yLims[2] <- yLims[2] * 1.25
    
    if (yLims[2] < 100) {
      yLims[2] <- ceiling(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- ceiling(yLims[2]/100) * 100
    } else {
      yLims[2] <- ceiling(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$PlotAds <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomResetAds, {
    req(facetBoxPlotAds())
    yLimits$PlotAds <- NULL
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  
  
  #### Zoom In/Out AdsPlot: ####--------------------------------------------------------
  
  observeEvent(input$ZoomInAdsRatio, {
    req(facetBoxPlotAdsRatio())
    yLims <- yLimits$PlotAdsRatio
    if (is_empty(yLims)) {
      g <- facetBoxPlotAdsRatio()
      yLims <- layer_scales(g)$y$range$range 
    } 
    
    yLims[2] <- yLims[2] * 0.75
    
    if (yLims[2] < 100) {
      yLims[2] <- floor(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- floor(yLims[2]/100) * 100
    } else {
      yLims[2] <- floor(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$PlotAdsRatio <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomOutAdsRatio, {
    req(facetBoxPlotAdsRatio())
    yLims <- yLimits$PlotAds
    if (is_empty(yLims)) {
      g <- facetBoxPlotAdsRatio()
      yLims <- layer_scales(g)$x$range$range 
    } 
    
    yLims[2] <- yLims[2] * 1.25
    
    if (yLims[2] < 100) {
      yLims[2] <- ceiling(yLims[2]/10) * 10
    } else if (yLims[2] < 5000) {
      yLims[2] <- ceiling(yLims[2]/100) * 100
    } else {
      yLims[2] <- ceiling(yLims[2]/1000) * 1000
    }
    
    yLims[2] <- max(yLims[1] + 5, yLims[2])
    
    yLimits$PlotAdsRatio <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomResetAdsRatio, {
    req(facetBoxPlotAdsRatio())
    yLimits$PlotAdsRatio <- NULL
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  
  
#########################################################################################
###################### Info Pop-up ######################################################
#########################################################################################
  
  url1 <- a("VDSG's", href="https://www.vdsg.at/")
  url2 <- a("data4good", href="https://www.vdsg.at/")
  url3 <- a("wahlbeobachtung.org", href = "https://www.wahlbeobachtung.org/")
  url4 <- a("CC BY_NC_SA 4.0", href = "https://creativecommons.org/licenses/by-nc-sa/4.0/")
  output$tab <- renderUI({
    tagList(h2("Social Media Monitoring for the Austrian Parliamentary Elections 2019"),
            p("For the Austrian parliamentary elections held on September 29, 2019, the ",
              url1, url2, 
              " initiative teamed up with ", url3, 
              " to monitor the public social media profiles of the political candidates, 
              leading influencers, and the major Austrian Press. Public Twitter and Facebook profiles were 
              monitored from September 8 until October 4. During this period, over 26000 politicians’ posts, 
              and 1.1 million generic user comments were collected, as well as data derived from Facebook's 
new Ad Library. Here are the most recent results of the ongoing 
              statistical and topic analysis of the data."),
            
            p(em("These materials are released under a Creative Commons ", url4, " license.  
                 Please cite wahlbeobachtung.org and VDSG/data4good when using them."))
    )
  })
  
  infoPopUp <- modalDialog(
    title = "Project Information",
    {
      uiOutput("tab")
    
    },
    footer = modalButton("Dismiss"),
    size = "l",
    easyClose = TRUE,
    fade = TRUE
  )
  
  observe({
    req(postsLoaded())
    postsLoaded()
    input$Info1
    input$Info2
    input$Info3
    input$Info4
    input$Info5
    input$Info6
    
    showModal(infoPopUp)
    
  })
  
}


shinyApp(ui, server)
