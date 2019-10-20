
# DATE: 27.09.2019
# This app is for exploring the NRW2019 dataset visually
 



library(shiny)
library(plyr)
library(tidyverse)
library(stringi)
library(stringr)
library(shinyjs)
library(lubridate)
library(shinyWidgets)

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

#### Define Global Variables, non-reactive #### -----------------------------------------

polChoices <- list("Party Leadership" = "Leader",
                   "Party - aggregated Politicians" = "PartyPol",
                   "National Party" = "PartyMain",
                   "Party - Regional" = "Party",
                   "Journalists" = "Journalist",
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


xChoices <- xChoicesAds <- list("By Day" = "Date",
                 "By Week" = "Week",
                 "Overall" = "All")

xChoicesAds2 <- list("Gender", "Age", "Regions", "Total Spending" = "Total")


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

get_yLabel <- function(yVal, countType = "") {
  yLab <- switch(yVal,
                 numPosts = "Number of Posts",
                 favoriteCount = "Number of Likes",
                 shareCount = "Number of Shares/Retweets",
                 commentCount = "Number of Comments Received",
                 repShareRatio = "Maximum Reply to Share Ratio",
                 interactionCount = "Number of Interactions",
                 mentionCount = "Number of Mentions")
  
  yLab <- paste0(countType, yLab) %>%
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

# All the Politician Posts
load("Data/PolPosts.RData")


PolPosts <- mutate(PolPosts, 
                   lType = ifelse(Type %in% c("Politician", "Party"), "Political", 
                                  ifelse(Type %in% c("Influencer", "Journalist"), "Influencer/Journalist", "Media")),
                   Name = ifelse(Name == "HAJNAL IVO", "IVO HAJNAL",
                                 ifelse(Name == "MULLA FAYAD", "FAYAD MULLA", Name)),
                   repShareRatio = ifelse(shareCount == 0L, 0, round(replyCount/shareCount, 2)))

# All the FB Posts, incl. reactions
load("Data/FBPostsAds.RData")

FBPosts <- filter(PolPosts, Site == "Facebook") %>%
  select(textID, favoriteCount, shareCount, replyCount, commentCount, interactionCount) %>%
  right_join(FBPosts)


# Summary Ads data
load("Data/summaryAdsData.RData")

if (exists("PolPosts", inherits = FALSE) && exists("FBPosts", inherits = FALSE)) postsLoaded(TRUE) else postsLoaded(FALSE)



#########################################################################################
############################ Define UI ##################################################
#########################################################################################

ui <- fluidPage(
  
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  tabsetPanel(type = "tabs",
              
              ############# Tab Panel TOTAL AGGREGATES ##################################
              tabPanel("Total Aggregates",
                       
                       sidebarLayout(
                         
                         #### SIDE BAR PANEL --------------------------------------------    
                         sidebarPanel(
                           br(),
                           br(),
                           checkboxGroupInput("plotCategory", 
                                              "Wähle eine Kategorie aus (mehrfach Selektion möglich)", 
                                              choices = polChoices, 
                                              selected = "Leader"),
                           br(),
                           br(),
                           radioButtons("xAxis",
                                        "Wie sollen die Zeitmessungen aggregiert werden?  Bitte nur eine Auswahl",
                                        choices = xChoices,
                                        selected = "Date"),
                           br(),
                           br(),
                           conditionalPanel(
                             condition = "input.xAxis == 'Date'",
                             sliderInput("timeInterval",
                                         "Welcher Zeitrahmen soll angezeigt werden?",
                                         min = minDate,
                                         max = maxDate,
                                         value = c(minDate, maxDate))
                           ),
                           br(),
                           br(),
                           radioButtons("yAxis",
                                        "Was soll gemessen werden?  Bitte nur eine Auswahl",
                                        choices = yChoices,
                                        selected = "numPosts"),
                           br(),
                           br(),
                           fluidRow(column(width = 3,
                                           downloadLink('downloadData', 'Download Data')),
                                    column(width = 3, offset = 3,
                                           downloadLink('downloadPlot', 'Download Plot'))
                           ),
                           br()
                         ),
                         
                         #### MAIN PANEL ------------------------------------------------      
                         mainPanel(
                           br(),
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
                                              "Wähle eine Kategorie aus (mehrfach Selektion möglich)", 
                                              choices = polChoices, 
                                              selected = "Leader"),
                           br(),
                           br(),
                           radioButtons("xAxis2",
                                        "Wie sollen die Zeitmessungen aggregiert werden?  Bitte nur eine Auswahl",
                                        choices = xChoices,
                                        selected = "Date"),
                           br(),
                           br(),
                           conditionalPanel(
                             condition = "input.xAxis2 == 'Date'",
                             sliderInput("timeInterval2",
                                         "Welcher Zeitrahmen soll angezeigt werden?",
                                         min = minDate,
                                         max = maxDate,
                                         value = c(minDate, maxDate))
                           ),
                           br(),
                           br(),
                           fluidRow(column(width = 5,
                                           radioButtons("yAxis2",
                                        "Was soll gemessen werden?  Bitte nur eine Auswahl",
                                        choices = yChoices2,
                                        selected = "shareCount")),
                                    column(width = 5, offset = 2,
                                           radioButtons("gType",
                                                        "Which kind of average?",
                                                        choices = c("yAvg", "yMed", "bPlot"),
                                                        selected = "yAvg"))
                           ),
                          br(),
                           br(),
                           fluidRow(column(width = 3,
                                           downloadLink('downloadData2', 'Download Data')),
                                    column(width = 3, offset = 3,
                                           downloadLink('downloadPlot2', 'Download Plot'))
                           ),
                           br()
                         ),
                         
                         #### MAIN PANEL ------------------------------------------------      
                         mainPanel(
                           br(),
                           fluidRow(plotOutput('interactionsPlot3')),
                           br(),
                           fluidRow(plotOutput('interactionsPlot4'))
                           
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
                             condition = "input.xAxisAds == 'Date'",
                             sliderInput("timeIntervalAds",
                                         "Which time interval should be displayed?",
                                         min = minDate,
                                         max = maxDate,
                                         value = c(minDate, maxDate))
                           ),
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
                           br()
                         ),
                         
                         #### MAIN PANEL ------------------------------------------------      
                         mainPanel(
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
                                    )
                           )# end Zoom Column
                         ) # end mainPanel
                         
                       ) # end sidebarLayout
              ), # end tabPanel Promoted Posts
              
              
              
              
              
              ############# Tab Panel ADVERTISING BUDGETS ###############################
              tabPanel("Advertising Budgets",
                       
                       sidebarLayout(
                         
                         #### SIDE BAR PANEL --------------------------------------------    
                         sidebarPanel(
                           br(),
                           br(),
                           checkboxGroupInput("plotCategoryAds2", 
                                              "Select a category (more than one selection possible).", 
                                              choices = polChoicesAds, 
                                              selected = "Leader"),
                           br(),
                           br(),
                           radioButtons("xAxisAds2",
                                        "How is the budget divided among the following demographics/regions? Please select only one.",
                                        choices = xChoicesAds2,
                                        selected = "Gender"),
                           br(),
                           br(),
                           br(),
                           fluidRow(column(width = 3,
                                           downloadLink('downloadDataAds2', 'Download Data')),
                                    column(width = 3, offset = 3,
                                           downloadLink('downloadPlotAds2', 'Download Plot'))
                           ),
                           br()
                         ),
                         
                         #### MAIN PANEL ------------------------------------------------      
                         mainPanel(
                           br(),
                           fluidRow(plotOutput('interactionsPlotAds2'))
                         ) # end mainPanel
                         
                       ) # end sidebarLayout
              ) # end tabPanel Promoted Posts
              
              
  ) # end tabsetPanel
) # end fluidPage





#########################################################################################
############### Define the server function ##############################################
#########################################################################################

server <- function(input, output, session) {

###### GLOBAL REACTIVE VALUES ######-----------------------------------------------------
  
  yLimits <- reactiveValues(Plot1 = NULL, Plot2 = NULL, Plot3 = NULL, Plot4 = NULL, PlotAds = NULL)

    
###### PREPROCESS DF for GRAPHING, Total Aggregates Tab ######---------------------------
  currentDF <- reactive({
    req(postsLoaded())
    req(input$plotCategory)
    
    currDF <- NULL
    
    plotCat <- input$plotCategory
    timeInt <- input$timeInterval
    yAxis <- input$yAxis
    xAxis <- input$xAxis
    
    if (yAxis == "mentionCount") {
      usePosts <- PolPosts
    } else {
      usePosts <- filter(PolPosts, !is.na(origPost))
    }
    
    if (xAxis == "All") {
      groupVars <- c("Type", "Party", "Site", "lType")
    } else {
      groupVars <- c("Type", "Party", xAxis, "Site", "lType")
    }
    
    colorVar <- "Party"
    
    if (any(plotCat %in% c("Leader", "Influencer", "Journalist"))) {
      groupVars <- union(groupVars, "Name")
      colorVar <- "Name"
      
      
      if ("Leader" %in% plotCat) {
        lDF <- filter(usePosts, Funktion == "Leader") %>%
          mutate(Type = "Leader")
        
        if (!is_empty(lDF)) {
          currDF <- filter(lDF, !(textID %in% currDF$textID)) %>%
            bind_rows(currDF)
        }
      }
      
      if (any(plotCat %in% c("Leader", "Influencer", "Journalist"))) {
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
    
    
    currDF <- currDF %>%
      filter(Date >= timeInt[1], Date <= timeInt[2])
    
    if (yAxis == "numPosts") {
      currDF <- group_by_at(currDF, groupVars) %>%
        summarize(yCount = n())
    } else if (yAxis == "repShareRatio") {
      currDF <- group_by_at(currDF, groupVars) %>%
        summarize(yCount = max(.data[[yAxis]], 0, na.rm = TRUE))
    } else {
      currDF <- group_by_at(currDF, groupVars) %>%
        summarize(yCount = sum(.data[[yAxis]]))
    }
    
    currDF$graphVar <- colorVar
 
    return(ungroup(currDF))
  })
  

###### PREPROCESS DF for GRAPHING, Average Aggregates Tab ######------------------------- 
  averageDF <- reactive({
    currDF <- NULL
    
    plotCat <- input$plotCategory2
    timeInt <- input$timeInterval2
    yAxis <- input$yAxis2
    xAxis <- input$xAxis2
    BoxPlot <- input$gType == "bPlot"
    
    print(yAxis)
    usePosts <- filter(PolPosts, !is.na(origPost))
    
    print(nrow(usePosts))
    if (xAxis == "All") {
      groupVars <- c("Type", "Party", "Site", "lType")
    } else {
      groupVars <- c("Type", "Party", xAxis, "Site", "lType")
    }
    
    colorVar <- "Party"
    
    if (any(plotCat %in% c("Leader", "Influencer", "Journalist"))) {
      groupVars <- union(groupVars, "Name")
      colorVar <- "Name"
      
      
      if ("Leader" %in% plotCat) {
        lDF <- filter(usePosts, Funktion == "Leader") %>%
          mutate(Type = "Leader")
        
        if (!is_empty(lDF)) {
          currDF <- filter(lDF, !(textID %in% currDF$textID)) %>%
            bind_rows(currDF)
        }
      }
      
      if (any(plotCat %in% c("Leader", "Influencer", "Journalist"))) {
        lDF <- filter(usePosts, Type %in% plotCat, Funktion != "Leader")
        
        if (!is_empty(currDF)) {
          currDF <- filter(currDF, !(textID %in% lDF$textID)) %>%
            bind_rows(lDF)
        } else currDF <- lDF
      }
    }
    
    
    if ("PartyPol" %in% plotCat) {
      print("in party pol")
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
      print("in party main")
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
    

    currDF <- currDF %>%
      filter(Date >= timeInt[1], Date <= timeInt[2])
    

    if (BoxPlot == FALSE) {
    currDF <- group_by_at(currDF, groupVars) %>%
      summarize(yAvg = mean(.data[[yAxis]]), yMed = median(.data[[yAxis]]))
    } else {
      currDF$yCount <- currDF[[yAxis]]
    }
    
    currDF$graphVar <- colorVar
    
    return(ungroup(currDF))
  })

  
###### PREPROCESS DF for GRAPHING, Promoted Posts Tab ######----------------------------- 
  adsDF <- reactive({
    print("in adsDF")
    currDF <- NULL
    
    plotCat <- input$plotCategoryAds
    timeInt <- input$timeIntervalAds
    yAxis <- input$yAxisAds
    xAxis <- input$xAxisAds
    BoxPlot <- input$gTypeAds == "bPlot"
    
    usePosts <- FBPosts
    
    print(nrow(usePosts))
    if (xAxis == "All") {
      groupVars <- c("Type", "Party", "isAd")
    } else {
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
      print(nrow(currDF))
    }
    
    
    if ("PartyPol" %in% plotCat) {
      print("in party pol")
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
      print("in party main")
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
    
    
    currDF <- currDF %>%
      filter(Date >= timeInt[1], Date <= timeInt[2])
    
    
    if (BoxPlot == FALSE) {
      currDF <- group_by_at(currDF, groupVars) %>%
        summarize(yAvg = mean(.data[[yAxis]]), yMed = median(.data[[yAxis]]))
    } else {
      currDF$yCount <- currDF[[yAxis]]
    }
    
    currDF$graphVar <- colorVar
    
    return(ungroup(currDF))
  })
  
  
###### PREPROCESS DF for GRAPHING, Advertising Budget Tab ######-------------------------
  
  
  
###### CREATE PLOTS  for TOTAL AGGREGATES ######-----------------------------------------

### interactions divided by Site ###
facetPlot <- reactive({
  req(postsLoaded())
  
  yLim <- yLimits$Plot1
  yAxis <- input$yAxis
  xAxis <- input$xAxis
  yLabel <- get_yLabel(yAxis)
  gDF <- currentDF() %>%
    mutate(Party = factor(Party)) %>%
    arrange(Type, Party)
  
  graphVar <- unique(gDF$graphVar)
  
  if (xAxis == "Date") {
    g <- ggplot(gDF, aes(x = Date, y = yCount)) +
      geom_line(aes_string(color = graphVar, group = graphVar, linetype = "lType")) +
      facet_wrap(~Site) +
      scale_color_manual(values = partyColors) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      labs(color = "Name", linetype = "Function")
  } else if (xAxis == "Week") {
    g <- ggplot(gDF, aes(x = Week, y = yCount)) +
      geom_col(aes_string(fill = "Party", color = "lType"), position = "dodge", size = 1.5) +
      facet_wrap(~Site) +
      scale_fill_manual(values = partyColors) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      labs(fill = "Name", color = "Function") +
      guides(linetype = FALSE)
  } else {
    g <- ggplot(gDF, aes_string(x = graphVar, weight = "yCount")) +
      geom_bar(aes_string(fill = "Party", color = "lType"), size = 2) +
      facet_wrap(~Site) +
      scale_fill_manual(values = partyColors) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      labs(fill = "Name", color = "Function") +
      guides(linetype = FALSE)
  }
  
  g <- g + 
    ylab(yLabel)
  
  if (!is_empty(yLim)) g <- g + coord_cartesian(ylim = yLim)
  
  return(g)
  
}) 

 
  output$interactionsPlot = renderPlot({
    req(facetPlot)
    facetPlot()
  })
  
  
  
### interactions combining both sites  ###
  
  allPlot <- reactive({
    req(postsLoaded())
    
    yLim <- yLimits$Plot2
    yAxis <- input$yAxis
    xAxis <- input$xAxis
    yLabel <- get_yLabel(yAxis)
    gDF <- currentDF() %>%
      mutate(Party = factor(Party))
    
    graphVar <- unique(gDF$graphVar)
    groupVars <- setdiff(names(gDF), c("Site", "yCount"))
    gDF <- group_by_at(gDF, groupVars) %>%
      summarize(allCount = sum(yCount))  %>%
      arrange(Type, Party)
    
    if (xAxis == "Date") {
      g <- ggplot(gDF, aes(x = Date, y = allCount)) +
        geom_line(aes_string(color = graphVar, group = graphVar, linetype = "lType")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = "Name", linetype = "Function")
    } else if (xAxis == "Week") {
      g <- ggplot(gDF, aes(x = Week, y = allCount)) +
        geom_col(aes_string(fill = "Party", color = "lType", linetype = "lType"), position = "dodge", size = 1.5) +
        scale_fill_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(fill = "Name", color = "Function") +
        guides(linetype = FALSE)
    } else {
      g <- ggplot(gDF, aes_string(x = graphVar, weight = "allCount")) +
        geom_bar(aes_string(fill = "Party", color = "lType", linetype = "lType"), size = 2) +
        scale_fill_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(fill = "Name", color = "Function") +
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
    graphVar = unique(DF$graphVar)

    if (X %in% c("Date", "Week")) {
      g <- ggplot(DF, aes_string(x = graphVar, y = Y)) +
        geom_boxplot(aes_string(fill = "Party", color = "Party")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = "Name") +
        guides(fill = FALSE)
      if (Facet == TRUE) {
        g <- g + facet_grid(rows = vars(Site), cols = vars(Week))
      } else {
        g <- g + facet_wrap(~Week)
      }
      
    } else {
      g <- ggplot(DF, aes_string(x = graphVar, y = Y)) +
        geom_boxplot(aes_string(fill = "Party", color = "Party")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = "Name") +
        guides(fill = FALSE)
      if (Facet == TRUE) g <- g + facet_wrap(~Site)
      
    }
    
    return(g)
  }
  
  
  ### interactions divided by Site ###
  facetBoxPlot <- reactive({
    req(postsLoaded())
    req(input$plotCategory2)
    
    
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
    
    graphVar <- unique(gDF$graphVar)
    
    if (bPlot == TRUE) {
      g <- boxGraph(gDF, xAxis, yAxis, Facet = TRUE)
    } else {
      if (xAxis == "Date") {
        g <- ggplot(gDF, aes_string(x = "Date", y = yVal)) +
          geom_line(aes_string(color = graphVar, group = graphVar, linetype = "lType")) +
          facet_wrap(~Site) +
          scale_color_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(color = "Name", linetype = "Function")
      } else if (xAxis == "Week") {
        g <- ggplot(gDF, aes_string(x = "Week", y = yVal)) +
          geom_col(aes_string(fill = "Party", color = "lType"), position = "dodge", size = 1.5) +
          facet_wrap(~Site) +
          scale_fill_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = "Name", color = "Function") +
          guides(linetype = FALSE)
      } else {
        g <- ggplot(gDF, aes_string(x = graphVar, weight = yVal)) +
          geom_bar(aes_string(fill = "Party", color = "lType"), size = 2) +
          facet_wrap(~Site) +
          scale_fill_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = "Name", color = "Function") +
          guides(linetype = FALSE)
      }
    }
    
    g <- g + 
      ylab(yLabel)
    
    return(g)
    
  }) 
  
  
  output$interactionsPlot3 = renderPlot({
    req(facetBoxPlot)
    facetBoxPlot()
  })
  
  
  ### interactions combining both sites  ###
  
  allBoxPlot <- reactive({
    req(postsLoaded())
    req(input$plotCategory2)
    
    
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
    
    graphVar <- unique(gDF$graphVar)
    

    groupVars <- setdiff(names(gDF), c("Site", "yCount", "yMed", "yAvg"))

    if (bPlot == TRUE) {
      gDF <- group_by_at(gDF, groupVars) %>%
        summarize(allCount = sum(.data[[yAxis]]))  %>%
        arrange(Type, Party)
      g <- boxGraph(gDF, xAxis, "allCount", Facet = FALSE)
    } else {
      gDF <- group_by_at(gDF, groupVars) %>%
        summarize(allCount = sum(.data[[yVal]]))  %>%
        arrange(Type, Party)
      
    if (xAxis == "Date") {
      g <- ggplot(gDF, aes(x = Date, y = allCount)) +
        geom_line(aes_string(color = graphVar, group = graphVar, linetype = "lType")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = "Name", linetype = "Function")
    } else if (xAxis == "Week") {
      g <- ggplot(gDF, aes(x = Week, y = allCount)) +
        geom_col(aes_string(fill = "Party", color = "lType", linetype = "lType"), position = "dodge", size = 1.5) +
        scale_fill_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(fill = "Name", color = "Function") +
        guides(linetype = FALSE)
    } else {
      g <- ggplot(gDF, aes_string(x = graphVar, weight = "allCount")) +
        geom_bar(aes_string(fill = "Party", color = "lType", linetype = "lType"), size = 2) +
        scale_fill_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(fill = "Name", color = "Function") +
        guides(linetype = FALSE)
    }
    }
    
    g <- g + 
      ggtitle("Facebook and Twitter Combined") +
      ylab(yLabel)
    
    return(g)
    
  })
  
  output$interactionsPlot4 = renderPlot({
    req(allBoxPlot())
    allBoxPlot()
  })
  
  
  
###### CREATE PLOTS  for PROMOTED POSTS ######-------------------------------------------
  
  boxGraphAds <- function(DF, X, Y) {
    graphVar = unique(DF$graphVar)
    
    if (X %in% c("Date", "Week")) {
      g <- ggplot(DF, aes_string(x = "isAd", y = Y)) +
        geom_boxplot(aes_string(fill = "Party", color = "Party")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = "Name") +
        guides(fill = FALSE)
      if (graphVar == "Name") {
        g <- g + facet_grid(cols = vars(Name), rows = vars(Week))
      } else {
        g <- g + facet_grid(cols = vars(Party), rows = vars(Week))
      }
      
    } else {
      g <- ggplot(DF, aes_string(x = "isAd", y = Y)) +
        geom_boxplot(aes_string(fill = "Party", color = "Party")) +
        scale_color_manual(values = partyColors) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs(color = "Name") +
        guides(fill = FALSE) 
        if (graphVar == "Name") {
          g <- g + facet_wrap(~Name, ncol = 1)
        } else {
          g <- g + facet_wrap(~Party, ncol = 1)
        }
      
    }
    
    return(g)
  }
  
  
  ### interactions divided by isAd ###
  facetBoxPlotAds <- reactive({
    req(postsLoaded())
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
    
    yLabel <- get_yLabel(yAxis, countType = cType)
    
    gDF <- adsDF() %>%
      mutate(Party = factor(Party)) %>%
      arrange(Type, Party)
    
    graphVar <- unique(gDF$graphVar)
    
    gDF <- mutate(gDF, groupVar = paste0(.data[[graphVar]], isAd))
    
    if (bPlot == TRUE) {
      g <- boxGraphAds(gDF, xAxis, yAxis)
    } else {
      if (xAxis == "Date") {
        g <- ggplot(gDF, aes_string(x = "Date", y = yVal)) +
          geom_line(aes_string(color = graphVar, group = "groupVar", linetype = "isAd")) +
          scale_color_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(color = "Name", linetype = "Advertised?")
      } else if (xAxis == "Week") {
        g <- ggplot(gDF, aes_string(x = "isAd", y = yVal)) +
          geom_col(aes_string(fill = graphVar, color = "isAd"), position = "dodge", size = 1.5) +
          facet_grid(row = vars(Party), col = vars(Week)) +
          scale_fill_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = "Name") +
          guides(color = FALSE)
      } else {
        g <- ggplot(gDF, aes_string(x = "isAd", weight = yVal)) +
          geom_bar(aes_string(fill = graphVar, color = "isAd"), size = 2) +
          facet_wrap(~Party, ncol = 1) +
          scale_fill_manual(values = partyColors) +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
          labs(fill = "Name") +
          guides(color = FALSE)
      }
    }
    
    g <- g + 
      ylab(yLabel)
    
    if (!is_empty(yLim)) g <- g + coord_cartesian(ylim = yLim)
    
    return(g)
    
  }) 
  
  
  output$interactionsPlotAds = renderPlot({
    req(facetBoxPlotAds)
    facetBoxPlotAds()
  })

  
  
  
  
  
  ###### CREATE PLOTS  for ADVERTISING BUDGET ######-------------------------------------  
  
  adsDF2 <- reactive({
    req(postsLoaded())
    req(input$plotCategoryAds2)
    
    plotCat <- input$plotCategoryAds2
    xAxis <- str_to_lower(input$xAxisAds2)
    
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
    
    print(names(useDF))
    
    currDF <- NULL
    groupVars <- "Party"
    colorVar <- xAxis
    
    print("hello")
    print(xAxis)
    print(plotCat)
    print("***")
    
    if (xAxis != "total") groupVars <- c(groupVars, xAxis)
    
    if ("Leader" %in% plotCat) {
      groupVars <- union("Name", groupVars)
      nameVar <- "Name"
      
      lDF <- filter(useDF, Funktion == "Leader") %>%
        mutate(Type = "Leader")
      
      
      if (!is_empty(lDF)) {
        currDF <- filter(lDF, !(Name %in% currDF$Name)) %>%
          bind_rows(currDF)
      }
      
      print(paste0("in leader, nrows lDF is: ", nrow(lDF), "and currDF is: ", nrow(currDF)))
    }
    
    
    if ("PartyPol" %in% plotCat) {
      print("in party pol")
      lDF <- filter(useDF, Type == "Politician", Funktion != "Leader")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(Name %in% lDF$Name)) %>%
          bind_rows(lDF)  %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      nameVar <- "Party"
    }
    
    
    if ("PartyMain" %in% plotCat) {
      print("in party main")
      lDF <- filter(useDF, Funktion == "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(Name %in% lDF$Name)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      nameVar <- "Party"
    }
    
    if ("Party" %in% plotCat) {
      lDF <- filter(useDF, Type == "Party", Funktion != "Bundespartei")
      
      if (!is_empty(currDF)) {
        currDF <- filter(currDF, !(Name %in% lDF$Name)) %>%
          bind_rows(lDF) %>%
          mutate(Type = "Party")
      } else currDF <- lDF
      
      groupVars <- setdiff(groupVars, "Name")
      nameVar <- "Party"
    }
    
    currDF <- group_by_at(currDF, groupVars) %>%
      summarize(TotAmount = sum(Spending),
                TotSD = calc_sdev(StDev))
    
    currDF <- filter(currDF, !is.na(Party)) %>%
      mutate(TotSD = ifelse(is.na(TotSD), 0.75*TotAmount, TotSD)) %>%
      mutate(TotPlus = round(TotAmount + TotSD),
             TotMinus = round(TotAmount - TotSD))
    
    print(nrow(currDF))
    print(colorVar)
    
    if (is_empty(currDF) || nrow(currDF) == 0L) return(NULL)
    
    currDF$colorVar <- colorVar
    currDF$nameVar <- nameVar
    
    return(currDF)
    
  })
  
  
  
  
  AdsPlot2 <- reactive({
    req(adsDF2())
    
    yLim <- yLimits$PlotAds2
    
    xAxis <- input$xAxisAds2
    gDF <- adsDF2()
    
    cVar <- unique(gDF$colorVar) 
    colorVar <- sym(cVar)
    
    nVar <- unique(gDF$nameVar) 
    nameVar <- sym(nVar)
    
    print(names(gDF))
    
    if (xAxis == "Total") {
      g <- ggplot(gDF, aes(x = reorder(!!nameVar, TotAmount), y = TotAmount, fill = factor(!!nameVar))) +
        geom_col() +
        labs(x = nVar, y = "Spending in Euro", color = nVar) +
        scale_fill_manual(values = partyColors) 
      
    } else {
    g <- ggplot(gDF, aes(x = reorder(!!nameVar, TotAmount), y = TotPlus, fill = factor(!!colorVar))) +
      geom_col(position = "dodge", alpha = 0.5, size = 0.2, color = "black") +
      geom_col(aes(x = reorder(!!nameVar, TotAmount), y = TotAmount, fill = factor(!!colorVar)), alpha = 0, color = "black", size = 0.25, position = "dodge") +
      geom_col(aes(x = reorder(!!nameVar, TotAmount), y = TotMinus, fill = factor(!!colorVar)), alpha = 1, size = 0.2, position = "dodge", color = "black") +
      labs(x = nVar, y = "Spending in Euro", color = cVar)
    }
    
    if (!is_empty(yLim)) g <- g + coord_flip(ylim = yLim) else g <- g + coord_flip()

    return(g)
  })
  
  output$interactionsPlotAds2 = renderPlot({
    req(AdsPlot2())
    AdsPlot2()
  })
  
  
#########################################################################################
###################### DOWNLOAD DATA  ###################################################
#########################################################################################

output$downloadData <- downloadHandler(
  filename = function() {
    req(postsLoaded())
    print("in function download")
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
    rename(!!yAxis := yCount) %>%
      select(-graphVar, -lType)
    
    write_csv(currDF, con)
  }
)

  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      print("in plot download")
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
      print("in function download2")
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

        currDF <- averageDF() %>%
        select(-graphVar, -lType)
      
      write_csv(currDF, con)
    }
  )
  
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      print("in plot download")
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

      currDF <- adsDF() %>%
        select(-graphVar)
      
      write_csv(currDF, con)
    }
  )
  
  
  output$downloadPlotAds <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      print("in plot download")
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
      
      currDF <- adsDF2() %>%
        select(-graphVar)
      
      write_csv(currDF, con)
    }
  )
  
  
  output$downloadPlotAds2 <- downloadHandler(
    filename = function() {
      req(postsLoaded())
      print("in plot download")
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
      print("empty yLim")
      g <- facetPlot()
      print(names(layer_scales(g)$y$range$range))
      yLims <- layer_scales(g)$y$range$range 
      print(yLims)
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
    
    print(paste0("yLims done ", yLims))
    yLimits$Plot1 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomOut1, {
    req(facetPlot())
    yLims <- yLimits$Plot1
    if (is_empty(yLims)) {
      print("empty yLim")
      g <- facetPlot()
      print(names(layer_scales(g)$y$range$range))
      yLims <- layer_scales(g)$y$range$range 
      print(yLims)
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
    
    print(paste0("yLims done ", yLims))
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
      print("empty yLim")
      g <- allPlot()
      print(names(layer_scales(g)$y$range$range))
      yLims <- layer_scales(g)$y$range$range 
      print(yLims)
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
    
    print(paste0("yLims done ", yLims))
    yLimits$Plot2 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomOut2, {
    req(allPlot())
    yLims <- yLimits$Plot2
    if (is_empty(yLims)) {
      print("empty yLim")
      g <- allPlot()
      print(names(layer_scales(g)$y$range$range))
      yLims <- layer_scales(g)$y$range$range 
      print(yLims)
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
    
    print(paste0("yLims done ", yLims))
    yLimits$Plot2 <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomReset2, {
    req(allPlot())
    yLimits$Plot2 <- NULL
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  
  #### Zoom In/Out AdsPlot: ####--------------------------------------------------------
  
  observeEvent(input$ZoomInAds, {
    req(facetBoxPlotAds())
    yLims <- yLimits$PlotAds
    if (is_empty(yLims)) {
      print("empty yLim")
      g <- facetBoxPlotAds()
      yLims <- layer_scales(g)$y$range$range 
      print(yLims)
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
    
    print(paste0("yLims done ", yLims))
    yLimits$PlotAds <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomOutAds, {
    req(facetBoxPlotAds())
    yLims <- yLimits$PlotAds
    if (is_empty(yLims)) {
      print("empty yLim")
      g <- facetBoxPlotAds()
      print(names(layer_scales(g)$x$range$range))
      yLims <- layer_scales(g)$x$range$range 
      print(yLims)
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
    
    print(paste0("yLims done ", yLims))
    yLimits$PlotAds <- yLims
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$ZoomResetAds, {
    req(facetBoxPlotAds())
    yLimits$PlotAds <- NULL
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
}


shinyApp(ui, server)
