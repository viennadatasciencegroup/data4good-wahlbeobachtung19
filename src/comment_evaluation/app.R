
# App for viewing and evaluating comments for 
# wahlbeobachtung.org's NRW2019  social media monitoring campaign.

# DEPLOYED ON: 

# Available on shinyapps.io as:
#  https://ai-italy.shinyapps.io/CommentEval App/,


# DATE: 16.09.2019

# App using Shiny Login

# Login is required to access the app.
# The user name will be used to determine where to look in Dropbox for the file to load.
# The file to load will be of the form prefix_PSPComments.RData

# username: letters and digits only!!!
# STEP 1: It will look in the user folder (same name as username - hence, please no special characters in usernames)
# In this case, prefix = TimeStamp_username_d
# Timestamp is the time the file was last saved, d is the file number (every comments file will be numbered when new)

# STEP 2: If the user folder is empty, it will look in the New4PSP Folder.
# Here prefix is just the file number
# The algo sets the seed based on Timestamp, and chooses a file to open at random 
# (to avoid two users trying to access the same folder at the same time)
# A file retrieved from the New4PSP folder will be saved, with Timestamp, to the user folder, 
# and deleted from the New4PSP folder.

# STEP 3: Once a file has been fully evaluated, it is saved to the folders Completed and Backup as
# timestamp_user_d_PSPComments.RData
# Once this has been successful, the user folder is cleared (all files deleted)

# INPUT: The file contains a list of dataframes.
# The list names are the politician names, with special characters (accents and apostrophes)
# removed, and spaces replaced by underscore.
# For example, the politician Angela Merkel would be represented as Angela_Merkel
# and the politician Arsène L'Upin would be represented as
# Arsene_LUpin.


######### ATTENTION:  Turn Rating into CHARACTER!!!
# Each dataframe corresponds to the comments made to and by a Politician.
# The dataframe columns are:
# - user (who posted the comment: as of GDPR, this should be anonymous unless user = Politician)
# - text (the text of the comment)
# - id (the message id: hashed, unless Politician comment)
# - replyToUser (the message was posted in reply to a message by this user: again, anonymous unless reply to Politician)
# - replyToID (ID of the message being replied to, hashed unless politician)
# - Rating (Default value: -10; integer: -10 = unrated; -2 = not able to rate, 0 = positive, 10 = neutral, 20 = negative (but not problematic), 30 = offensive  ... characters)
# - Topic (Default value should be "None")
# - Level (0 = politician's post, 1 = reply to pol, 2 = reply to level 1, 3 = reply to level 2, ...
#          10 = comment posted on Politician's page, 11 = reply to level 10, 12 = reply to level 11, ...
#          20 = all other comments that mention the Politician)
# - origPost (id of the original politician post being replied to)
# - Site (FaceBook or Twitter)
# - Include ("Y", or "N" - for messages that should not be evaluated, but are in the list as replyTo messages)
# - isFake (default FALSE; indicator that the comment might contain Fake News)



# BEWARE the SCOPING RULES!!!!
# Objects visible across all sessions
# 
# You might want some objects to be visible across all sessions. For example, if you have 
# large data structures, or if you have utility functions that are not reactive 
# (ones that don’t involve the input or output objects), then you can create these objects 
# once and share them across all user sessions (within the same R process), by placing 
# them in app.R, but outside of the server function definition.

# The objects fixed, but:  it is NOT possible to use stopApp() without stopping the whole instance!!
# So: either allow only browser window closing, or else, one user, one instance


library(shiny)
library(plyr)
library(tidyverse)
library(stringi)
library(stringr)
library(rdrop2)
library(shinyjs)
library(shinycssloaders)

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

#########################################################################################
#### Define Global Variables, non-reactive #### -----------------------------------------
#########################################################################################


##################### Global Variables for the Various CheckBoxes #######################

## Topic Button Labels and Values -----------------------------------
topicBoxHelp <- "Es können mehrere Themen ausgewählt werden"
topicBoxID <- "postTopic"
topicBoxLabel <- "Der Post/Kommentar betrifft einen oder mehrere der folgenden Themen:"
topicBoxNames0 <- list("Ein anderer Politiker",
                       "Umwelt",
                       "Wahlkampffinanzierung",
                       "Medien/Pressefreiheit",
                       "Frauen oder Genderdiskriminierung",   # when the comment is a politicians' post (Level 0 comment)
                      "Lgbti",
                      "Personen mit Behinderung",
                      "Ausländer/Migranten/Personen mit Migrationshintergrund",
                      "Religiöse Minderheiten",
                      "Armut",
                      "Europa",
                      "Anderes")
topicBoxNames1 <- list("Der Seitenbesitzer - Politiker/Influencer/Partei",
                       "Ein anderer Politiker",
                       "Umwelt",
                       "Wahlkampffinanzierung",
                       "Medien/Pressefreiheit",
                       "Frauen oder Genderdiskriminierung",   # when the comment is a politicians' post (Level 0 comment)
                       "Lgbti",
                       "Personen mit Behinderung",
                       "Ausländer/Migranten/Personen mit Migrationshintergrund",
                       "Religiöse Minderheiten",
                       "Armut",
                       "Europa",
                       "Anderes")
topicBoxValues0 <- list("otherPol", 
                        "environment",
                        "campaign_finance",
                        "press",
                        "women",
                        "lgbti", 
                        "disabili", 
                        "rifugiati", 
                        "religioni", 
                        "poverta", 
                        "europa", 
                        "Other")
topicBoxValues1 <- list("pol", 
                        "otherPol", 
                        "environment",
                        "campaign_finance",
                        "press",
                        "women",
                        "lgbti", 
                        "disabili", 
                        "rifugiati", 
                        "religioni", 
                        "poverta", 
                        "europa", 
                        "Other")



## Is the content possibly fake news? -------------------------------
isFakeHelp <- ""
isFakeID <- "FakeNews"
isFakeLabel <- "Ich glaube dieser Post/Kommentar beinhaltet eine Fake News."


## Is it possible to evaluate content -------------------------------
nonValHelp <- "Bitte nur anklicken wenn: der Post/Tweet in einer Fremdsprache ist; der Post/Tweet auf eine Seite verlinkt, die nicht mehr existiert; der Post/Tweet auf ein Video verweist, ohne dessen Besichtigung eine Bewertung des Kommentars nicht möglich ist."
nonValID <- "nonValutabile"
nonValLabel <- "NICHT bewertbar"


## Offensiveness Button Labels and Values Part 1 --------------------
checkBoxHelpA <- "Nur eine Auswahl möglich"
checkBoxIDA <- "prelimOffenseA"
checkBoxLabelA <- "Suche die passendste Sentimentbewertung aus:"
checkBoxNamesA <- list("Offensiv", "Negativ", "Neutral", "Positiv", "Später bewerten")
checkBoxValuesA <- list(30L, 20L, 10L, 0L, -10L)


############################# Other Variables ###########################################

## Directory and Global File Names ----------------------------------


mainDir <- "New4PSP"   # This directory contains the newest unrated comment files
completeDir <- "Complete4PSP"  # This directory is for saving completed files
backupDir <- "Backup4PSP"  # This directory is for saving backups

localDir <- "Data"


#########################################################################################
#### Global Functions:  #### ------------------------------------------------------------
#########################################################################################

# To avoid this error, disable http/2 :
# Warning: Error in curl::curl_fetch_memory: Error in the HTTP2 framing layer
httr::set_config(httr::config(http_version = 0))

#### Define Functions Load, Save, and Clear Directory ---------------

clean_dropDirectory <- function(dName) {
  ## This function removes all files from a given DropBox directory ##
  closeAllConnections()

  
  if (drop_exists(dName)) {
    dFiles <- drop_dir(dName)
    
    if (!is_empty(dFiles)) {   
      deleteFiles <- dFiles$path_display
      keepFile <- str_detect(deleteFiles, "AmnestyEvalSummary.RData")
      deleteFiles <- deleteFiles[!keepFile]
      
      if (!is_empty(deleteFiles)) {
        lapply(deleteFiles, drop_delete)
      }
    }
    
  }
  closeAllConnections()
  return(NULL)
}


clean_Directory <- function(dName) {
  ## Remove all files from a given local directory. ##

  if (file.exists(dName)) {
    dFiles <- list.files(dName)
    if (!is_empty(dFiles)) {
      deleteFiles <- file.path(dName, dFiles)
      lapply(deleteFiles, file.remove)
    }
  } else dir.create(dName)
  return(NULL)
}

clean_Directory(localDir)


save_Data <- function(dropDir, data, useFileName, numToRate = 10, backup = FALSE) {
  ## Save the given data to the DropBox directory dropDir, using file name useFileName. ##
  closeAllConnections()
  
  failSave <- TRUE  # decide if we want to make a last attmetp to save when saving fails
  # Read all the files into a list
  allFilesInfo <- drop_dir(dropDir)
  fileNames <- allFilesInfo$name
  print(fileNames)
  isPSPfile <- str_detect(fileNames, "PSPComments.RData")
  fileNames <- fileNames[isPSPfile]
  print(paste0("in save data, file names are: ", fileNames, " and useFileName is: ", useFileName))
  
  # Read the time stamps, sort in decreading order (so most recent to oldest)
  leading_digits <- as.integer(str_extract(fileNames, "^\\d+"))
  m <- sort(leading_digits, decreasing = TRUE)
  numFiles <- length(m)
  
  # delete the oldest file, if there are more than 2 in the directory
  if (numFiles > 2) { 
    oldFile <- which(str_detect(allFilesInfo$name, as.character(m[numFiles])))
    deletePath <- allFilesInfo$path_display[oldFile] 
    drop_delete(path = deletePath)
  }
  
  # Create a unique file name
  if (numToRate == 0) {
    backup <- failSave <- TRUE
    useDir <- completeDir
    clearDir <- TRUE
  } else { 
    useDir <- dropDir
    clearDir <- FALSE
  }
  
  fileName <- paste(as.integer(Sys.time()), useFileName, sep = "_")
  # Write the data to a temporary file locally
  filePath <- file.path(localDir, fileName)
  save(data, file = filePath)
  
  # Upload the file to Dropbox, include Backup if backup is TRUE
  if (backup == TRUE) {
    drop_upload(filePath, path = backupDir)
  }
  
  response <- FALSE
  attempts <- 0L
  while (!is.null(response) && response == FALSE) { # try to save the file 4 times
    if (attempts > 4L) {
      clearDir <- FALSE
      break
    }
  response <- tryCatch(drop_upload(filePath, path = useDir), error = function(e) FALSE)
  attempts <- attempts + 1
  }
  
  if (!is.null(response) && response == FALSE && failSave == TRUE) {
    drop_upload(filePath, path = useDir)
  } else if (clearDir == TRUE) {
    clean_dropDirectory(dropDir)
  }
  
  closeAllConnections()
  
  return(response)
}


load_Data <- function(dropDir) {
  ## Get the file with comments to be evaluated.  ##
  ## Look first in the user directory, and pick the file with the latest timestamp.  ##
  ## Files in the user directory are saved as <Timestamp>_<user>_<FileNumber>_PSPComments.RData ##
  ## if none there, get a new file from the main directory, starting from the smallest file number available ##
  ## Files in the main dir are saved as <FileNumber>_PSPComments.RData ##
  closeAllConnections()
  

  print(paste0("in load data, dropDir is: ", dropDir))
  # We will need to write the data to a temporary file locally
  timeStamp <- as.integer(Sys.time())
  fileName <- paste(timeStamp, dropDir, "PSPComments.RData", sep = "_")
  datafile <- file.path(localDir, fileName)
  response <- FALSE
  dropFN <- c()
  
  print(showConnections())
  # Check whether the user already has a directory, otherwise create one:
  if (!drop_exists(dropDir)) {
    drop_create(dropDir)
    allFilesInfo <- NULL
    fileNames <- NULL
  } else {
    # Read all the files into a list
    allFilesInfo <- drop_dir(dropDir)
    fileNames <- allFilesInfo$name
    print(paste0("in load data, file names are: ", fileNames))
    isPSPfile <- str_detect(fileNames, "PSPComments.RData")
    fileNames <- fileNames[isPSPfile]
  }
  
  # If the user directory is empty, get the files from mainDir
  if (is_empty(fileNames)) {
    allFilesInfo <- drop_dir(mainDir)
    fileNames <- allFilesInfo$name
    l <- length(fileNames)
    leading_digits <- as.integer(str_extract(fileNames, "^\\d+"))  # extract the file number, because we want to pick the smallest file number first
    m <- order(leading_digits, decreasing = FALSE)
    print(m)
    i <- 1
    while (i < l + 1) { # keep trying to download files until success
      j <- m[i]
      print(paste0("in load data, the ,", i, "th attempt to download from New4PSP, and the file index is ", j))
      
      filePath <- allFilesInfo$path_display[j]
      response <- tryCatch(drop_download(filePath, local_path = datafile, overwrite = TRUE),
                           error = function(e) FALSE)
      if (response == TRUE) {
        dropFN <- paste(dropDir, fileNames[j], sep = "_")
        saveName <- paste(timeStamp, dropFN, sep = "_")
        savePath <- file.path(dropDir, saveName)
        drop_move(from_path = filePath, to_path = savePath)
        break
      }
      i <- i + 1
    }
  } else {    # get the files from the user directory
    
    leading_digits <- as.integer(str_extract(fileNames, "^\\d+"))
    m <- order(leading_digits, decreasing = TRUE)
    numFiles <- length(m)
    i <- 1
    while (i < numFiles + 1) {  # keep trying to download files until it works
      j <- m[i]
      filePath <- allFilesInfo$path_display[j]
      response <- tryCatch(drop_download(filePath, local_path = datafile, overwrite = TRUE), 
                           error = function(e) FALSE)
      
      if (response == TRUE) {
        print(paste0("in load data, the ,", i, "th attempt to download, and the file index is ", j))
        j <- m[i]
        pattern <- paste0(as.character(leading_digits[j]), "_")
        dropFN <- str_replace(allFilesInfo$name[j], pattern, "")
        break
      }
      i <- i + 1
    }
  }  
  
  if (response == TRUE) {
    attach(datafile)
    savedData <- ls(paste0("file:", datafile))
    
    currentComments <- get(savedData, envir = .GlobalEnv)
    
    detach(paste0("file:", datafile), character.only = TRUE)
  } else {
    currentComments <- NULL
  }
  
  closeAllConnections()
  
  return(list(dropFile = dropFN, dataFile = currentComments))
}


#### Function to separate out links from the normal text ------------

split_Text <- function(oldText) {
  
  linkPattern <- "(https?://[^\\s]+)" 
  linkLoc <- str_locate(oldText, linkPattern)
  
  picPattern <- "(pic.twitter.com/[^\\s]+)"
  picLoc <- str_locate(oldText, picPattern)
  
  if (all(is.na(linkLoc))) {
    linkLoc <- picLoc
  }
  
  if (all(is.na(linkLoc))) return(list(oldText))
  
  if (!all(is.na(picLoc)) && picLoc[1, 1] < linkLoc[1, 1]) {
    linkLoc <- picLoc
  }
  
  myText <- str_sub(oldText, start = 1, end = linkLoc[1, 1] - 1)
  myLink <- str_sub(oldText, start = linkLoc[1, 1], end = linkLoc[1, 2])
  myLinkTag <- substitute(a(href = myLink, myLink, target = "_blank")) # we use substitute, and not quote.
  # quote would also leave the variable myLink unevaluated, whereas substitute returns variables bound in env
  # We need to include the link label, or else it doesn't work 
  # (i.e a(href = myLink, link label), and not a(href = myLink))
  # target = '_blank' ensures that the link opens in a new window.
  
  myLinkText <- list(myText, myLinkTag)
  otherText <- str_sub(oldText, start = linkLoc[1, 2] + 1)
  if (!(is_empty(otherText) || otherText == "")) {
    newLinkText <- split_Text(otherText)
    myLinkText <- c(myLinkText, newLinkText)
  }
  return(myLinkText)
}


#########################################################################################
#########################################################################################
#### Define UI #### ---------------------------------------------------------------------
#########################################################################################
#########################################################################################

ui <- fluidPage(
  
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  
  sidebarLayout(

## Layout of left selection panel -----------------------------------   
sidebarPanel(
  br(),
  br(),
  wellPanel(uiOutput("selectPol")),
  br(),
  br(),
  br(),
  fluidRow(helpText("Anzahl der noch zu bewertenden Kommentare ")),
  wellPanel(uiOutput("info")),
  fluidRow(helpText("Wenn du den ganzen Inhalt dieses Kommentarpakets bewertet hast, kannst du entweder auf 'SAVE' oder auf 'EXIT' drücken.  
Deine Daten werden gespeichert, und, wenn du 'SAVE' geklickt hast, wird ein neues Kommentarpaket -- sofern noch vorhanden -- hochgeladen. ")),
  br(),
  br(),
  br(),
  fluidRow(helpText("ZUR INFO: deine Daten werden automatisch alle 6 Minuten gespeichert, und zusätzlich noch jedes Mal wenn 
du auf 'SAVE' oder 'EXIT' drückst.  Bitte schließe nicht das Fenster ohne zuerst auf 'SAVE' oder 'EXIT' zu drücken,
sonst gehen die letzten Minuten Arbeit verloren.")),
  br(),
  # fluidRow(column(width = 1, offset = 4, 
  #                 actionButton("Save", label = "SALVA"))
  # ),
  fluidRow(   ### include EXIT button
    column(width = 1, offset = 1, 
           actionButton("Save", label = "SAVE")),
    column(width = 1, offset = 3, 
           actionButton("Exit", label = "EXIT"))
  ),
  br(),
  wellPanel(uiOutput("savedInfo") %>% 
              withSpinner(color = "#0000FF"))
),

## Layout of main panel ---------------------------------------------    
    mainPanel(
      uiOutput('activePage')
    )
  )
  
)

#########################################################################################
#########################################################################################
#### Define the server function #### ----------------------------------------------------
#########################################################################################
#########################################################################################

server <- function(input, output, session) {

  
#### Variable Definitions #### ----------------------------------------------------------
## Useful global variables, but not to share across sessions!!! -----
# dropFileName will be modified once, when we download our data file
# But we don't need to make it into a reactive value, because it will be changed exactly once
# Put this inside the server function, otherwise it will get shared with other sessions!!!
  dropFileName <- c()
  
# We define these globally here - we will change them only once, when we load the data,
# so we will not turn them into reactive Values
  
  currentComments <- list()  # The list of comments to evaluate, separated by politician
  commentLists <- c()  # The names of the politicians in the comment list
  numTabs <- 0  # How many politicians are in the list (i.e. list length)
  
# These will contain, per politician (hence, list), number of comments, number already rated, 
# number of comments not to be rated (i.e. comments included just as replyTo)
  numPages <- numRated <- notIncluded <- list() 
  
# The total (over all politicians) number of comments in the list of comments
  totalPages <- 0
  
## Variables needed to circumvent weird start-up behaviour ----------
  startNow <- TRUE   # startNow is used in currentTab(), to determine which panel to show on start up
  firstPanel <- NULL # which Panel to use on start up
  changeCounter <- 0
  saveNow <- FALSE



## Define global reactive values ## ---------------------------------
  
  commentLevel <- reactiveValues()
  textList <- reactiveValues()
  numToBeRated <- reactiveValues()
  reload <- reactiveValues(Data = 0) # Used to trigger loading of new dataset
  whichTab <- reactiveValues(changeCurrent = 0L, prev = NULL, justChanged = 0) 
  # whichTab$changeCurrent is used to trigger the move to the next politician tab, 
  # if the next arrow is pressed when we are already at the last comment of the current politician
  
  saveResponse <- reactiveValues(latest = "start")
  downloadFile <- reactiveValues(Done = FALSE)
  

#### Utility Functions #### -------------------------------------------------------------
  
## Check if the evalutaions are complete before proceeding ## -------
  check_Ratings <- function(pID) {
    isolate({
      row <- commentLevel[[pID]]
      
      # If the comment is rated as NON valutabile, then we are done with it and can move on
      nonValPanelID <- paste0(nonValID, pID)
      if (isTruthy(input[[nonValPanelID]]) == TRUE) return("Done")  
      
      
      topicBoxPanelID <- paste0(topicBoxID, pID)
      noTopic <- is_empty(input[[topicBoxPanelID]])
      
      
      checkBoxPanelIDA <- paste0(checkBoxIDA, pID)
      Rating <- as.integer(input[[checkBoxPanelIDA]])
      
      needTopic <- noTopic && (Rating > -1L)
      
      if (needTopic == TRUE) {
        return("needTopic")
      } else {
        return("Done")
      }
    })
  }
  
  
  print_Warning <- function(msg, saveIt = FALSE) {
    if (saveIt == TRUE) {
      msgText <- "Alle Daten gespeichert ausser diese Seite, die noch nicht fertig bewertet wurde.  
Wenn du auch diese Seite speichern willst, dann bitte fertig bewerten."
    } else if (msg == "needTopic") {
      msgText <- "Bitte suche das Thema/die Themen des Posts aus bevor du weitergehst.  
Wenn es nicht möglich ist, diesen Kommentar zu bewerten, klicke bitte auf 'NICHT bewertbar'.  Wenn du drüber nachdenken willst, und später zurückkommen, 
suche bitte die Auswahl 'Später bewerten' in der Rubrik 'Sentimentanalyse' aus. "
    } 
    
    showModal(modalDialog(
      msgText,
      easyClose = TRUE,
      fade = TRUE,
      footer = modalButton("Alles Klar"),
      size = "l"
    ))
  }

    
  #########################################################################################  
  ###############################  LOGIN WINDOW ###########################################
  #########################################################################################  
  # Only need this when not using Shiny with authentication
  LOGIN <- reactiveValues(Success = FALSE, Attempts = 0)
  
  #### Get the user name #### -------------------------------------------------------------
  
  userDir <- eventReactive(LOGIN$Success, {
    pattern <- "@.+"
    replaceString <- input$userName
    
    response <- str_replace(replaceString, pattern, "") %>%
      str_replace_all("[^A-z0-9]", "")
    
    response
    
  })
  
  output$loginMod = renderUI({
    list(
      textInput(inputId = "userName", label = "User Name", placeholder = "username"),
      br(),
      passwordInput(inputId = "userPwd", label = "Password")
    )
  })
  
  observeEvent(LOGIN$Attempts, {
    showModal(modalDialog(
      title = "Barometro LOGIN",
      uiOutput("loginMod"),
      easyClose = FALSE,
      fade = TRUE,
      footer = actionButton(inputId = "Submit", label = "Submit"),
      size = "l"
    ))
  },
  priority = 60)
  
  observeEvent(input$Submit, {
    if (input$userPwd == "NRW2019") {
      removeModal()
      LOGIN$Success <- TRUE
    } else {
      LOGIN$Success <- FALSE
      LOGIN$Attempts <- LOGIN$Attempts + 1
      if (LOGIN$Attempts == 5) {
        js$closeWindow()
        stopApp()
      }
    }
  },
  ignoreInit = TRUE,
  priority = 50)
  
  
  ############################# END LOGIN WINDOW ##########################################
  
  
#########################################################################################  
###############################  Making Panels ##########################################
#########################################################################################   

  #### Set the current Tab #### -----------------------------------------------------------  

  # based on sidebar selection, and page moves (submit, prev, etc)   
  currentTab <- reactive({
    req(downloadFile$Done)

    input$currentPolitician
    whichTab$changeCurrent

        if (startNow == TRUE) {  
          startNow <<- FALSE
          newTab <- firstPanel
          
          updateSelectInput(session, "currentPolitician", label = "Selezionare politico da valutare", 
                            choices = commentLists, 
                            selected = newTab)
          
          whichTab$prev <- newTab
          return(newTab)
        }
        
        oldTab <- isolate(whichTab$prev)
        newTab <- input$currentPolitician
        
        noTabs <- is_empty(oldTab) || is_empty(newTab) || (newTab == oldTab)
        
        doneRating <- check_Ratings(oldTab)
        if (doneRating != "Done" && noTabs == FALSE) {
          newTab <- oldTab
          updateSelectInput(session, "currentPolitician", label = "Selezionare politico da valutare", 
                            choices = commentLists, 
                            selected = newTab)
          print_Warning(doneRating)
        }
        
        print(paste0("in currentTab, oldTab is: ", oldTab, " and newTab is: ", newTab))
        
        
        changeTab <- whichTab$changeCurrent
        if (changeTab > changeCounter) {
          if (changeTab == changeCounter + 1) {
            changeCounter <<- changeTab
            
            i <- which(commentLists == newTab)
            if (i == numTabs) {
              i <- 1
            } else {
              i <- i + 1
            } 
            newTab <- commentLists[i]
          } else {
            changeCounter <<- changeTab
            
            numRounds <- 1
            while (numRounds < numTabs) {
              i <- which(commentLists == newTab)
              if (i == numTabs) {
                i <- 1
              } else {
                i <- i + 1
              }
              
              newTab <- commentLists[i]
              if (numToBeRated[[newTab]] > 0) break
              numRounds <- numRounds + 1
            }
          }
          
          updateSelectInput(session, "currentPolitician", label = "Selezionare politico da valutare", 
                            choices = commentLists, 
                            selected = newTab)
          
        } else if (changeTab < changeCounter) {
          if (changeTab == changeCounter - 1) {
            changeCounter <<- changeTab
            
            i <- which(commentLists == newTab)
            if (i == 1) {
              i <- numTabs
            } else {
              i <- i - 1
            } 
            newTab <- commentLists[i]
          } else {
            changeCounter <<- changeTab
            
            repeat {
              i <- which(commentLists == newTab)
              if (i == 1) {
                i <- numTabs
              } else {
                i <- i - 1
              }
              
              newTab <- commentLists[i]
              if (numToBeRated[[newTab]] > 0) break
            }
          }
          
          updateSelectInput(session, "currentPolitician", label = "Wähle den zu bewertenden Politiker", 
                            choices = commentLists, 
                            selected = newTab)
          
        }
          

        if (newTab != oldTab) {
          whichTab$justChanged <- isolate(whichTab$justChanged) + 1
        }
        
        
        print(paste0("in currentTab 3, whichTab$justChanged is: ", isolate(whichTab$justChanged)))
        

        return(newTab)
      }) 
  
  
  
  ## update the main panel when we navigate forwards or backwards -----------------------
  update_PanelData <- function(panelID) {
    print(paste0("in update panel data, panel ID is: ", panelID))
    row <- isolate(commentLevel[[panelID]])

    UIoutputID <- paste0("Panel", panelID)
    textReplyID <- paste0("ReplyToText", panelID)
    textID <- paste0('Comment', panelID)
    
    nonValPanelID <- paste0(nonValID, panelID)
    isFakePanelID <- paste0(isFakeID, panelID)
    topicBoxPanelID <- paste0(topicBoxID, panelID)
    
    checkBoxPanelIDA <- paste0(checkBoxIDA, panelID)

    
    LinksID <- paste0("myLinks", panelID)

    currentText <- currentComments[[panelID]]$text[row]

    currentLevel <- currentComments[[panelID]]$Level[row]
    if (currentLevel == 0) {
      topicBoxNames <- topicBoxNames0
      topicBoxValues <- topicBoxValues0
    } else {
      topicBoxNames <- topicBoxNames1
      topicBoxValues <- topicBoxValues1
    }
    
    output[[textID]] <- renderText({
      fromUser <- currentComments[[panelID]]$user[row]
      byPhrase <- ifelse(is.na(fromUser) || fromUser == "Anonymous", " von einem unbekannten user ", paste0(" von ", fromUser))
      
      paste0('Bitte bewerte den folgenden Kommentar ', byPhrase, " der auf der Twitter/Facebook Seite von ", 
              panelID, " erschien:")
    })
    
    output[[textReplyID]] <- renderText({ 
      replyToID <- currentComments[[panelID]]$replyToID[row]
      replyToUser <- currentComments[[panelID]]$replyToUser[row]
      isPol <- currentComments[[panelID]]$Level[row] == 0L
      if (isPol || is.na(replyToID)) {
        useText <- ""
      } else {
        if (is.na(replyToUser) || replyToUser == "Anonymous") {
          replyToPhrase <- "un utente sconosciuto: "
        } else {
          replyToPhrase <- paste0(replyToUser, ": ")
        }
        useText <- paste0("In risposta ad un commento o post pubblicato da ", replyToPhrase)
      }
    })
    
    output[[UIoutputID]] <- renderText({ 
      replyToID <- currentComments[[panelID]]$replyToID[row]
      replyToUser <- currentComments[[panelID]]$replyToUser[row]
      if (is.na(replyToID)) {
        useText <- ""
      } else {
        replyRow <- which(currentComments[[panelID]]$id == replyToID)
        useText <- currentComments[[panelID]]$text[replyRow]
      }
      useText
    })
    
    
    textList <- split_Text(currentText)
    
    output[[LinksID]] = renderUI({
      llply(textList, function(l) {eval(l)})
    }) 
    
   
    # Determine what goes into the checkBox Labels (Parts A & B)
    selection <- str_split(currentComments[[panelID]]$Rating[row], " ")[[1]] %>%
      as.integer()
    
    nonValSelection <- FALSE
    
    if (any(selection < 0L)) {
      if (any(selection == "-2")) {
        selectionA <- NULL
        nonValSelection <- TRUE
      }
      if (any(selection == "-10")) {
        selectionA <- -10L
      } else {
        selectionA <- selection
      }
    } else {
      selectionA <- selection
    }
    

    selectionTopic <- str_split(currentComments[[panelID]]$Topic[row], " ")[[1]]
    
    isFakeSelection <- currentComments[[panelID]]$isFake[row]
    
    updateCheckboxInput(session, 
                             inputId = isFakePanelID,
                             label = isFakeLabel,
                             value = isFakeSelection)
    
    updateCheckboxInput(session, 
                        inputId = nonValPanelID,
                        label = nonValLabel,
                        value = nonValSelection)
    
    
    updateRadioButtons(session, 
                       inputId = checkBoxPanelIDA,
                       label = checkBoxLabelA,
                       choiceNames = checkBoxNamesA,
                       choiceValues = checkBoxValuesA,
                       selected = selectionA) 
    
    
    
    updateCheckboxGroupInput(session,
                             inputId = topicBoxPanelID,
                             label = topicBoxLabel,
                             choiceNames = topicBoxNames,
                             choiceValues = topicBoxValues,
                             selected = selectionTopic)
    
    return(invisible)
  }


  ## set up the main panel whenever we switch between politicians -----------------------  
  make_Panel <- function(panelID) {
    print(paste0("in make panel, panel ID is: ", panelID))
    row <- isolate(commentLevel[[panelID]])
    
    # Determine what goes into the checkBox Labels (Parts A & B)
    selection <- str_split(currentComments[[panelID]]$Rating[row], " ")[[1]] %>%
      as.integer()
    
    nonValSelection <- FALSE
    if (any(selection < 0L)) {
      if (any(selection == "-2")) {
        selectionA <- NULL
        nonValSelection <- TRUE
      }
      if (any(selection == "-10")) {
        selectionA <- -10L
      } else {
        selectionA <- selection
      }
    } else {
      selectionA <- selection
    }
    
    selectionTopic <- str_split(currentComments[[panelID]]$Topic[row], " ")[[1]]
    
    isFakeSelection <- currentComments[[panelID]]$isFake[row]
    print(isFakeSelection)
    textID <- paste0('Comment', panelID)
    textReplyID <- paste0("ReplyToText", panelID)
    
    UIoutputID <- paste0("Panel", panelID)
    
    checkBoxPanelIDA <- paste0(checkBoxIDA, panelID)

    nonValPanelID <- paste0(nonValID, panelID)
    isFakePanelID <- paste0(isFakeID, panelID)
    topicBoxPanelID <- paste0(topicBoxID, panelID)
    
    LinksID <- paste0("myLinks", panelID)
    InfoID <- paste0("Info", panelID)
    
    prevButton <- paste0("prevBtn")
    prevButton2 <- paste0("prevBtn2")
    nextButton <- paste0("submit")
    nextButton2 <- paste0("submit2")
    
    currentText <- currentComments[[panelID]]$text[row]
    
    currentLevel <- currentComments[[panelID]]$Level[row]
    if (currentLevel == 0) {
      topicBoxNames <- topicBoxNames0
      topicBoxValues <- topicBoxValues0
    } else {
      topicBoxNames <- topicBoxNames1
      topicBoxValues <- topicBoxValues1
    }
    
    output[[textID]] <- renderText({
      fromUser <- currentComments[[panelID]]$user[row]
      byPhrase <- ifelse(is.na(fromUser) || fromUser == "Anonymous", " von einem unbekannten user", paste0(" von ", fromUser))
      
      paste0('Bitte bewerte den folgenden Kommentar ', byPhrase, " auf der Twitter/Facebook Seite von ", 
             panelID, "erschienen:")
    })
    
    output[[textReplyID]] <- renderText({ 
      replyToID <- currentComments[[panelID]]$replyToID[row]
      replyToUser <- currentComments[[panelID]]$replyToUser[row]
      isPol <- currentComments[[panelID]]$Level[row] == 0L
      if (isPol || is.na(replyToID)) {
        useText <- ""
      } else {
        if (is.na(replyToUser) || replyToUser == "Anonymous") {
          replyToPhrase <- "einem unbekannten User: "
        } else {
          replyToPhrase <- paste0(replyToUser, ": ")
        }
        useText <- paste0("In Antwort auf einen Kommentar oder Post von ", replyToPhrase)
      }
    })
    
    output[[UIoutputID]] <- renderText({ 
      replyToID <- currentComments[[panelID]]$replyToID[row]
      replyToUser <- currentComments[[panelID]]$replyToUser[row]
      if (is.na(replyToID)) {
        useText <- ""
      } else {
        replyRow <- which(currentComments[[panelID]]$id == replyToID)
        useText <- currentComments[[panelID]]$text[replyRow]
      }
      useText
    })
    
    
    textList <- split_Text(currentText)
    
    output[[LinksID]] = renderUI({
      llply(textList, function(l) {eval(l)})
    }) 
    

    myTab <- {list(
      titlePanel(panelID),
      br(),
      br(),
      textOutput(textID),
      br(),
      wellPanel(uiOutput(LinksID)),
      br(),
      textOutput(textReplyID),
      conditionalPanel(
        condition = "output.replyExists == true", 
        wellPanel(textOutput(UIoutputID)),
        br()),
      conditionalPanel(
        condition = "output.notRated == true", 
        fluidRow(column(width = 6, offset = 2,
                        uiOutput("notRatedText"))),
        br()),
      br(),
      fluidRow(column(width = 5, offset = 1,
                      checkboxInput(inputId = nonValPanelID,
                                    label = nonValLabel,
                                    value = nonValSelection)),
               column(width = 5, offset = 1,
                      checkboxInput(inputId = isFakePanelID,
                                    label = isFakeLabel,
                                    value = isFakeSelection))
      ),
      br(),
      fluidRow(
        column(width = 4, offset = 1,
               helpText(topicBoxHelp),
               checkboxGroupInput(inputId = topicBoxPanelID,
                                  label = topicBoxLabel,
                                  choiceNames = topicBoxNames,
                                  choiceValues = topicBoxValues,
                                  selected = selectionTopic)),
        column(width = 4, offset = 2,
               helpText(checkBoxHelpA),
               radioButtons(inputId = checkBoxPanelIDA,
                            label = checkBoxLabelA,
                            choiceNames = checkBoxNamesA,
                            choiceValues = checkBoxValuesA,
                            selected = selectionA))
      ),
      br(),
      
      br(),
      fluidRow(column(width = 4, offset = 1, 
                      helpText("Wenn du auf  'Vorig' klickst, gelangst du zum Kommentar gleich vor diesem."),
                      actionButton(prevButton, label = "< Vorig <", width = "100%"),
                      br(),
                      actionButton(prevButton2, label = "<< Vorig nicht bewertet <<", width = "100%"),
                      helpText("Klickst du stattdessen auf 'Vorig nicht bewertet' gelangst du zum ersten vorigen Kommentar, der noch nicht bewertet worden ist.")),
               column(width = 4, offset = 2, 
                      helpText("Wenn du auf 'Nächst' klickst, kommst du zum Kommentar gleich nach diesem."),
                      actionButton(inputId = nextButton, label = "> Nächst >", width = "100%"),
                      br(),
                      actionButton(inputId = nextButton2, label = ">> Nächst nicht bewertet >>", width = "100%"),
                      helpText("Klickst du stattdesssen auf 'Nächst nicht bewertet' gelangst du zum ersten Kommentar nach diesem, der noch nicht bewertet worden ist."))
      ),
      br(),
      br()
    )
    }
    
    return(myTab)
  }
  
  
  update_NumToRate <- function(currentPage) {isolate({
    
    print(paste0("in update num to rate 1a, whichTab$justChanged is: ", isolate(whichTab$justChanged)))
    checkBoxPanelIDA <- paste0(checkBoxIDA, currentPage)
    nonValPanelID <- paste0(nonValID, currentPage)
    
    nonValutabile <- isolate(isTruthy(input[[nonValPanelID]]))
    
    if ((!isTruthy(input[[checkBoxPanelIDA]])) && (nonValutabile == FALSE)) return(100L)  # just a number to indicate that updating is not possible
    
    print(paste0("in update num to rate 1b, whichTab$justChanged is: ", isolate(whichTab$justChanged)))
    currentRow <- commentLevel[[currentPage]]
    
    if (nonValutabile == TRUE) {
      rating <- -2L
    } else {
      rating <- as.integer(isolate(input[[checkBoxPanelIDA]]))
    }
    prevRating <- str_split(currentComments[[currentPage]]$Rating[currentRow], " ")[[1]] %>%
      as.integer()
    
    if ((rating == -10) && (any(prevRating != -10))) {
      numToRate <- 1
    } else if ((rating != -10) && (any(prevRating == -10))) {
      numToRate <- -1
    } else {
      numToRate <- 0
    }
    
    print(paste0("in update num to rate 2, whichTab$justChanged is: ", isolate(whichTab$justChanged)))
    print(numToRate)
    
    return(numToRate)
  })
  }
  
  
  update_Ratings <- function(currentPage) {
    
    print(paste0("in update ratings 1, whichTab$justChanged is: ", isolate(whichTab$justChanged)))
    nonValPanelID <- paste0(nonValID, currentPage)
    isFakePanelID <- paste0(isFakeID, currentPage)
    
    checkBoxPanelIDA <- paste0(checkBoxIDA, currentPage)

    topicBoxPanelID <- paste0(topicBoxID, currentPage)
    
    nonValutabile <- isTruthy(input[[nonValPanelID]])
    
    row <- commentLevel[[currentPage]]
    print("going to update num to rate")
    addTo <- update_NumToRate(currentPage) # addTo is 100 if the currentPage has not been fully rated yet
    if (addTo < 50) {
      numToBeRated[[currentPage]] <- numToBeRated[[currentPage]] + addTo
      numToBeRated[["Total"]] <- numToBeRated[["Total"]] + addTo
      
      if (nonValutabile == TRUE) {
        rating <- -2L
        useRating <- "-2"
      } else {
        rating <- as.integer(isolate(input[[checkBoxPanelIDA]]))  ## don't need to check for truthiness because already done in update_NumToRate
        useRating <- str_trim(paste(rating, sep = " ", collapse = " "), side = "both")
      }
      
      currentComments[[currentPage]]$Rating[row] <<- useRating
      
      isFakeNews <- isolate(input[[isFakePanelID]])
      
      if (rating %in% c(-10, -2)) {
        useTopic <- "None" 
        isFakeNews <- FALSE
      } else {
        postTopics <- isolate(input[[topicBoxPanelID]])
        useTopic <- str_trim(paste(postTopics, sep = " ", collapse = " "), side = "both")
      }
      
      currentComments[[currentPage]]$Topic[row] <<- useTopic
      
      currentComments[[currentPage]]$isFake[row] <<- isFakeNews
      
      
    }
    print(paste0("in update ratings 2, whichTab$justChanged is: ", isolate(whichTab$justChanged)))
    return(invisible)
  } 
  
  
 
   
#########################################################################################  
############# Page Navigation Functions #################################################
######################################################################################### 
  
  navPage <- function(direction, currentPage) {
    print("in navPage")
    newRow <- commentLevel[[currentPage]] + direction
    if ((newRow > notIncluded[[currentPage]]) && (newRow < notIncluded[[currentPage]] + numPages[[currentPage]] + 1)) {
      commentLevel[[currentPage]] <<- newRow
    } else if (newRow > notIncluded[[currentPage]] + numPages[[currentPage]]) {  # need to move up one panel
      print("in navPage, exceeded currentPage comments, moving up")
      whichTab$changeCurrent <<- whichTab$changeCurrent + 1
      newPage <- isolate(currentTab())
      newRow <- notIncluded[[newPage]] + 1
      commentLevel[[newPage]] <<- newRow
    } else if (newRow < notIncluded[[currentPage]] + 1) {  # need to move down one panel
      print("in navPage, moving down")
      whichTab$changeCurrent <<- whichTab$changeCurrent - 1
      newPage <- isolate(currentTab())
      newRow <- notIncluded[[newPage]] + numPages[[newPage]]
      commentLevel[[newPage]] <<- newRow
    }
    return(NULL)
  }
  
  
  
  find_NextUnrated <- function(currentPage, currentRow = NULL, direction = "+") {
    print("in next unrated")
    if (is.null(currentRow)) {
      currentRow <- commentLevel[[currentPage]]
    }
    newRating <- currentComments[[currentPage]]$Rating
    unRated <- which(str_detect(newRating, "-10"))
    if (direction == "+") {
      nextUnrated <- unRated[unRated > currentRow]
    } else {
      nextUnrated <- unRated[unRated < currentRow]
    }
    whereRow <- which.min(abs(nextUnrated - currentRow))
    if (is_empty(whereRow)) {
      if (direction == "+") {
        whichTab$changeCurrent <<- isolate(whichTab$changeCurrent) + 5
      } else {
        whichTab$changeCurrent <<- isolate(whichTab$changeCurrent) - 5
      }
        isolate(i <- currentTab())
        newDF <- filter(currentComments[[i]], Include == "Y")      
        newRating <- newDF$Rating
        unRated <- which(str_detect(newRating, "-10"))
        if (!is_empty(unRated)) {
          unRated <- min(unRated)
          commentLevel[[i]] <<- min(notIncluded[[i]] + unRated, numPages[[i]] + notIncluded[[i]])
        }
      newDirection <- 0 
      # this means for prev unrated, we will not move, and for nextUnrated, we will proceeed to the next politician
    } else {
      newRow <- nextUnrated[whereRow]
      newDirection <- newRow - currentRow
    }
    
    return(newDirection)
    }

 
#### Submit Buttons: Moving Forwards or Backwards - the observe events ####--------------


## Submit Button: move to the next comment (rated or not) ----------- 
## - including if for the next politician 
  
  observeEvent(input$submit, {
    req(downloadFile$Done)
    
    panelID <- currentTab()
    
    evalDone <- check_Ratings(panelID)
    
    if (evalDone == "Done") {
      
      update_Ratings(panelID)
      
      navPage(1, panelID)
      newPanelID <- currentTab()
      
      update_PanelData(newPanelID)
    } else {
      print_Warning(evalDone)
    }
  })
  
         
## Submit Button 2: move to the next unrated comment ----------------
## - including if for the next politician 

  observeEvent(input$submit2, {
    req(downloadFile$Done)
    
    panelID <- currentTab()
    
    evalDone <- check_Ratings(panelID)
    
    if (evalDone == "Done") {
      
      update_Ratings(panelID)
      
      numSteps <- find_NextUnrated(panelID, direction = "+")
      newPanelID <- currentTab()
      
      navPage(numSteps, newPanelID)
      update_PanelData(newPanelID)
    } else {
      print_Warning(evalDone)
    }
  })
  
  
  
### The previous buttons do not have the ability to navigate away from a politician.  The next buttons do.
## Previous Button 1: -----------------------------------------------
## move to the previous comment (evaluated or not) 
  
  
  observeEvent(input$prevBtn, {
    req(downloadFile$Done)
    
    panelID <- currentTab()
    
    evalDone <- check_Ratings(panelID)
    
    if (evalDone == "Done") {
      
      update_Ratings(panelID)
      
      navPage(-1, panelID)
      update_PanelData(panelID)
    } else {
      print_Warning(evalDone)
    }
    
  })
  
  
## Previous Button 2: -----------------------------------------------
## move to the previous unrated comment 
  
  observeEvent(input$prevBtn2, {
    req(downloadFile$Done)
    
    panelID <- currentTab()
    
    evalDone <- check_Ratings(panelID)
    
    if (evalDone == "Done") {
      
      update_Ratings(panelID)
      
      numSteps <- find_NextUnrated(panelID, direction = "-")
      navPage(numSteps, panelID)
      update_PanelData(panelID)
    } else {
      print_Warning(evalDone)
    }
    
  })
  
  
## Save ratings when change to a different politician ---------------
  
  observeEvent(whichTab$justChanged, {
    print(paste0("in observe event current pol, whichTab$prev is: ", isolate(whichTab$prev)))
    req(isolate(whichTab$prev))

    update_Ratings(isolate(whichTab$prev))
    whichTab$prev <- isolate(currentTab())
    print(paste0("end of observe event current pol, whichTab$justChanged is: ", whichTab$justChanged))
  }, priority = 10, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
#########################################################################################  
############################ FILE SAVING OPERATIONS #####################################
#########################################################################################  
  
### Automatic save every 6 minutes ###
  observe({
    req(downloadFile$Done)
    invalidateLater(320000)
    if (saveNow == FALSE) {
      saveNow <<- TRUE
    } else if (saveNow == TRUE) {
      isolate({
        filePath <- file.path("Data", dropFileName)
        saveResponse$latest <- "saving"
        response <- save_Data(dropDir = userDir(), data = currentComments, useFileName = dropFileName)
        if (is.null(response)) saveResponse$latest <- as.character(Sys.time() + 3600) else saveResponse$latest <- "failure"
      })
    }
    })


### Save button action ###  
  observeEvent(input$Save, {
    req(downloadFile$Done)
    closeAllConnections()
    print("in observe event input save")
    panelID <- currentTab()
    print("after current tab, back in observe event input save")
    ratingsDone <- check_Ratings(panelID)
    
    if (ratingsDone == "Done") {
    update_Ratings(panelID)
    }
    
    stillUnrated <- numToBeRated[["Total"]]
    response <- save_Data(dropDir = userDir(), data = currentComments, 
                          useFileName = dropFileName, numToRate = stillUnrated, backup = TRUE)
    if (is.null(response)) {
      saveResponse$latest <- as.character(Sys.time() + 3600)
      
      if (ratingsDone != "Done") print_Warning(ratingsDone, saveIt = TRUE)  # Warn that current page was not saved
      
      if (stillUnrated == 0) {  # reset lots of variables, so we can start afresh with a new dataset
        print("setting downloadFile Done to False")
        downloadFile$Done <- FALSE
        reload$Data <- reload$Data + 1
        saveResponse$latest <- "reloaded"
      }
    } else {
      saveResponse$latest <- "fail"
    }
  })


  ### with observe Event EXIT  
  observeEvent(input$Exit, {
    req(downloadFile$Done)
    closeAllConnections()
    
    panelID <- currentTab()
    ratingsDone <- check_Ratings(panelID)
    
    if (ratingsDone == "Done") {
      update_Ratings(panelID)
    }
    
    stillUnrated <- numToBeRated[["Total"]]
    response <- save_Data(dropDir = userDir(), data = currentComments, 
                          useFileName = dropFileName, numToRate = stillUnrated, backup = TRUE)
    if (is.null(response)) {
      saveResponse$latest <- as.character(Sys.time() + 3600)
      
      js$closeWindow()
      stopApp()
    } else {
      saveResponse$latest <- "fail"
    }  
  })
  
  
### Save action information ###  
  output$savedInfo <- renderUI({
    req(downloadFile$Done)
    latest <- saveResponse$latest
    saveText <- switch(latest,
                       start = "Dati appena caricati con successo",
                       fail = "Dati non salvati.  Riprova per favore.",
                       saving = "Dati in caricamento.",
                       reloaded = paste0("Dati completi salvati con successo alle ", 
                                         as.character(Sys.time() + 3600), " e nuovi dati caricati."),
                       paste0("Dati salvati con successo alle ", latest)
    )
    list(saveText)
  })


######################################################################################### 
###################### Info for Conditional Panels ###################################### 
#########################################################################################
  
  output$notRatedText <- renderUI({  
    req(downloadFile$Done)
      withTags({
        div(class = "Unrated Comment", style = "background-color: #DE2D26; color: #f4a582;",
        h4("Questo commento non è ancora stato valutato."))
      })
  })
        

  output$replyExists <- reactive({
    req(downloadFile$Done)
    panelID <- currentTab()
    row <- commentLevel[[panelID]]
    print("in replyExists")
    if (currentComments[[panelID]]$Level[row] == 0) {
      reply <- FALSE
      print(reply)
    } else {
      reply <- !is.na(currentComments[[panelID]]$replyToID[row])
    }
    
    return(reply)
  })

  output$notRated <- reactive({
    req(downloadFile$Done)
    panelID <- currentTab()
    row <- commentLevel[[panelID]]
    unrated <- any(as.integer(str_split(currentComments[[panelID]]$Rating[row], " ")[[1]]) < -5)
    return(unrated)
  })
  
  
  
  
  
#########################################################################################  
################ Sidebar Output ######################################################### 
#########################################################################################
  
  
  output$selectPol <- renderUI({
    req(userDir())
    req(downloadFile$Done)
    
    reload$Data
    selectInput(inputId = "currentPolitician", label = "Wähle den Politiker/Influencer/Partei aus, den du bewerten möchtest", 
                choices = commentLists, selected = firstPanel, multiple = FALSE)
  })
  
  
  output$info <- renderUI({
    print("in output$info")
    req(downloadFile$Done)
    panelID <- currentTab()
    list(
      paste0("Für ", panelID, ":  ", numToBeRated[[panelID]], "\n"),
      br(),
      hr(),
      br(),
      paste0("In Summe:  ", numToBeRated[["Total"]])
    )
  })

   
  
#########################################################################################  
################## GET & PREPARE DATA UPON DOWNLOAD #####################################
#########################################################################################  
  

## First, what to do when there are no more comment files to download:  
  output$noCommentsMod = renderText({
    "Derzeit keine Daten zum Aufladen bereit.  Wenn diese Nachricht in einer Stunde wieder erscheint, bitte den IT Support kontaktieren.  
Bitte 'Dismiss' anklicken um das Fenster zu schließen."
  })
  

  observeEvent(input$errorExit, {
    js$closeWindow()
    stopApp()
  })
  
## The download command center: -------------------------------------
  
  populate_Data <- function() {
    loadedData <- load_Data(userDir())
    dropFileName <<- loadedData$dropFile
    currentComments <<- loadedData$dataFile
    if (is_empty(currentComments)) {  # Give a warning that there are no comments to download and exit
      showModal(modalDialog(
        title = "Dati mancanti",
        textOutput("noCommentsMod"),
        easyClose = TRUE,
        fade = TRUE,
        footer = actionButton("errorExit", "Dismiss"),
        size = "l"
      ))
      downloadFile$Done <- FALSE
      return(invisible)
      
    } 
      
    
    commentLists <<- names(currentComments)
    numTabs <<- length(commentLists)
    
    
    # Initializing the reactiveValues that have to be lists, with names the same as the
    # politician data list
    # This may not be absolutely necessary, but is better form.
    numPages <<- numRated <<- notIncluded <<- vector("list", numTabs)
    names(numPages) <<- names(numRated) <<- names(notIncluded) <<- commentLists
    
    
    ## Prepare Data: Utility function ## 
    # Get the number of pages for each politician, number of already rated comments, and respective totals.
    # Rearrange the DF so that rated comments go first, and can start from the first unrated comment.
    
    asInt <- function(R, op, num) {
      newR <- vapply(R, function(x) {
        y <- str_split(x, " ")[[1]] %>%
        as.integer()
        if (op == ">") {
        res <- any(y > num)
        } else {
          res <- any(y < num)
        }
        return(res)
      }, FUN.VALUE = FALSE)
      
      return(newR)
    }
    
    
    prepare_Data <- function(l) {
      currentDF <- currentComments[[l]]
      numPages[[l]] <<- sum(currentDF$Include == "Y")
      ratedDF <- filter(currentDF, asInt(Rating, op = ">", num = -5) | Include == "N") %>%
        arrange(Include)
      notIncluded[[l]] <<- sum(ratedDF$Include == "N")
      numRated[[l]] <<- sum(ratedDF$Include == "Y")
      unratedDF <- filter(currentDF, asInt(Rating, op = "<", num = -8), Include == "Y")
      currentComments[[l]] <<- bind_rows(ratedDF, unratedDF)
      
      return(invisible)
    }
    
    
    lapply(commentLists, prepare_Data)
    
    totalPages <<- reduce(numPages, `+`)
    
    ## Initialize the tabs to the first unrated comment
    lapply(commentLists, function(i) {
      commentLevel[[i]] <<- min(notIncluded[[i]] + numRated[[i]] + 1, numPages[[i]] + notIncluded[[i]])
      numToRate <- numPages[[i]] - numRated[[i]]
      numToBeRated[[i]] <<- numToRate

      # Set the first Panel to the first Politician with non-zero comments to be rated:
      if (numToRate > 0 && is.null(firstPanel)) {
        firstPanel <<- i
        print(paste0("in prepare data, firstPanel is: ", firstPanel))
      }
      
    })
    
    # In case all Politicians have already been rated, we can just start at the first Politician
    if (is.null(firstPanel)) firstPanel <<- commentLists[1]
    
    # We also want to keep track of the total number to be rated
    numToBeRated[["Total"]] <<- totalPages - reduce(numRated, `+`)
    
    downloadFile$Done <- TRUE
    
    return(invisible)
  }
  
  

  
## ObserveEvents related to loading and reloading data: -------------
  
  observeEvent(userDir(), {
    req(userDir())
    populate_Data()
  })
  
  
  observeEvent(reload$Data, {
    
        msgText <- "Hai finito di valutare questo pacchetto di commenti.  Vuoi caricare il prossimo?"
      
      showModal(modalDialog(
        msgText,
        easyClose = TRUE,
        fade = TRUE,
        footer = tagList(
#          modalButton("No"),
          actionButton("noReload", label = "No"),
          actionButton("reload", label = "Si")
        ),
        size = "l"
      ))
  }, priority = 10, ignoreInit = TRUE)
  
  observeEvent(input$noReload, { # close window and Exit
    js$closeWindow()
    stopApp()
  })
  
  
  # This event will be triggered by the save action, when the number of comments still to be rated is zero
  observeEvent(input$reload, {
    req(userDir())
    print("in observe event input$reload")
    
    removeModal()
    startNow <<- TRUE
    saveNow <<- FALSE
    firstPanel <<- NULL
    whichTab$prev <- NULL
    changeCounter <<- whichTab$changeCurrent
    whichTab$justChanged <- whichTab$justChanged + 1
    
    populate_Data()
  }, priority = 100, ignoreInit = TRUE)
  

  output$activePage = renderUI({
    req(downloadFile$Done)
    req(whichTab$justChanged)
    print(paste0("in active page, just changed = ", whichTab$justChanged))
    isolate({

      currentPol <- currentTab()
      print(paste0("in active page, whichTab$prev is: ", isolate(whichTab$prev)))
      print(paste0("in active page, startNow is: ", startNow))
      print(paste0("in active page, currentPol is: ", currentPol, " and input$currentPol is: ", input$currentPolitician))
      if (!is_empty(currentPol)) {
        make_Panel(currentPol)
      }
    })
    
  })
  
  
#########################################################################################
#################   outputOptions #######################################################
#########################################################################################
  
  outputOptions(output, "replyExists", suspendWhenHidden = FALSE) 
  outputOptions(output, "notRated", suspendWhenHidden = FALSE)
  
  
}
  


shinyApp(ui, server)
