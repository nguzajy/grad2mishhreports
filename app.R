library(shinydashboard)
require(plyr)
library(dplyr)
library(shiny)
library(RCurl)
require(RPostgreSQL)
require(reshape2)
library(rpivotTable)
require(tables)
library(DT)
library(xlsx)
library(rsconnect)
library(xml2)
library(rvest)
library(tidyr)
source("sitesettings.R")

options(knitr.table.format="html")
options(scipen=999) #avoids printing exponential notations such 1e+10

options(warn = -1) #suppresses warnings

year <- c(format(Sys.Date(), "%Y"))

#Website Log in Information
Logged = FALSE

#Overrall dashboard
dashTitle <- paste0("GRAD2MIS HH")
ui <- dashboardPage(
  dashboardHeader(title = dashTitle),
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    menuItem(
      "VESA Households", tabName = "vhdashboard", icon = icon("dashboard")
    ),
    menuItem(
      "VESA Saving", tabName = "vsdashboard", icon = icon("dashboard")
    ),
    menuItem("VESA Loan", tabName = "vldashboard", icon = icon("dashboard")),
    menuItem("VESA Training", tabName = "vtdashboard", icon = icon("dashboard")),
    menuItem("VESA Discussion", tabName = "vddashboard", icon = icon("dashboard")),
    menuItem("FFS Loan", tabName = "mldashboard", icon = icon("dashboard")),
    menuItem("FFS Saving", tabName = "msdashboard", icon = icon("dashboard")),
    menuItem("Value Chains", tabName = "vcdashboard", icon = icon("dashboard")),
    menuItem("Wage Employment", tabName = "wedashboard", icon = icon("dashboard")),
    menuItem("Off Farm", tabName = "ofdashboard", icon = icon("dashboard")),
    menuItem("Other Livelihood Pathways", tabName = "lpdashboard", icon = icon("dashboard"))
  )),
  
  ## Body content
  dashboardBody(tabItems(
    # VESA Households tab content
    tabItem(tabName = "vhdashboard",
            fluidPage(
              titlePanel("VESA Households"),
              
              downloadButton(outputId = "download_vesahhqtr_filtered",
                             label = "Download Data"),
              fluidRow(tags$head(tags$style( type = 'text/css',  '#pivot_vesa_hhQtr{ overflow-x: scroll; overflow-y: scroll; }')),
                       rpivotTableOutput("pivot_vesa_hhQtr", width = "100%", height = "800px"))
            )),
    
    # First tab content - VESA Saving body
    tabItem(tabName = "vsdashboard",
            fluidPage(
              titlePanel("VESA Household Saving - Time frame"),
              
              # Select Quarter or Month 
              dateRangeInput("monthslide", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                             end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
              selectInput(
                inputId =  "save2", 
                label = "Select data", 
                c("saved", "not saved"),
                selected = "saved"
              ),
              #verbatimTextOutput("savingqtr_filtered_row"),
              downloadButton(outputId = "download_savingqtr_filtered",
                             label = "Download Data"),
              fluidRow(tags$head(tags$style( type = 'text/css',  '#pivot_vesa_savingQtr{ overflow-x: scroll; overflow-y: scroll; }')),
                       rpivotTableOutput("pivot_vesa_savingQtr", width = "100%", height = "800px"))
            )),
    
    
    # Second tab content - VESA Loans body
    tabItem(tabName = "vldashboard",
            fluidPage(
              titlePanel("VESA Household Loan - Time frame"),
              
              # Select Quarter or Month 
              dateRangeInput("monthslide2", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                             end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
              selectInput(
                inputId =  "loan2", 
                label = "Select data", 
                c("received loan", "not received loan"),
                selected = "received loan"
              ),
              downloadButton(outputId = "download_loanqtr_filtered",
                             label = "Download Data"),
              fluidRow(tags$head(tags$style( type = 'text/css',  '#pivot_vesa_loanQtr{ overflow-x: scroll; overflow-y: scroll; }')),
                       rpivotTableOutput("pivot_vesa_loanQtr", width = "100%", height = "800px"))
              
            )),
    
    # Third tab content - VESA Training Body
    tabItem(tabName = "vtdashboard",
            fluidPage(
              titlePanel("VESA Household Training - Time frame"),
              # Select Quarter or Month 
              dateRangeInput("monthslide3", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                             end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
              selectInput(
                inputId =  "training2", 
                label = "Select data", 
                c("participated", "did not participate"),
                selected = "participated"
              ),
              downloadButton(outputId = "download_trainingQtr",
                             label = "Download Data"),
              fluidRow(
                tags$head(tags$style( type = 'text/css',  '#pivot_vesa_trainingQtr{ overflow-x: scroll; overflow-y: scroll; }')),
                rpivotTableOutput("pivot_vesa_trainingQtr", width = "100%", height = "800px"))
              
            )),
    
    # Fourth tab content - VESA Discussions Body
    tabItem(tabName = "vddashboard",
            fluidPage(
              titlePanel("VESA Household Discussion - Time frame"),
              verbatimTextOutput("discussionqtr_filtered_row"),
              # Select Quarter or Month 
              dateRangeInput("monthslide4", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                             end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
              selectInput(
                inputId =  "discussion2", 
                label = "Select data", 
                c("participated", "did not participate"),
                selected = "participated"
              ),
              downloadButton(outputId = "download_discussionQtr",
                             label = "Download Raw Data"),
              fluidRow(
                tags$head(tags$style( type = 'text/css',  '#pivot_vesa_discussionQtr{ overflow-x: scroll; overflow-y: scroll; }')),
                rpivotTableOutput("pivot_vesa_discussionQtr", width = "100%", height = "800px")),hr(),
              
              titlePanel("List of HHs that completed all Discussions- Time frame"),
              
              
              fluidRow(DT::dataTableOutput('discussion_dtq'))
              
            )),
    
    # Fifth tab content - FFS Loan Body
    tabItem(tabName = "mldashboard",
            fluidPage(
              titlePanel("Formal Financial Services - Loans"),
              # Select Quarter or Month 
              dateRangeInput("monthslide5", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                             end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
              selectInput(
                inputId =  "loantype", 
                label = "Select FFS", 
                c("MFI", "RuSACCO"),
                selected = "MFI"
              ),
              selectInput(
                inputId =  "ffsloan", 
                label = "Select data", 
                c("participated", "did not participate"),
                selected = "participated"
              ),
              downloadButton(outputId = "download_ffsloan",
                             label = "Download Raw Data"),
              fluidRow(
                tags$head(tags$style( type = 'text/css',  '#pivot_ffsloan{ overflow-x: scroll; overflow-y: scroll; }')),
                rpivotTableOutput("pivot_ffsloan", width = "100%", height = "800px"))
              
            )),
    
    # Sixth tab content - FFS Saving Body
    tabItem(tabName = "msdashboard",
            fluidPage(
              titlePanel("Formal Financial Services - Saving"),
              # Select Quarter or Month 
              dateRangeInput("monthslide6", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                             end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
              selectInput(
                inputId =  "savetype", 
                label = "Select FFS", 
                c("MFI", "RuSACCO")),
              selectInput(
                inputId =  "ffssave", 
                label = "Select data", 
                c("participated", "did not participate"),
                selected = "participated"
              ),
              downloadButton(outputId = "download_ffssave",
                             label = "Download Raw Data"),
              fluidRow(
                tags$head(tags$style( type = 'text/css',  '#pivot_ffssave{ overflow-x: scroll; overflow-y: scroll; }')),
                rpivotTableOutput("pivot_ffssave", width = "100%", height = "800px"))
              
            )),
    
    # Seventh tab content - Value Chains Body
    tabItem(tabName = "vcdashboard",
            fluidPage(
              titlePanel("Value Chains - Time frame"),
              # Select Quarter or Month 
              dateRangeInput("monthslide7", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                             end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
              selectInput(
                inputId =  "vcselect", 
                label = "Select data", 
                c("participated", "did not participate"),
                selected = "participated"
              ),
              downloadButton(outputId = "download_vc",
                             label = "Download Raw Data"),
              fluidRow(
                tags$head(tags$style( type = 'text/css',  '#pivot_vc{ overflow-x: scroll; overflow-y: scroll; }')),
                rpivotTableOutput("pivot_vc", width = "100%", height = "800px"))
              
            )),
    
    #Eigth Tab - Wage Employment
    tabItem(tabName = "wedashboard",
            fluidPage(
              titlePanel("Wage Employment - Time frame"),
              
              # Select Quarter or Month 
              dateRangeInput("monthslide8", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                             end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
              selectInput(
                inputId =  "wage2", 
                label = "Select data", 
                c("linked", "not linked"),
                selected = "linked"
              ),
              downloadButton(outputId = "download_wage",
                             label = "Download Data"),
              fluidRow(tags$head(tags$style( type = 'text/css',  '#pivot_vesa_wage{ overflow-x: scroll; overflow-y: scroll; }')),
                       rpivotTableOutput("pivot_vesa_wage", width = "100%", height = "800px"))
              
            )),
    
    #Nineth Tab - Off farm
    tabItem(tabName = "ofdashboard",
            fluidPage(
              titlePanel("Off farm Activities - Time frame"),
              
              # Select Quarter or Month 
              dateRangeInput("monthslide9", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                             end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
              selectInput(
                inputId =  "ofselect", 
                label = "Select data", 
                c("participated", "did not participate"),
                selected = "participate"
              ),
              downloadButton(outputId = "download_offfarm",
                             label = "Download Data"),
              fluidRow(tags$head(tags$style( type = 'text/css',  '#pivot_offfarm{ overflow-x: scroll; overflow-y: scroll; }')),
                       rpivotTableOutput("pivot_offfarm", width = "100%", height = "800px"))
              
            )),
    
    #Tenth Tab - Other Livelihood Pathways
    tabItem(tabName = "lpdashboard",
            fluidPage(
              titlePanel("Other Livelihood Pathways - Time frame"),
              
              # Select Quarter or Month 
              dateRangeInput("monthslide10", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                             end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
              selectInput(
                inputId =  "lpselect", 
                label = "Select data", 
                c("participated", "did not participate"),
                selected = "participate"
              ),
              downloadButton(outputId = "download_lp",
                             label = "Download Data"),
              fluidRow(tags$head(tags$style( type = 'text/css',  '#pivot_lp{ overflow-x: scroll; overflow-y: scroll; }')),
                       rpivotTableOutput("pivot_lp", width = "100%", height = "800px"))
              
            ))
    
  )
  
  )
  
)



server <- function(input, output, session) {
  
  #year3 <- "2018"
  #################################Authentication######################################
  values <- reactiveValues(authenticated = FALSE)
  
  #Gets Household Graduation status
  hhGraduation <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/Px4p2D1QTty/data.csv"), userpwd=userpass, httpauth = 1L)
  hhGraduation <- read.table(text = hhGraduation, sep =",", header = TRUE, stringsAsFactors = FALSE)
  
  hhGraduation$graduation_status[hhGraduation$graduation_status == "false"] <- "null"
  hhGraduation$graduation_status[hhGraduation$graduation_status == "true"] <- "Graduated"
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      title = "Enter HH reports credentials",
      passwordInput("password", "Password:"),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.  
  # This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal. 
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Password <- input$password
    })
    Id.password <- which(my_password == Password)
    if (length(Id.password) > 0) {
      if (Id.password == Id.password) {
        Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()
        
      } else {
        values$authenticated <- FALSE
        
      }     
    }
  })
  
  
  #########################################################
  
  
  #Quarterly Data
  
  #VESA Household
  vesaHHData <- reactive({
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/RkAOfvnQxTp/data.csv"), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    #Gets a list of of VESA, PSNP and Saving amount for the year
    
    vesaHH <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/WGO4WfPbJXL/data.csv"), userpwd=userpass, httpauth = 1L)
    
    vesaHH <- read.table(text = vesaHH, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    vesaHHDropOut <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/TkpDy3nZbKL/data.csv"), userpwd=userpass, httpauth = 1L)
    
    vesaHHDropOut <- read.table(text = vesaHHDropOut, sep =",", header = TRUE, stringsAsFactors = FALSE)
    vesaHHDropOut$hh_dropout[vesaHHDropOut$hh_dropout == "true"] <- "HH Dropout"
    
    
    if(is.data.frame(vesaHH) && nrow(vesaHH)!=0){
      vesaHH$beneficiary_status[vesaHH$beneficiary_status == "MHH"] <- "ADULT"
      vesaHH$beneficiary_status[vesaHH$beneficiary_status == "FHH"] <- "ADULT"
      vesaHH$beneficiary_status[vesaHH$beneficiary_status == "Dual"] <- "ADULT"
      vesaHH$beneficiary_status[vesaHH$beneficiary_status == "SPOUSE"] <- "ADULT"
    }
    vesaHH <- merge(x = vesaHH, y = vesaHHDropOut, by = "psnp_number", all.x = TRUE)
    vesaHH <- unique(vesaHH)
    
    vesaHH <- merge(x = vesaHH, y = hhTypeQtr, by = "psnp_number", all.x = TRUE)
    vesaHH <- unique(vesaHH)
    
    vesaHH <- merge(x = vesaHH, y = hhGraduation, by = "psnp_number", all.x = TRUE)
    vesaHH <- unique(vesaHH)
    
    
    return(vesaHH)})
  
  #VESA Saving Data
  savingDataQuarter <- reactive({
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/QiPM5jhPlzh/data.csv?var=period1:",input$monthslide[1],"&var=period2:",input$monthslide[2]), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    savingAmountQuarter <- data.frame(Date=as.Date(character()),
                                      File=character(), 
                                      User=character(), 
                                      stringsAsFactors=FALSE) 
    
    #Gets a list of of VESA, PSNP and Saving amt per quarter
    if(input$save2 == "saved"){
      savingAmountQuarter <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/M4a8hSJRXfa/data.csv?var=period1:",input$monthslide[1],"&var=period2:",input$monthslide[2]), userpwd=userpass, httpauth = 1L)
      savingAmountQuarter <- read.table(text = savingAmountQuarter, sep =",", header = TRUE, stringsAsFactors = FALSE)
      if(is.data.frame(savingAmountQuarter) && nrow(savingAmountQuarter)!=0){
        savingAmountQuarter$beneficiary_status[savingAmountQuarter$beneficiary_status == "MHH"] <- "ADULT"
        savingAmountQuarter$beneficiary_status[savingAmountQuarter$beneficiary_status == "FHH"] <- "ADULT"
        savingAmountQuarter$beneficiary_status[savingAmountQuarter$beneficiary_status == "Dual"] <- "ADULT"
        savingAmountQuarter$beneficiary_status[savingAmountQuarter$beneficiary_status == "SPOUSE"] <- "ADULT"
      }
      
      savingAmountQuarter <- merge(x = savingAmountQuarter, y = hhTypeQtr, by = "psnp_number", all.x = TRUE)
      savingAmountQuarter <- unique(savingAmountQuarter)
      
      savingAmountQuarter <- merge(x = savingAmountQuarter, y = hhGraduation, by = "psnp_number", all.x = TRUE)
      savingAmountQuarter <- unique(savingAmountQuarter)
      
    }
    else{
      #Saving data
      savingAmountQuarter2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/M4a8hSJRXfa/data.csv?var=period1:",input$monthslide[1],"&var=period2:",input$monthslide[2]), userpwd=userpass, httpauth = 1L)
      savingAmountQuarter2 <- read.table(text = savingAmountQuarter2, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      savingAmountQuarter2 <- merge(x = savingAmountQuarter2, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      savingAmountQuarter2 <- unique(savingAmountQuarter2)
      
      savingAmountQuarter2 <- merge(x = savingAmountQuarter2, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      savingAmountQuarter2 <- unique(savingAmountQuarter2)
      
      #Removes the unrequired columns/variables saving and report date
      savingAmountQuarter2 <- select(savingAmountQuarter2, -saving)
      savingAmountQuarter2 <- select(savingAmountQuarter2, -report_date)
      
      #All PSNP numbers
      allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
      allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      
      #selects all records PSNP numbers that are not present in the Saving Data table
      savingAmountQuarter <- subset(allPSNPnumbers, !(psnp_number %in% savingAmountQuarter2$psnp_number))
    }
    
    #Summarize and aggregate the savings data
    #savingAmountQuarter <- ddply(savingAmountQuarter, .(report_date, region,zone,woreda,kebele,vesa,psnp_number, household_type), summarise, saving_amount=sum(as.numeric(saving)))
    
    return(savingAmountQuarter)})
  
  loanDataQuarter <- reactive({
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/QiPM5jhPlzh/data.csv?var=period1:",input$monthslide2[1],"&var=period2:",input$monthslide2[2]), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    
    
    #Gets a list of of VESA, PSNP and loan amount
    if(input$loan2 == "received loan"){
      loanAmountQuarter <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/DlTcpawtIC1/data.csv?var=period1:",input$monthslide2[1],"&var=period2:",input$monthslide2[2]), userpwd=userpass, httpauth = 1L)
      
      loanAmountQuarter <- read.table(text = loanAmountQuarter, sep =",", header = TRUE, stringsAsFactors = FALSE)
      if(is.data.frame(loanAmountQuarter) && nrow(loanAmountQuarter)!=0){
        loanAmountQuarter$beneficiary_status[loanAmountQuarter$beneficiary_status == "MHH"] <- "ADULT"
        loanAmountQuarter$beneficiary_status[loanAmountQuarter$beneficiary_status == "FHH"] <- "ADULT"
        loanAmountQuarter$beneficiary_status[loanAmountQuarter$beneficiary_status == "Dual"] <- "ADULT"
        loanAmountQuarter$beneficiary_status[loanAmountQuarter$beneficiary_status == "SPOUSE"] <- "ADULT"
      }
      
      loanAmountQuarter <- merge(x = loanAmountQuarter, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      loanAmountQuarter <- unique(loanAmountQuarter)
      loanAmountQuarter <- merge(x = loanAmountQuarter, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      loanAmountQuarter <- unique(loanAmountQuarter)
    }
    else{
      loanAmountQuarter2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/DlTcpawtIC1/data.csv?var=period1:",input$monthslide2[1],"&var=period2:",input$monthslide2[2]), userpwd=userpass, httpauth = 1L)
      
      loanAmountQuarter2 <- read.table(text = loanAmountQuarter2, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      
      loanAmountQuarter2 <- merge(x = loanAmountQuarter2, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      loanAmountQuarter2 <- unique(loanAmountQuarter2)
      loanAmountQuarter2 <- merge(x = loanAmountQuarter2, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      loanAmountQuarter2 <- unique(loanAmountQuarter2)
      
      #Removes the unrequired columns/variables loan and report date
      loanAmountQuarter2 <- select(loanAmountQuarter2, -loan)
      loanAmountQuarter2 <- select(loanAmountQuarter2, -report_date)
      
      #All PSNP numbers
      allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
      allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      
      #selects all records PSNP numbers that are not present in the Loan Data table
      loanAmountQuarter <- subset(allPSNPnumbers, !(psnp_number %in% loanAmountQuarter2$psnp_number))
    }
    return(loanAmountQuarter)
  })
  
  
  #Training Data Quarterly
  trainingDataQuarter <- reactive({
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/QiPM5jhPlzh/data.csv?var=period1:",input$monthslide3[1],"&var=period2:",input$monthslide3[2]), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    #Gets a list of of VESA, PSNP and loan amount
    
    if(input$training2 == "participated"){
      trainingQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/I8MTZhh3sR7/data.csv?var=period1:",input$monthslide3[1],"&var=period2:",input$monthslide3[2]), userpwd=userpass, httpauth = 1L)
      
      trainingQtr <- read.table(text = trainingQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      trainingQtr <- merge(x = trainingQtr, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      trainingQtr <- unique(trainingQtr)
      trainingQtr <- merge(x = trainingQtr, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      trainingQtr <- unique(trainingQtr)
    }
    else
    {
      #training data
      trainingQtr2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/I8MTZhh3sR7/data.csv?var=period1:",input$monthslide3[1],"&var=period2:",input$monthslide3[2]), userpwd=userpass, httpauth = 1L)
      
      trainingQtr2 <- read.table(text = trainingQtr2, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      trainingQtr2 <- merge(x = trainingQtr2, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      trainingQtr2 <- unique(trainingQtr2)
      trainingQtr2 <- merge(x = trainingQtr2, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      trainingQtr2 <- unique(trainingQtr2)
      
      #Removes the unrequired columns/variables vesa_training and report date
      trainingQtr2 <- select(trainingQtr2, -vesa_training)
      trainingQtr2 <- select(trainingQtr2, -report_date)
      
      #All PSNP numbers
      allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
      allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      
      #selects all records PSNP numbers that are not present in the 
      trainingQtr <- subset(allPSNPnumbers, !(psnp_number %in% trainingQtr2$psnp_number))
      
    }
    return(trainingQtr)
  })
  
  #Discussion Data Quarterly
  discussionDataQuarter <- reactive({
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/QiPM5jhPlzh/data.csv?var=period1:",input$monthslide4[1],"&var=period2:",input$monthslide4[2]), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    #Gets a list of of VESA, PSNP and loan amount
    if(input$discussion2 == "participated"){
      discussionQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/nUrDo1BipCl/data.csv?var=period1:",input$monthslide4[1],"&var=period2:",input$monthslide4[2]), userpwd=userpass, httpauth = 1L)
      
      discussionQtr <- read.table(text = discussionQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      discussionQtr <- merge(x = discussionQtr, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      discussionQtr <- unique(discussionQtr)
      discussionQtr <- merge(x = discussionQtr, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      discussionQtr <- unique(discussionQtr)
      
    }
    
    else{
      #Discussion data
      discussionQtr2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/nUrDo1BipCl/data.csv?var=period1:",input$monthslide3[1],"&var=period2:",input$monthslide3[2]), userpwd=userpass, httpauth = 1L)
      
      discussionQtr2 <- read.table(text = discussionQtr2, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      discussionQtr2 <- merge(x = discussionQtr2, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      discussionQtr2 <- unique(discussionQtr2)
      discussionQtr2 <- merge(x = discussionQtr2, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      discussionQtr2 <- unique(discussionQtr2)
      
      #Removes the unrequired columns/variables vesa_discussion and report date
      discussionQtr2 <- select(discussionQtr2, -vesa_discussion)
      discussionQtr2 <- select(discussionQtr2, -report_date)
      
      #All PSNP numbers
      allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
      allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      
      #selects all records PSNP numbers that are not present in the 
      discussionQtr <- subset(allPSNPnumbers, !(psnp_number %in% discussionQtr2$psnp_number))
    }
    
    return(discussionQtr)
  })
  
  #Completed Discussion Data Quarterly
  CompletedDiscussionData <- reactive({
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/QiPM5jhPlzh/data.csv?var=period1:",input$monthslide4[1],"&var=period2:",input$monthslide4[2]), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    completedDiscussions <- data.frame(Date=as.Date(character()),
                                       File=character(), 
                                       User=character(), 
                                       stringsAsFactors=FALSE) 
    
    #Gets a list of of VESA, PSNP and loan amount
    if(input$discussion2 == "participated"){
      discussionQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/nUrDo1BipCl/data.csv?var=period1:",input$monthslide4[1],"&var=period2:",input$monthslide4[2]), userpwd=userpass, httpauth = 1L)
      
      discussionQtr <- read.table(text = discussionQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      discussionQtr <- merge(x = discussionQtr, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      discussionQtr <- unique(discussionQtr)
      discussionQtr <- merge(x = discussionQtr, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      discussionQtr <- unique(discussionQtr)
      
      completedDiscussions <- select(discussionQtr, -report_date)
      
      
      completedDiscussions <- ddply(completedDiscussions, .(region,zone,woreda,kebele,vesa,psnp_number), summarise, vesa_discussions=length(unique(vesa_discussion)))
      completedDiscussions <- filter(completedDiscussions, vesa_discussions >= 3 )
    }
    
    return(completedDiscussions)
  })
  
  
  #FFS Loan
  ffsloanData <- reactive({
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/QiPM5jhPlzh/data.csv?var=period1:",input$monthslide5[1],"&var=period2:",input$monthslide5[2]), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    #Gets a list of of VESA, PSNP and loan amount
    
    #Checks if selected input is MFI or RuSACCO
    if (input$loantype == "MFI"){
      if(input$ffsloan == "participated"){
        mfiloan <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/vKKgZNvykzu/data.csv?var=period1:",input$monthslide5[1],"&var=period2:",input$monthslide5[2]), userpwd=userpass, httpauth = 1L)
        
        mfiloan <- read.table(text = mfiloan, sep =",", header = TRUE, stringsAsFactors = FALSE)
        
        mfiloan <- merge(x = mfiloan, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        mfiloan <- unique(mfiloan)
        mfiloan <- merge(x = mfiloan, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        ffsloan <- unique(mfiloan)
      }
      else
      {
        #MFI Loan data
        mfiloan2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/vKKgZNvykzu/data.csv?var=period1:",input$monthslide5[1],"&var=period2:",input$monthslide5[2]), userpwd=userpass, httpauth = 1L)
        
        mfiloan2 <- read.table(text = mfiloan2, sep =",", header = TRUE, stringsAsFactors = FALSE)
        
        mfiloan2 <- merge(x = mfiloan2, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        mfiloan2 <- unique(mfiloan2)
        mfiloan2 <- merge(x = mfiloan2, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        mfiloan2 <- unique(mfiloan2)
        
        #Removes the unrequired columns/variables vesa_training and report date
        mfiloan2 <- select(mfiloan2, -mfi_loan)
        mfiloan2 <- select(mfiloan2, -report_date)
        
        #All PSNP numbers
        allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
        allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
        allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        allPSNPnumbers <- unique(allPSNPnumbers)
        allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        allPSNPnumbers <- unique(allPSNPnumbers)
        
        #selects all records PSNP numbers that are not present in the 
        ffsloan <- subset(allPSNPnumbers, !(psnp_number %in% mfiloan2$psnp_number))
        
      }}
    else {
      
      if(input$ffsloan == "participated"){
        rusloan <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/ihfsZtbXZRJ/data.csv?var=period1:",input$monthslide5[1],"&var=period2:",input$monthslide5[2]), userpwd=userpass, httpauth = 1L)
        
        rusloan <- read.table(text = rusloan, sep =",", header = TRUE, stringsAsFactors = FALSE)
        
        rusloan <- merge(x = rusloan, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        rusloan <- unique(rusloan)
        rusloan <- merge(x = rusloan, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        ffsloan <- unique(rusloan)
      }
      else
      {
        #RUSACCO Loan data
        rusloan2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/ihfsZtbXZRJ/data.csv?var=period1:",input$monthslide5[1],"&var=period2:",input$monthslide5[2]), userpwd=userpass, httpauth = 1L)
        
        rusloan2 <- read.table(text = rusloan2, sep =",", header = TRUE, stringsAsFactors = FALSE)
        
        rusloan2 <- merge(x = rusloan2, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        rusloan2 <- unique(rusloan2)
        rusloan2 <- merge(x = rusloan2, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        rusloan2 <- unique(rusloan2)
        
        #Removes the unrequired columns/variables vesa_training and report date
        rusloan2 <- select(rusloan2, -rusacco_loan)
        rusloan2 <- select(rusloan2, -report_date)
        
        #All PSNP numbers
        allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
        allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
        allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        allPSNPnumbers <- unique(allPSNPnumbers)
        allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        allPSNPnumbers <- unique(allPSNPnumbers)
        
        #selects all records PSNP numbers that are not present in the 
        ffsloan <- subset(allPSNPnumbers, !(psnp_number %in% rusloan2$psnp_number))
        
      }
      
    }
    return(ffsloan)
  })
  
  #FFS Saving
  ffsSaveData <- reactive({
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/QiPM5jhPlzh/data.csv?var=period1:",input$monthslide6[1],"&var=period2:",input$monthslide6[2]), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    #Gets a list of of VESA, PSNP and saving amount
    
    #Checks if selected input is MFI or RuSACCO
    if (input$savetype == "MFI"){
      if(input$ffssave == "participated"){
        mfisave <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/eHCvECi4psW/data.csv?var=period1:",input$monthslide6[1],"&var=period2:",input$monthslide6[2]), userpwd=userpass, httpauth = 1L)
        
        mfisave <- read.table(text = mfisave, sep =",", header = TRUE, stringsAsFactors = FALSE)
        
        mfisave <- merge(x = mfisave, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        mfisave <- unique(mfisave)
        mfisave <- merge(x = mfisave, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        ffssave <- unique(mfisave)
      }
      else
      {
        #MFI Saving data
        mfisave2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/eHCvECi4psW/data.csv?var=period1:",input$monthslide6[1],"&var=period2:",input$monthslide6[2]), userpwd=userpass, httpauth = 1L)
        
        mfisave2 <- read.table(text = mfisave2, sep =",", header = TRUE, stringsAsFactors = FALSE)
        
        mfisave2 <- merge(x = mfisave2, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        mfisave2 <- unique(mfisave2)
        mfisave2 <- merge(x = mfisave2, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        mfisave2 <- unique(mfisave2)
        
        #Removes the unrequired columns/variables vesa_training and report date
        mfisave2 <- select(mfisave2, -saving)
        mfisave2 <- select(mfisave2, -report_date)
        
        #All PSNP numbers
        allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
        allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
        allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        allPSNPnumbers <- unique(allPSNPnumbers)
        allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        allPSNPnumbers <- unique(allPSNPnumbers)
        
        #selects all records PSNP numbers that are not present in the 
        ffssave <- subset(allPSNPnumbers, !(psnp_number %in% mfisave2$psnp_number))
        
      }}
    else {
      
      if(input$ffssave == "participated"){
        russave <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/g2ceaJpudiR/data.csv?var=period1:",input$monthslide6[1],"&var=period2:",input$monthslide6[2]), userpwd=userpass, httpauth = 1L)
        
        russave <- read.table(text = russave, sep =",", header = TRUE, stringsAsFactors = FALSE)
        
        russave <- merge(x = russave, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        russave <- unique(russave)
        russave <- merge(x = russave, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        ffssave <- unique(russave)
      }
      else
      {
        #RUSACCO Saving data
        russave2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/g2ceaJpudiR/data.csv?var=period1:",input$monthslide6[1],"&var=period2:",input$monthslide6[2]), userpwd=userpass, httpauth = 1L)
        
        russave2 <- read.table(text = russave2, sep =",", header = TRUE, stringsAsFactors = FALSE)
        
        russave2 <- merge(x = russave2, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        russave2 <- unique(russave2)
        russave2 <- merge(x = russave2, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        russave2 <- unique(russave2)
        
        #Removes the unrequired columns/variables vesa_training and report date
        russave2 <- select(russave2, -saving)
        russave2 <- select(russave2, -report_date)
        
        #All PSNP numbers
        allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
        allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
        allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        allPSNPnumbers <- unique(allPSNPnumbers)
        allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        allPSNPnumbers <- unique(allPSNPnumbers)
        
        #selects all records PSNP numbers that are not present in the 
        ffssave <- subset(allPSNPnumbers, !(psnp_number %in% russave2$psnp_number))
        
      }
      
    }
    return(ffssave)
  })
  
  #Value Chains
  vcData <- reactive({
    
    vc <- data.frame(Date=as.Date(character()),
                     File=character(), 
                     User=character(), 
                     stringsAsFactors=FALSE) 
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/QiPM5jhPlzh/data.csv?var=period1:",input$monthslide7[1],"&var=period2:",input$monthslide7[2]), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    #Gets a list of of VESA, PSNP and Value Chain Participation
    
    if(input$vcselect == "participated"){
      #Fetch VC Complete data
      vcComplete <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/iab9IL26rLm/data.csv?var=period1:",input$monthslide7[1],"&var=period2:",input$monthslide7[2]), userpwd=userpass, httpauth = 1L)
      
      vcComplete <- read.table(text = vcComplete, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      if(is.data.frame(vcComplete) && nrow(vcComplete)!=0){
        if (vcComplete$completed_technical_training_curriculum == "true"){
          vcComplete$completed_technical_training_curriculum  <- "Completed Technical Training"
        }
        # Rename a column in R
        colnames(vcComplete)[colnames(vcComplete)=="completed_technical_training_curriculum"] <- "vc_activities"
      }
      #Fetch VC Business Plan Data
      vcBusiness <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/ino0zDv67lr/data.csv?var=period1:",input$monthslide7[1],"&var=period2:",input$monthslide7[2]), userpwd=userpass, httpauth = 1L)
      
      vcBusiness <- read.table(text = vcBusiness, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      if(is.data.frame(vcBusiness) && nrow(vcBusiness)!=0){
        if (vcBusiness$business_plan == "true"){
          vcBusiness$business_plan  <- "Business Plan"
        }
        # Rename a column in R
        colnames(vcBusiness)[colnames(vcBusiness)=="business_plan"] <- "vc_activities"
      }
      #Fetch VC Participation Data
      vcParticipation <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/p7JxUvZahIu/data.csv?var=period1:",input$monthslide7[1],"&var=period2:",input$monthslide7[2]), userpwd=userpass, httpauth = 1L)
      
      vcParticipation <- read.table(text = vcParticipation, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      if(is.data.frame(vcParticipation) && nrow(vcParticipation)!=0){
        if (vcParticipation$participation_in_development_discussions == "true"){
          vcParticipation$participation_in_development_discussions  <- "Participation in Development Discussions"
        }
        # Rename a column in R
        colnames(vcParticipation)[colnames(vcParticipation)=="participation_in_development_discussions"] <- "vc_activities"
        
        #Merge The Three VC tables
        vc <- vcComplete
        vc <- merge(x=vc, y = vcBusiness, by = c("region","zone","woreda","kebele","vesa","psnp_number", "beneficiary_status","report_date","value_chain", "vc_activities"), all.x = TRUE, all.y = TRUE)
        vc <- unique(vc)
        
        vc <- merge(x=vc, y = vcParticipation, by = c("region","zone","woreda","kebele","vesa","psnp_number","beneficiary_status","report_date","value_chain", "vc_activities"), all.x = TRUE, all.y = TRUE)
        vc <- unique(vc)
        
        
        vc <- merge(x = vc, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        vc <- unique(vc)
        vc <- merge(x = vc, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        vc <- unique(vc)
        
        if(is.data.frame(vc) && nrow(vc)!=0){
          vc$beneficiary_status[vc$beneficiary_status == "MHH"] <- "ADULT"
          vc$beneficiary_status[vc$beneficiary_status == "FHH"] <- "ADULT"
          vc$beneficiary_status[vc$beneficiary_status == "Dual"] <- "ADULT"
          vc$beneficiary_status[vc$beneficiary_status == "SPOUSE"] <- "ADULT"
        }
      }
      #print(vc, quote = TRUE, row.names = FALSE)
    }
    else
    {
      #VC data
      vcValueChain <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/kRCFa2S0Aex/data.csv?var=period1:",input$monthslide7[1],"&var=period2:",input$monthslide7[2]), userpwd=userpass, httpauth = 1L)
      
      vcValueChain <- read.table(text = vcValueChain, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      
      vcValueChain <- merge(x = vcValueChain, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      vcValueChain <- unique(vcValueChain)
      vcValueChain <- merge(x = vcValueChain, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      vcValueChain <- unique(vcValueChain)
      
      #Removes the unrequired columns/variables vesa_training and report date
      vcValueChain <- select(vcValueChain, -value_chain)
      vcValueChain <- select(vcValueChain, -report_date)
      
      #All PSNP numbers
      allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
      allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      
      #selects all records PSNP numbers that are not present in the 
      vc <- subset(allPSNPnumbers, !(psnp_number %in% vcValueChain$psnp_number))
      
    }
    return(vc)
  })
  
  #Wage Employment
  WageDataQuarter <- reactive({
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/QiPM5jhPlzh/data.csv?var=period1:",input$monthslide8[1],"&var=period2:",input$monthslide8[2]), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    #Gets a list of of VESA, PSNP and Saving amt per quarter
    if(input$wage2 == "linked"){
      WageQuarter <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/NUareAo2fDM/data.csv?var=period1:",input$monthslide8[1],"&var=period2:",input$monthslide8[2]), userpwd=userpass, httpauth = 1L)
      WageQuarter <- read.table(text = WageQuarter, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      WageQuarter <- merge(x = WageQuarter, y = hhTypeQtr, by = "psnp_number", all.x = TRUE)
      WageQuarter <- unique(WageQuarter)
      
      WageQuarter <- merge(x = WageQuarter, y = hhGraduation, by = "psnp_number", all.x = TRUE)
      WageQuarter <- unique(WageQuarter)
      if(is.data.frame(WageQuarter) && nrow(WageQuarter)!=0){
        WageQuarter$beneficiary_status[WageQuarter$beneficiary_status == "MHH"] <- "ADULT"
        WageQuarter$beneficiary_status[WageQuarter$beneficiary_status == "FHH"] <- "ADULT"
        WageQuarter$beneficiary_status[WageQuarter$beneficiary_status == "Dual"] <- "ADULT"
        WageQuarter$beneficiary_status[WageQuarter$beneficiary_status == "SPOUSE"] <- "ADULT"
      }
      
    }
    else{
      #Wage data
      WageQuarter2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/NUareAo2fDM/data.csv?var=period1:",input$monthslide8[1],"&var=period2:",input$monthslide8[2]), userpwd=userpass, httpauth = 1L)
      WageQuarter2 <- read.table(text = WageQuarter2, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      WageQuarter2 <- merge(x = WageQuarter2, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      WageQuarter2 <- unique(WageQuarter2)
      
      WageQuarter2 <- merge(x = WageQuarter2, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      WageQuarter2 <- unique(WageQuarter2)
      
      #Removes the unrequired columns/variables saving and report date
      WageQuarter2 <- select(WageQuarter2, -wage_employment)
      WageQuarter2 <- select(WageQuarter2, -date_linked)
      WageQuarter2 <- select(WageQuarter2, -report_date)
      
      #All PSNP numbers
      allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
      allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      
      #selects all records PSNP numbers that are not present in the Wage Data table
      WageQuarter <- subset(allPSNPnumbers, !(psnp_number %in% WageQuarter2$psnp_number))
    }
    
    #Summarize and aggregate the savings data
    #savingAmountQuarter <- ddply(savingAmountQuarter, .(report_date, region,zone,woreda,kebele,vesa,psnp_number, household_type), summarise, saving_amount=sum(as.numeric(saving)))
    
    return(WageQuarter)})
  
  #Off Farm
  ofData <- reactive({
    
    of <- data.frame(Date=as.Date(character()),
                     File=character(), 
                     User=character(), 
                     stringsAsFactors=FALSE) 
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/QiPM5jhPlzh/data.csv?var=period1:",input$monthslide9[1],"&var=period2:",input$monthslide9[2]), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    #Gets a list of of VESA, PSNP and Off Farm
    
    if(input$ofselect == "participated"){
      #Fetch of Microfranchise data
      ofMicrofranchise <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/orcrmI5IsuI/data.csv?var=period1:",input$monthslide9[1],"&var=period2:",input$monthslide9[2]), userpwd=userpass, httpauth = 1L)
      
      ofMicrofranchise <- read.table(text = ofMicrofranchise, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      if(is.data.frame(ofMicrofranchise) && nrow(ofMicrofranchise)!=0){
        if (ofMicrofranchise$microfranchise == "true"){
          ofMicrofranchise$microfranchise <- "Microfranchise"
        }
        # Rename a column in R
        colnames(ofMicrofranchise)[colnames(ofMicrofranchise)=="microfranchise"] <- "off_Farm"
      }
      #Fetch of BYOB Plan Data
      ofBYOB <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/WZSgy8sBV6o/data.csv?var=period1:",input$monthslide9[1],"&var=period2:",input$monthslide9[2]), userpwd=userpass, httpauth = 1L)
      
      ofBYOB <- read.table(text = ofBYOB, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      if(is.data.frame(ofBYOB) && nrow(ofBYOB)!=0){
        if (ofBYOB$byob == "true"){
          ofBYOB$byob  <- "BYOB skills training"
        }
        # Rename a column in R
        colnames(ofBYOB)[colnames(ofBYOB)=="byob"] <- "off_Farm"
      }
      #Fetch of Other Data
      ofOther <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/fdsgep1S3ff/data.csv?var=period1:",input$monthslide9[1],"&var=period2:",input$monthslide9[2]), userpwd=userpass, httpauth = 1L)
      
      ofOther <- read.table(text = ofOther, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      if(is.data.frame(ofOther) && nrow(ofOther)!=0){
        if (ofOther$other_off_farm == "true"){
          ofOther$other_off_farm  <- "Other Off Farm"
        }
        # Rename a column in R
        colnames(ofOther)[colnames(ofOther)=="other_off_farm"] <- "off_Farm"
        
        
        #Merge The Three of tables
        of <- ofMicrofranchise
        of <- merge(x=of, y = ofBYOB, by = c("region","zone","woreda","kebele","vesa","psnp_number","report_date","beneficiary_status","off_Farm"), all.x = TRUE, all.y = TRUE)
        of <- unique(of)
        
        of <- merge(x=of, y = ofOther, by = c("region","zone","woreda","kebele","vesa","psnp_number","report_date","beneficiary_status","off_Farm"), all.x = TRUE, all.y = TRUE)
        of <- unique(of)
        
        if(is.data.frame(of) && nrow(of)!=0){
          of$beneficiary_status[of$beneficiary_status == "MHH"] <- "ADULT"
          of$beneficiary_status[of$beneficiary_status == "FHH"] <- "ADULT"
          of$beneficiary_status[of$beneficiary_status == "Dual"] <- "ADULT"
          of$beneficiary_status[of$beneficiary_status == "SPOUSE"] <- "ADULT"
        }
        
        of <- merge(x = of, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        of <- unique(of)
        of <- merge(x = of, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        of <- unique(of)
      }
      #print(of, quote = TRUE, row.names = FALSE)
    }
    else
    {
      #of data
      of2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/JnITxvfuLPz/data.csv?var=period1:",input$monthslide9[1],"&var=period2:",input$monthslide9[2]), userpwd=userpass, httpauth = 1L)
      
      of2 <- read.table(text = of2, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      of2 <- merge(x = of2, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      of2 <- unique(of2)
      of2 <- merge(x = of2, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      of2 <- unique(of2)
      
      #Removes the unrequired columns/variables vesa_training and report date
      of2 <- select(of2, -off_farm)
      of2 <- select(of2, -report_date)
      
      #All PSNP numbers
      allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
      allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
      allPSNPnumbers <- unique(allPSNPnumbers)
      
      #selects all records PSNP numbers that are not present in the 
      of <- subset(allPSNPnumbers, !(psnp_number %in% of2$psnp_number))
      
    }
    return(of)
  })
  
  #Other Livelihood Pathways
  lpData <- reactive({
    
    lp <- data.frame(Date=as.Date(character()),
                     File=character(), 
                     User=character(), 
                     stringsAsFactors=FALSE) 
    
    #Gets HH type information from database
    hhTypeQtr <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/QiPM5jhPlzh/data.csv?var=period1:",input$monthslide10[1],"&var=period2:",input$monthslide10[2]), userpwd=userpass, httpauth = 1L)
    hhTypeQtr <- read.table(text = hhTypeQtr, sep =",", header = TRUE, stringsAsFactors = FALSE)
    
    #Gets a list lp lp VESA, PSNP and lpf Farm
    
    if(input$lpselect == "participated"){
      #Fetch lp vocational_training data
      lpVocationalTraining <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/IuLuk4Ls5FC/data.csv?var=period1:",input$monthslide10[1],"&var=period2:",input$monthslide10[2]), userpwd=userpass, httpauth = 1L)
      
      lpVocationalTraining <- read.table(text = lpVocationalTraining, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      if(is.data.frame(lpVocationalTraining) && nrow(lpVocationalTraining)!=0){
        if (lpVocationalTraining$vocational_training == "true"){
          lpVocationalTraining$vocational_training <- "Vocational Training"
        }
        # Rename a column in R
        colnames(lpVocationalTraining)[colnames(lpVocationalTraining)=="vocational_training"] <- "livelihood_activity"
      }
      #Fetch lp wrn Plan Data
      lpWRN <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/iYiaYENmSCp/data.csv?var=period1:",input$monthslide10[1],"&var=period2:",input$monthslide10[2]), userpwd=userpass, httpauth = 1L)
      
      lpWRN <- read.table(text = lpWRN, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      if(is.data.frame(lpWRN) && nrow(lpWRN)!=0){
        if (lpWRN$wrn == "true"){
          lpWRN$wrn  <- "WRN skills training"
        }
        # Rename a column in R
        colnames(lpWRN)[colnames(lpWRN)=="wrn"] <- "livelihood_activity"
        
        
        
        #Merge The Three lp tables
        lp <- lpVocationalTraining
        lp <- merge(x=lp, y = lpWRN, by = c("region","zone","woreda","kebele","vesa","psnp_number","report_date","beneficiary_status","livelihood_activity"), all.x = TRUE, all.y = TRUE)
        lp <- unique(lp)
        
        if(is.data.frame(lp) && nrow(lp)!=0){
          lp$beneficiary_status[lp$beneficiary_status == "MHH"] <- "ADULT"
          lp$beneficiary_status[lp$beneficiary_status == "FHH"] <- "ADULT"
          lp$beneficiary_status[lp$beneficiary_status == "Dual"] <- "ADULT"
          lp$beneficiary_status[lp$beneficiary_status == "SPOUSE"] <- "ADULT"
        }
        
        lp <- merge(x = lp, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        lp <- unique(lp)
        lp <- merge(x = lp, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        lp <- unique(lp)
        
        #print(lp, quote = TRUE, row.names = FALSE)
      }}
    else
    {
      #Fetch lp vocational_training data
      lpVocationalTraining2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/IuLuk4Ls5FC/data.csv?var=period1:",input$monthslide10[1],"&var=period2:",input$monthslide10[2]), userpwd=userpass, httpauth = 1L)
      
      lpVocationalTraining2 <- read.table(text = lpVocationalTraining2, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      if(is.data.frame(lpVocationalTraining2) && nrow(lpVocationalTraining2)!=0){
        if (lpVocationalTraining2$vocational_training == "true"){
          lpVocationalTraining2$vocational_training <- "vocational_training"
        }
        # Rename a column in R
        colnames(lpVocationalTraining2)[colnames(lpVocationalTraining2)=="vocational_training"] <- "Livelihood_Pathway_Activities"
      }
      #Fetch lp wrn Plan Data
      lpWRN2 <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/iYiaYENmSCp/data.csv?var=period1:",input$monthslide10[1],"&var=period2:",input$monthslide10[2]), userpwd=userpass, httpauth = 1L)
      
      lpWRN2 <- read.table(text = lpWRN2, sep =",", header = TRUE, stringsAsFactors = FALSE)
      
      if(is.data.frame(lpWRN2) && nrow(lpWRN2)!=0){
        if (lpWRN2$wrn == "true"){
          lpWRN2$wrn  <- "WRN skills training"
        }
        # Rename a column in R
        colnames(lpWRN2)[colnames(lpWRN2)=="wrn"] <- "Livelihood_Pathway_Activities"
        
        
        #Merge The Three lp tables
        lp2 <- lpVocationalTraining2
        lp2 <- merge(x=lp2, y = lpWRN2, by = c("region","zone","woreda","kebele","vesa","psnp_number","report_date","beneficiary_status","Livelihood_Pathway_Activities"), all.x = TRUE, all.y = TRUE)
        lp2 <- unique(lp2)
        
        if(is.data.frame(lp2) && nrow(lp2)!=0){
          lp2$beneficiary_status[lp2$beneficiary_status == "MHH"] <- "ADULT"
          lp2$beneficiary_status[lp2$beneficiary_status == "FHH"] <- "ADULT"
          lp2$beneficiary_status[lp2$beneficiary_status == "Dual"] <- "ADULT"
          lp2$beneficiary_status[lp2$beneficiary_status == "SPOUSE"] <- "ADULT"
        }
        
        lp2 <- merge(x = lp2, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        lp2 <- unique(lp2)
        lp2 <- merge(x = lp2, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        lp2 <- unique(lp2)
        
        
        #Removes the unrequired columns/variables vesa_training and report date
        lp2 <- select(lp2, -Livelihood_Pathway_Activities)
        lp2 <- select(lp2, -report_date)
        
        #All PSNP numbers
        allPSNPnumbers <- getURL(paste0("https://dev.grad2mis.com/api/26/sqlViews/oWQYVUf32Ma/data.csv"), userpwd=userpass, httpauth = 1L)
        allPSNPnumbers <- read.table(text = allPSNPnumbers, sep =",", header = TRUE, stringsAsFactors = FALSE)
        allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhTypeQtr, by = "psnp_number", all.x =TRUE)
        allPSNPnumbers <- unique(allPSNPnumbers)
        allPSNPnumbers <- merge(x = allPSNPnumbers, y = hhGraduation, by = "psnp_number", all.x =TRUE)
        allPSNPnumbers <- unique(allPSNPnumbers)
        
        #selects all records PSNP numbers that are not present in the 
        lp <- subset(allPSNPnumbers, !(psnp_number %in% lp2$psnp_number))
        
      }}
    return(lp)
  })
  
  #Linking VESA Household Data to Pivot table
  output$pivot_vesa_hhQtr <- renderRpivotTable({
    
    rpivotTable(vesaHHData(),
                rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                cols=c("household_type"),width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData0', 
                                            document.getElementById('pivot_vesa_hhQtr').innerHTML); 
  }"))

})
  
  #VESA HH the filtered saving data
  summarydfvesahhQtr <- eventReactive(input$myData0,{
    input$myData0 %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  
  #Download filtered data for VESA HH
  output$download_vesahhqtr_filtered <- 
    downloadHandler(
      filename = "Vesa Household.xlsx",
      content = function(file){
        write.xlsx(summarydfvesahhQtr(), file, sheetName = "VESA Household", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
      }
    )
  
  
  #Linking Saving Quarter Data to Pivot table
  output$pivot_vesa_savingQtr <- renderRpivotTable({
    if(input$save2 == "saved"){
      rpivotTable(savingDataQuarter(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  cols=c("household_type"),width="100%", height="500px", vals = "saving", aggregatorName = "Sum",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData4', 
                                              document.getElementById('pivot_vesa_savingQtr').innerHTML); 
    }"))}
    else{
      rpivotTable(savingDataQuarter(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData4', 
                                              document.getElementById('pivot_vesa_savingQtr').innerHTML); 
  }"))
    }
    
  })
  
  #Saves the filtered saving data
  summarydfsavingQtr <- eventReactive(input$myData4,{
    input$myData4 %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  
  
  #Linking Loan Data to Pivot table
  output$pivot_vesa_loanQtr <- renderRpivotTable({
    if(input$loan2 == "received loan"){
      rpivotTable(loanDataQuarter(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  cols=c("household_type"),width="100%", height="500px", vals = "loan", aggregatorName = "Sum",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData5', 
                                              document.getElementById('pivot_vesa_loanQtr').innerHTML); 
    }"))}
    else{
      rpivotTable(loanDataQuarter(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData5', 
                                              document.getElementById('pivot_vesa_loanQtr').innerHTML); 
  }"))
    }
    
  })
  
  #Saves the filtered saving data
  summarydfloanQtr <- eventReactive(input$myData5,{
    input$myData5 %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  
  
  #Download handlers for Saving and Loan
  #For Saving
  output$download_savingqtr_filtered <- 
    downloadHandler(
      filename = "Vesa Saving Periodically.xlsx",
      content = function(file){
        write.xlsx(summarydfsavingQtr(), file, sheetName = "VESA Saving", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  #Download for Loan
  output$download_loanqtr_filtered <- 
    downloadHandler(
      filename = "Vesa Loan Quarterly.xlsx",
      content = function(file){
        write.xlsx(summarydfloanQtr(), file, sheetName = "VESA Loans", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  #VESA Training Server side conf
  output$download_trainingQtr <- 
    downloadHandler(
      filename = "Vesa Training Data.xlsx",
      content = function(file){
        write.xlsx(summarydfTrainingQtr(), file, sheetName = "VESA Training", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$pivot_vesa_trainingQtr <- renderRpivotTable({
    if(input$training2 == "participated"){ 
      rpivotTable(trainingDataQuarter(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  cols=c("vesa_training"), width="100%", height="500px", 
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData2', 
                                              document.getElementById('pivot_vesa_trainingQtr').innerHTML); 
    }"))}
    else{
      rpivotTable(trainingDataQuarter(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData2', 
                                              document.getElementById('pivot_vesa_trainingQtr').innerHTML); 
  }"))
    }
    
    
  })
  
  summarydfTrainingQtr <- eventReactive(input$myData2,{
    input$myData2 %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  
  #VESA Discussion Server side conf
  output$download_discussionQtr <- 
    downloadHandler(
      filename = "Vesa Discussion Data.xlsx",
      content = function(file){
        write.xlsx(summarydfDiscussionQtr(), file, sheetName = "VESA Discussion", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$pivot_vesa_discussionQtr <- renderRpivotTable({
    if(input$discussion2 == "participated"){ 
      rpivotTable(discussionDataQuarter(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  cols=c("vesa_discussion"), width="100%", height="500px", vals = "vesa_discussion", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData6', 
                                              document.getElementById('pivot_vesa_discussionQtr').innerHTML);}"))}
    else{
      rpivotTable(discussionDataQuarter(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData6', 
                                              document.getElementById('pivot_vesa_discussionQtr').innerHTML); 
    }"))
    }
    
  })
  
  summarydfDiscussionQtr <- eventReactive(input$myData6,{
    input$myData6 %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  
  #VESA Completed discussions
  output$discussion_dtq = DT::renderDataTable(
    CompletedDiscussionData(),
    filter = 'top',
    options = list(scrollX = TRUE)
  )
  
  output$discussionqtr_filtered_row <- 
    renderText({
      paste0(length(input$discussion_dtq_rows_all)," Households Completed VESA Discussions")})
  
  
  #FFS Loan Server side conf
  output$download_ffsloan <-
    downloadHandler(
      filename = "FFS Loan Data.xlsx",
      content = function(file){
        write.xlsx(summarydfffsloan(), file, sheetName = "Loans", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$pivot_ffsloan <- renderRpivotTable({
    
    if(input$loantype == "MFI"){  
      if(input$ffsloan == "participated"){ 
        rpivotTable(ffsloanData(),
                    rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                    cols=c("household_type"),width="100%", height="500px", vals = "mfi_loan", aggregatorName = "Sum",
                    onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData7', 
                                                document.getElementById('pivot_ffsloan').innerHTML);}"))}
      else{
        rpivotTable(ffsloanData(),
                    rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                    width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                    onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData7', 
                                                document.getElementById('pivot_ffsloan').innerHTML); 
      }"))
    }}
    else{
      if(input$ffsloan == "participated"){ 
        rpivotTable(ffsloanData(),
                    rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                    cols=c("household_type"),width="100%", height="500px", vals = "rusacco_loan", aggregatorName = "Sum",
                    onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData7', 
                                                document.getElementById('pivot_ffsloan').innerHTML);}"))}
      else{
        rpivotTable(ffsloanData(),
                    rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                    width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                    onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData7', 
                                                document.getElementById('pivot_ffsloan').innerHTML); 
      }"))
    }
    }
    
    })
  
  summarydfffsloan <- eventReactive(input$myData7,{
    input$myData7 %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  
  
  #FFS Saving Server side conf
  output$download_ffssave <-
    downloadHandler(
      filename = "FFS Saving Data.xlsx",
      content = function(file){
        write.xlsx(summarydfffssave(), file, sheetName = "Saving", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$pivot_ffssave <- renderRpivotTable({
    
    if(input$savetype == "MFI"){  
      if(input$ffssave == "participated"){ 
        rpivotTable(ffsSaveData(),
                    rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                    cols=c("household_type"),width="100%", height="500px", vals = "saving", aggregatorName = "Sum",
                    onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData8', 
                                                document.getElementById('pivot_ffssave').innerHTML);}"))}
      else{
        rpivotTable(ffsSaveData(),
                    rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                    width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                    onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData8', 
                                                document.getElementById('pivot_ffssave').innerHTML); 
      }"))
      }}
    else{
      if(input$ffssave == "participated"){ 
        rpivotTable(ffsSaveData(),
                    rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                    cols=c("household_type"),width="100%", height="500px", vals = "saving", aggregatorName = "Sum",
                    onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData8', 
                                                document.getElementById('pivot_ffssave').innerHTML);}"))}
      else{
        rpivotTable(ffsSaveData(),
                    rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                    width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                    onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData8', 
                                                document.getElementById('pivot_ffssave').innerHTML); 
      }"))
      }
    }
    
    })
  
  summarydfffssave<- eventReactive(input$myData8,{
    input$myData8 %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  
  #Value Chains Server side conf
  output$download_vc <- 
    downloadHandler(
      filename = "Value Chains.xlsx",
      content = function(file){
        write.xlsx(summarydfvc(), file, sheetName = "Value Chains", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$pivot_vc <- renderRpivotTable({
    if(input$vcselect == "participated"){ 
      rpivotTable(vcData(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  cols=c("vc"), width="100%", height="500px", vals = "value_chain", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData9', 
                                              document.getElementById('pivot_vc').innerHTML);}"))}
    else{
      rpivotTable(vcData(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData9', 
                                              document.getElementById('pivot_vc').innerHTML); 
    }"))
    }
    
  })
  
  summarydfvc <- eventReactive(input$myData9,{
    input$myData9 %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  
  #Wage Employment server side conf
  output$download_wage <- 
    downloadHandler(
      filename = "Wage Employment Data.xlsx",
      content = function(file){
        write.xlsx(summarydfWage(), file, sheetName = "Wage Employment", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$pivot_vesa_wage <- renderRpivotTable({
    if(input$wage2 == "linked"){ 
      rpivotTable(WageDataQuarter(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  cols=c("household_type"), width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData10', 
                                              document.getElementById('pivot_vesa_wage').innerHTML); 
    }"))}
    else{
      rpivotTable(WageDataQuarter(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData10', 
                                              document.getElementById('pivot_vesa_wage').innerHTML); 
  }"))
    }
    
    
    })
  
  summarydfWage <- eventReactive(input$myData10,{
    input$myData10 %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  
  #Off Farm server side conf
  output$download_offfarm <- 
    downloadHandler(
      filename = "Off farm Data.xlsx",
      content = function(file){
        write.xlsx(summarydfOffFarm(), file, sheetName = "Off Farm", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$pivot_offfarm<- renderRpivotTable({
    if(input$ofselect == "participated"){ 
      rpivotTable(ofData(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  cols=c("household_type"), width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData11', 
                                              document.getElementById('pivot_offfarm').innerHTML); 
    }"))}
    else{
      rpivotTable(ofData(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData11', 
                                              document.getElementById('pivot_offfarm').innerHTML); 
  }"))
    }
    
    
  })
  
  summarydfOffFarm <- eventReactive(input$myData11,{
    input$myData11 %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  
  #Other Livelihood Pathways server side conf
  output$download_lp <- 
    downloadHandler(
      filename = "Other Livelihood Pathways Data.xlsx",
      content = function(file){
        write.xlsx(summarydfLp(), file, sheetName = "Livelihood Pathways", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$pivot_lp<- renderRpivotTable({
    if(input$lpselect == "participated"){ 
      rpivotTable(lpData(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  cols=c("household_type"), width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData12', 
                                              document.getElementById('pivot_lp').innerHTML); 
    }"))}
    else{
      rpivotTable(lpData(),
                  rows=c("region","zone","woreda","kebele","vesa","psnp_number"), 
                  width="100%", height="500px", vals = "psnp_number", aggregatorName = "Count Unique Values",
                  onRefresh = htmlwidgets::JS("function(config) {Shiny.onInputChange('myData12', 
                                              document.getElementById('pivot_lp').innerHTML); 
  }"))
    }
  })
  summarydfLp <- eventReactive(input$myData12,{
    input$myData12 %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  }

shinyApp(ui, server)