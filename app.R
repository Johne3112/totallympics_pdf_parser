###Shiny App to Parse PDF Results files for the Totallympics website###

library(shiny)
library(rvest)
library(tidyverse)
library(tabulizer)
library(shinyjs)
library(shinydashboard)

ui <- dashboardPage(
    
    # Application title
    dashboardHeader(title = "Totallympics Results Scraper"),
    dashboardSidebar(collapsed = FALSE,
                     textInput("url", label = "FIS Results URL:"),
                     hr(),
                     selectInput("pdf_sport", label = "PDF Sport:", choices = c("Diving","Swimming")),
                     fileInput("pdf_file", "Choose PDF Results File",
                               accept = c(".pdf")
                     )                     
    ),
    dashboardBody(
        
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),

        # must turn shinyjs on
        shinyjs::useShinyjs(), 

        tabsetPanel(
            tabPanel("FIS HTML Results",
                     DT::dataTableOutput("fisres")
            ),
            tabPanel("PDF Results",
                     hidden(
                         div(id = "pdf_parse",
                             h1("Parsing PDF File...")
                         )
                     ),
                     DT::dataTableOutput("pdfres")
            )
        )
        
    )
)

server <- function(input, output,session) {

    getFISRes <- reactive({
        
        url <- input$url
        
        html <- read_html(url)
        
        heads <- html %>% html_nodes(".thead")
        heads <- heads %>% html_nodes(".g-row") %>% html_text()
        
        for(headrow in heads){
            
            headrow <- as.character(headrow)
            headrow <- strsplit(headrow,"\n")
            headrow <- trimws(headrow[[1]])
            headrow <- as.data.frame(headrow)
            headrow <- headrow %>% dplyr::filter(headrow != "")
            headrow$headrow <- as.character(headrow$headrow)
            if(headrow$headrow[1] == "Rank"){
                break
            }
            
        }
        
        temp <- html %>% html_nodes(".justify-sb") %>% html_text()
        
        bfirst <- TRUE
        
        for(myrow in temp){
            
            myrow <- as.character(myrow)
            myrow <- strsplit(myrow,"\n")
            myrow <- trimws(myrow[[1]])
            myrow <- as.data.frame(myrow)
            myrow <- myrow %>% dplyr::filter(myrow != "")
            myrow$myrow <- as.character(myrow$myrow)
            
            if(nrow(myrow) == nrow(headrow)){
                myrow <- cbind(headrow,myrow)
                colnames(myrow)[2] <- "val"
                myrow <- myrow %>% tidyr::spread(headrow,val)
                
                if(bfirst == TRUE){
                    dfout <- myrow
                    bfirst <- FALSE
                } else {
                    dfout <- rbind(dfout,myrow)
                }
            }
            
        }
        
        dfout <- dfout[,headrow$headrow]
        dfout <- dfout %>% dplyr::filter(Rank != 'Rank')
        return(dfout)
        
    })
    
    output$fisres <- DT::renderDataTable({
        df <- getFISRes()
        
        DT::datatable(df,extensions = "Buttons", options = list(
            dom = 'Brtip',
            buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
            pageLength = 500),
            rownames = FALSE)
    })
    
   
    getDivingPDF <- function(fn){
        
        out <- extract_tables(fn,guess=TRUE)
        
        dfhead <- as.data.frame(out[[1]])
        dfhead <- dfhead %>% dplyr::filter(V1 == "Rank")
        if(dfhead$V2 == "Name"){
            head <- c("Rank","Name","NAT","Dive No.","Dive Points","Dive Rank","Total Points","Overall Rank","Points Behind")
        } else {
            head <- c("Rank","NAT","Name","Dive No.","Dive Points","Dive Rank","Total Points","Overall Rank","Points Behind")
        }
        
        bfirst <- TRUE
        for(i in 1:length(out)){
            print(i)
            dfres <- as.data.frame(out[[i]])
            dfres <- data.frame(lapply(dfres, as.character), stringsAsFactors=FALSE)
            
            if(ncol(dfres) == 14){
                
                dfres <- dfres[,c(1,10:14)]
                colnames(dfres) <- c("Dive No.","Dive Points","Dive Rank","Total Points","Overall Rank","Points Behind")
                dfres$Rank <- ""
                dfres$Name <- ""
                dfres$NAT <- ""
                
            } else if(ncol(dfres) == 15){
                
                dfres <- dfres[,c(1:2,11:15)]
                colnames(dfres) <- c("Name","Dive No.","Dive Points","Dive Rank","Total Points","Overall Rank","Points Behind")
                dfres$Rank <- ""
                dfres$NAT <- ""
                
            } else if(ncol(dfres) == 19){
                
                dfres <- dfres[,c(1:2,15:19)]
                colnames(dfres) <- c("Name","Dive No.","Dive Points","Dive Rank","Total Points","Overall Rank","Points Behind")
                dfres$Rank <- ""
                dfres$NAT <- ""
                
            } else {
                
                if(is.na(as.numeric(dfres[1,1]))){
                    repeat{
                        dfres <- dfres[-1,]
                        if(!is.na(as.numeric(dfres[1,1]))){
                            break
                        }
                    }
                }
                
                if(ncol(dfres) == 12){
                    dfres <- dfres[,c(1:4,8:12)]
                } else if(ncol(dfres) == 16){
                    dfres <- dfres[,c(1:4,12:16)]
                } else {
                    dfres <- dfres[,c(1:4,13:17)]
                }
                
                colnames(dfres) <- head 
            }
            
            
            if(bfirst == TRUE){
                dfout <- dfres
                bfirst <- FALSE
            } else {
                dfout <- rbind(dfout,dfres)
            }
            
        }
        
        return(dfout)
        
    }
     
    getSwimPDF <- function(fn){
        
        out <- extract_tables(fn,guess=TRUE)
        
        dfhead <- out[[2]]
        dfhead[1,] <- paste(dfhead[1,],dfhead[2,], sep = "")
        
        bfirst <- TRUE
        
        for(i in 3:length(out)){
            
            dfres <- as.data.frame(out[[i]])
            if(dfres[1,1] != "Rank"){
                dfres <- data.frame(lapply(dfres, as.character), stringsAsFactors=FALSE)
                dfres$V1 <- as.numeric(dfres$V1)
                dfres <- dfres %>% dplyr::filter(!(is.na(V1)))
                
                dfres <- dfres[,!(colnames(dfres) == "V5")]
                
                lane <- strsplit(dfres$V3," ")
                lane <- as.numeric(lapply(lane, `[[`, 1))
                dfres$V4 <- substr(dfres$V3,nchar(lane)+1,9999)
                dfres$V3 <- lane
                colnames(dfres) <- dfhead[1,]
                if(ncol(dfres) == 10 ){
                    colnames(dfres)[10] <- "Qual"
                } else {
                    dfres$Qual <- ""
                }
                
                if(bfirst == TRUE) {
                    dfout <- dfres
                    bfirst <- FALSE
                } else {
                    dfout <- rbind(dfout,dfres)
                }

            }

        }
        
        return(dfout)

    }
    
    
    
    getPDFRes <- function(fn,sport){
        
        print(fn)
        
        if(sport == "Diving"){ df <- getDivingPDF(fn)}
        else if(sport == "Swimming"){ df <- getSwimPDF(fn)}
        
        return(df)

    }

    output$pdfres <- DT::renderDataTable({
        
        inFile <- input$pdf_file
        if (is.null(inFile))
            return(NULL)
        
        shinyjs::show(id = "pdf_parse")

        sport <- input$pdf_sport
        
        df <- getPDFRes(inFile$datapath,sport)

        shinyjs::hide(id = "pdf_parse")
        
        DT::datatable(df,extensions = "Buttons", options = list(
            dom = 'Brtip',
            buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
            pageLength = 500),
            rownames = FALSE)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
