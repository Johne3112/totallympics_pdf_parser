#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tabulizer)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Totallympics Extract Results from FIS-Ski PDF Files"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("results")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    pdf_file <- "D:/OneDrive - Blue Cat Technical Ltd/Sports Data Analysis/totallympics/2019AL0088RLR1.pdf"

    output$results <- renderTable({
        
        out <- extract_areas(pdf_file,widget = "reduced")
        
        final <- do.call(rbind, out[])
        final[1,] <- paste(final[1,],final[2,],final[3,],sep = " ")
        final[1,] <- trimws(final[1,])
        
        final <- final[-c(2:3),]
        colnames(final) <- as.character(unlist(final[1,]))
        final <- final[-1,]
        final <- as.data.frame(final)
        final <- final %>% dplyr::mutate_all(as.character)
        
        final$Rank <- as.numeric(final$Rank)
        final <- final %>% dplyr::filter(!is.na(Rank))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
