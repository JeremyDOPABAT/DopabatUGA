#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",
                    #menu
                    dashboardHeader(title = ""),
                    dashboardSidebar( sidebarMenu(
                        
                        menuItem("table", tabName = "table", icon = icon(name = "arrow-circle-up")),
                        #menuItem("Import_PDF", tabName = "import_pdf", icon = icon("file-pdf")),
                        #si le probleme est  regle decommenter laligne du dessous 
                        
                        menuItem("About us", tabName = "about", icon = icon("address-card"))
                        
                        #-------------------------------------------------------------------  
                        
                        #-------------------------------------------------------------------  
                    )),
    dashboardBody(
        tabItems(
                tabItem(tabName = "table",
                fluidPage(
                    fluidRow(width=12,radioButtons("periode_to_show",label = "Choose a periode",choices = c(Periode1="1",Periode2="2",Periode3="3"),selected = "1" ,inline = TRUE)),
                    fluidRow(width=12,column(width = 4,selectInput("lab_selection", "Select lab", choices = "", width = "300px")),column(width = 4,textOutput("periode",inline = TRUE)),column(width = 4,fluidRow(column(width = 4,htmlOutput("line_same",inline = TRUE)),column(width = 4,htmlOutput("line_dif",inline = TRUE)),column(width=4,htmlOutput("taux_diff",inline = TRUE))))),
                    fluidRow(width=12, dataTableOutput("table_result")),
                    downloadButton("downloadData", "Download the table")

                )
            ),
            tabItem(tabName = "about",mainPanel(
                titlePanel("A Propos"),
                htmlOutput("text"))
            )
                
        )
    )
)


server <- function(input, output, session) {
    
    output$text <- renderText({
    "Les adresses sont celles presentes dans les publications referencees dans le Web of Science sur la periode de reference qui est mentionnee lors de la visualisation.

L'ensemble des lignes d'adresses du perimetre univ grenoble alpes sont analysees au sein d'une meme publication.

La recherche des adresses du perimetre sont obtenues par la recherche du nom de laboratoire et du lieu geographique. Pour la plupart des laboratoires, differentes variations de noms deja utilises sont prises en compte.
"
})
    #---------------------------------- fonction de mise en forme -----------------------------
    
    reactive_values <- reactiveValues(
        data1 = as.data.frame(data.table::fread("WWW/table1.csv",
                                                                     header = TRUE,
                                                                     sep = ";"),stringsAsFactors = FALSE),
        stat1=as.data.frame(data.table::fread("WWW/stat1.csv",
                                             header = TRUE,
                                             sep = ";"),stringsAsFactors = FALSE),
        data2 = as.data.frame(data.table::fread("WWW/table2.csv",
                                                header = TRUE,
                                                sep = ";"),stringsAsFactors = FALSE),
        stat2=as.data.frame(data.table::fread("WWW/stat2.csv",
                                              header = TRUE,
                                              sep = ";"),stringsAsFactors = FALSE),
        data3 = as.data.frame(data.table::fread("WWW/table3.csv",
                                                header = TRUE,
                                                sep = ";"),stringsAsFactors = FALSE),
        stat3=as.data.frame(data.table::fread("WWW/stat3.csv",
                                              header = TRUE,
                                              sep = ";"),stringsAsFactors = FALSE)
        
        
    )
    
    
observeEvent(input$periode_to_show,{
    updateSelectInput(session, inputId = "lab_selection", selected =unique(tolower(reactive_values[[paste0("data",input$periode_to_show)]]$Unite))[1] ,choices = unique(tolower(reactive_values[[paste0("data",input$periode_to_show)]]$Unite)))
    print(dim(reactive_values$data3))
    precedant=reactive_values[[paste0("data",input$periode_to_show)]]$Unite[1]
    for(i in 2:dim(reactive_values[[paste0("data",input$periode_to_show)]])[1]){
        
        if(reactive_values[[paste0("data",input$periode_to_show)]]$Unite[i]==""){
            reactive_values[[paste0("data",input$periode_to_show)]]$Unite[i]=precedant
            # print(precedant)
        }
        else {
            precedant=reactive_values[[paste0("data",input$periode_to_show)]]$Unite[i]}
        
    }
    output$periode<-renderText({paste("Periode",reactive_values[[paste0("stat",input$periode_to_show)]][1,"Debut"],":",reactive_values[[paste0("stat",input$periode_to_show)]][1,"Fin"])})
    
})
observeEvent(input$lab_selection,{
    ind=which(tolower(reactive_values[[paste0("data",input$periode_to_show)]]$Unite)==input$lab_selection)
    ind2=which(tolower(reactive_values[[paste0("stat",input$periode_to_show)]]$Unite)==input$lab_selection)
    
    
    output$table_result <- renderDataTable({
        #test_table<-reactive_values$df_pdf
        table_data=datatable((reactive_values[[paste0("data",input$periode_to_show)]][ind,]), options = list(sDom  = '<"top">lrt<"bottom">ip',scrollX = TRUE, columnDefs = list(list(
            targets = "_all" ,render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 70 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                "}")
        ))))
    })
    
    output$line_same<-renderText({(paste( "<b>Lignes adresse:</b>",reactive_values[[paste0("stat",input$periode_to_show)]][ind2,2]))})
    output$line_dif<-renderText({(paste("<b>Lignes non UGA:</b>",reactive_values[[paste0("stat",input$periode_to_show)]][ind2,3]))})
    output$taux_diff<-renderText({(paste("<b>taux d'erreur:</b>",reactive_values[[paste0("stat",input$periode_to_show)]][ind2,4]))})
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$lab_selection,"period",input$periode_to_show, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(as.data.frame(reactive_values[[paste0("data",input$periode_to_show)]][ind,]), file, row.names = FALSE)
        }
    )
    View(as.data.frame(reactive_values[[paste0("data",input$periode_to_show)]][ind,]))

})




}
# Run the application 
shinyApp(ui = ui, server = server)
