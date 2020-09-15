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
                    fluidRow(width=12,column(width = 4,selectInput("lab_selection", "Select lab", choices = "", width = "300px")),column(width = 4,textOutput("periode",inline = TRUE)),column(width = 4,fluidRow(column(width = 6,htmlOutput("line_same",inline = TRUE)),fluidRow(column(width = 6,htmlOutput("line_dif",inline = TRUE)))))),
                    fluidRow(width=12, dataTableOutput("table_result"))

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
    df_flatten<-function(res){
        res# data tble 
        #fonction qui permet le bonne affichage des tables dans l'interface  
        res_f=res
        #date_name="date"
        
        for(i in names(res_f)){# on parcourt les colonne 
            ind=which(is.null(res_f[,i]))
            res_f[ind,i]=NA
            if(length(unlist(res_f[,i]))>length(res_f[,i])){
                res_f[,i]=sapply(1:length(res_f[,i]),FUN = function(x) paste(res_f[,i][[x]],collapse = ";"))# si il y a plusieur element sur une m?me ligne 
            }
            ind=which(is.na(res_f[,i]))
            res_f[ind,i]="NULL"#na ne peu pas ?tre afficher dans la table donc on le remplace par "null" 
            if(typeof(i)=="list") res_f[,i]<-unlist(res_f[,i]) #si c'est une liste on l'enl'f
            #if(date_name!="") res_f[date_name]=as.character(res_f[date_name])
        }
        
        return(as.data.frame(res_f,stringsAsFactors = FALSE))
    }
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
    updateSelectInput(session, inputId = "lab_selection", selected =unique(tolower(reactive_values[[paste0("data",input$periode_to_show)]]$V1))[1] ,choices = unique(tolower(reactive_values[[paste0("data",input$periode_to_show)]]$V1)))
    print(dim(reactive_values$data3))
    precedant=reactive_values[[paste0("data",input$periode_to_show)]]$V1[1]
    for(i in 2:dim(reactive_values[[paste0("data",input$periode_to_show)]])[1]){
        
        if(reactive_values[[paste0("data",input$periode_to_show)]]$V1[i]==""){
            reactive_values[[paste0("data",input$periode_to_show)]]$V1[i]=precedant
            # print(precedant)
        }
        else {
            precedant=reactive_values[[paste0("data",input$periode_to_show)]]$V1[i]}
        
    }
    output$periode<-renderText({paste("Periode",reactive_values[[paste0("stat",input$periode_to_show)]][1,"Debut"],":",reactive_values[[paste0("stat",input$periode_to_show)]][1,"Fin"])})
    
})
observeEvent(input$lab_selection,{
    ind=which(tolower(reactive_values[[paste0("data",input$periode_to_show)]]$V1)==input$lab_selection)
    ind2=which(tolower(reactive_values[[paste0("stat",input$periode_to_show)]]$V1)==input$lab_selection)
    
    
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
    
})




}
# Run the application 
shinyApp(ui = ui, server = server)
