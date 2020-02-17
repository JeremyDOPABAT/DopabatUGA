#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
##reset("title_selection")
setwd("~/R_programs/interface_shiny/test")
setwd('..')
setwd('..')
getwd()
source("functions_analyses.R")


library(shiny)
library(shinydashboard)
library(shinyjs)
library(lubridate)
#library(ggplot2)

library(shinycssloaders)
library(DT)
# a faire, combler les partie, finir les lien bd ,sincro pdf et autre 
# Define UI for data upload app ----
ui <-dashboardPage(skin = "red",
    
    dashboardHeader(title = "DOBABAT"),
    dashboardSidebar( sidebarMenu(
        menuItem("Import_csv", tabName = "import_csv", icon = icon(name = "arrow-circle-up")),
        menuItem("Import_PDF", tabName = "import_pdf", icon = icon("file-pdf")),
        menuItem("Import_WOS", tabName = "import_wos", icon = icon("file-export")),
        menuItem("Worcloud Graphics", tabName = "wordcloud", icon = icon("th")),
        menuItem("Notwork Graphics", tabName = "network", icon = icon("project-diagram")),
        menuItem("interdiciplinarity", tabName = "DB", icon = icon("database")),
        menuItem("About us", tabName = "about", icon = icon("address-card"))
        
    )),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "import_csv",
                    
                    # Sidebar layout with input and output definitions ----
                   fluidRow(
                        
                           box(width = 4, title = "Uploading files", solidHeader = TRUE,status = "danger",
                            
                            # Input: Select a file ----
                            fileInput("file1", "Choose CSV File",
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Checkbox if file has header ----
                            checkboxInput("header", "Header", TRUE),
                            
                            # Input: Select separator ----
                            radioButtons("sep", "Separator",
                                         choices = c(Semicolon = ";",
                                                     Comma = ",",
                                                     Tab = "\t"),
                                         selected = ""),
                            
                            # Input: Select quotes ----
                            radioButtons("quote", "Quote",
                                         choices = c("Double Quote" = '"',
                                                     "Single Quote" = "'"),
                                         selected = NA),
                            
                            # Input: Select number of rows to display ----
                            radioButtons("disp", "Display",
                                         choices = c(Head = "head",
                                                     All = "all"),
                                         selected = "head"),
                            
                            tags$div(title="the order of the author need to be told, the surname don't need to be complete abreviation works",radioButtons("position_name_CSV", "disposition of name and surname",
                                         choices = c("Name Surname" = "2",
                                                     "Surname Name" = "1"),
                                         selected = "")),
                            
                            
                            # checkboxInput("contain_ref", "This file contain the reference colonne", FALSE),
                            # checkboxInput("contain_cit", "This file contain the citation colonne", FALSE),
                            # conditionalPanel("output.show_ref",textInput("sep_ref",label = "separator",value =";",width ="30px"  )),
                            conditionalPanel('output.show_header',actionButton("valid_table", "validate"))
                            
                            
                        ),
                        
                    
                   
                        
                        # Main panel for displaying outputs ----
                        column(8,
                            
                            # Output: Data file ----
                            dataTableOutput("contents"),
                            br(),
                            fluidRow(
                               conditionalPanel('output.show_header',
                                                tags$div(title="complete the column selection before analysing the corpus",box(width = 12, title = "Headers selection", solidHeader = TRUE, status = "danger",
                                                    fluidRow( 
                                                       column(width=3, selectInput("title_selection", "Title column", choices = "", width = "300px"),
                                                              selectInput("author_selection", "Author column", choices = "", width = "300px")),
                                                       column(3,offset = 1,selectInput("keyword_selection", "Keywords column", choices = "", width = "300px"),
                                                              selectInput("domain_selection", "Domain column", choices = "", width = "300px")),
                                                       column(3,offset = 1,selectInput("date_selection", "Date publication column", choices = "", width = "300px"))
                                                       #,conditionalPanel('output.show_ref',
                                                                        #column(3,offset = 1,selectInput("ref_selection", "Reference column", choices = "", width = "300px"))
                                                      #),
                                                      # conditionalPanel('output.show_cit',
                                                      #                  column(3,selectInput("cit_selection", "Citation column", choices = "", width = "300px"))
                                                      # )
                                                    )
                                                    
                                                ))
                               )
                               
                            )
                        )
                   )
            ),
            
            # Second tab content
            tabItem(tabName = "import_pdf",
                    fluidRow(
                       
                       box(width = 5, title = "Uploading files", solidHeader = TRUE, status = "danger",
                           tags$div(title="the pdf files in pdf folder will be shown ",actionButton("pdf_path", "Click to select you pdf folder")),
                           tags$hr(),
                           tags$div(title="the order of the author need to be told, the surname don't need to be complete abreviation works, use the data vision if you need",
                                    radioButtons("position_name_pdf", "disposition of name and surname",
                                                                        choices = c("Name Surname" = "2",
                                                                                "Surname Name" = "1"),
                                                                                selected = "")),
                           conditionalPanel('output.show_pdf_valid',actionButton("valid_pdf", "validate"))
                          
                           )
                       ,column(12,
                           mainPanel(
                            
                            # Output: Data file ----
                               dataTableOutput("table_pdf")
                           )
                        )
                    )
                
            ),
            tabItem(tabName = "import_wos",
                    fluidRow(
                      
                      box(width = 5, title = "Uploading WOS files", solidHeader = TRUE, status = "danger",
                          tags$div(title="Choose WOS file, bibtext only",fileInput("file2", "Choose WOS File",
                                    multiple = TRUE,
                                    accept = c(".bib"))),
                          tags$hr(),
                          checkboxInput("sup_wos_for_ref", "this file already countain reference, don't reasearch then"),
                          conditionalPanel('output.show_wos_valid',actionButton("valid_wos", "validate"))
                          #
                      ),
                      mainPanel(
                        
                        # Output: Data file ----
                        withSpinner(dataTableOutput("table_wos"),color="#c50d3e")
                      )
                    )
                      
            ),
            
           
            tabItem(tabName = "wordcloud",
                    titlePanel("Words cloud graphics (soon)"),
                    
                    
                    #wordcloud
                    # Sidebar layout with input and output definitions ----
                    fluidRow( sidebarPanel(
                      selectInput("intervalyear","number of year per graphics",choices = ""),
                       numericInput("maxprint","max number of words on graph(s)",value=20,min=1,max=50,step=1)
                       
                       
                       
                    ),
                    box(width = 4,checkboxInput("Simpleword", "literaly word analyse one by one"),
                        numericInput("minfreq","minimum freqency to be on graph(s)",value=1,min=1,max=50,step=1))
                    
                    
                    
                    
                ),
                fluidRow(
                  uiOutput("plot_wordcloud")%>% withSpinner(color="#0dc5c1"),
                  tags$div(title="Download all the graphics of page in pdf ",downloadButton("downloadPlot", "Download Plot"))
                )
                    
                    
                    
                    
            ),
                #selectInput("title_selection", "Title column", choices = "", width = "300px")
                tabItem(tabName = "network",
                    titlePanel("network graphics (soon)"),
                    fluidRow(column(width=3,   tags$div(title="show only the bigest nodes with their relations ",selectInput("networktop","number of words for top graph",choices =c("None",1:20)))),
                                    column(width=3, tags$div(title="Supress th nodes taken only one time in the graph",checkboxInput("supones", "supress words taken one time only", value = FALSE)),
                                           tags$div(title="Reduce the size of the nodes to root",checkboxInput("root", "passe the weight of nood to roots", value = FALSE))),
                             column(width=3,tags$div(title="One graph per number of year",selectInput("networkintervalyear","number of year per graphics",choices =c("None",1:20)))),
                             column(width=3, tags$div(title="switch to domain network",checkboxInput("domain", "show domain study ", value = FALSE)))
                             
                             
                              #table_infos
                              ),
                    fluidRow(uiOutput("plots")%>% withSpinner(color="#0dc5c1"),
                             dataTableOutput("table_infos")
                             )
                    
                    ),
            tabItem(tabName = "DB",
                    titlePanel("interdiciplinarity cit and ref "),
                    fluidRow(
                      
                      box(width = 4, title = "Chose the databases you want to use", solidHeader = TRUE, status = "danger",
                          
                          
                          checkboxInput("ads", "ADS", FALSE),
                          checkboxInput("pubmed", "Pubmed", FALSE),
                          checkboxInput("arxiv", "ArXiv", FALSE),
                          
                          radioButtons("type", "What data do you want to fetch ?",
                                       choices = c(Citations = "cit",
                                                   References = "ref",
                                                   Both = "all"),
                                       selected = "cit"),
                          
                          actionButton("valid_DB", "Process"),
                          conditionalPanel('output.show_token_ads',textInput("token",label ="Token" ))
                          
                          # Horizontal line ----
                    ),
                    conditionalPanel('output.show_token_ads', tabBox(width = 8, title = "result data tab ",
                           tabPanel("ADS",actionButton("ads_ref_accept", "Show references"),
                        actionButton("ads_cit_accept", "Show citations"),
                        actionButton("ads_error", "Show error(s)"),
                        dataTableOutput("table_data_ref1")),
                        tabPanel("ArXiv",actionButton("arxiv_ref_accept", "Show references"),
                                 actionButton("arxiv_cit_accept", "Show citations"),
                                 actionButton("arxiv_error", "Show error(s)"),
                                 dataTableOutput("table_data_ref2")),
                        tabPanel("Pubmed",actionButton("pubmed_ref_accept", "Show references"),
                                 actionButton("pubmed_cit_accept", "Show citations"),
                                 actionButton("pumed_error", "Show error(s)"),
                                 dataTableOutput("table_data_ref3"))
                        
                      
                      
                    
                    # Horizontal line ----
                    
                    ))
            )
          ),
            
            tabItem(tabName = "about",
                    titlePanel("DOPABAT Project"),
                    fluidPage(
                      # Application title
                     
                      
                      
                      
                      # Show Word Cloud
                      mainPanel(
                        #uiOutput("plot_wordcloud")
                      )
                    )
                )
                
                            # Output: Data file ---
        )
    )
)


# Define server logic to read selected file ----
server <- function(input, output, session) {
  
    options(shiny.maxRequestSize=32*1024^2)
   
    reactive_values <- reactiveValues(
       show_header = FALSE,
       df_csv = NULL,
       df_pdf = NULL,
       df_global=NULL,
       data_wos=NULL,
       show_pdf_valid=FALSE,
       valide_csv=FALSE,
       valide_pdf=FALSE,
       valide_wos=FALSE,
       privious_datapath_csv=NULL,
       privious_datapath_pdf=NULL,
       privious_datapath_wos=NULL,
       path_folder=NULL,
       ok_analyse=FALSE,
       graph=NULL,
       numberwordcloud=1,
       show_token_ads=FALSE,
       show_wos_valid=FALSE,
       show_ref=FALSE,
       show_cit=FALSE,
       fmts=c("%d/%m/%y","%Y", "%Y-%m", "%m/%d/%y","%d-%m-%Y","%Y-%m-%d %I:%M:%S %p","%B %d, %Y","%Y-%m-%d")
    )
    
    output$show_header <- reactive({
       reactive_values$show_header
    })
    output$show_pdf_valid<- reactive({
       reactive_values$show_pdf_valid
    })
    
    output$show_wos_valid<- reactive({
      reactive_values$show_wos_valid
    })
    output$show_token_ads<- reactive({
      reactive_values$show_token_ads
    })
    output$show_ref<- reactive({
      reactive_values$show_ref
    })
    output$show_cit<- reactive({
      reactive_values$show_cit
    })
    outputOptions(output, "show_header", suspendWhenHidden = FALSE)
    outputOptions(output, "show_pdf_valid", suspendWhenHidden = FALSE)
    outputOptions(output, "show_token_ads", suspendWhenHidden = FALSE)
    outputOptions(output, "show_wos_valid", suspendWhenHidden = FALSE)
    outputOptions(output, "show_ref", suspendWhenHidden = FALSE)
    outputOptions(output, "show_cit", suspendWhenHidden = FALSE)
    observeEvent(input$file1, {
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, head of that data file by default,
            # or all rows if selected, will be shown.
            
      observeEvent({
        input$header
        input$sep
        input$quote
        },{
          test_error=tryCatch({
      
            df <- read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote,stringsAsFactors = FALSE)
            
          },
          error=function(cond){
            return(-400)
          }) 
           if(typeof(test_error)=="double"){
             showModal(modalDialog(
               title = "invalid CSV",
               "the csv modality are not good, make sure to use a good separator and quote and retry. If your csv don't fit the modality proposed for now we can't analyse it.",
               easyClose = TRUE,
               footer = NULL
             ))
           }else{
              reactive_values$df_csv <- df
              table_data=datatable(df_flatten(reactive_values$df_csv), options = list(scrollX = TRUE, columnDefs = list(list(
                targets = "_all" ,render = JS(
                  "function(data, type, row, meta) {",
                  "return type === 'display' && data.length > 70 ?",
                  "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                  "}")
              ))))
              
              reactive_values$show_header <- TRUE
              
              updateSelectInput(session, inputId = "title_selection", choices = names(df))
              updateSelectInput(session, inputId = "keyword_selection", choices = names(df))
              updateSelectInput(session, inputId = "author_selection", choices = names(df))
              updateSelectInput(session, inputId = "domain_selection", choices = names(df))
              updateSelectInput(session, inputId = "date_selection", choices = names(df))
              updateSelectInput(session, inputId = "ref_selection", choices = names(df))
              updateSelectInput(session, inputId = "cit_selection", choices = names(df))
              
              
              output$contents <- renderDataTable({
              if(input$disp == "head") {
                  return(datatable(df_flatten(df[1:2,]), options = list(scrollX = TRUE, columnDefs = list(list(
                      targets = "_all",render = JS(
                          "function(data, type, row, meta) {",
                          "return type === 'display' && data.length > 70 ?",
                          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                          "}")
                  )))))
              }
              else {
                  return(table_data)
              }
          })#end render___________________________
      }    
              
    })
            
  })
    observeEvent(
       input$valid_table, {
         print("you clique on validate")
           if(input$file1$datapath %in% reactive_values$privious_datapath_csv){
             showModal(modalDialog(
               title = "Invalid path",
               "This file as been analysed already ",
               easyClose = TRUE,
               footer = NULL
             ))
             reactive_values$ok_analyse=FALSE
           }else{
              reactive_values$privious_datapath_csv=c(reactive_values$privious_datapath_csv,input$file1$datapath)
              reactive_values$ok_analyse=TRUE
           }
         
          
           if(reactive_values$ok_analyse==TRUE){
             print("dans le if" )
             col_key<-reactive_values$df_csv[[input$keyword_selection]]
             col_dom<-reactive_values$df_csv[[input$domain_selection]]
             #col_date<-as.Date(as.POSIXct(strptime(c(reactive_values$df_csv[[input$date_selection]]), "%d/%m/%Y"),origin="1970-01-01"))
             
             col_date<-parse_date_time(x = reactive_values$df_csv[[input$date_selection]],
                                   orders = reactive_values$fmts,
                                   locale = "eng")
             
             
             
             col_title<-reactive_values$df_csv[[input$title_selection]]
             col_auth<-reactive_values$df_csv[[input$author_selection]]
             if(input$position_name_CSV==1) name_position=1 else name_position=2 
             if(is.null(reactive_values$df_global)==TRUE) {
               
                reactive_values$df_global=as.data.frame(cbind(col_title,col_auth,col_key,col_dom,col_date),stringsAsFactors = FALSE)
                names(reactive_values$df_global)<-c('titre','auteur','keywords','domain','date')
                reactive_values$df_global["date"]=col_date
                reactive_values$df_global["source"]="CSV"
                reactive_values$df_global["position_name"]=name_position
                
    
             }else {
                #print("je passe dans le else")
                temp=as.data.frame(cbind(col_title,col_auth,col_key,col_dom,col_date),stringsAsFactors = FALSE)
                names(temp)<-c('titre','auteur','keywords','domain','date')
                temp["date"]=col_date
                temp["source"]="CSV"
                temp["position_name"]=name_position
                reactive_values$df_global=rbind(reactive_values$df_global,temp)
                #reactive_values$df_global["keywords"]=gsub(";",",",reactive_values$df_global["keywords"])
               # print("hors du else")
             }
          reactive_values$df_global[["keywords"]]=gsub(";",",",reactive_values$df_global[["keywords"]])
           reactive_values$valide_csv<-TRUE
           
          
         }
    })
    
    
    observe({ if(reactive_values$valide_csv==TRUE || reactive_values$valide_pdf==TRUE || reactive_values$valide_wos==TRUE){
      if(sum(duplicated(reactive_values$df_global[c("titre","auteur")]))>0) reactive_values$df_global<-reactive_values$df_global[-(which(duplicated(reactive_values$df_global[c("titre","auteur")])==TRUE)),]
      
      
      
       keywords<-reactive_values$df_global[["keywords"]]
       
       
       
       
       #primary_domaine<-reactive_values$df_csv["primaryDomain_s"]
       domainall<-reactive_values$df_global[["domain"]]
       
       dict_lang="en_GB"#on se fixe d'abord en anglais , différent selon dico 
       lang<-"en"
       
       
       
       
      
       
       year<-as.double(format(reactive_values$df_global[["date"]],"%Y"))
       print(table(year))
       
       reactive_values$df_global[["year"]]<-as.factor(year)
       if(max(year,na.rm = TRUE)-min(year,na.rm = TRUE)!=0){
         updateSelectInput(session, inputId = "intervalyear",choices =c("None",1:(max(year,na.rm = TRUE)-min(year,na.rm = TRUE))))
         updateSelectInput(session, inputId = "networkintervalyear",choices =c("None",1:(max(year,na.rm = TRUE)-min(year,na.rm = TRUE))))
       }else{
         updateSelectInput(session, inputId = "intervalyear",choices =c("None",1:1))
         updateSelectInput(session, inputId = "networkintervalyear",choices =c("None",1:1))
       } 
       
       print("laaaa")
       
       
       keywords_tok<-strsplit(keywords,",",fixed=TRUE)
       
       
       keywords_lem<-sapply(1:length(keywords_tok),FUN =function(x,l=lang) {stemDocument(keywords_tok[[x]],language = l)})
       c1<-unique(unlist(keywords_tok))
       
       c2<-sapply(1:length(c1)[1],FUN =function(x,l=lang) {stemDocument(c1[x],language = l)})
       
       
       if(is_empty(which(c2==""))==FALSE){ 
          c2<-c2[-(which(c2==""))]
       }
       
       if(is_empty(which(c1==""))==FALSE){ 
          c1<-c1[-(which(c1==""))]
       }
       
       length(c1)
       length(c2)
       table_lema<-as.data.frame(cbind(c1,c2),stringsAsFactors = FALSE)
       
       keywords_lem_complet<-keywords_lem
       ind=which(keywords_lem_complet=="")
       
       
       if(length(ind)!=0) keywords=keywords_lem_complet[-ind]
       
     
       for(i in (1:length(keywords_lem_complet))){
          mot=keywords_lem_complet[[i]]
          temp=c()
          for(k in 1:length(mot)){
             trad=""
             if(mot[k]!=""){
                ind=which(mot[k]==table_lema[2])
                trad=table_lema[ind,1]
                
                if(length(trad)>1){ 
                   trad<-trad[nchar(trad)==min(nchar(trad))][1]
                }
             }
             temp=c(temp,trad)
          }
          keywords_lem_complet[[i]]<-temp
          
                
       }
       ind=which(is.na(keywords_lem_complet))
       if(length(ind)!=0) keywords_lem_complet[ind]<-NULL
      
       
       reactive_values$valide_csv=FALSE
       reactive_values$valide_pdf=FALSE
       reactive_values$valide_wos=FALSE
       
       #________________________travail sur netwooork ___________________________________________________________________
       
       observeEvent({
         input$networkintervalyear
         input$networktop
         input$supones
         input$root
         input$domain
       },{
        
          if(input$domain==FALSE){
           
            reactive_values$graph=make_network_graph(keywords_lem_complet,year,top_number=input$networktop,interval_year=input$networkintervalyear,sup_ones=input$supones,root_weight = input$root,domain = FALSE)
          }else{
            reactive_values$graph=make_network_graph(domainall,year,top_number=input$networktop,interval_year=input$networkintervalyear,sup_ones=input$supones,root_weight = input$root,domain = TRUE)
         }
         
         output$plots <- renderUI({
           plot_output_list <- lapply(1:length(reactive_values$graph), function(i) {
             plotname <- paste("plot", i, sep="")
             visNetworkOutput(plotname, height = "400px")
             #
           })

           # Convert the list to a tagList - this is necessary for the list of items
           # to display properly.
           do.call(tagList, plot_output_list)
         })
         

         print("je suis vers le for")
         for (i in 1:length(reactive_values$graph)) {
           
           # Need local so that each item gets its own number. Without it, the value
           # of i in the renderPlot() will be the same across all instances, because
           # of when the expression is evaluated.
           local({
             my_i <- i
             plotname <- paste("plot", my_i, sep="")
             
             suppressWarnings(output[[plotname]] <- renderVisNetwork({
               if(my_i<=length(reactive_values$graph)){
                 reactive_values$graph[[my_i]]%>%
                   visEvents(select = "function(nodes) {
                    Shiny.onInputChange('current_node_id', nodes.nodes);
                    ;}")
               }
              }))
             
             
           })
           
         }
         
        
         observeEvent(input$current_node_id, {
           
           #print(reactive_values$df_global["keywords"])
           output$table_infos <- renderDataTable({
             if (!is.null(input$current_node_id)){
               if(!input$domain){
                  #ind<-(grep(input$current_node_id,reactive_values$df_global[["keywords"]]))
                 ind=unlist(sapply(1:length(keywords_lem_complet),FUN=function(x,y=input$current_node_id){
                   ind_c=grep(paste0("^",y,"$"),unlist(keywords_lem_complet[[x]]))
                   if(length(ind_c)>0) return(x)
                   
                   
                 }))
               }else{
                  #ind<-(grep(input$current_node_id,reactive_values$df_global[["domain"]])) 
                 ind=unlist(sapply(1:length(reactive_values$df_global[["domain"]]),FUN=function(x,y=input$current_node_id){
                   ind_c=grep(paste0("^",y,"$"),unlist(reactive_values$df_global[["domain"]][[x]]))
                   if(length(ind_c)>0) return(x)
                   
                   
                 }))
               }
               
               table_data=datatable(df_flatten(reactive_values$df_global[ind,]),filter = 'top', options = list(scrollX = TRUE, columnDefs = list(list(
                 targets = "_all" ,render = JS(
                   "function(data, type, row, meta) {",
                   "return type === 'display' && data.length > 70 ?",
                   "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                   "}")
               ))))
               return(table_data)
              
             }
           })
         })
       })
       
       #_________________________travaille sur le wordcloud__________________________________________
       observeEvent({
         input$intervalyear
         input$Simpleword
         input$maxprint
         input$minfreq
         },{
         
         # updateNumericInput(session, inputId = "minfreq",min=add[1],max=add[2])
         set.seed(1234)
            
         suppressWarnings(
           if(!is.na(as.numeric(input$intervalyear))){ 
             if(as.numeric(input$intervalyear>0)){
               diff<-max(year,na.rm = TRUE)-min(year,na.rm = TRUE)
              
               iter<-ceiling(diff/as.numeric(input$intervalyear))
               if(iter<1) iter=1
               reactive_values$numberwordcloud=iter
               
             }
             
           }else {
             reactive_values$numberwordcloud=1
              }
           
         ) 
           
            
            output$plot_wordcloud <- renderUI({
              
              plot_output_list <- lapply(1:reactive_values$numberwordcloud, function(j) {
                plotname <- paste("wplot", j, sep="")
                plotOutput(plotname)
                
              })
              
              
              do.call(tagList, plot_output_list)
            })
            
            
           
              
              
              
              # Need local so that each item gets its own number. Without it, the value
              # of i in the renderPlot() will be the same across all instances, because
              # of when the expression is evaluated.
            
              
              #???print(is.na(as.numeric(input$intervalyear)))
              #suppressWarnings(if(!is.na(as.numeric(input$intervalyear))) input$intervalyear<-as.numeric(input$intervalyear))
              if( suppressWarnings(!is.na(as.numeric(input$intervalyear))) ){
                print("je passe dans le if wordcloud")
                
                for (i in 1:iter) {
                  local({
                    my_i <- i
                    print(my_i)
                    plotname <- paste("wplot", my_i, sep="")
                    by_year=FALSE
                    add_title=""
                    p_res=c()
                    
                    
                   
                      
                      
                      if(my_i!=iter){# si on est pas sur la dernière date
                        index_year<-((year>=min(year,na.rm = TRUE)+(my_i-1)*as.numeric(input$intervalyear)) &(year<min(year,na.rm = TRUE)+my_i*as.numeric(input$intervalyear)))
                        add_brack="["
                        
                      }else{
                        index_year<-((year>=min(year,na.rm = TRUE)+(my_i-1)*as.numeric(input$intervalyear)) &(year<=min(year,na.rm = TRUE)+my_i*as.numeric(input$intervalyear)))
                        add_brack="]"
                      }
                      #print(table(year[index_year]))
                      key_c<-keywords_lem_complet[index_year]
                      
                      
                      
                      vrac<-unlist(key_c)
                      if( length(which(vrac=="")!=0)){
                        vrac<-vrac[-( which(vrac==""))]
                      }
                      
                      
                      
                      if(input$Simpleword==TRUE){
                        corp <- Corpus(VectorSource(vrac))
                        dtm <-TermDocumentMatrix(corp)
                        m <- as.matrix(dtm)
                        v <- sort(rowSums(m),decreasing=TRUE)
                        d <- data.frame(word = names(v),freq=v)
                        t_word=d$word
                        t_freq=d$freq
                        add_title="simple"
                      }else{
                        t_word=unique(vrac)
                        t_freq=table(vrac)
                        t_word<-t_word[order(factor(t_word, levels=names(t_freq)))]
                      }
                      if(length(t_freq)!=0){
                        b=min(year,na.rm = TRUE)+(my_i)*as.numeric(input$intervalyear)
                        if(b>max(year,na.rm = TRUE)) b<-max(year,na.rm = TRUE)
                        output[[plotname]] <- renderPlot({
                         
                          if(input$minfreq>max(t_freq)) inputreal=max(t_freq) else inputreal=input$minfreq 
                          updateNumericInput(session, inputId = "minfreq",min=1,max=max(t_freq))
                          wordcloud(words = t_word, freq = t_freq, min.freq = inputreal,
                                    max.words=input$maxprint, random.order=FALSE, rot.per=0.35,
                                    colors=brewer.pal(8, "Dark2"),scale = c(1.5, 0.3))
                          
                          suppressWarnings(titre<-paste("top",input$maxprint,"keywords",add_title,"year(s) [",min(year,na.rm = TRUE)+(my_i-1)*as.numeric(input$intervalyear),":",b,add_brack,"( minimum frequency:",input$minfreq,")"))
                          mtext(titre, side=2) 
                          
                         
                        })
                        
                        
                      }
                    
                  })
                }
              }else{
                local({
                  print("je passe dans le else wordcloud")
                  my_i <- 1
                  plotname <- paste("wplot", my_i, sep="")
                  by_year=FALSE
                  add_title=""
                  
                  p_res=c()
                  vrac<-unlist(keywords_lem_complet)
                  if( length(which(vrac=="")!=0)){
                    vrac<-vrac[-( which(vrac==""))]
                  }
                  
                  
                  
                  if(input$Simpleword==TRUE){
                    corp <- Corpus(VectorSource(vrac))
                    dtm <-TermDocumentMatrix(corp)
                    m <- as.matrix(dtm)
                    v <- sort(rowSums(m),decreasing=TRUE)
                    d <- data.frame(word = names(v),freq=v)
                    t_word=d$word
                    t_freq=d$freq
                    add_title="simple"
                  }else{
                    t_word=unique(vrac)
                    t_freq=table(vrac)
                    t_word<-t_word[order(factor(t_word, levels=names(t_freq)))]
                  }
                  if(length(t_freq)!=0){
                    print("je passe dans le else wordcloud2")
                    output[[plotname]] <- renderPlot({
                      print("jarrive dans le render plot")
                      if(input$minfreq>max(t_freq)) inputreal=max(t_freq) else inputreal=input$minfreq 
                      updateNumericInput(session, inputId = "minfreq",min=1,max=max(t_freq))
                      wordcloud(words = t_word, freq = t_freq, min.freq = inputreal,
                                max.words=input$maxprint, random.order=FALSE, rot.per=0.35,
                                colors=brewer.pal(8, "Dark2"),scale = c(1.5, 0.3))
                      
                      suppressWarnings(titre<-paste("top",input$maxprint,"keywords",add_title,",in whole dataset( minimum frequency:",input$minfreq,")"))
                      mtext(titre,side=2
                            )
                      
                    })
                    
                  
                  }
              })
             }
            
            output$downloadPlot <- downloadHandler(
              filename = function(){paste(input$dataset, '.pdf', sep = '')},
              
              content = function(file) {
                pdf(file)
                make_wordcloud(keywords_lem_complet,year,simple_word = input$Simpleword,max_word_print = input$maxprint,interval_year = input$intervalyear,minfreq =input$minfreq  )
                dev.off()
              },
              
              contentType = "application/pdf"
            )
            
      })
    
    }
  })
  #_________________________________travail sur pdf_______________________________________________________  
  
    
    observeEvent(input$pdf_path, {
      reactive_values$path_folder <- choose.dir(default = "", caption = "Select folder with pdf file you want to add")
      print(reactive_values$path_folder )
     
        reactive_values$show_pdf_valid <- TRUE
        if(is.na(reactive_values$path_folder)){
           showModal(modalDialog(
              title = "Invalid path",
              "Please select a good path",
              easyClose = TRUE,
              footer = NULL
           ))
           reactive_values$show_pdf_valid <- FALSE
        } else {
           reactive_values$df_pdf <- pdf_extract_data(reactive_values$path_folder)
           if(length(reactive_values$df_pdf)==1){
              if(reactive_values$df_pdf==-400 ){
                 reactive_values$df_pdf<-NULL
                 showModal(modalDialog(
                    title = "Invalid path",
                    "this path doesn't contain any pdf files",
                    easyClose = TRUE,
                    footer = NULL
                 ))
                 reactive_values$show_pdf_valid <- FALSE
              }
                
           }
           
        }
         
    })
    
    output$table_pdf <- renderDataTable({
       validate(
          need(reactive_values$df_pdf, "No pdf data"),
          need(reactive_values$df_pdf!=-400, "")
       )
       #test_table<-reactive_values$df_pdf
       table_data=datatable((reactive_values$df_pdf), options = list(scrollX = TRUE, columnDefs = list(list(
          targets = "_all" ,render = JS(
             "function(data, type, row, meta) {",
             "return type === 'display' && data.length > 70 ?",
             "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
             "}")
       ))))
       
       
    })
    observeEvent(input$valid_pdf, {
      print("validation du pdf")
      if(reactive_values$path_folder %in% reactive_values$privious_datapath_pdf){
        showModal(modalDialog(
          title = "Invalid path",
          "This file as been analysed already ",
          easyClose = TRUE,
          footer = NULL
        ))
        reactive_values$ok_analyse=FALSE
      }else{
        reactive_values$privious_datapath_pdf=c(reactive_values$privious_datapath_pdf,reactive_values$path_folder)
        reactive_values$ok_analyse=TRUE
      }
      if( reactive_values$ok_analyse==TRUE){
        if(input$position_name_pdf==1) name_position=1 else name_position=2
       
        col_date<-parse_date_time(x=reactive_values$df_pdf[["date"]],
                                  orders = reactive_values$fmts,
                                  locale = "eng")
        
        if(is.null(reactive_values$df_global)==TRUE) {
            print("dans le if pdf")
            
            reactive_values$df_global=reactive_values$df_pdf
            names(reactive_values$df_global)<-c('titre','auteur','keywords','domain','date')
            
            
            reactive_values$df_global["source"]="PDF"
            reactive_values$df_global["date"]=col_date
            reactive_values$df_global["position_name"]=name_position
            #write.csv(reactive_values$df_global,"~/R_programs/interface_shiny/test/File Name.csv", row.names = FALSE)
         }else{
           print("dans le else ")
           temp=reactive_values$df_pdf
           temp["year"]<-NA
           names(temp)<-c('titre','auteur','keywords','domain','date','year')
           temp["source"]="PDF"
           temp["position_name"]=name_position
           reactive_values$df_global["date"]=col_date
           reactive_values$df_global=rbind(reactive_values$df_global,temp)
           print("hors du else ")
           #browser()
         
         }
        reactive_values$df_global[["keywords"]]=gsub(";",",",reactive_values$df_global[["keywords"]])
        reactive_values$valide_pdf<-TRUE
      }
    })
  

    
# tavaillle sur l'interdiciplinarité _______________________________________________________________
    observeEvent(input$ads,{
      if(input$ads==TRUE){
        reactive_values$show_token_ads <- TRUE
      }else {
        reactive_values$show_token_ads <- FALSE
      }
      
    })
    
    
    observeEvent(input$valid_DB, {
      if(input$ads==FALSE && input$pubmed==FALSE && input$arxiv==FALSE ){
        showModal(modalDialog(
          title = "Invalid entry",
          "No database selected",
          easyClose = TRUE,
          footer = NULL
        ))
        reactive_values$ok_analyse=FALSE
      }else {
        if(is.null(reactive_values$df_global)==TRUE){
          showModal(modalDialog(
            title = "Invalid data",
            "any data upload",
            easyClose = TRUE,
            footer = NULL
          ))
        }else {
          reactive_values$ok_analyse=TRUE
        }
        
      }
      
      if(reactive_values$ok_analyse==TRUE){
        if(input$ads==TRUE){
          if(input$token==""){
            showModal(modalDialog(
              title = "Invalid token",
              "token empty",
              easyClose = TRUE,
              footer = NULL
            ))
          }else {
            if(input$sup_wos_for_ref==TRUE){# a changer 
              res_data_nasa_ads=extraction_data_api_nasa_ads(data_pub=reactive_values$df_global,ti_name="titre",au_name="auteur",token=input$token,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0.85,type =input$type,source_name = "source")
            }else{
              res_data_nasa_ads=extraction_data_api_nasa_ads(data_pub=reactive_values$df_global,ti_name="titre",au_name="auteur",token=input$token,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0.85,type =input$type)
            }
          }
        }
        if(input$arxiv==TRUE){
          if(input$sup_wos_for_ref==TRUE){
            res_arxiv=extraction_data_api_arxiv(data_pub=reactive_values$df_global,ti_name="titre",au_name="auteur",pas=8,value_same_min_accept=0.95,value_same_min_ask = 0.85,type = input$type,source_name = "source")
          }else{
            res_arxiv=extraction_data_api_arxiv(data_pub=reactive_values$df_global,ti_name="titre",au_name="auteur",pas=8,value_same_min_accept=0.95,value_same_min_ask = 0.85,type = input$type)
          }
        } 
        if(input$pubmed==TRUE){
          if(input$sup_wos_for_ref==TRUE){
            res_pumed=extract_data_api_pumed(data_pub=reactive_values$df_global,ti_name="titre",au_name="auteur",pas=8,value_same_min_accept=0.85, value_same_min_ask=0.95,type = input$type,source_name = "source")
          }else {
            res_pumed=extract_data_api_pumed(data_pub=reactive_values$df_global,ti_name="titre",au_name="auteur",pas=8,value_same_min_accept=0.85, value_same_min_ask=0.95,type = input$type)
          }
        } 
        
      
      } 
  # checkboxInput("ads", "ADS", FALSE),
      # 
      # checkboxInput("pubmed", "Pubmed", FALSE),
      # checkboxInput("arxiv", "ArXiv", FALSE),
      # actionButton("valid_DB", "Process")
      #  reactive_values$df_global
      
    })
    
    
    #________________importation wos___________________________________________________________
    observeEvent(input$file2,{
      library(bibliometrix)
      print(input$file2$datapath)
      print(grep(input$file2$datapath,".bib"))
      
         suppressWarnings(reactive_values$data_wos <-convert2df(readFiles(input$file2$datapath),dbsource = "wos",format = "bibtex"))
        # print(names(reactive_values$data_wos))
        #[c(2,4),c("TI","AU","DE","SC","PY")]
        output$table_wos <- renderDataTable({
           # validate(
           #   need(reactive_values$data_wos, "No bibtext data"),
           #   need(!is.null(reactive_values$data_wos), "")
           # )
            
           table_data=datatable(df_flatten(reactive_values$data_wos), options = list(scrollX = TRUE, columnDefs = list(list(
             targets = "_all" ,render = JS(
               "function(data, type, row, meta) {",
               "return type === 'display' && data.length > 70 ?",
               "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
               "}")
           ))))
           
           
         })
        # 
            
        
        reactive_values$show_wos_valid<-TRUE
      
    })
    observeEvent(input$valid_wos, {
    
      if(input$file2$datapath %in% reactive_values$privious_datapath_wos){
        showModal(modalDialog(
          title = "Invalid path",
          "This file as been analysed already ",
          easyClose = TRUE,
          footer = NULL
        ))
        reactive_values$ok_analyse=FALSE
        
      }else{
        reactive_values$privious_datapath_wos=c(reactive_values$privious_datapath_wos,input$file2$datapath)
        reactive_values$ok_analyse=TRUE
      }
      if( reactive_values$ok_analyse==TRUE){
        print("validation du wos")
        if(is.null(reactive_values$df_global)==TRUE) {
          
          
          reactive_values$df_global=reactive_values$data_wos[,c("TI","AU","DE","SC","PY")]
          
          names(reactive_values$df_global)<-c('titre','auteur','keywords','domain','date')
          reactive_values$df_global["source"]="WOS"
          reactive_values$df_global["position_name"]=2
          
          col_date<-parse_date_time(x = reactive_values$df_global[["date"]],
                                    orders = reactive_values$fmts,
                                    locale = "eng")
          
          reactive_values$df_global[["date"]]=col_date
          
          print("je passe dans le wos ")
        }else{
          
          temp=reactive_values$data_wos[,c("TI","AU","DE","SC","PY")]
          names(temp)<-c('titre','auteur','keywords','domain','date')
          temp["year"]<-NA
          temp["source"]="WOS"
          temp["position_name"]=2
          print("woooooooooooooooooooooooooooooooos")
          
         
          #browser()
          col_date<-parse_date_time(x = temp[["date"]],
                                    orders = reactive_values$fmts,
                                    locale = "eng")
          temp[["date"]]=col_date
          reactive_values$df_global=rbind(reactive_values$df_global,temp)
          print("je passe dans le else wos ")
        }
        reactive_values$df_global[["keywords"]]=gsub(";",",",reactive_values$df_global[["keywords"]])
        reactive_values$valide_wos=TRUE
        
      }
    })
    observeEvent(input$ads_ref_accept,{
      output$table_data_ref1 <- renderDataTable({
        # validate(
        #   need(reactive_values$data_wos, "No bibtext data"),
        #   need(!is.null(reactive_values$data_wos), "")
        # )
        
        table_data=datatable(df_flatten(res_data_nasa_ads$dataframe_ref), options = list(scrollX = TRUE, columnDefs = list(list(
          targets = "_all" ,render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 70 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
            "}")
        ))))
        
        
      })
     })
    
    observeEvent(input$ads_cit_accept,{
      output$table_data_ref1 <- renderDataTable({
        # validate(
        #   need(reactive_values$data_wos, "No bibtext data"),
        #   need(!is.null(reactive_values$data_wos), "")
        # )
        
        table_data=datatable(df_flatten(res_data_nasa_ads$dataframe_citation_accept), options = list(scrollX = TRUE, columnDefs = list(list(
          targets = "_all" ,render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 70 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
            "}")
        ))))
        
        
      })
    })
    
    
    observeEvent(input$ads_error,{
      output$table_data_ref1 <- renderDataTable({
        # validate(
        #   need(reactive_values$data_wos, "No bibtext data"),
        #   need(!is.null(reactive_values$data_wos), "")
        # )
        
        table_data=datatable(df_flatten(res_data_nasa_ads$error_querry_publi), options = list(scrollX = TRUE, columnDefs = list(list(
          targets = "_all" ,render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 70 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
            "}")
        ))))
        
        
      })
    })
    
    
  ###############################arxvie ------------------------
    observeEvent(input$arxiv_ref_accept,{
      output$table_data_ref2 <- renderDataTable({
        # validate(
        #   need(reactive_values$data_wos, "No bibtext data"),
        #   need(!is.null(reactive_values$data_wos), "")
        # )
        test=df_flatten(res_arxiv$res_reference_accept)
        table_data=datatable(test, options = list(scrollX = TRUE, columnDefs = list(list(
          targets = "_all" ,render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 70 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
            "}")
        ))))
        
        
      })
    })
    
    observeEvent(input$arxiv_cit_accept,{
      output$table_data_ref2 <- renderDataTable({
        # validate(
        #   need(reactive_values$data_wos, "No bibtext data"),
        #   need(!is.null(reactive_values$data_wos), "")
        # )
        #test=df_flatten(res_arxiv$res_citation_accept)
        table_data=datatable(df_flatten(res_arxiv$res_citation_accept), options = list(scrollX = TRUE, columnDefs = list(list(
          targets = "_all" ,render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 70 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
            "}")
        ))))
        
        
      })
    })
    
    observeEvent(input$arxiv_error,{
      output$table_data_ref2 <- renderDataTable({
        # validate(
        #   need(reactive_values$data_wos, "No bibtext data"),
        #   need(!is.null(reactive_values$data_wos), "")
        # )
        
        table_data=datatable(df_flatten(res_arxiv$error_querry), options = list(scrollX = TRUE, columnDefs = list(list(
          targets = "_all" ,render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 70 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
            "}")
        ))))
        
        
      })
    })
    observeEvent(input$pubmed_ref_accept,{
      output$table_data_ref3 <- renderDataTable({
        # validate(
        #   need(reactive_values$data_wos, "No bibtext data"),
        #   need(!is.null(reactive_values$data_wos), "")
        # )
        #test=df_flatten(res_arxiv$res_citation_accept)
        table_data=datatable(df_flatten(res_pumed$dataframe_ref_accept), options = list(scrollX = TRUE, columnDefs = list(list(
          targets = "_all" ,render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 70 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
            "}")
        ))))
        
        
      })
    })
    
    observeEvent(input$pubmed_cit_accept,{
      output$table_data_ref3 <- renderDataTable({
        
        table_data=datatable(df_flatten(res_pumed$dataframe_citation_accept), options = list(scrollX = TRUE, columnDefs = list(list(
          targets = "_all" ,render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 70 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
            "}")
        ))))
        
        
      })
    })
    
    
    observeEvent(input$pumed_error,{
      output$table_data_ref3 <- renderDataTable({
        # validate(
        #   need(reactive_values$data_wos, "No bibtext data"),
        #   need(!is.null(reactive_values$data_wos), "")
        # )
        
        table_data=datatable(df_flatten(res_pumed$error_querry_publi), options = list(scrollX = TRUE, columnDefs = list(list(
          targets = "_all" ,render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 70 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
            "}")
        ))))
        
        
      })
    })
    
    #citing subject fait planté 
    
    # tabPanel("ArXiv",actionButton("arxiv_ref_accept", "Show references"),
    #          actionButton("arxiv_cit_accept", "Show citations"),
    #          dataTableOutput("table_data_ref2")),
    # tabPanel("Pubmed",actionButton("pubmed_ref_accept", "Show references"),
    #          actionButton("pubmed_cit_accept", "Show citations"),
    #          dataTableOutput("table_data_ref3"))
    # 
    
    
    # actionButton("ads_ref_accept", "Show references"),F
    # actionButton("ads_cit_accept", "Show citations"),
    # dataTableOutput("table_data_ref")
    # 
    # observeEvent(input$contain_ref,{
    #     if(input$contain_ref==TRUE) reactive_values$show_ref <- TRUE else reactive_values$show_ref <- FALSE
    # })
    # 
    # observeEvent(input$contain_cit,{
    #   if(input$contain_cit==TRUE) reactive_values$show_cit <- TRUE else reactive_values$show_cit <- FALSE
    # })
    # 
   
}#end serveur 


# Run the app ----
shinyApp(ui, server)









