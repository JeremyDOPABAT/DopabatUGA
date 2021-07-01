# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#Bonjour a toi succeceur, ici tu es au coeur de l'application  DOPABAT ET BIEN QUE CHAQUE FONCTION SOIT DOCUMENTER, JE VAIS TE FAIRE UN PETIT TOPO
# l'appli permet d'analyse un corpus de métadonne de publication envoyer par bibtext ou csv. une partie du code est cacher de l'appli, il sagit de la pmartie qui traites des fichier pdf en teste brut 
# car il n'était pas possible de le mettre sur l'application. 

#il y a un distingo d'import entre les bibtest du wos et les bibtest d'ailleur 

# une fois importé on commence par traiter les mot clefs (si la colonnes est renseignée) on en fait un nuage de mot et un réseau.(voir les partie correspondantes)

# une fois que cela est fait , l'utilisateur peut demander, des citation des reférence ou les deux. sur différente base (a l'heure acteuelle ads contre une clef ou pumed )
# les traitement par base sont très complexe car il faut si adapter(voir fonction globale de chaque base pour plus d'information () )

# une fois que cela est fait on identifie les journaux grace a un fichier fournie et on calcule le rayonnement interdiciplinaire du corpus selon se qui a été demander par l'utilisateur.

# les différentes fonctions sont dans le fichier global.r
# 
# le ui gere tout l'aspec graphie '
# le serveur tout l'apec calcule et variable '


source("global.R",encoding = "UTF-8")

library(utils)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(lubridate)
#library(ggplot2)
library(plotly)
library(shinycssloaders)
library(DT)

# l'ui c'est toute la partie qui définie l'exterieur de l'interface, les boutons et les boite. 
ui <-dashboardPage(skin = "red",
                   #menu
                   dashboardHeader(title = "DOPABAT"),
                   dashboardSidebar( sidebarMenu(
                     menuItem("Home", tabName = "home", icon = icon("house-user")),
                     menuItem("Import_csv", tabName = "import_csv", icon = icon(name = "arrow-circle-up")),
                    
                      #menuItem("Import_PDF", tabName = "import_pdf", icon = icon("file-pdf")),
                     #si le probleme est  regle decommenter laligne du dessous 
                     menuItem("Import_bibext", tabName = "import_wos", icon = icon("file-export")),
                     menuItem("History", tabName = "history", icon = icon("history")),
                     menuItem("Wordcloud graphics", tabName = "wordcloud", icon = icon("cloud")),
                     menuItem("Network graphics", tabName = "network", icon = icon("project-diagram")),
                     menuItem("interdisciplinarity reasech", tabName = "DB", icon = icon("database")),
                     menuItem("Interdisciplinarity results", tabName = "calculinterdisciplinarity", icon = icon("chart-pie")),
                     menuItem("About us", tabName = "about", icon = icon("address-card"))
                     
                     #-------------------------------------------------------------------  
                   )),
                   dashboardBody(
                     tabItems(
                       tabItem(tabName = "home",
                         titlePanel("DOPABAT"),
                         htmlOutput("text_home"),
                         
                         
      #                    
      #                    tags$head(tags$script(src="js.cookie.js")),
      #                    
      #                    # a shiny element to display unformatted text
      #                    box(title ="click the gray square to view cookies!", verbatimTextOutput("results"),actionButton("go","click me")),
      #                    
      #                    # javascript code to send data to shiny server
      #                    tags$script('
      #         document.getElementById("go").onclick = function() {
      #         var number = Math.random();
      #         
      #         Cookies.set(\'name\', \'value\', { expires: 7 });
      #         Cookies.set(\'cookie_2\', \'value\', { expires: 7 });
      #         
      #         var my_cookie = Cookies.get(); 
      # 
      #         Shiny.onInputChange("mydata", my_cookie);
      #         };
      # ')
      
                       
                         
                         
                       ),#onglet import csv ----
                              #cet onglet permet d'imprter un csv comme donnees. 
                              # First tab content
                              tabItem(tabName = "import_csv",
                                      
                                      # Sidebar layout with input and output definitions
                                      fluidRow(
                                        
                                        box(width = 4, title = "Uploading files", solidHeader = TRUE,status = "danger", #danger=rouge 
                                            
                                            # Input: Select a file
                                            tags$div(title="select à file, after validation re select a file and validate it will add the data to the curent data not replace them", fileInput("file1", "Choose CSV File",
                                                      multiple = TRUE,
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv"))),
                                            #parame du fichier ---------------------
                                            # Horizontal line
                                            tags$hr(),
                                            
                                            # Input: Checkbox if file has header
                                            tags$div(title="the first line of file is the title colum?",checkboxInput("header", "Header", TRUE)),
                                            
                                            # Input: Select separator
                                            tags$div(title="choose the type of separator of the file ",radioButtons("sep_csv", "Separator",
                                                         choices = c(Semicolon = ";",
                                                                     Comma = ",",
                                                                     Tab = "\t"),
                                                         selected = ";")),
                                                    # Input: Select quotes
                                            tags$div(title="is in the file quote ?",radioButtons("quote", "Quote",
                                                         choices = c("Double Quote" = '"',
                                                                     "Single Quote" = "'"),
                                                         selected = '"')),
                                            
                                            #selected encoding 
                                            tags$div(title="enoding of the file",radioButtons("encoding", "Encoding",
                                                         choices = c("UTF-8" = "UTF-8",
                                                                     "Latin-1" = "Latin-1"
                                                         ),
                                                         selected = "Latin-1")),
                                            #display viewer 
                                            tags$hr(),
                                            tags$div(title="display of the viewed table on side",radioButtons("disp", "Display",
                                                         choices = c("Few lines" = "head",
                                                                     All = "all"),
                                                         selected = "head")),
                                            #order of name author 
                                            tags$hr(),
                                            tags$div(title="the order of the author need to be told, the firstname don't need to be complete abreviation works",radioButtons("position_name_CSV", "disposition of name and firstname for author",
                                                                                                                                                                             choices = c("Lastname Firstname" = "2",
                                                                                                                                                                                         "Firstname Lastname" = "1"),
                                                                                                                                                                             selected = "1")),
                                            tags$div(title="The character which sep each author in the column ",radioButtons("sep_author_csv", "Separator of author names",
                                                                                                                             choices = c("," = ",",
                                                                                                                                         ";"=";",
                                                                                                                                 "saut de ligne"="\n"),
                                                                                                                             selected = ",")),
                                            textInput("other_sep_author_csv", "other sparateur of author"), 
                                            
                                            # permet d'afficher la selection de header après l'import 
                                            conditionalPanel('output.show_header',tags$div(title="Validation of the table start the analysis",actionButton("valid_table", "validate")))# boutton de validation du fichier 
                                            
                                            
                                        ),
                                        
                                        
                                        
                                        
                                        # Main panel for displaying outputs
                                        column(8,
                                               
                                               # Output: Data file
                                               dataTableOutput("contents"),
                                               br(),
                                               fluidRow(
                                                 #bouttons de selection de colone (header) poue fichuer csv 
                                                 conditionalPanel('output.show_header',
                                                                  tags$div(title="complete the column selection before analysing the corpus",box(width = 12, title = "Headers selection", solidHeader = TRUE, status = "danger",
                                                                                                                                                 fluidRow( 
                                                                                                                                                   column(width=3, selectInput("title_selection", "Title column", choices = "", width = "300px"),
                                                                                                                                                          selectInput("author_selection", "Author column", choices = "", width = "300px")),
                                                                                                                                                   column(3,offset = 1,selectInput("keyword_selection", "Keywords column", choices = "", width = "300px"),
                                                                                                                                                          selectInput("domain_selection", "Domain column", choices = "", width = "300px")),
                                                                                                                                                   column(3,offset = 1,selectInput("date_selection", "Date publication column", choices = "", width = "300px"),
                                                                                                                                                          selectInput("doi_selection", "DOI column", choices = "", width = "300px"),
                                                                                                                                                          conditionalPanel('output.show_arxiv_abilities',selectInput("id_arxiv_selection", "arxiv id column", choices = "", width = "300px"))),
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
                                      #import pdf ----
                                      #cet onglet permet d'importe un pdf a partir d'un dossier 
                                      # il est actuellement hors service car la version de shiny ne permet pas un element central de la fct principal.
                                      
                                      fluidRow(
                                        
                                        box(width = 5, title = "Uploading files", solidHeader = TRUE, status = "danger",
                                            tags$div(title="the pdf files in pdf folder will be shown ",actionButton("pdf_path", "Click to select you pdf folder")),
                                            tags$hr(),
                                            tags$div(title="the order of the author need to be told, the firstname don't need to be complete abreviation works, use the data vision if you need",
                                                     radioButtons("position_name_pdf", "disposition of name and firstname for author",
                                                                  choices = c("Lastname Firstname" = "2",
                                                                              "Firstname Lastname" = "1"),
                                                                  selected = "1")),
                                            tags$div(title="The character which sep each author in the column ",radioButtons("sep_author_pdf", "Separator of author names",
                                                                                                                             choices = c("," = ",",
                                                                                                                                         ";" = ";"),
                                                                                                                             selected = ",")),
                                            
                                            conditionalPanel('output.show_pdf_valid',actionButton("valid_pdf", "validate"))
                                            
                                        )
                                        ,column(12,
                                                mainPanel(
                                                  
                                                  # Output: Data file
                                                  dataTableOutput("table_pdf")
                                                )
                                        )
                                      )
                                      
                              ),
                              tabItem(tabName = "import_wos",
                                      #Onglet import bibtex----
                                      #cet onglet permet d'importer de fichier bibtext 
                                      #les variable se nom "wos" car avant on importait uniquement des fichier wos 
                                      fluidRow(
                                        
                                        box(width = 5, title = "Uploading bibtex files", solidHeader = TRUE, status = "danger",# le status "danger" permet la couleur rouge 
                                            tags$div(title="Choose bibtex file, bibtext only",fileInput("file2", "Choose bibtext File",
                                                                                                        multiple = TRUE,
                                                                                                        accept = c(".bib"))),
                                            tags$div(title="If the file is coming from wos database OR if the regular importation dont work ,check this case",
                                                     checkboxInput("is_wos", "this file is comming from WOS(wos importation methode) ",value = FALSE)),
                                            
                                            htmlOutput("text_wos"),
                                            tags$hr(),
                                            tags$div(title="the order of the author need to be told, the firstname don't need to be complete abreviation works",radioButtons("position_name_wos", "disposition of name and firstname for author",
                                                                                                                                                                             choices = c("Lastname Firstname" = "2",
                                                                                                                                                                                         "Firstname Lastname" = "1"),
                                                                                                                                                                             selected = "1")),
                                            radioButtons("disp_wos", "Display",
                                                         choices = c("Few lines" = "head",# mode de visualisation du fichier 
                                                                     All = "all"),
                                                         selected = "head"),
                                            
                                            conditionalPanel('input.is_wos',checkboxInput("sup_wos_for_ref", "this file already countain reference, don't reasearch them",value = FALSE)),# rechercher les ref dans le fichier ou pas 
                                            conditionalPanel('output.show_wos_valid',actionButton("valid_wos", "validate"))# boutton de valiation 
                                            #
                                        ),
                                        
                                        
                                        column(7,withSpinner(dataTableOutput("table_wos"),color="#c50d3e"))
                                        # visualisation de l'import 
                                        
                                      )
                                      
                              ),
                              tabItem(tabName = "history",
                                      #onglet history----
                                      #cet onglet permet de voir quel fichier sont deja dans l'etude. il permet aussi de rest l'appli si l'utilisateur veut recommencer.
                                      # doit etre amelirer pour etre plus utile sur le serveur 
                                      titlePanel("Files already in the analyse"),
                                      htmlOutput("text_history"),
                                      textOutput("list_file"),
                                      useShinyjs(),                                           # Include shinyjs in the UI
                                      extendShinyjs(text = jsResetCode,functions = "reset"),        # Add the js code to the page
                                      actionButton("reset_button", "Reset Page")
                              ),
                              
                              tabItem(tabName = "wordcloud",
                                      #onglet wordcloud ----
                                      #cet onglet permet la visualisation des mot clefs du corpus sous plusieurs forme. 
                                      titlePanel("Word cloud graphics"),
                                      
                                      
                                      #wordcloud
                                      # Sidebar layout with input and output definitions ----
                                      fluidRow( sidebarPanel(
                                        tags$div(title="One graph per number of year",selectInput("intervalyear","number of year per graphics",choices = "")),
                                        numericInput("maxprint","max number of keywords on graphic(s)",value=20,min=1,max=50,step=1)
                                        
                                        
                                        
                                      ),
                                      box(width = 4,checkboxInput("Simpleword", "literaly words analyse one by one"),
                                          #numericInput("minfreq","minimum freqency to be on graph(s)",value=1,min=1,max=50,step=1))
                                          sliderInput("minfreq", label = "minimum freqency to be on graphic(s)", min = 0, 
                                                      max = 100, value = 50))
                                      
                                      
                                      
                                      ),
                                      fluidRow(
                                        uiOutput("plot_wordcloud")%>% withSpinner(color="#0dc5c1"),
                                        tags$div(title="Download all the graphics of page in pdf ",downloadButton("downloadPlot", "Download Plot(s)"))
                                      )
                                      
                                      
                                      
                                      
                              ),
                              
                              tabItem(tabName = "network",
                                      titlePanel("Network graphics"),
                                      htmlOutput("text_network"),
                                      downloadButton("downloadData", "Download acticles data"),
                                      fluidRow(column(width=3,   tags$div(title="show only the bigest nodes with their relations ",selectInput("networktop","number of keywords for top graph",choices =c("None",1:20)))),
                                               column(width=3, tags$div(title="Supress th nodes taken only one time in the graphic",checkboxInput("supones", "supress keywords taken one time only", value = FALSE)),
                                                      tags$div(title="Reduce the size of the nodes to root",checkboxInput("root", "passe the weight of nood to roots", value = FALSE))),
                                               column(width=3,tags$div(title="One graphic per number of year",selectInput("networkintervalyear","number of year per graphics",choices =c("All year",1:20)))),
                                               column(width=3, tags$div(title="switch to domain network",checkboxInput("domain", "show domain study ", value = FALSE)))
                                               
                                               
                                               
                                      ),
                                      #visualisation 
                                      fluidRow(uiOutput("plots")%>% withSpinner(color="#0dc5c1"),
                                               dataTableOutput("table_infos")
                                      )
                                      
                              ),
                              tabItem(tabName = "DB",
                                      #onglet database----
                                      titlePanel("Research citations and references"),
                                      htmlOutput("text_database"),
                                      fluidRow(
                                        
                                        box(width = 4, title = "Chose the databases you want to use", solidHeader = TRUE, status = "danger",
                                            
                                            #choix de la ou des bases 
                                            checkboxInput("ads", "ADS", FALSE),
                                            checkboxInput("lens", "Lens", FALSE),
                                            conditionalPanel('output.show_pumed_box',checkboxInput("pubmed", "Pubmed", FALSE)),
                                            conditionalPanel('output.show_arxiv_abilities',checkboxInput("arxiv", "ArXiv", FALSE)),
                                            
                                            tags$div(title="Citation : aticle which cite an other aticle in the corpus. Reference: articles use to build the articles on the corpus.",radioButtons("type", "What data do you want to fetch ?",
                                                                                                                                                                                                   choices = c(Citations = "cit",
                                                                                                                                                                                                               References = "ref",
                                                                                                                                                                                                               Both = "all"),
                                                                                                                                                                                                   selected = "cit")),
                                            
                                            
                                            
                                            actionButton("valid_DB", "Process the reaseach"),
                                            br(),
                                            actionButton("valid_data_research", "Calculate interdisiplinarity"),
                                            conditionalPanel('output.show_token_lens',textInput("token_lens",label ="Lens'token" )),
                                            conditionalPanel('output.show_token_ads',textInput("token_ads",label ="ADS'Token" )),
                                            
                                            
                                            
                                        ),
                                        tabBox(width = 8, title = "Results data tab ",
                                               #onglet resultat citation----
                                               # dans cette onglet lutilisateur va pouvoir voir le resultat des recherche sur les et faire dfferent chois. 
                                               tabPanel("ADS", conditionalPanel('output.show_ads_res_window',
                                                                                #un onglet par base, chaque base possede defferent boutons  
                                                                                radioButtons("col_journal_ads", "How is the journal names?",
                                                                                             choices = c("Complet journal name" = "Full.Journal.Title",
                                                                                                         "Abreviation journal name" = "JCR.Abbreviated.Title"
                                                                                             ),
                                                                                             selected = "Full.Journal.Title",inline = TRUE),
                                                                                conditionalPanel('output.show_ref',
                                                                                                 actionButton("ads_ref_accept", "Show references")),
                                                                                
                                                                                conditionalPanel('output.show_cit',
                                                                                                 actionButton("ads_cit_accept", "Show citations")
                                                                                ),
                                                                                actionButton("ads_error", "Show error(s)"),
                                                                                actionButton("ads_ask","Show publications with doute"),
                                                                                actionButton("ads_res_publi","Show publications founded"),
                                                                                dataTableOutput("table_data_ref1"))),
                                               
                                               tabPanel("Pubmed", conditionalPanel('output.show_pumed_res_window',
                                                                                   radioButtons("col_journal_pumed", "How is the journal names?",
                                                                                                choices = c("Complet journal name" = "Full.Journal.Title",
                                                                                                            "Abreviation journal name" = "JCR.Abbreviated.Title"
                                                                                                ),
                                                                                                selected = "Full.Journal.Title",inline = TRUE),
                                                                                   conditionalPanel('output.show_ref',
                                                                                                    actionButton("pubmed_ref_accept", "Show references")),
                                                                                   conditionalPanel('output.show_cit',
                                                                                                    actionButton("pubmed_cit_accept", "Show citations")),
                                                                                   actionButton("pubmed_error", "Show error(s)"),
                                                                                   actionButton("pubmed_ask","Show publication with doute"),
                                                                                   actionButton("pubmed_res_publi","Show publications founded"),
                                                                                   dataTableOutput("table_data_ref3"))),
                                               tabPanel("LENS", conditionalPanel('output.show_lens_res_window',
                                                                                #un onglet par base, chaque base possede defferent boutons  
                                                                                radioButtons("col_journal_lens", "How is the journal names?",
                                                                                             choices = c("Complet journal name" = "Full.Journal.Title",
                                                                                                         "Abreviation journal name" = "JCR.Abbreviated.Title"
                                                                                             ),
                                                                                             selected = "Full.Journal.Title",inline = TRUE),
                                                                                conditionalPanel('output.show_ref',
                                                                                                 actionButton("lens_ref_accept", "Show references")),
                                                                                
                                                                                conditionalPanel('output.show_cit',
                                                                                                 actionButton("lens_cit_accept", "Show citations")
                                                                                ),
                                                                                actionButton("lens_error", "Show error(s)"),
                                                                                actionButton("lens_ask","Show publication with doute"),
                                                                                actionButton("lens_res_publi","Show publications founded"),
                                                                                dataTableOutput("table_data_ref5"))),
                                               
                                               
                                               tabPanel("Bibtext files references",conditionalPanel('output.show_wos_res_window',radioButtons("col_journal_wos", "How is the journal names?",inline = TRUE,
                                                                                                                             choices = c("Complet journal name" = "Full.Journal.Title",
                                                                                                                                         "Abreviation journal name" = "JCR.Abbreviated.Title"
                                                                                                                             ),
                                                                                                                             selected = "JCR.Abbreviated.Title"), dataTableOutput("table_data_ref4"))),
                                               conditionalPanel('output.show_arxiv_abilities',tabPanel("ArXiv", conditionalPanel('output.show_arxiv_res_window',
                                                                                                                                 radioButtons("col_journal_arxiv", "How is the journal names?",
                                                                                                                                              choices = c("Complet journal name" = "Full.Journal.Title",
                                                                                                                                                          "Abreviation journal name" = "JCR.Abbreviated.Title"
                                                                                                                                              ),
                                                                                                                                              selected = "Full.Journal.Title",inline = TRUE),
                                                                                                                                 conditionalPanel('output.show_ref',
                                                                                                                                                  actionButton("arxiv_ref_accept", "Show references")),
                                                                                                                                 conditionalPanel('output.show_cit',
                                                                                                                                                  actionButton("arxiv_cit_accept", "Show citations")),
                                                                                                                                 actionButton("arxiv_error", "Show error(s)"),
                                                                                                                                 actionButton("arxiv_ask","Show publication found with doute"),
                                                                                                                                 dataTableOutput("table_data_ref2"))))
                                               
                                               
                                               
                                               
                                               
                                        ),
                                        
                                      )
                              ),
                              tabItem(tabName = "calculinterdisciplinarity",
                                      #onglet resuultat et calcule d'interdiciplinarite 
                                      #dans cette onglet l'utilisateur vas pouvoir voir les resultat de l'interdiciplinarite, par article.
                                      fluidPage(
                                        htmlOutput("text_interdis_graph"),
                                        selectInput("select_article", "Select the number of the article for the graph",NULL),
                                        tabBox(width = 12,height = 600, title = "result interdisciplinarity",
                                               tabPanel("references",
                                                        column(width = 6,
                                                               textOutput("stat_journ_ref_article"),
                                                               plotlyOutput("plot_article_ref"),#article ref 
                                                               #downloadButton("downloadPlot_article_ref", "Download Plot(s)")
                                                        ),
                                                        column(width = 6,
                                                               textOutput("stat_journ_ref_total"),
                                                               plotlyOutput("plot_total_ref"),
                                                               #   downloadButton("downloadPlot_total_ref", "Download Plot(s)"),#total ref 
                                                               downloadButton("downloadref", "Download data references")
                                                        ),
                                                        textOutput("text_ref"),
                                                        dataTableOutput("table_data_ref_interdi")
                                               ),
                                               tabPanel("citations",
                                                        column(width = 6,
                                                               textOutput("stat_journ_cit_article"),
                                                               plotlyOutput("plot_article_cit"),
                                                               #  downloadButton("downloadPlot_article_cit", "Download Plot(s)")
                                                        ),
                                                        column(width = 6,
                                                               textOutput("stat_journ_cit_total"),
                                                               plotlyOutput("plot_total_cit"),
                                                               # downloadButton("downloadPlot_total_cit", "Download Plot(s)"),
                                                               downloadButton("downloadcit", "Download data citation")
                                                        ),
                                                        textOutput("text_cit"),
                                                        dataTableOutput("table_data_cit_interdi")
                                                        
                                                        
                                                        
                                               )
                                        )
                                      )         
                              ),
                              
                              
                              tabItem(tabName = "about",
                                      #onglet about----
                                      
                                      titlePanel("DOPABAT Project"),
                                      fluidPage(
                                        # Application title
                                        
                                        
                                        
                                        
                                        # Show Word Cloud
                                        mainPanel(
                                          htmlOutput("text"),
                                          HTML('<br>'),
                                          HTML('<br>'),
                                          tags$div(HTML('<img src="logo_ads.png" hight="130" width="130">'),
                                                   HTML('<img src = "logo_pub.png" hight="120" width="120">' ),
                                                   HTML('<img src = "logo_lens.png" hight="120" width="120">' )),
                                          HTML('<br>'),
                                          HTML('<br>'),
                                          HTML('<br>'),
                                          HTML('<br>'),
                                          tags$div(HTML('<img src="logo_uga.png" hight="120" width="120">'),
                                                   HTML('<img src = "logo_colex.png" hight="135" width="135">' )),
                                          HTML('<br>'),
                                          tags$div(HTML('<img src = "logo_cnrs.png" hight="100" width="100">' ),HTML('<img src = "logo_psl.png" hight="100" width="100">' ))
                                        )
                                      )
                              )
                              
                              # Output: Data file ---
                              
                     )
                   )
)


#server ----
server <- function(input, output, session) {
  #Sys.setlocale( Sys.getlocale(category = "LC_TIME"), "C")
  
  options(shiny.maxRequestSize=32*1024^2) # permet l'importation de "gros" fichier de donner jusqua 32mo 
  
  #variable reactive permetant l'interation utilisateur chaque variable utiliser dans un obsverve doit etre interactive a moins quel soit ephemere 
  reactive_values <- reactiveValues(
    show_header = FALSE,# booleen qui permet l'aparition de la selection de coloenne des csv 
    df_csv = NULL, # dataframe pour stocker les csv 
    df_pdf = NULL,#  "          "     "       " PDF
    data_wos=NULL,# "            "    "        " wos et bibtext 
    df_global=NULL,# dataframe permetant le rassemblement des données 
    
    show_pdf_valid=FALSE,# boutons permettant l'appartion des bouton validation de pdf (actuellement enlever de l'interface )
    valide_csv=FALSE,
    valide_pdf=FALSE,
    valide_wos=FALSE,
    privious_datapath_csv=NULL,# vecteur d'histoirque csv
    privious_datapath_pdf=NULL,# "            "      pdf 
    privious_datapath_wos=NULL,#  "            "    wo et bib 
    path_folder=NULL, # path dossier pour pdf 
    ok_analyse=FALSE, # boulen de validation marquand le debut de l'analyse 
    graph=NULL,# netwoork 
    plots=NULL,# wordcloud 
    numberwordcloud=1,#nombre de graphique wordcloud par defaut 
    show_token_ads=FALSE, # bouton du token  ads 
    show_token_lens=FALSE,
    show_id_arxiv=FALSE,# bouton idi arxiv 
    show_wos_valid=FALSE, # bouton valid wos 
    show_ref=FALSE,# boutons d'apartion des resultat de reference 
    show_cit=FALSE,# "        "          "    "          "
    show_ads_res_window=FALSE,# les boutons et dataframe pour les reultat ads 
    show_pumed_res_window=FALSE,# "  "          "         "         "     pubmed 
    show_lens_res_window=FALSE,#  "   "          "        "          "   lens 
    show_arxiv_res_window=FALSE,# "   "           "      "          "    arxiv 
    show_wos_res_window=FALSE, #  "   "            "     "          "   bib et lens 
    res_ads=NULL,#res_data_nasa_ads,#temporaire 
    res_arxiv=NULL,#res_arxiv, #temporaire
    res_pumed=NULL,##res_pumed, #temporaire 
    res_lens=NULL,
    ref_wos=c(),
    table_to_show_ref=NULL,#table de ref(et cit) resultat
    journal_table_ref=NULL,# table d'importation des journaux 
    table_dist=NULL,# table de calcule des distance 
    fmts=c("%d/%m/%y","%Y", "%Y-%m", "%m/%d/%y","%d-%m-%Y","%B %d, %Y","%Y-%m-%d"),# type de dat pour importation de fichier 
    active_source=NULL,#permet de savoir sur quel onglet clic
    value_same_min_accept=0.95,# valeur minimal de l'interval de la verification de ttitre 
    value_same_min_ask=0.85,
    matrice_res_ref=NULL,
    matrice_res_cit=NULL,
    table_categ_gd=NULL,
    wos_data=NULL,
    plots_article_ref=NULL,
    data_merge=NULL,
    secteur_is_finish=FALSE,
    states=list(source = c("plot_article_ref", "plot_total_ref"), value = c(-99,-99), changed = c(FALSE,FALSE),key=NULL),#permet de savoir sur quel graphic clic l'utilisteur
    states_cit=list(source = c("plot_article_cit", "plot_total_cit"), value = c(-99,-99), changed = c(FALSE,FALSE),key=NULL),
    pct_ref=c(NULL,NULL),#caclule pourcentage exactitude sur les citation 
    pct_cit=c(NULL,NULL),
    transfer_done=list(ads=NULL,arxiv=NULL,pumed=NULL,lense=NULL), #liste de boulllean pour l'état  
    cal_temp=NULL,
    show_pumed_box=TRUE,# when lens is selected pubmed desapear 
    show_arxiv_abilities=FALSE #temps que arxiv n'a pas un moyen de fonctionner cela restera a faux 
    
  )  
  #copy des variable reactive dans les output pour leurs permetre d'etre invisible 
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
  output$show_token_lens<- reactive({
    reactive_values$show_token_lens
  })
  output$show_id_arxiv<- reactive({
    reactive_values$show_id_arxiv
  })
  output$show_ref<- reactive({
    reactive_values$show_ref
  })
  output$show_cit<- reactive({
    reactive_values$show_cit
  })
  output$show_ads_res_window<- reactive({
    reactive_values$show_ads_res_window
  })
  output$show_lens_res_window<- reactive({
    reactive_values$show_lens_res_window
  })
  output$show_pumed_res_window<- reactive({
    reactive_values$show_pumed_res_window
  })
  output$show_arxiv_res_window<- reactive({
    reactive_values$show_arxiv_res_window
  })
  
  output$show_wos_res_window<- reactive({
    reactive_values$show_wos_res_window
  })
  output$show_arxiv_abilities<- reactive({
    reactive_values$show_arxiv_abilities
  })
  
  output$show_pumed_box<- reactive({
    reactive_values$show_pumed_box
  })
  
  
  # on rand invisible les variable de conditionnemetn ainsi tous ce qui leurs et lie ne sera pas montrees 
  outputOptions(output, "show_arxiv_abilities", suspendWhenHidden = FALSE)
  outputOptions(output, "show_header", suspendWhenHidden = FALSE)
  outputOptions(output, "show_pdf_valid", suspendWhenHidden = FALSE)
  outputOptions(output, "show_token_ads", suspendWhenHidden = FALSE)
  outputOptions(output, "show_token_lens", suspendWhenHidden = FALSE)
  outputOptions(output, "show_wos_valid", suspendWhenHidden = FALSE)
  outputOptions(output, "show_ref", suspendWhenHidden = FALSE)
  outputOptions(output, "show_cit", suspendWhenHidden = FALSE)
  outputOptions(output, "show_ads_res_window", suspendWhenHidden = FALSE)
  outputOptions(output, "show_lens_res_window", suspendWhenHidden = FALSE)
  outputOptions(output, "show_pumed_res_window", suspendWhenHidden = FALSE)
  outputOptions(output, "show_arxiv_res_window", suspendWhenHidden = FALSE)
  outputOptions(output, "show_id_arxiv", suspendWhenHidden = FALSE)
  outputOptions(output, "show_wos_res_window", suspendWhenHidden = FALSE)
  outputOptions(output, "show_pumed_box", suspendWhenHidden = FALSE)
  
  
  
  #texte help----
  
  
  #ce qui suit est le texte present dans la partie about 
  output$text_home <- renderText({
    paste(h3("DOPABAT, what is it ?"),"\n","DOPABAT (Développement d'outils d'analyse bibliométrique et d'audience des thèses) is a project funded by Collex-Persée.
           A national infrastructure of technique and sciences which supports French researchers. The objectives are, on the one hand, to know the importance of theses in the scientific production and, on the other hand, to know the importance of the cooperation between laboratories on the themes of Physics and Astronomy. 
           At first this project was a researcheress request that consisted in the analysis of PHDs coming from two universities. DOPABAT aims to analyse all the bibliometric data, keywords, domains of study, citations, references. You can now, use it too, you can importe cvs files or bibtext files to start the analyse. After importation you can chose wich database you want to analyse!
          
           NB: you have to have a token for some.
          
          If you need exemple or help we invite you to click on",a("user guide",target="_blank",href="User_help_doc.pdf"),"for more details"
          )
  })
  output$text <- renderText({
    paste(h3("DOPABAT,who is it?"),"
  <ul>
  <li>l'Université Grenoble Alpes</li>
	  -DGD RIV\n
	  -BAPSO\n
	<li>L'Observatoire de Paris</li> 
	  -BIbliothèque de l'Observatoire Paris
	<li>l'Inist-CNRS</li>
</ul>",h3("The Team" ),"<ul>
  <li>Anne-Marie Badolato (INIST-CNRS)</li>
	 <li>Aurélie Fayard (bibliothèque de l'Observatoire Paris)</li> 
	 	<li>Frédéric Saconnet  (bibliothèque de l'Observatoire Paris)</li>
	 	<li>Jeremy Moro--Guibbert (Université Grenoble Alpes)</li>
	 <li>Didier Vercueil  (Université Grenoble Alpes)</li>
  <li>Lucie Albaret  (Université Grenoble Alpes)</li>
</ul>",
            "to contact us: dopabat@univ-grenoble-alpes.fr",
          "If you need exemple or help we invite you to click on",a("user guide",target="_blank",href="User_help_doc.pdf"),
            'This project is funded  by <a href="https://www.collexpersee.eu/">  GIS Collex Persée</a> according to  <a href="https://https://www.collexpersee.eu/les-projets//">  APP 2019</a> ',
            h3("The Blog"), "here is the adress of the blog to see the back ground of the project : <a href='https://dopabat.inist.fr/'> : https://dopabat.inist.fr/ </a>",
            "This blog is describing the problem, the methodes and the steps of the project",h3("Thanks"),"
           
All the DOPABAT team would like to thank ADS, PUMED and LENS for their disposal and support during all the development phase. We personally thank all the databases that allow the platform to work. 
We ask all the users to   cite the different souces they use to make the graphics.
"
            
    )
    
    
    
  })
  output$results = renderPrint({
    input$mydata
  })
  # trst present dans le network pour aider a le comprehension, ces textes sont de l'html  
  output$text_network <- renderText({
    paste(h4("Help:"), "When you have your network you could click on nodes. In doing so a table will apear under the graph with details of publications concerning that node.")
  })
  
  output$text_interdis_graph <- renderText({
    paste(h4("Help:"), "Here you can see the interdiciplinarity of each article or the copus. If you click on a part of the graph you will have data details on a table below.")
  })
  
  #meme chose pour la rescer interdis 
  output$text_database <- renderText({
    paste(h4("Help:"),"First you will select a database and do 'process' (exectpte if you only have ref for wos) then you can manage your data as you wich and when it's ok clic on the right button.")
  })
  
  #text d'aide pour l'onglet bibtext 
  output$text_wos <- renderText({
    paste(h4("Help:"), "if the table is stuck on  'Processing' clik on 'this file is comming from WOS' this method works better for some file.
          AU is the author column, TI the title column, PY is the date of publication and CR is the column which contain references")
  })
  # text aide pour historic 
  output$text_history <- renderText({
    paste(h4("Help:"), "when you valid a file that add their data to the whole analysis and that not delete the privious result. to delete analysis clic on the 'reset' buttons")
  })
  
  #historique ________________________________________________________
  observeEvent({c( # a chaque fois que les variable d'historique change on modifie le test  
    reactive_values$privious_datapath_wos,
    reactive_values$privious_datapath_csv)
  }, {  
    
    output$list_file <- renderText({
      c("CSV:",paste(reactive_values$privious_datapath_csv,collapse ="," ), "Bibtext:",paste(reactive_values$privious_datapath_wos,collapse = ","))
    })
    
  })
  
  
  
  #reset----
  observeEvent(input$reset_button, {js$reset()})# permet la fonction reset dans history 
  observeEvent(input$file1, {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    
    #import csv----
    # on refait l'importation chaque fois qu'un parametre est changer 
    observeEvent({c(
      input$header,
      input$sep_csv,
      input$quote,
      input$encoding)
    },{
      
      test_error=tryCatch({
        df <- as.data.frame(data.table::fread(input$file1$datapath,
                                              header = input$header,
                                              sep = input$sep_csv,
                                              quote = input$quote,encoding = input$encoding),stringsAsFactors = FALSE)
        
      },
      error=function(cond){
        return(NA)
      })
      if(length(test_error)==1){
        if(is.na(test_error)){
          showModal(modalDialog(
            title = "invalid CSV",
            "the csv modality are not good, make sure to use a good separator and quote and retry. If your csv don't fit the modality proposed for now we can't analyse it.",
            easyClose = TRUE,
            footer = NULL
          ))
        }
      }else{
        
        reactive_values$df_csv <- df
        #mise en forme de la table afficher 
        table_data=datatable(df_flatten(reactive_values$df_csv), options = list(scrollX = TRUE, columnDefs = list(list(
          targets = "_all" ,render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 70 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
            "}")
        ))))
        
        reactive_values$show_header <- TRUE# affichage du tableau des entete 
        # on met a jour les input en fct de se qui a ete charger pour pouvoir les selectionner les entete 
        updateSelectInput(session, inputId = "title_selection", choices = names(df))
        updateSelectInput(session, inputId = "keyword_selection", choices = c("none",names(df)))
        updateSelectInput(session, inputId = "author_selection", choices = names(df))
        updateSelectInput(session, inputId = "domain_selection", choices = c("none",names(df)))
        updateSelectInput(session, inputId = "date_selection", choices = names(df))
        updateSelectInput(session, inputId = "doi_selection", choices = c("none",names(df)))
        
        
        conditionalPanel('output.show_arxiv_abilities',updateSelectInput(session, inputId = "id_arxiv_selection", choices = c("none",names(df))))
        # affichage de la table 
        output$contents <- renderDataTable({
          if(input$disp == "head") {# si head on affiche que les deux premiere ligne 
            return(datatable(df_flatten(df[c(1,2),]), options = list(scrollX = TRUE, columnDefs = list(list(
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
  
  observeEvent(input$other_sep_author_csv, {
    if(!is.null(input$other_sep_author_csv) && input$other_sep_author_csv != "")
      updateRadioButtons(session, "sep_author_csv", choices = c(c("," = ",",
                                                                  ";"=";",
                                                                  "saut de ligne"="\n"), input$other_sep_author_csv), 
                         selected = input$other_sep_author_csv)})
  observeEvent(
    input$valid_table, {#une fois que la table est valide l'analyse se met en marche 
      if(input$file1$name %in% reactive_values$privious_datapath_csv){ # detection de doublon de fichier 
        showModal(modalDialog(
          title = "File already analysed",
          "This file as been analysed already. Please go see the result in wordcloud and network sessions.",
          easyClose = TRUE,
          footer = NULL
        ))
        reactive_values$ok_analyse=FALSE
      }else{
        reactive_values$privious_datapath_csv=c(reactive_values$privious_datapath_csv,input$file1$name)
        reactive_values$ok_analyse=TRUE
      }
      
      
      if(reactive_values$ok_analyse==TRUE){# si il n'y a pas d'erreur on fait l'analyse de mot clef et domaine 
        # on commence par faire une table qui sera comune au different type de fichier, ou on reporte les differente colonnes choisis par l'utilisateur  
        
        if(input$keyword_selection!="none") col_key<-reactive_values$df_csv[[input$keyword_selection]] else col_key=NA 
        if(input$domain_selection!="none") col_dom<-reactive_values$df_csv[[input$domain_selection]] else col_dom=NA
        if(input$doi_selection!="none") col_doi<-reactive_values$df_csv[[input$doi_selection]] else col_doi=NA
        
        # mise en forme de la date
        col_date<-parse_date_time(x = reactive_values$df_csv[[input$date_selection]],
                                  orders = reactive_values$fmts,
                                  locale =  Sys.getlocale(category = "LC_TIME"))
        
        
        
        col_title<-reactive_values$df_csv[[input$title_selection]]
        col_auth<-reactive_values$df_csv[[input$author_selection]]
        if(input$id_arxiv_selection!="none") arxiv_col=reactive_values$df_csv[[input$id_arxiv_selection]] else arxiv_col=NA
        
        if(is.null(reactive_values$df_global)==TRUE) {# si la table est vide (pas d'autre ficher valider avant, il faut la cree)
          
          reactive_values$df_global=as.data.frame(cbind(col_title,col_auth,col_key,col_dom,col_date,col_doi),stringsAsFactors = FALSE)
          names(reactive_values$df_global)<-c('titre','auteur','keywords','domain','date','doi')
          reactive_values$df_global["date"]=col_date
          reactive_values$df_global["source"]="CSV"
          reactive_values$df_global["position_name"]=input$position_name_CSV#♦ dans les deux car traitement de pusieur fichier qui peuvent avoir des odre différent
          reactive_values$df_global["sep"]=input$sep_author_csv
          reactive_values$df_global["id_arxiv"]=arxiv_col
          
        }else {# si elle est deja cree (autre fichier csv ou bib ou pdf valider avant  ) on cree un dataframe temporere et on la fusionne 
          #print("je passe dans le else")
          temp=as.data.frame(cbind(col_title,col_auth,col_key,col_dom,col_date,col_doi),stringsAsFactors = FALSE)
          names(temp)<-c('titre','auteur','keywords','domain','date','doi')
          temp["date"]=col_date
          temp["source"]="CSV"
          temp["position_name"]=input$position_name_CSV
          temp["sep"]=input$sep_author_csv
          temp["year"]<-NA
          temp["id_arxiv"]=arxiv_col
          reactive_values$df_global=rbind(reactive_values$df_global,temp)
          
        }
        reactive_values$valide_csv<-TRUE
        
        
      }
    })
  
  #traitment keyword----
  observe({ if(reactive_values$valide_csv==TRUE || reactive_values$valide_pdf==TRUE || reactive_values$valide_wos==TRUE){
    # a chaque ajout de donnes on refais l'etude 
  
    if(sum(duplicated(reactive_values$df_global[c("titre","auteur")]))>0) reactive_values$df_global<-reactive_values$df_global[-(which(duplicated(reactive_values$df_global[c("titre","auteur")])==TRUE)),]#supprésion des doublons 
    reactive_values$df_global=reactive_values$df_global[order(reactive_values$df_global[["doi"]],na.last = TRUE,decreasing = TRUE),]#on tri par le doi, utile pour l'utilisation des api 
    
    reactive_values$df_global[["keywords"]]=gsub(";",",",gsub(" and ",",",reactive_values$df_global[["keywords"]]))
    reactive_values$df_global[["domain"]]=gsub(";",",",reactive_values$df_global[["domain"]])
    
    
    keywords<-reactive_values$df_global[["keywords"]]
    
    
    
    
    #primary_domaine<-reactive_values$df_csv["primaryDomain_s"]
    domainall<-reactive_values$df_global[["domain"]]
    
    dict_lang="en_GB"#on se fixe d'abord en anglais , diff?rent selon dico
    lang<-"en"
    
    
    
    
    
    
    year<-as.double(format(reactive_values$df_global[["date"]],"%Y"))
    
    
    reactive_values$df_global[["year"]]<-as.factor(year)
    if(max(year,na.rm = TRUE)-min(year,na.rm = TRUE)!=0){# on met a jour les parametre des graphiques 
      updateSelectInput(session, inputId = "intervalyear",choices =c("Full year",1:(max(year,na.rm = TRUE)-min(year,na.rm = TRUE))))
      updateSelectInput(session, inputId = "networkintervalyear",choices =c("Full year",1:(max(year,na.rm = TRUE)-min(year,na.rm = TRUE))))
    }else{
      updateSelectInput(session, inputId = "intervalyear",choices =c("Full year",1:1))
      updateSelectInput(session, inputId = "networkintervalyear",choices =c("Full year",1:1))
    }
    
    
    keywords_tok<-strsplit(keywords,",",fixed=TRUE)# mise en forme des mot clef , tokanisation
    
    # lematisation
    keywords_lem<-sapply(1:length(keywords_tok),FUN =function(x,l=lang) {stemDocument(keywords_tok[[x]],language = l)})
    # tous se qui suis va permete de completer la lematisation par la version la plus courte du mot entier , cela evite les faute d'ortographe dans les graphiques
    c1<-unique(unlist(keywords_tok))
    
    c2<-sapply(1:length(c1)[1],FUN =function(x,l=lang) {stemDocument(c1[x],language = l)})
    
    
    if(is_empty(which(c2==""))==FALSE){
      c2<-c2[-(which(c2==""))]
    }
    
    if(is_empty(which(c1==""))==FALSE){
      c1<-c1[-(which(c1==""))]
    }
    
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
    
    if(reactive_values$valide_csv==TRUE || reactive_values$valide_pdf==TRUE || reactive_values$valide_wos==TRUE){
      print("inthe if graphic")
      showModal(modalDialog(
        title = "Graphics are build up",
        "Results are ready on Word cloud graphics and Network graphics pages",
        easyClose = TRUE,
        footer = NULL
      ))
      
    }
    
    reactive_values$valide_csv=FALSE
    reactive_values$valide_pdf=FALSE
    reactive_values$valide_wos=FALSE
    
    #________________________travail sur netwooork ___________________________________________________________________
    
    observeEvent({c(
      input$networkintervalyear,
      input$networktop,
      input$supones,
      input$root,
      input$domain)
    },{
      
      error=tryCatch({
        if(input$domain==FALSE){
          reactive_values$graph=make_network_graph(keywords_lem_complet,year,top_number=input$networktop,interval_year=input$networkintervalyear,sup_ones=input$supones,root_weight = input$root,domain = FALSE)
        }else{
          reactive_values$graph=make_network_graph(domainall,year,top_number=input$networktop,interval_year=input$networkintervalyear,sup_ones=input$supones,root_weight = input$root,domain = TRUE)
          
        }
        
        
        
      },
      
      error=function(cond){
        return(NA)
      })
      if(is.na(error)){
        showModal(modalDialog(
          title = "Network critical error ",
          "error during the realisation of network, keywords maybe corrupted. Please check you keyword coulmns or your bibtxt file.",
          easyClose = TRUE,
          footer = NULL
        ))
        
      }else {
        
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
                targets = "[0,4]" ,render = JS(
                  "function(data, type, row, meta) {",
                  "return type === 'display' && data.length > 70 ?",
                  "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                  "}")
              ))))
              return(table_data)
              
            }
          })
        })
      }
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
        
        for (i in 1:iter) {
          local({
            my_i <- i
            print(my_i)
            plotname <- paste("wplot", my_i, sep="")
            by_year=FALSE
            add_title=""
            p_res=c()
            
            
            
            
            
            if(my_i!=iter){# si on est pas sur la derni?re date
              index_year<-((year>=min(year,na.rm = TRUE)+(my_i-1)*as.numeric(input$intervalyear)) &(year<min(year,na.rm = TRUE)+my_i*as.numeric(input$intervalyear)))
              add_brack="["
              
            }else{
              index_year<-((year>=min(year,na.rm = TRUE)+(my_i-1)*as.numeric(input$intervalyear)) &(year<=min(year,na.rm = TRUE)+my_i*as.numeric(input$intervalyear)))
              add_brack="]"
            }
            #print(table(year[index_year]))
            key_c<-keywords_lem_complet[index_year]
            
            
            
            vrac<-unlist(key_c)
            if( length(which(vrac=="")!=0  )){
              vrac<-vrac[-( which(vrac==""))]
            }
            
            if( length(which(vrac=="NULL")!=0  )){
              vrac<-vrac[-( which(vrac=="NULL"))]
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
          
          if( length(which(vrac=="NULL")!=0  )){
            vrac<-vrac[-( which(vrac=="NULL"))]
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
              updateSliderInput(session, inputId = "minfreq",min=1,max=max(t_freq),step = 1)
              wordcloud(words = t_word, freq = t_freq, min.freq = inputreal,
                        max.words=input$maxprint, random.order=FALSE, rot.per=0.35,
                        colors=brewer.pal(8, "Dark2"),scale = c(1.5, 0.3))
              
              suppressWarnings(titre<-paste("top",input$maxprint,"keywords",add_title,",in whole dataset( minimum frequency:",input$minfreq,")"))
              mtext(titre,side=2)
              
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
    reactive_values$path_folder <-  choose.dir(default = "", caption = "Select folder with pdf file you want to add")
    # print(reactive_values$path_folder )
    
    
    reactive_values$show_pdf_valid <- TRUE
    if(is.na(reactive_values$path_folder)){
      showModal(modalDialog(
        title = "Invalid path",
        "Please select a good path, this one semms to be false",
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
        title = "File already analysed",
        "This file as been analysed already.Please go see the result in wordcloud and network tab.",
        easyClose = TRUE,
        footer = NULL
      ))
      reactive_values$ok_analyse=FALSE
    }else{
      reactive_values$privious_datapath_pdf=c(reactive_values$privious_datapath_pdf,reactive_values$path_folder)
      reactive_values$ok_analyse=TRUE
    }
    if( reactive_values$ok_analyse==TRUE){
      
      col_date<-parse_date_time(x=reactive_values$df_pdf[["date"]],
                                orders = reactive_values$fmts,
                                locale =  Sys.getlocale(category = "LC_TIME"))
      
      if(is.null(reactive_values$df_global)==TRUE) {
        
        reactive_values$df_global=reactive_values$df_pdf
        names(reactive_values$df_global)<-c('titre','auteur','keywords','domain','date')
        
        
        reactive_values$df_global["source"]="PDF"
        reactive_values$df_global["date"]=col_date
        reactive_values$df_global["position_name"]=input$position_name_pdf
        reactive_values$df_global["sep"]=input$sep_author_pdf
        reactive_values$df_global["id_arxiv"]=NA
        
        
        #write.csv(reactive_values$df_global,"~/R_programs/interface_shiny/test/File Name.csv", row.names = FALSE)
      }else{
        temp=reactive_values$df_pdf
        temp["year"]<-NA
        names(temp)<-c('titre','auteur','keywords','domain','date','year')
        temp["source"]="PDF"
        temp["position_name"]=input$position_name_pdf
        temp["sep"]=input$sep_author_pdf
        
        temp["date"]=col_date
        temp["id_arxiv"]=NA
        
        reactive_values$df_global=rbind(reactive_values$df_global,temp)
        print("hors du else ")
        #browser()
        
      }
      #reactive_values$df_global[["keywords"]]=gsub(";",",",reactive_values$df_global[["keywords"]])
      #reactive_values$df_global[["domain"]]=gsub(";",",",reactive_values$df_global[["domain"]])
      
      reactive_values$valide_pdf<-TRUE
    }
  })
  
  ####________________importation wos ou bib ___________________________________________________________
  observeEvent(c(input$file2),{
    error=NULL
    observeEvent(input$is_wos,{
      library(bibliometrix)
      error=tryCatch({
        if(input$is_wos==TRUE){
          
          suppressWarnings(reactive_values$data_wos <-convert2df(input$file2$datapath,dbsource = "wos",format = "bibtex"))# nouvel données wos(ou bib)
          reactive_values$data_wos <-df_flatten(conforme_bibtext(reactive_values$data_wos,data_base = "WOS"))# methode de mise en form wos 
          
          
          
        }else{
          suppressWarnings(reactive_values$data_wos <-(bib2df::bib2df(input$file2$datapath, separate_names = FALSE)))
          reactive_values$data_wos <-df_flatten(conforme_bibtext(reactive_values$data_wos,data_base = "BIB"))#mthode de mise en forme bib 
          
        }
        
      },
      
      error=function(cond){
        return(NA)
      })
      
      
      if(length(error)==1){ 
        if(is.na(error)){
          showModal(modalDialog(
            title = "Importation probleme",
            "error during importation the data maybe inapropriate. Try to change the methode by select or unselect the wos methode",
            easyClose = TRUE,
            footer = NULL
          ))
        }
      }else {
        # print(names(reactive_values$data_wos))
        #[c(2,4),c("TI","AU","DE","SC","PY")]
        output$table_wos <- renderDataTable({
          # validate(
          #   need(reactive_values$data_wos, "No bibtext data"),
          #   need(!is.null(reactive_values$data_wos), "")
          # )
          
          if(input$disp_wos == "head") {
            return(datatable(reactive_values$data_wos[c(1,2),], options = list(scrollX = TRUE, columnDefs = list(list(
              targets = "_all",render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 70 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                "}")
            )))))
          }
          else {
            table_data=datatable(reactive_values$data_wos, options = list(scrollX = TRUE, columnDefs = list(list(
              targets = "_all" ,render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 70 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                "}")
            ))))
            return(table_data)
          }
          
          
          
        })
        
      }
      
      
      
      reactive_values$show_wos_valid<-TRUE
      
    })
  })
  observeEvent(input$valid_wos, {
    
    if(input$file2$name %in% reactive_values$privious_datapath_wos){
      showModal(modalDialog(
        title = "file already analysed",
        "This file as been analysed already. Please go see the result in wordcloud and network tab.",
        easyClose = TRUE,
        footer = NULL
      ))
      reactive_values$ok_analyse=FALSE
      
    }else{
      reactive_values$privious_datapath_wos=c(reactive_values$privious_datapath_wos,input$file2$name)
      reactive_values$ok_analyse=TRUE
    }
    if( reactive_values$ok_analyse==TRUE){
      print("validation du wos")
      if(input$is_wos==TRUE){#si le fichier bib est un fichier wos 
        reactive_values$data_wos=reactive_values$data_wos[,c("TI","AU","DE","SC","PY","CR","DI")]
        if(input$sup_wos_for_ref==TRUE){# si on veut les reference
          
          reactive_values$wos_data=rbind(reactive_values$wos_data,reactive_values$data_wos)# agregation des données wos 
          reactive_values$ref_wos=extract_ref_wos(reactive_values$wos_data)
          
          reactive_values$show_wos_res_window=TRUE
          output$table_data_ref4 <- renderDataTable({
             validate(
               need(dim(reactive_values$ref_wos)[1]>0, "No references bibtext or wos")
             )
            
            table_data=datatable(df_flatten(reactive_values$ref_wos), options = list(scrollX = TRUE, columnDefs = list(list(
              targets = "_all" ,render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 70 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                "}")
            ))))
            
            
          })
          
        }
        
        reactive_values$data_wos<-reactive_values$data_wos[,-which(names(reactive_values$data_wos)=="CR")]
        #on enleve la colonne cr pour ne pas quel gene la suite des opération 
      }else{
        reactive_values$data_wos=reactive_values$data_wos[,c("TITLE","AUTHOR","KEYWORDS","RESARCH.AREAS","YEAR","DOI")]
        #names(reactive_values$data_wos)<-c('titre','auteur','keywords','domain','date')  
        
      }
      
      if(is.null(reactive_values$df_global)==TRUE) {
        
        
        reactive_values$df_global=reactive_values$data_wos
        names(reactive_values$df_global)<-c('titre','auteur','keywords','domain','date',"doi")
        
        if(input$sup_wos_for_ref==TRUE){#doublons avex temps car chaque fichier est indépendant et pas forcément de même source 
          reactive_values$df_global["source"]="WOS"# la source n'est importante que si on enl?ve les r?f?rences du wos 
          print("passage source wos")
        }else{
          reactive_values$df_global["source"]="BIB"# la source n'est importante que si on enl?ve les r?f?rences du wos
          print("passage source wos")
        } 
        
        
        
        reactive_values$df_global["sep"]=";"
        reactive_values$df_global["position_name"]=input$position_name_wos
        reactive_values$df_global["id_arxiv"]=NA
        
        
        col_date<-parse_date_time(x = reactive_values$df_global[["date"]],
                                  orders = reactive_values$fmts,
                                  locale =  Sys.getlocale(category = "LC_TIME"))
        
        reactive_values$df_global["date"]=col_date
        
        print("je passe dans le wos ")
      }else{
        
        temp=reactive_values$data_wos
        names(temp)<-c('titre','auteur','keywords','domain','date',"doi")
        temp["year"]<-NA
        temp["position_name"]=input$position_name_wos
        temp["sep"]=";"
        temp["id_arxiv"]=NA
        if(input$sup_wos_for_ref==TRUE){
          temp["source"]="WOS"# la source n'est importante que si on enl?ve les r?f?rences du wos 
        }else{
          temp["source"]="BIB"# la source n'est importante que si on enl?ve les r?f?rences du wos
        } 
        
        
        
        #browser()
        col_date<-parse_date_time(x = temp[["date"]],
                                  orders = reactive_values$fmts,
                                  locale =  Sys.getlocale(category = "LC_TIME"))
        temp["date"]=col_date
        reactive_values$df_global=rbind(reactive_values$df_global,temp)
        print("je passe dans le else wos ")
      }
      
      #reactive_values$df_global[["keywords"]]=gsub(";",",",reactive_values$df_global[["keywords"]])
      #reactive_values$df_global[["domain"]]=gsub(";",",",reactive_values$df_global[["domain"]])
      
      
      reactive_values$valide_wos=TRUE
      
    }
  })
  
  
  
  # tavaillle sur les donnée citation reference ? _______________________________________________________________
  observeEvent(c(input$ads, input$lens,input$pubmed),{ # observe event permettant de gerrer les token et les particularité d'admission des base 
    reactive_values$pubmed<-input$pubmed #initialise se value but modifier after with lens value 
    if(input$ads==TRUE){
      reactive_values$show_token_ads <- TRUE
    }else {
      reactive_values$show_token_ads <- FALSE
    }
    if(input$lens==TRUE){
      reactive_values$show_token_lens <- TRUE
      if(reactive_values$pubmed==TRUE){
        showModal(modalDialog(
          title = "Lens database contain pubmed database",
          "Lens contain pubmed, no need to select it if lens is selected.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      reactive_values$show_pumed_box=FALSE
      reactive_values$pubmed<-FALSE# if lens =no pubmed 
      print(reactive_values$pubmed)
    }else{
      reactive_values$show_token_lens <- FALSE
      print(reactive_values$pubmed)
      reactive_values$show_pumed_box=TRUE
    }
  })
  observeEvent(input$arxiv,{
    if(input$arxiv==TRUE &&  length( reactive_values$privious_datapath_csv>0)){
      reactive_values$show_id_arxiv <- TRUE
    }else {
      reactive_values$show_id_arxiv <- FALSE
    }
    
  })
  
  
  observeEvent(input$valid_DB, { 
    if(input$ads==FALSE && reactive_values$pubmed==FALSE && input$arxiv==FALSE && input$lens==FALSE ){
      showModal(modalDialog(
        title = "Invalid entry",
        "No database selected, Please select at least one database",
        easyClose = TRUE,
        footer = NULL
      ))
      reactive_values$ok_analyse=FALSE
    }else {
      if(is.null(reactive_values$df_global)==TRUE){
        showModal(modalDialog(
          title = "Invalid data",
          "no  data upload. Please add a file to the analysis.",
          easyClose = TRUE,
          footer = NULL
        ))
      }else {
        reactive_values$ok_analyse=TRUE
      }
      
    }
    
    if(reactive_values$ok_analyse==TRUE){
      
      if(dim(reactive_values$df_global)[1]>=10000){
        showModal(modalDialog(
          title = "Warning exceed capacity ",
          "you have a dataframe of 10000 entries or more, the app can't analyse that much data, only the 10000 first will be analysed",
          easyClose = TRUE,
          footer = NULL
        ))
        reactive_values$df_global=as.data.frame(reactive_values$df_global[10000,],stringsAsFactors = FALSE)
      }
      if(input$type=="ref" ||input$type=="all" ) reactive_values$show_ref=TRUE
      if(input$type=="cit" ||input$type=="all" ) reactive_values$show_cit=TRUE
      if(input$ads==TRUE){
        if(input$token_ads==""){
          showModal(modalDialog(
            title = "Invalid token",
            "token empty",
            easyClose = TRUE,
            footer = NULL
          ))
        }else {
          reactive_values$show_ads_res_window=TRUE
          reactive_values$res_ads=extraction_data_api_nasa_ads(data_pub=reactive_values$df_global,ti_name="titre",au_name="auteur",doi_name = "doi",token=input$token_ads,pas=8,value_same_min_accept=reactive_values$value_same_min_accept,value_same_min_ask = reactive_values$value_same_min_ask,type =input$type,source_name = "source",sep_vector_in_data ="sep",position_vector_in_data = "position_name")
        }
      }
      if(input$arxiv==TRUE){
        
        reactive_values$show_arxiv_res_window=TRUE
        #if(input$sup_wos_for_ref==TRUE){
        reactive_values$res_arxiv=extraction_data_api_arxiv(data_pub=reactive_values$df_global,ti_name="titre",au_name="auteur",pas=8,value_same_min_accept=reactive_values$value_same_min_accept,value_same_min_ask = reactive_values$value_same_min_ask,type = input$type,source_name = "source",sep_vector_in_data ="sep",position_vector_in_data = "position_name",id_name ="id_arxiv" )
        # }else{
        #   reactive_values$res_arxiv=extraction_data_api_arxiv(data_pub=reactive_values$df_global,ti_name="titre",au_name="auteur",pas=8,value_same_min_accept=reactive_values$value_same_min_accept,value_same_min_ask = reactive_values$value_same_min_ask,type = input$type,sep_vector_in_data ="sep",position_vector_in_data = "position_name",id_name = "id_arxiv")
        # }
      }
      if(reactive_values$pubmed==TRUE){
        reactive_values$show_pumed_res_window=TRUE
        reactive_values$res_pumed=extract_data_api_pumed(data_pub=reactive_values$df_global,ti_name="titre",au_name="auteur",doi_name="doi",pas=8,value_same_min_accept=reactive_values$value_same_min_accept, value_same_min_ask=reactive_values$value_same_min_ask,type = input$type,source_name = "source",sep_vector_in_data ="sep",position_vector_in_data = "position_name")
        
      }
      if(input$lens==TRUE){
        if(input$token_lens==""){
          showModal(modalDialog(
            title = "Invalid token",
            "token empty on lens",
            easyClose = TRUE,
            footer = NULL
          ))
        }else { 
        print("passe")
        reactive_values$show_lens_res_window=TRUE
        reactive_values$res_lens=extraction_data_api_lens(data_pub=reactive_values$df_global,ti_name="titre",au_name="auteur",doi_name = "doi",pas=8,value_same_min_accept=reactive_values$value_same_min_accept, value_same_min_ask=reactive_values$value_same_min_ask,type = input$type,source_name = "source",sep_vector_in_data ="sep",position_vector_in_data = "position_name",token = input$token_lens)
        #browser("arret pour verifier ce qui a dans lens ")
      }
    }  
      #mark3
      reactive_values$cal_temp=length(reactive_values$res_ads$dataframe_citation_accept$`cited identifier`)+
        length(reactive_values$res_ads$dataframe_ref_accept$`refering identifier`)+
        length(reactive_values$res_arxiv$res_citation_accept$`cited identifier`)+
        length(reactive_values$res_arxiv$res_reference_accept$`refering identifier`)+
        length(reactive_values$res_pumed$dataframe_citation_accept$`cited identifier`)+
        length(reactive_values$res_pumed$dataframe_ref_accept$`refering identifier`)+
        length(reactive_values$res_lens$dataframe_citation_accept$`cited identifier`)+
        length(reactive_values$res_lens$dataframe_ref_accept$`refering identifier`)
      if(reactive_values$cal_temp>=1){
        showModal(modalDialog(
          title = "research ended",
          "research ended you can press the calcul interdisciplinarity boutton.",
          easyClose = TRUE,
          footer = NULL
        ))
      }else{
        showModal(modalDialog(
          title = "research ended",
          "the research ended but no resultat was found.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
    }
    # checkboxInput("ads", "ADS", FALSE),
    #
    # checkboxInput("pubmed", "Pubmed", FALSE),
    # checkboxInput("arxiv", "ArXiv", FALSE),
    # actionButton("valid_DB", "Process")
    #  reactive_values$df_global
    
  }) 
  
  
  
  observeEvent(input$ads_ref_accept,{
    print("ref")
    output$table_data_ref1 <- renderDataTable({
      # validate(
      #   need(reactive_values$data_wos, "No bibtext data"),
      #   need(!is.null(reactive_values$data_wos), "")
      # )
      
      table_data=datatable(df_flatten(reactive_values$res_ads$dataframe_ref_accept), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "[1,2,5,6,7]" ,render = JS(
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
      
      table_data=datatable(df_flatten(reactive_values$res_ads$dataframe_citation_accept), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "[1,2,5,6,7]" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))
      
      
    })
  })
  
  observeEvent({c(input$ads_res_publi)},{
    
    output$table_data_ref1 <- renderDataTable({
      
       validate(
       need(dim(reactive_values$res_ads$dataframe_publi_found)[1]>0, "No publication founded")
      #   need(!is.null(reactive_values$data_wos), "")
       )
      
      table_data=datatable(df_flatten(reactive_values$res_ads$dataframe_publi_found), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "_all" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))
      return(table_data)
      
    })
  },ignoreInit = TRUE)
  
  
  observeEvent(input$ads_error,{
    output$table_data_ref1 <- renderDataTable({
      validate(
         need(dim(reactive_values$res_ads$error_querry_publi)[1]>0, "No error")
      #   need(!is.null(reactive_values$data_wos), "")
       )

      table_data=datatable(df_flatten(reactive_values$res_ads$error_querry_publi), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "_all" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))
      
      
    })
  })
  
  observeEvent({
    c(input$ads_ask,
      reactive_values$transfer_done$ads)}
    ,{
        reactive_values$table_to_show_ref=reactive_values$res_ads$dataframe_publi_found[(reactive_values$res_ads$dataframe_publi_found$check_title_pct<reactive_values$value_same_min_accept) &(reactive_values$res_ads$dataframe_publi_found$check_title_pct>=reactive_values$value_same_min_ask),]
        if(dim(reactive_values$table_to_show_ref)[1]>0) rownames(reactive_values$table_to_show_ref)<-1:nrow(reactive_values$table_to_show_ref)
        
        if(!is.null(reactive_values$transfer_done$ads)){ 
          
          ind_temp=reactive_values$table_to_show_ref$bibcode%in%reactive_values$transfer_done$ads
          reactive_values$table_to_show_ref=reactive_values$table_to_show_ref[!ind_temp,]  
        }
        
        df <-reactiveValues(data =cbind(Actions = shinyInput( FUN = actionButton, n=nrow(reactive_values$table_to_show_ref), id='button_', label = "Transfer",  onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})' ),reactive_values$table_to_show_ref
        ) )
        
        output$table_data_ref1<- DT::renderDataTable(
          validate(
            need(dim(as.data.frame(df$data,stringsAsFactors = FALSE))[1]>0, "No publication with doute")
          #   need(!is.null(reactive_values$data_wos), "")
          ), 
         df_flatten(as.data.frame(df$data,stringsAsFactors = FALSE)), escape = FALSE, options = list( lengthMenu = c(5, 25, 50), pageLength = 25,
                                                                                                       
                                                                                                       scrollX = TRUE, columnDefs = list(list(
                                                                                                         targets = "_all" ,render = JS(
                                                                                                           "function(data, type, row, meta) {",
                                                                                                           "return type === 'display' && data.length > 200 ?",
                                                                                                           "'<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>' : data;",
                                                                                         "}")
                                                                                                       )))
        )
        reactive_values$active_source="ADS"
      
    },ignoreInit = TRUE)
  
  observeEvent(input$select_button, {
    
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    if(reactive_values$active_source=="ADS"){
      if(input$type=="cit"||input$type=="all"){
        ind=which(reactive_values$table_to_show_ref$bibcode[[selectedRow]]==unlist(reactive_values$res_ads$dataframe_citation_ask$`cited identifier`))
        ind2=which(reactive_values$table_to_show_ref$bibcode[[selectedRow]]==unlist(reactive_values$res_ads$dataframe_citation_accept$`cited identifier`))# il  dej  été ajouter
        if(length(ind2)==0) if(!is.null(dim(reactive_values$res_ads$dataframe_citation_ask[ind,])[1])) if(dim(reactive_values$res_ads$dataframe_citation_ask[ind,])[1]>0) reactive_values$res_ads$dataframe_citation_accept=rbind(reactive_values$res_ads$dataframe_citation_accep,reactive_values$res_ads$dataframe_citation_ask[ind,])
      }
      
      if(input$type=="ref"||input$type=="all"){
        ind_ref_1=which(reactive_values$table_to_show_ref$bibcode[[selectedRow]]==unlist(reactive_values$res_ads$dataframe_ref_ask$`refering identifier`))# ligne a ajouter
        ind_ref_2=which(reactive_values$table_to_show_ref$bibcode[[selectedRow]]==unlist(reactive_values$res_ads$dataframe_ref_accept$`refering identifier`))# ligne déja ajouter
        if(length(ind_ref_2)==0)if(!is.null(dim(reactive_values$res_ads$dataframe_ref_ask[ind_ref_1,])[1]))  if(dim(reactive_values$res_ads$dataframe_ref_ask[ind_ref_1,])[1]>0)reactive_values$res_ads$dataframe_ref_accept=rbind(reactive_values$res_ads$dataframe_ref_accept,reactive_values$res_ads$dataframe_ref_ask[ind_ref_1,])
      }
      reactive_values$transfer_done$ads=c(reactive_values$transfer_done$ads,reactive_values$table_to_show_ref$bibcode[[selectedRow]])  
          
    }
    
    if(reactive_values$active_source=="ARXIV"){
      if(input$type=="cit"||input$type=="all"){
        ind=which(reactive_values$table_to_show_ref$abs_link[[selectedRow]]==unlist(reactive_values$res_arxiv$res_citation_ask$`cited identifier`))
        ind2=which(reactive_values$table_to_show_ref$abs_link[[selectedRow]]==unlist(reactive_values$res_arxiv$res_citation_accept$`cited identifier`))# il  dej  été ajouter
        if(length(ind2)==0) if(!is.null(dim(reactive_values$res_arxiv$res_citation_ask[ind,])[1])) if(dim(reactive_values$res_arxiv$res_citation_ask[ind,])[1]>0) reactive_values$res_arxiv$res_citation_accept=rbind(reactive_values$res_arxiv$res_citation_accept,reactive_values$res_arxiv$res_citation_ask[ind,])
      }
      
      if(input$type=="ref"||input$type=="all"){
        ind_ref_1=which(reactive_values$table_to_show_ref$abs_link[[selectedRow]]==unlist(reactive_values$res_arxiv$res_reference_ask$`refering identifier`))# ligne a ajouter
        ind_ref_2=which(reactive_values$table_to_show_ref$abs_link[[selectedRow]]==unlist(reactive_values$res_arxiv$res_reference_accept$`refering identifier`))# ligne déja ajouter
        
        
        if(length(ind_ref_2)==0) if(!is.null(dim(reactive_values$res_arxiv$res_reference_ask[ind_ref_1,])[1])) if(dim(reactive_values$res_arxiv$res_reference_ask[ind_ref_1,])[1]>0) reactive_values$res_arxiv$res_reference_accept=rbind(reactive_values$res_arxiv$res_reference_accept,reactive_values$res_arxiv$res_reference_ask[ind_ref_1,])
      }
      reactive_values$transfer_done$arxiv=c(reactive_values$transfer_done$arxiv,reactive_values$table_to_show_ref$abs_link[[selectedRow]]) 
    }  
    
    if(reactive_values$active_source=="PUBMED"){
      if(input$type=="cit"||input$type=="all"){
        ind=which(reactive_values$table_to_show_ref$id[[selectedRow]]==unlist(reactive_values$res_pumed$dataframe_citation_ask$`cited identifier`))
        ind2=which(reactive_values$table_to_show_ref$id[[selectedRow]]==unlist(reactive_values$res_pumed$dataframe_citation_accept$`cited identifier`))# il  dej  été ajouter
        if(length(ind_2)==0)  if(dim(reactive_values$res_pumed$dataframe_citation_ask[ind,])[1]) if(dim(reactive_values$res_pumed$dataframe_citation_ask[ind,])[1]>0)reactive_values$res_pumed$dataframe_citation_accept=rbind(reactive_values$res_pumed$dataframe_citation_accep,reactive_values$res_ads$dataframe_citation_ask[ind,])      
      }
      if(input$type=="ref"||input$type=="all"){
        ind_ref_1=which(reactive_values$table_to_show_ref$id[[selectedRow]]==unlist(reactive_values$res_pumed$dataframe_ref_ask$`refering identifier`))# ligne a ajouter
        ind_ref_2=which(reactive_values$table_to_show_ref$id[[selectedRow]]==unlist(reactive_values$res_pumed$dataframe_ref_accept$`refering identifier`))# ligne déja ajouter
        if(length(ind_ref_2)==0) if(dim(reactive_values$res_pumed$dataframe_ref_ask[ind_ref_1,])[1])  if(dim(reactive_values$res_pumed$dataframe_ref_ask[ind_ref_1,])[1]>0) reactive_values$res_pumed$dataframe_ref_accept=rbind(reactive_values$res_pumed$dataframe_ref_accept,reactive_values$res_ads$dataframe_ref_ask[ind_ref_1,])
      }
      reactive_values$transfer_done$pumed=c(reactive_values$transfer_done$pumed,reactive_values$table_to_show_ref$id[[selectedRow]])   
    }
    
    if(reactive_values$active_source=="LENS"){
      if(input$type=="cit"||input$type=="all"){
        ind=which(reactive_values$table_to_show_ref$id[[selectedRow]]==unlist(reactive_values$res_pumed$dataframe_citation_ask$`cited identifier`))
        ind2=which(reactive_values$table_to_show_ref$id[[selectedRow]]==unlist(reactive_values$res_pumed$dataframe_citation_accept$`cited identifier`))# il  dej  été ajouter
        if(length(ind_2)==0)  if(dim(reactive_values$res_lens$dataframe_citation_ask[ind,])[1]) if(dim(reactive_values$res_lens$dataframe_citation_ask[ind,])[1]>0)reactive_values$res_lens$dataframe_citation_accept=rbind(reactive_values$res_lens$dataframe_citation_accep,reactive_values$res_ads$dataframe_citation_ask[ind,])      
      }
      if(input$type=="ref"||input$type=="all"){
        ind_ref_1=which(reactive_values$table_to_show_ref$id[[selectedRow]]==unlist(reactive_values$res_lens$dataframe_ref_ask$`refering identifier`))# ligne a ajouter
        ind_ref_2=which(reactive_values$table_to_show_ref$id[[selectedRow]]==unlist(reactive_values$res_lens$dataframe_ref_accept$`refering identifier`))# ligne déja ajouter
        if(length(ind_ref_2)==0) if(dim(reactive_values$res_lens$dataframe_ref_ask[ind_ref_1,])[1])  if(dim(reactive_values$res_lens$dataframe_ref_ask[ind_ref_1,])[1]>0) reactive_values$res_lens$dataframe_ref_accept=rbind(reactive_values$res_lens$dataframe_ref_accept,reactive_values$res_ads$dataframe_ref_ask[ind_ref_1,])
      }
      reactive_values$transfer_done$pumed=c(reactive_values$transfer_done$pumed,reactive_values$table_to_show_ref$id[[selectedRow]])   
    }  
  })
  
  
  
  
  
  ###############################arxvie ------------------------
  observeEvent(input$arxiv_ref_accept,{
    output$table_data_ref2 <- renderDataTable({
      # validate(
      #   need(reactive_values$data_wos, "No bibtext data"),
      #   need(!is.null(reactive_values$data_wos), "")
      # )
      
      table_data=datatable(df_flatten(as.data.frame(reactive_values$res_arxiv$res_reference_accept),stringsAsFactors = FALSE), options = list(scrollX = TRUE, columnDefs = list(list(
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
      table_data=datatable(df_flatten(reactive_values$res_arxiv$res_citation_accept), options = list(scrollX = TRUE, columnDefs = list(list(
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
      table_data=datatable(df_flatten(reactive_values$res_arxiv$error_querry), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "_all" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))
      
      
    })
  })
  observeEvent({
    c(
      input$arxiv_ask,
      reactive_values$transfer_done$arxiv
    )},{
      if(input$arxiv_ask!=0){
        reactive_values$table_to_show_ref=reactive_values$res_arxiv$res_publi_foundt[(reactive_values$res_arxiv$res_publi_foundt$check_pct<reactive_values$value_same_min_accept),]
        
        
        
        if(dim(reactive_values$table_to_show_ref)[1]>0) rownames(reactive_values$table_to_show_ref)<-1:nrow(reactive_values$table_to_show_ref)
        
        if(!is.null(reactive_values$transfer_done$arxiv)){ 
          
          ind_temp=reactive_values$table_to_show_ref$abs_link%in%reactive_values$transfer_done$arxiv
          reactive_values$table_to_show_ref=reactive_values$table_to_show_ref[!ind_temp,]  
        }
        
        
        df <- reactiveValues(data =cbind(Actions = shinyInput( FUN = actionButton, n=nrow(reactive_values$table_to_show_ref), id='button_', label = "Transfer",  onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})' ),reactive_values$table_to_show_ref
        ) )
        
        output$table_data_ref2<- DT::renderDataTable(
          df_flatten(as.data.frame(df$data,stringsAsFactors = FALSE)), escape = FALSE, options = list( lengthMenu = c(5, 25, 50), pageLength = 25,
                                                                                                       
                                                                                                       scrollX = TRUE, columnDefs = list(list(
                                                                                                         targets = "_all" ,render = JS(
                                                                                                           "function(data, type, row, meta) {",
                                                                                                           "return type === 'display' && data.length > 200 ?",
                                                                                                           "'<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>' : data;",
                                                                                                           "}")
                                                                                                       )))
        )
        
        
        reactive_values$active_source="ARXIV"
      }
    },ignoreInit = TRUE)
  observeEvent(input$pubmed_ref_accept,{
    output$table_data_ref3 <- renderDataTable({
      # validate(
      #   need(reactive_values$data_wos, "No bibtext data"),
      #   need(!is.null(reactive_values$data_wos), "")
      # )
      #test=df_flatten(res_arxiv$res_citation_accept)
      table_data=datatable(df_flatten(reactive_values$res_pumed$dataframe_ref_accept), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "[1,2,4,5,6]" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))
      
      
    })
  })
  
  observeEvent(input$pubmed_cit_accept,{
    output$table_data_ref3 <- renderDataTable({
      
      table_data=datatable(df_flatten(reactive_values$res_pumed$dataframe_citation_accept), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "[1,2,4,5,6]" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))
      
      
    })
  })
  
   observeEvent({c(input$pubmed_res_publi)},{

    output$table_data_ref3 <- renderDataTable({

      validate(
         need(dim(reactive_values$res_pumed$dataframe_publi_found)[1]>0, "No publication founded")
  #       #   need(!is.null(reactive_values$data_wos), "")
      )
  #
      table_data=datatable(df_flatten(reactive_values$res_pumed$dataframe_publi_found), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "[1,2,3]" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))


    })
   },ignoreInit = TRUE)
  # 
  
  observeEvent(input$pubmed_error,{
    output$table_data_ref3 <- renderDataTable({
      
       validate(
         need(dim(reactive_values$res_pumed$error_querry_publi)[1]>0, "No error"),
         need(!is.na(reactive_values$res_pumed$error_querry_publi)>0, "No error")
       )
      
      table_data=datatable(df_flatten(reactive_values$res_pumed$error_querry_publi), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "_all" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))
      
      
    })
  })
  observeEvent({
    c(input$pubmed_ask,
      reactive_values$transfer_done$pumed)
  },{
      print("on rentre")
      reactive_values$table_to_show_ref=reactive_values$res_pumed$dataframe_publi_found[(reactive_values$res_pumed$dataframe_publi_found$check_title_pct<reactive_values$value_same_min_accept),]
      
      if(dim(reactive_values$table_to_show_ref)[1]>0) rownames(reactive_values$table_to_show_ref)<-1:nrow(reactive_values$table_to_show_ref)
      
      if(!is.null(reactive_values$transfer_done$pumed)){ 
        
        ind_temp=reactive_values$table_to_show_ref$id%in%reactive_values$transfer_done$pumed
        reactive_values$table_to_show_ref=reactive_values$table_to_show_ref[!ind_temp,]  
      }
      
      
      df <- reactiveValues(data =cbind(Actions = shinyInput( FUN = actionButton, n=nrow(reactive_values$table_to_show_ref), id='button_', label = "Transfer",  onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})' ),reactive_values$table_to_show_ref    ) )
      
      output$table_data_ref3<- DT::renderDataTable(
        validate(
          need(dim(as.data.frame(df$data,stringsAsFactors = FALSE))[1]>0, "No publication with doute")
          #   need(!is.null(reactive_values$data_wos), "")
        ), 
        
          df_flatten(as.data.frame(df$data,stringsAsFactors = FALSE)), escape = FALSE, options = list( lengthMenu = c(5, 25, 50), pageLength = 25,
                                                                                                     
                                                                                                     scrollX = TRUE, columnDefs = list(list(
                                                                                                       targets = "_all" ,render = JS(
                                                                                                         "function(data, type, row, meta) {",
                                                                                                         "return type === 'display' && data.length > 200 ?",
                                                                                                         "'<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>' : data;",
                                                                                                         "}")
                                                                                                     )))
      )
      
      
      reactive_values$active_source="PUBMED"
    
  },ignoreInit = TRUE,ignoreNULL = TRUE)
  
  
  
  observeEvent(input$lens_ref_accept,{
    output$table_data_ref5 <- renderDataTable({
       validate(
         need(dim(reactive_values$res_lens$dataframe_ref_accept)[1]>0, "No references founded sorry, check errors")
       )
      #test=df_flatten(res_arxiv$res_citation_accept)
      table_data=datatable(df_flatten(reactive_values$res_lens$dataframe_ref_accept), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "[1,2,4,5,6]" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))
      
      
    })
  })
  
  observeEvent(input$lens_cit_accept,{
    output$table_data_ref5 <- renderDataTable({
      validate(
        need(dim(reactive_values$res_lens$dataframe_citation_accept)[1]>0, "No citations founded sorry, check errors")
      )
      table_data=datatable(df_flatten(reactive_values$res_lens$dataframe_citation_accept), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "[1,2,4,5,6]" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))
      
      
    })
  })
  
  
  observeEvent(input$lens_error,{
    output$table_data_ref5 <- renderDataTable({
       validate(
         need(length(reactive_values$res_lens$error_querry_publi)!=0, "No data,no error")
       )
      
      table_data=datatable(df_flatten(reactive_values$res_lens$error_querry_publi), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "_all" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))
      
      
    })
  })
  
  observeEvent({c(input$lens_res_publi)},{
    
    output$table_data_ref5 <- renderDataTable({
      
       validate(
         need(dim(reactive_values$res_lens$dataframe_publi_found)[1]>1, "No publication fouded, sorry check erros")
       )
      
      table_data=datatable(df_flatten(reactive_values$res_lens$dataframe_publi_found), options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "_all" ,render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 70 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
          "}")
      ))))
      
      
    })
  },ignoreInit = TRUE)
  
  observeEvent({
    c(input$lens_ask,
      reactive_values$transfer_done$lens)
  },{
    
      reactive_values$table_to_show_ref=df_flatten(reactive_values$res_lens$dataframe_publi_found[(reactive_values$res_lens$dataframe_publi_found$check_title_pct<reactive_values$value_same_min_accept),])
      
      if(dim(reactive_values$table_to_show_ref)[1]>0) rownames(reactive_values$table_to_show_ref)<-1:nrow(reactive_values$table_to_show_ref)
      
      if(!is.null(reactive_values$transfer_done$lens)){ 
        
        ind_temp=reactive_values$table_to_show_ref$id%in%reactive_values$transfer_done$lens
        reactive_values$table_to_show_ref=reactive_values$table_to_show_ref[!ind_temp,]  
      }
      
      
      df <- reactiveValues(data =cbind(Actions = shinyInput( FUN = actionButton, n=nrow(reactive_values$table_to_show_ref), id='button_', label = "Transfer",  onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})' ),reactive_values$table_to_show_ref    ) )
      
      output$table_data_ref5<- DT::renderDataTable(
        validate(
          need(dim(as.data.frame(df$data,stringsAsFactors = FALSE))[1]>0, "No publication with doute")
          #   need(!is.null(reactive_values$data_wos), "")
        ), 
        
        df_flatten(as.data.frame(df$data,stringsAsFactors = FALSE)), escape = FALSE, options = list( lengthMenu = c(5, 25, 50), pageLength = 25,
                                                                                                     
                                                                                                     scrollX = TRUE, columnDefs = list(list(
                                                                                                       targets = "_all" ,render = JS(
                                                                                                         "function(data, type, row, meta) {",
                                                                                                         "return type === 'display' && data.length > 200 ?",
                                                                                                         "'<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>' : data;",
                                                                                                         "}")
                                                                                                     )))
      )
      
      
      reactive_values$active_source="LENS"
    
  },ignoreInit = TRUE,ignoreNULL = TRUE)
  
  
  
  
  #partie interdiciplinarité ----
  
  
  # reactive_values$journal_table_ref=read.csv("data/data_journal/table_categ_wos.csv", sep = ";",header = TRUE,stringsAsFactors = FALSE)
  # reactive_values$journal_table_ref$Source_title<-gsub("\\s*\\([^\\)]+\\)","",journal_table_ref$Full.Journal.Title)
  # reactive_values$journal_table_ref$Source_title<-gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",journal_table_ref$Full.Journal.Title)
  # #plot2
  
  observeEvent({input$valid_data_research}, {
    
    
    
    
    
    reactive_values$journal_table_ref=read.csv("data/table_categ_wos.csv", sep = ";",header = TRUE,encoding = "Latin-1",stringsAsFactors = FALSE)
    reactive_values$journal_table_ref=reactive_values$journal_table_ref[order(reactive_values$journal_table_ref[["JCR.Abbreviated.Title"]],reactive_values$journal_table_ref[["Full.Journal.Title"]],na.last = TRUE,decreasing = TRUE),]#on tri par le doi, utile pour l'utilisation des api 
    
    reactive_values$table_dist<-read.table(file="data/category_similarity_matrix.txt",header = TRUE,sep = " ",dec ="." )
    reactive_values$table_categ_gd=read.csv(file="data/categ_wos.csv",header = TRUE,stringsAsFactors = FALSE,encoding = "Latin-1",sep = ";")
    
    if(length(reactive_values$privious_datapath_csv)==0 && length(reactive_values$privious_datapath_wos)==0){
      showModal(modalDialog(
        title = "no file in analyse",
        "The is no file in the current analyse, please add some data",
        easyClose = TRUE,
        footer = NULL
      ))
      
      
    }else{
      #browser()
      
      error=tryCatch({
        res_temp<-global_merge_and_cal_interdis(ads=reactive_values$res_ads,arxiv=reactive_values$res_arxiv,pumed=reactive_values$res_pumed,wos=reactive_values$ref_wos,reactive_values$res_lens,journal_table_ref = reactive_values$journal_table_ref,table_categ_gd = reactive_values$table_categ_gd,type = input$type,table_dist =reactive_values$table_dist,col_journal=c(input$col_journal_ads,input$col_journal_arxiv,input$col_journal_pumed,input$col_journal_wos,input$col_journal_lens))  
        
      },
      error=function(cond){ 
        print("error in global")  #reactive_values$ok_analyse=FALSE
        showModal(modalDialog(
          title = paste("ERROR : in",input$type, "analyse"),
          paste0("Could be an error on the parameters choice, or else there is no",input$type,"in your data"),
          easyClose = TRUE,
          footer = NULL
        ))
        
        
        return(NULL)
      })
      
      if(!is.null(error)){
        
        if(input$type=="ref"||input$type=="all" ){    
          if(input$type=="ref"){
            reactive_values$matrice_res_ref$res=res_temp$res 
            reactive_values$matrice_res_ref$data=res_temp$data
          }else {
            reactive_values$matrice_res_ref$res=res_temp$res_ref
            reactive_values$matrice_res_ref$data=res_temp$data_ref
          }
          
          
          
          
          if(!is.null(reactive_values$matrice_res_ref$res)){
            reactive_values$secteur_is_finish<-FALSE
            updateSelectInput(session, inputId = "select_article", choices = unique(c(reactive_values$matrice_res_ref$res$prop[["IDENTIFIANT"]],reactive_values$matrice_res_cit$res$prop[["IDENTIFIANT"]])))
            #     View(reactive_values$matrice_res_ref$res$prop_grande_discipline)   
            #View(reactive_values$matrice_res_ref$res$prop)
            output$plot_article_ref<-renderPlotly({
              error=tryCatch({
                
                line=which(reactive_values$matrice_res_ref$res$prop[["IDENTIFIANT"]]%in%input$select_article)
                # print(paste0("_",input$select_article,"_")%in%unlist(reactive_values$matrice_res_ref$res$prop[["IDENTIFIANT"]]))
                
                ind=which(reactive_values$matrice_res_ref$res$prop_grande_discipline[line,]!=0)# secteur selectionner 
                title_graph=paste0("Main subjects  \n of article ",reactive_values$matrice_res_ref$res$prop[["TITLE"]][[line]])
                print(title_graph)
                df <- data.frame(
                  group = Unaccent(names(reactive_values$matrice_res_ref$res$prop_grande_discipline[line,])[ind]),
                  value = unlist(reactive_values$matrice_res_ref$res$prop_grande_discipline[line,ind])/sum(reactive_values$matrice_res_ref$res$prop_grande_discipline[line,ind])
                )
                
                
                
                
              },
              error=function(cond){
                return(NA)
              })
              if(length(error)==1){ 
                if(is.na(error)){
                  title_graph="no data"
                  df <- data.frame(
                    group = "no data",
                    value = 1)
                  
                }
              }
              
              plot_article_ref <- plot_ly(df, labels = ~group, values = ~value,key=~group, type = 'pie',source = "plot_article_ref")
              plot_article_ref <- plot_article_ref %>% layout(title = title_graph,font=list(size=8),
                                                              legend = list(font = list(size = 9)),
                                                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
              #reactive_values$plots_article_ref <- bp + coord_polar("y", start=0)+ggtitle(paste0("Main subjects  \n of article",input$select_article))
              
              
              ind_stat=which(reactive_values$matrice_res_ref$data[["refering identifier"]]==input$select_article)
              reactive_values$pct_ref[1]=mean(unlist(reactive_values$matrice_res_ref$data[ind_stat,]$refered_indice_pct_found))
              # View(reactive_values$matrice_res_ref$data[ind_stat,])
              reactive_values$secteur_is_finish<-TRUE
              return(plot_article_ref)
              
            })
            output$stat_journ_ref_article<-renderText({paste("pourcentage accuracy of graphique aticle",round(reactive_values$pct_ref[1],digits =2))})          
            # output$downloadPlot_article_ref <- downloadHandler(
            #   filename = function(){paste("test",'.pdf',sep='')},
            #   
            #   content = function(file) {
            #     ggsave(file,plot=reactive_values$plots_article_ref)
            #   })
            #   
            
            
            #View(as.data.frame(reactive_values[[paste0("data",input$periode_to_show)]][ind,]))
            
            
            
            output$plot_total_ref<-renderPlotly({
              
              
              df <- data.frame(
                group = Unaccent(names(reactive_values$matrice_res_ref$res$prop_grande_discipline)),
                value = unlist(reactive_values$matrice_res_ref$res$prop_grande_discipline[dim(reactive_values$matrice_res_ref$res$prop_grande_discipline)[1],]/sum(unlist(reactive_values$matrice_res_ref$res$prop_grande_discipline[dim(reactive_values$matrice_res_ref$res$prop_grande_discipline)[1],])))
              )
              
              
              plot_total_ref <- plot_ly(df, labels = ~group, values = ~value,key=~group, type = 'pie',source = "plot_total_ref")
              plot_total_ref <- plot_total_ref %>% layout(title = paste0("Main subjects  \n of the whole corpus"),
                                                          font=list(size=8),
                                                          legend = list(font = list(size = 9)),
                                                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
              
              
              
              reactive_values$pct_ref[2]=mean(unlist(reactive_values$matrice_res_ref$data$refered_indice_pct_found))
              
              reactive_values$secteur_is_finish<-TRUE
              
              return(plot_total_ref)
              #output$text_ref<-renderText({paste("ID:",round(reactive_values$matrice_res_ref$res$id,2),"DD;",round(reactive_values$matrice_res_ref$res$dd,2),"MD:",round(reactive_values$matrice_res_ref$res$md,2),"DIA:",paste(round(unlist(reactive_values$matrice_res_ref$res$dia[[as.numeric(line)]]),digits = 2),collapse = ","),collapse = "\n")})
              
            })
            output$stat_journ_ref_total<-renderText({paste("pourcentage accuracy of graphique total",round(reactive_values$pct_ref[2],digits = 2))})  
            states_2 <- reactiveValues(source =reactive_values$states$source, value = c(-1,-1), changed = c(FALSE,FALSE),key=NULL)
            observeEvent({c(event_data("plotly_click", source =states_2$source[[2]],priority = "event"),event_data("plotly_click", source =states_2$source[[1]],priority = "event") ) },{
              if(reactive_values$secteur_is_finish==TRUE){
                for(src in states_2$source){
                  clicked<-event_data("plotly_click", source = src)
                  
                  if( !is.null(clicked) ){
                    value <- clicked$pointNumber
                    if(states_2$value[states_2$source==src]!=value ){
                      
                      states_2$value[states_2$source==src] <- value
                      states_2$changed[states_2$source==src] <- TRUE
                      states_2$key=clicked$key
                    }
                  }
                }
                if(sum(states_2$changed)>0){
                  # pour eviter la réactualisation 2 fois de l'event quand une des variable change on passe a une autre variable non vu 
                  reactive_values$states$changed=states_2$changed
                  reactive_values$states$source=states_2$source
                  reactive_values$states$key=states_2$key
                  
                  output$table_data_ref_interdi <- renderDataTable({
                    ind_global=NULL
                    col=which(reactive_values$states$key[[1]]==Unaccent(names(reactive_values$matrice_res_ref$res$prop_grande_discipline)))
                    
                    if(reactive_values$states$source[reactive_values$states$changed]=="plot_total_ref"){
                      ind=dim(reactive_values$matrice_res_ref$res$prop_grande_discipline)[1]
                      
                      # id=reactive_values$matrice_res_ref$res$contribution[ind,col]
                      #browser()
                      
                    }else{
                      
                      ind=which(reactive_values$matrice_res_ref$res$prop[["IDENTIFIANT"]]==input$select_article)
                      
                    }
                    id=reactive_values$matrice_res_ref$res$contribution[ind,col]
                    ind_global=as.numeric(unique(strsplit(gsub(" ","",(id)),split = ",")[[1]]))
                    #
                    # if(!is.null(ind_global))
                    table_data=datatable(df_flatten(reactive_values$matrice_res_ref$data[ind_global,]), options = list(scrollX = TRUE, columnDefs = list(list(
                      targets = "_all" ,render = JS(
                        "function(data, type, row, meta) {",
                        "return type === 'display' && data.length > 70 ?",
                        "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                        "}")
                    ))))
                    # View(df_flatten(reactive_values$matrice_res_ref$data[ind_global,]))
                    return(table_data)
                    #
                    
                    
                  })
                  states_2$changed <- c(FALSE,FALSE)
                }    
                
              }
              
              
            },ignoreNULL = TRUE)
            
            output$downloadPlot_total_ref <- downloadHandler(
              filename = function(){paste("test",'.pdf',sep='')},
              
              content = function(file) {
                ggsave(file,plot=reactive_values$plot_total_ref)
              },
              
              
            )
            
            
            
            output$downloadref <- downloadHandler(
              
              filename = function() {
                paste("table_ref","period",".csv", sep = "")
              },
              content = function(file) {
                write.csv2(as.data.frame(df_flatten(reactive_values$matrice_res_ref$data)), file, row.names = FALSE)
              }
            )
            
            
          }
        }
        
        if(input$type=="cit"||input$type=="all") {
          
          if(input$type=="cit"){
            reactive_values$matrice_res_cit$res=res_temp$res
            reactive_values$matrice_res_cit$data=res_temp$data
            
          }else{
            reactive_values$matrice_res_cit$res=res_temp$res_cit
            reactive_values$matrice_res_cit$data=res_temp$data_cit
          }
          
          if(!is.null(reactive_values$matrice_res_cit$res)){
            
            reactive_values$secteur_is_finish<-FALSE
            updateSelectInput(session, inputId = "select_article", choices = unique(c(reactive_values$matrice_res_ref$res$prop[["IDENTIFIANT"]],reactive_values$matrice_res_cit$res$prop[["IDENTIFIANT"]])))
            
            output$plot_article_cit<-renderPlotly({
              
              error=tryCatch({
                line=which(reactive_values$matrice_res_cit$res$prop[["IDENTIFIANT"]]%in%input$select_article)
                ind=which(reactive_values$matrice_res_cit$res$prop_grande_discipline[line,]!=0)
                title_graph=paste0("Main subjects  \n of article ",reactive_values$matrice_res_cit$res$prop[["TITLE"]][[line]])
                df <- data.frame(
                  group = Unaccent(names(reactive_values$matrice_res_cit$res$prop_grande_discipline[line,])[ind]),
                  value = unlist(reactive_values$matrice_res_cit$res$prop_grande_discipline[line,ind])/sum(reactive_values$matrice_res_cit$res$prop_grande_discipline[line,ind])
                )
                
                
              },
              error=function(cond){
                return(NA)
              })
              if(length(error)==1){ 
                if(is.na(error)){
                  title_graph="no data"
                  df <- data.frame(
                    group = "no data",
                    value = 1)
                  
                }
              } 
              
              
              
              plot_article_cit <- plot_ly(df, labels = ~group, values = ~value, type = 'pie',key=~group,source ="plot_article_cit")
              plot_article_cit <- plot_article_cit %>% layout(title = title_graph,
                                                              font=list(size=8),
                                                              legend = list(font = list(size = 9)),
                                                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
              ind_stat=which(reactive_values$matrice_res_cit$data[["cited identifier"]]==input$select_article)
              reactive_values$pct_cit[1]=mean(unlist(reactive_values$matrice_res_cit$data[ind_stat,]$citing_indice_pct_found))
              
              return(plot_article_cit)
              
            })
            
            output$stat_journ_cit_article<-renderText({paste("pourcentage accuracy of graphique aticle",round(reactive_values$pct_cit[1],digits = 2))})
            
            
            output$downloadPlot_article_cit <- downloadHandler(#telechargement du fichier 
              filename = function(){paste("test",'.pdf',sep='')},
              
              content = function(file) {
                ggsave(file,plot=reactive_values$plot_article_cit)
              })
            
            
            output$plot_total_cit<-renderPlotly({# plot des theme citations 
              
              
              df <- data.frame(
                group = Unaccent(names(reactive_values$matrice_res_cit$res$prop_grande_discipline)),
                value = unlist(reactive_values$matrice_res_cit$res$prop_grande_discipline[dim(reactive_values$matrice_res_cit$res$prop_grande_discipline)[1],]/sum(unlist(reactive_values$matrice_res_cit$res$prop_grande_discipline[dim(reactive_values$matrice_res_cit$res$prop_grande_discipline)[1],])))
              )
              
              
              plot_total_cit <- plot_ly(df, labels = ~group,key=~group, values = ~value, type = 'pie',source = "plot_total_cit")
              plot_total_cit <- plot_total_cit %>% layout(title = paste0("Main subjects  \n of the whole corpus"),
                                                          font=list(size=8),
                                                          legend = list(font = list(size = 9)),
                                                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
              reactive_values$secteur_is_finish<-TRUE
              reactive_values$pct_cit[2]=mean(unlist(reactive_values$matrice_res_cit$data$citing_indice_pct_found))
              
              return(plot_total_cit)
              
              
              
            })   
            output$stat_journ_cit_total<-renderText({paste("pourcentage accuracy of graphique total",round(reactive_values$pct_cit[2],digits = 2))})
            
            
            states_cit_2 <- reactiveValues(source =reactive_values$states_cit$source, value = c(-1,-1), changed = c(FALSE,FALSE),key=NULL)
            observeEvent({c(event_data("plotly_click", source =states_cit_2$source[[2]],priority = "event"),event_data("plotly_click", source =states_cit_2$source[[1]],priority = "event") ) },{
              #kk<<-kk+1
              # if(kk==2) browser() 
              if(reactive_values$secteur_is_finish==TRUE){
                for(src in states_cit_2$source){
                  clicked<-event_data("plotly_click", source = src)
                  
                  if( !is.null(clicked) ){
                    value <- clicked$pointNumber
                    if(states_cit_2$value[states_cit_2$source==src]!=value ){
                      
                      states_cit_2$value[states_cit_2$source==src] <- value
                      states_cit_2$changed[states_cit_2$source==src] <- TRUE
                      states_cit_2$key=clicked$key
                    }
                  }
                }
                if(sum(states_cit_2$changed)>0){
                  # pour eviter la réactualisation 2 fois de l'event quand une des variable change on passe a une autre variable non vu 
                  reactive_values$states_cit$changed=states_cit_2$changed
                  reactive_values$states_cit$source=states_cit_2$source
                  reactive_values$states_cit$key=states_cit_2$key
                  
                  output$table_data_cit_interdi <- renderDataTable({
                    ind_global=NULL
                    col=which(reactive_values$states_cit$key[[1]]==Unaccent(names(reactive_values$matrice_res_cit$res$prop_grande_discipline)))
                    
                    if(reactive_values$states_cit$source[reactive_values$states_cit$changed]=="plot_total_cit"){
                      ind=dim(reactive_values$matrice_res_cit$res$prop_grande_discipline)[1]
                      
                    }else{
                      
                      ind=which(reactive_values$matrice_res_cit$res$prop[["IDENTIFIANT"]]==input$select_article)
                      
                    }
                    
                    
                    #
                    # if(!is.null(ind_global))
                    table_data=datatable(df_flatten(reactive_values$matrice_res_cit$data[ind_global,]), options = list(scrollX = TRUE, columnDefs = list(list(
                      targets = "_all" ,render = JS(
                        "function(data, type, row, meta) {",
                        "return type === 'display' && data.length > 70 ?",
                        "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                        "}")
                    ))))
                    
                    return(table_data)
                    #
                    
                    
                  })
                  states_cit_2$changed <- c(FALSE,FALSE)
                }    
                
              }
              
              
            },ignoreNULL = TRUE)  
            #output$text_cit<-renderText({paste("ID:",round(reactive_values$matrice_res_cit$res$id,2),"DD:",round(reactive_values$matrice_res_cit$res$dd,2),"MD:",round(reactive_values$matrice_res_cit$res$md,2),"DIA:",paste(round(unlist(reactive_values$matrice_res_cit$res$dia[[as.numeric(input$select_article)]]),digits = 2),collapse = ","),collapse = "\n")})
            output$downloadcit <- downloadHandler(
              
              filename = function() {
                paste("table_cit","period",".csv", sep = "")
              },
              content = function(file) {
                write.csv2(as.data.frame(df_flatten(reactive_values$matrice_res_cit$data)), file, row.names = FALSE)
              }
            )
            
          }
        }
        showModal(modalDialog(
          title = "Result ready",
          "The calculation is over you can see your result in the interdiciplinarity result page.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("brut data set", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_values$df_global, file, row.names = FALSE)
    }
  )
  
  
  
  
}#end serveur 


# Run the app ----
shinyApp(ui, server)









