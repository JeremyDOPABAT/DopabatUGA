# ____________________ comment charger les nouveau  fichier _______________________

# nomé les fichier de table et les fichiers de stat respectivement  "table1" "table2" "table3" et "stat1" "stat2" "stat3"  dans l'ordre croissant des périodes. 
# les fichier stat et table qui vont enssemble doivent avoir les même numéros.

# une fois que cela est fait , remplacer les fichier présent dans le dossier www 

#une fois que cela est fait lancé l'app et résoudre les éventuelles erreurs :
#-attention : attentionna  la premiere colonne UNITE , pas d'accent sur les datte 
 #- ne pas changer les nom de colones 

# regarder l'import, la dimention et les nom si erreur  
# comparer un fichier qui fonctionne avec celuit qui ne fonctionne pas pour trouvez les différences





#________________________________







library(shiny)
library(shinydashboard)
library(DT)

# # le ui permet de gerer le coté graphiques
ui <- dashboardPage(skin = "red",
                    #menu
                    dashboardHeader(title =span("", style = "font-size: 8px")),
                    dashboardSidebar( sidebarMenu(# menu des different onglet 
                        
                        menuItem("table", tabName = "table", icon = icon(name = "arrow-circle-up")),
                        
                        menuItem("About us", tabName = "about", icon = icon("address-card"))
                        
                        #-------------------------------------------------------------------  
                        
                        #-------------------------------------------------------------------  
                    )),
    dashboardBody(
        tabItems(
                tabItem(tabName = "table",
                fluidPage(
                    titlePanel("SUIVI DE LA MISE EN OEUVRE DE LA SIGNATURE  UNIV. GRENOBLE ALPES" ),
                    fluidRow(width=12,radioButtons("periode_to_show",label = "Choose a periode",choices = c(Periode1="1",Periode2="2",Periode3="3"),selected = "1" ,inline = TRUE)),#initiation de la liste des lab
                    fluidRow(width=12,column(width = 4,selectInput("lab_selection", "Select lab", choices = "", width = "300px")),column(width = 4,textOutput("periode",inline = TRUE)),column(width = 4,fluidRow(column(width = 4,htmlOutput("line_same",inline = TRUE)),column(width = 4,htmlOutput("line_dif",inline = TRUE)),column(width=4,htmlOutput("taux_diff",inline = TRUE))))),#test a droite 
                    fluidRow(width=12, dataTableOutput("table_result")),# table rresultat 
                    downloadButton("downloadData", "Download the table")# bouttons de telechargement 

                )
            ),
            tabItem(tabName = "about",mainPanel(
                titlePanel("A Propos"),
                htmlOutput("text"))
            )
                
        )
    )
)


server <- function(input, output, session) {# cote r , voici le programme 
    require(purrr)# une seul librery nécéssaire 
    #functions ___________________________________________________________
    
    #fonction qui permet le bon affichage de la table de resultat
    df_flatten<-function(res_f){
        # data tble 
        #fonction qui permet le bonne affichage des tables dans l'interface  
        
        #date_name="date"
        
        for(i in names(res_f)){# on parcourt les colonnes 
            
            ind=which(is.null(res_f[[i]]))
            if(length(ind)>0 ) res_f[ind,i]=NA
            ind=which(is.na(res_f[[i]]))
            if(length(ind)>0 )res_f[ind,i]="NULL"#na ne peu pas ?tre afficher dans la table donc on le remplace par "null" 
            if(class(res_f[[i]])!="character" && class(res_f[[i]])!="numeric" ){
                if(length(unlist(res_f[[i]]))>length(res_f[[i]])){
                    res_f[[i]]=as.character(res_f[[i]])#sapply(1:length(res_f[j,i]),FUN = function(x) paste(res_f[j,i][[x]],collapse = ";"))# si il y a plusieur element sur une m?me ligne
                    }
                }
            }
        
        return(as.data.frame(res_f,stringsAsFactors = FALSE))
    }
    #-------------------------------------------------------------------------
    
    
    
    
    output$text <- renderText({
    "L’analyse porte sur les publications presentes dans le web of science, par consequent ces donnes peuvent être utilisees et diffusees compte tenu de notre abonnement a cette base de donnees.

Les adresses sont celles presentes dans les publications referencees dans le Web of Science sur la periode de reference qui est mentionnee lors de la visualisation, 3 periodes sont disponibles (de 2018 à 2020).

L'ensemble des lignes d'adresses du perimetre univ grenoble alpes sont analysees au sein d'une meme publication. Les adresses  de chaque unité sont obtenues par la recherche du nom de l'unite et du lieu geographique. Pour la plupart des unites, differentes variations de noms deja utilises par le passe sont prises en compte.

Les unites non presentes sont celles qui n’ont aucune publications sans univ.grenoble alpes, ou qui n’ont aucune publication dans le Web of Science (unite en SHS en general).

Lignes adresses est le nombre de lignes d’adresses de l’unite sur la période (supérieur en general au nombre de publications)

Lignes non UGA est le nombre de ces mêmes lignes sans univ. Grenoble alpes

Le taux d’erreur est le pourcentage de lignes sans univ. Grenoble alpes "
})
    
    
    #---------------------importation des differentes table--------------------
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
                                              sep = ";"),stringsAsFactors = FALSE),
        table_data=NULL
    #__________________________________________________________________________________________    
        
    )
    
    observeEvent(reactive_values$stat3,{
        # quand la dernière table est importé on met toutes les variables à jours 
        updateRadioButtons(session,"periode_to_show","choix d'une periode ",choices =
                               set_names(c(1,2,3),c(paste0(reactive_values[["stat1"]][1,"Debut"],":",reactive_values[["stat1"]][1,"Fin"]),paste0(reactive_values[["stat2"]][1,"Debut"],":",reactive_values[["stat2"]][1,"Fin"]),paste0(reactive_values[["stat3"]][1,"Debut"],":",reactive_values[["stat3"]][1,"Fin"]))),selected = "1" ,inline = TRUE)

        updateSelectInput(session, inputId = "lab_selection", selected =unique(tolower(reactive_values[[paste0("data",1)]]$UNITE))[1] ,choices =unique(tolower(c(reactive_values[[paste0("data",1)]]$UNITE,reactive_values[[paste0("data",2)]]$UNITE,reactive_values[[paste0("data",3)]]$UNITE))))     
        # on prend les 5 premier colonnes de chaque table.
        reactive_values$data1=reactive_values$data1[c("UNITE","NOM JOURNAL OU CONF","NOM ARTICLE","AUTEURS","ADRESSE")]
        reactive_values$data2=reactive_values$data2[c("UNITE","NOM JOURNAL OU CONF","NOM ARTICLE","AUTEURS","ADRESSE")]
        reactive_values$data3=reactive_values$data3[c("UNITE","NOM JOURNAL OU CONF","NOM ARTICLE","AUTEURS","ADRESSE")]
        
     })
    
        observeEvent(input$periode_to_show,{
        #names(reactive_values[[paste0("data",input$periode_to_show)]])=tolower(names(reactive_values[[paste0("data",input$periode_to_show)]]))
        #names(reactive_values[[paste0("stat",input$periode_to_show)]])=tolower(names(reactive_values[[paste0("stat",input$periode_to_show)]]))
        
    
    
    precedant=reactive_values[[paste0("data",input$periode_to_show)]]$UNITE[1]
    for(i in 2:dim(reactive_values[[paste0("data",input$periode_to_show)]])[1]){
        #print(paste0("data",input$periode_to_show))
        
        if(reactive_values[[paste0("data",input$periode_to_show)]]$UNITE[i]==""){
            reactive_values[[paste0("data",input$periode_to_show)]]$UNITE[i]=precedant
            # print(precedant)
        }
        else {
            precedant=reactive_values[[paste0("data",input$periode_to_show)]]$UNITE[i]}
        
    }
    #output$periode<-renderText({paste("Periode",reactive_values[[paste0("stat",input$periode_to_show)]][1,"Debut"],":",reactive_values[[paste0("stat",input$periode_to_show)]][1,"Fin"])})
    
})
observeEvent(c(input$lab_selection,input$periode_to_show),{
    #on cherche l'unite dans les differente table 
    ind=which(tolower(reactive_values[[paste0("data",input$periode_to_show)]]$UNITE)==input$lab_selection)
    ind2=which(tolower(reactive_values[[paste0("stat",input$periode_to_show)]]$UNITE)==input$lab_selection)
    
    
    #On affiche la bonne table et les bonne info 
    output$table_result <- renderDataTable({
        #test_table<-reactive_values$df_pdf
        table_data=datatable(df_flatten(reactive_values[[paste0("data",input$periode_to_show)]][ind,]), options = list(sDom  = '<"top">lrt<"bottom">ip',scrollX = TRUE, columnDefs = list(list(
            targets = "_all" ,render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 70 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                "}")
        ))))
    })
    # changement des chiffre 
    output$line_same<-renderText({(paste( "<b>Lignes adresse:</b>",reactive_values[[paste0("stat",input$periode_to_show)]][ind2,"Nombre de lignes d'adresses"]))})
    output$line_dif<-renderText({(paste("<b>Lignes non UGA:</b>",reactive_values[[paste0("stat",input$periode_to_show)]][ind2,3]))})
    output$taux_diff<-renderText({(paste("<b>taux d'erreur:</b>",reactive_values[[paste0("stat",input$periode_to_show)]][ind2,4]))})
    output$downloadData <- downloadHandler(#perrmet le donlowd de la table 
        filename = function() {
            paste(input$lab_selection,"period",input$periode_to_show, ".csv", sep = "")
        },
        content = function(file) {
            write.csv2(as.data.frame(df_flatten(reactive_values[[paste0("data",input$periode_to_show)]][ind,])), file, row.names = FALSE)
        }
    )
    #View(as.data.frame(reactive_values[[paste0("data",input$periode_to_show)]][ind,]))

})




}
# Run the application 
shinyApp(ui = ui, server = server)
