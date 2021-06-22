# lens api test ____________________________________________________________________________________________________________

token="W7eLvEax6chBRSgFjMSVbUOAPVAQgyFyZDz6o3OeSVdDasBgH9f3"# token a verifier sur le compte lens 
#20000 requetes par mois =) 
#https://api.lens.org/scholarly/search?token={your-access-token}
#3req/S

lens_get_publi<-function(au_data,ti_data,doi_data="",position_reel,pas,value_same_min_ask,value_same_min_accept,token,sep){
  #pour traiter doi on sort par doi et ensuite on traite les réquest par doi puis ensuite non
  
  error_querry=c()  
  position_togo=rep(1,length(au_data))
  res=c()
  res_temp=supress_unfit_entry(ti_data,au_data,doi_vector =doi_data ,sep=sep,max_aut = 1)#permet d'aclimater les do,n?es a la bd (possiblement sortable de la fonction )  
  
  ti_data=res_temp[[1]]
  au_data=res_temp[[2]]
  doi_data=res_temp[[3]]
  
  #if(doi_data=="none") doi_data=""  
  for(type_requetage in c("TI_AU","DOI" )){
   #if(type_requetage=="DOI") browser()  
    
    if(type_requetage=="TI_AU"){
      au_data=fit_name_position(au_data,position_reel,position_togo =position_togo,sep )
      au_data=sapply(1:length(au_data),FUN = function(x) paste(au_data[[x]],collapse = ";"))
      # nombre d'iteration
      data_to_use=no_doi=which(is.na(doi_data)| doi_data=="" )#pas de doi = doi vide 
    }else{#on peu se permet car les élément perturbateur on été supprimer 
      data_to_use=doi=which(doi_data!="" & !is.na(doi_data))
    }
    if(length(data_to_use)>0){
      inter=ceiling(length(data_to_use)[1]/pas)
      print("interrrr")
      print(inter)
      # withProgress(
      #   message='Please wait',
      #   detail='Doing reasearche of publication in lens...',
      #   value=0, {
      #     
      for(h in 1:inter){# on parcoure les auteur et les titre par pas et on fait les roquette correspondante 
        print(h)
        first<-(h-1)*pas+1
        last<-h*pas
        #incProgress(1/inter)
        if(last>length(data_to_use)) last<-length(data_to_use)
        
        
        
        #browser("arret pour verifier les requet en cit ")
        request=lens_make_main_request(gsub(","," ",Unaccent(au_data[data_to_use][first:last])),Unaccent(ti_data[data_to_use][first:last]),doi_data[data_to_use][first:last])
        
        Sys.sleep(3)  
        data <- getScholarlyData(token, request)
        data$status_code
        result <-jsonlite::fromJSON(txt = httr::content(data, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)
        
        
        error=tryCatch({#rep?rage des erreur 
          querry_warning_message(data)
          
        },
        
        warning=function(cond){# mise en forme en cas d'erreur 
          titre_error=as.data.frame(ti_data[first:last])
          names(titre_error)=c("Publication title")
          titre_error["Status error"]=data$status
          titre_error$Message=message_error(data)
          titre_error["Data impact"]="ref & cit"
          titre_error$h=h
          return(titre_error)
        })
        
        
        if(length(error)>0){
          error_querry<-rbind(error_querry,error)
          error=c()
          
        }else {#si il n'y a pas d'erreur 
          
          
          if(result$total>0){
            print("ouiiiiiii")
            title=result$data$title
            id=result$data$lens_id
            author=sapply(1:length(result$data$authors),FUN = function(x) return(list(result$data$authors[[x]]$last_name)))
            date=result$data$date_published
            reference=result$data$references
            citation=result$data$scholarly_citations
            journal=result$data$source$title
            
            
            if(is.null(reference)) reference=NA
            if(is.null(citation)) citation=NA 
            if(is.null(journal)) journal=NA 
            
            if(is.null(date)) date=NA
            
            res<-rbind(res,cbind(h,id,author,title, date,journal,citation,reference))
            
          }
          
          # if(value_same_min_ask<1) reject=resdt[resdt$check_title_pct<value_same_min_ask,]# les rejet? sont ceux qui non pas assez de similitudfe pour aire dans les demande 
          # resdt=resdt[resdt$check_title_pct>value_same_min_ask,]
          # #ask est pas accepte car on le garde dans la dataframe pour que ceux ou on a un doute soit quand même treter 
          
          
        }
      }
      #})
      
      resdt=as.data.frame(res,stringsAsFactors = FALSE)    
      
      if(type_requetage=="TI_AU"){# très imortant que le titre auteur soit fait en premier 
        indic_compaire_title<-compaire_title(Unaccent(resdt$title),Unaccent(ti_data),doi_data = doi_data)
        no_doi=data_to_use
        # on sauvegarde le reusultat du test pour la fin du programme 
      }else{
        doi=data_to_use      
      }    
      
    }
    
  } 
  
  if(dim(res)[1]>0){
    tit_pct<-c(unlist(indic_compaire_title[1,]),rep(1,length(doi)))
    tit_ind<-c(unlist(indic_compaire_title[2,]),rep(NA,length(doi)))
    resdt["check_title_pct"]<-tit_pct
    resdt["check_title_ind"]<-tit_ind
    
    
    if(value_same_min_ask<1) reject=resdt[resdt$check_title_pct<value_same_min_ask,]# les rejet? sont ceux qui non pas assez de similitudfe pour aire dans les demande 
    resdt=resdt[resdt$check_title_pct>value_same_min_ask,]
    
  }
  
  return(list(res=resdt, error=error_querry,reject=reject))
}



require(httr)
getScholarlyData <- function(token, query){
  url <- 'https://api.lens.org/scholarly/search'
  headers <- c('Authorization' = token, 'Content-Type' = 'application/json')
  httr::POST(url = url, add_headers(.headers=headers), body = query)
}


# requete type ____________________________
request<- '{
    "query": {
        "bool": {
            "should": [
                { 
                    "bool": {
                        "must": [
                            {"match_phrase": {"title": "Role of interface dipole in metal gate/high-k effective work function modulation by aluminum incorporation"}},
                            {"match_phrase": {"author.display_name": "Zhenya Yang"}}
                        ]
                    }
                },
                { 
                    "bool": {
                        "must": [
                            {"match_phrase": {"title": "Based on the User-Centered Design and Development of Fiber Sensor Monitoring System"}},
                            {"match_phrase": {"author.display_name": "Juan Ni Li"}}
                            
                        ]   
                    }
                }
            ]
        }
    }, 
    "include": ["scholarly_citations","references","lens_id","source","authors","date_published"]
}'

data <- getScholarlyData(token, request)
data$status_code
result <-jsonlite::fromJSON(txt = httr::content(data, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)

result$total
result$data$scholarly_citations
names(result$data)
result$data$lens_id
result$data$authors[[1]]$first_name
result$data$source$publisher
result$data$date_published
result$data$references[2]
#result$data$

#__________________________________________________  
#exemple requet complet     
request_vrai<- paste0('{
    "query": {
        "bool": {
            "should": [
                { 
                    "bool": {
                        "must": [
                            {"match_phrase": {"title": "Role of interface dipole in metal gate/high-k effective work function modulation by aluminum incorporation"}},
                            {"match_phrase": {"author.display_name": "Zhenya Yang"}}
                        ]
                    }
                },
                { 
                    "bool": {
                        "must": [
                            {"match_phrase": {"title": "CAES by design: A user-centered approach to designing Compressed Air Energy Storage (CAES) systems for future electrical grid: A case study for Ontario"}},
                            {"match_phrase": {"author.display_name": "Kamyar Rouindej"}}
                            
                        ]   
                    }
                }
                
            ]
        }
    }, 
    "include": ["title","scholarly_citations","references","lens_id","source","authors","date_published"]
}')


#exemple avec doi 
request_vrai<- paste0('{
  "query": {
    "match":{
      "doi": "10.1109/ee.1934.6540358"
    }
  },
  "include":["title","patent_citations"]
}, 
    "include": ["title","scholarly_citations","references","lens_id","source","authors","date_published"]
}')


data <- getScholarlyData(token, request_vrai)
data$status_code
result <-jsonlite::fromJSON(txt = httr::content(data, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)

result$total
result$data$scholarly_citations
names(result$data)
result$data$lens_id
result$data$authors[[1]]$first_name
result$data$source$publisher
result$data$date_published
result$data$references[2]










ti_data=c("", "Simulation and Experimental Demonstration of the Importance of IR-Drops During Laser Fault-Injection")
au_data=list("","Bastos")



doi_data=c("10.1109/ee.1934.6540358","")
request=lens_make_main_request(au_data,ti_data,doi_data)
  




data <- getScholarlyData(token, request)
data$status_code
result <-jsonlite::fromJSON(txt = httr::content(data, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)
result$total

result$data$title

# Pr?non nom--> nom,pr?non  
#pe,ser a séparer les ref des wos 



# cit=result$data$scholarly_citations[[1]]

ths=ths[order(ths$doiId_s,na.last = TRUE,decreasing = FALSE),]
ths$position_name=1
ths$sep=","
ths$source="csv"
data_pub=ths
source_name="source"
sep_vector_in_data="sep"
position_vector_in_data="position_name"
id_data=""
pas=8
value_same_min_ask=0.85
value_same_min_accept=0.95

ti_data_t=ths$title_s
#doi_data_t=rep(NA,dim(ths)[1])
doi_data_t=ths$doiId_s
au_data_t=ths$authFullName_s


test4=lens_get_publi(au_data = au_data_t,ti_data = ti_data_t,doi_data = "",position_reel = ths$position_name,pas = 10,value_same_min_ask = value_same_min_ask,value_same_min_accept = value_same_min_accept,token = token,sep = ths$sep)

View(test4$res)
dim(test4$error)
dim(test4$reject)


test5=lens_get_publi_new(au_data = au_data_t,ti_data = ti_data_t,doi_data = doi_data_t,position_reel = ths$position_name,pas = 10,value_same_min_ask = value_same_min_ask,value_same_min_accept = value_same_min_accept,token = token,sep = ths$sep)
dim(test5$res)
View(test5$res)









path_data=choose.files(caption = "chosse your data file")# choisir le fichier concerner 
au_name <- readline(prompt="Nom de Variable 'Nom_auteur': ")#authFullName_s
ti_name <- readline(prompt="Nom de Variable 'Titre_publication': ")#en_title_s
date_name <- readline(prompt="Nom de colonne 'date_publication': ")#defenseDate_s producedDate_s
ths<-read.csv(path_data, sep = ";",header = TRUE,stringsAsFactors = FALSE)
dim(ths)
if(sum(duplicated(ths[c(au_name,ti_name)]))>0) ths<-ths[-(which(duplicated(ths[c(au_name,ti_name)])==TRUE)),]
dim(ths)

source("applitodeploy/global.R")
library(plyr)

ths$position_name=1
ths$sep=","
ths$source="csv"
data_pub=ths
source_name="source"
sep_vector_in_data="sep"
position_vector_in_data="position_name"
id_data=""
pas=10
value_same_min_ask=0.85
value_same_min_accept=0.95







test_global=extraction_data_api_lens(ths,ti_name,au_name,token,pas=10,value_same_min_accept, value_same_min_ask,type="all",source_name,sep_vector_in_data,position_vector_in_data)

dim(test_global$dataframe_publi_found)
names(test_global$dataframe_citation_accept)

dim(test_global$dataframe_ref_accept)

head(test_global$dataframe_citation_accept$`cited journal`)


test_global_ref=extraction_data_api_lens(ths,ti_name,au_name,token,pas=10,value_same_min_accept, value_same_min_ask,type="ref",source_name="source",position_vector_in_data = "position_name")

dim(test_global_ref$dataframe_publi_found)
dim(test_global_ref$dataframe_citation_accept)





test_global_cit=extraction_data_api_lens(ths,ti_name,au_name,token,pas=10,value_same_min_accept, value_same_min_ask,type="cit",source_name="source",position_vector_in_data = "position_name")

dim(test_global_cit$dataframe_publi_found)
dim(test_global_cit$dataframe_citation_accept)

au_data=ths[[au_name]]
ti_data=ths[[ti_name]]
test_global$error_querry_publi


dim(test_global$dataframe_citation_ask)

dim(test_global$reject_analyse)
dim(test_global$dataframe_citation_accept)
dim(test_global$dataframe_ref_accept)
dim(test_global$dataframe_ref_ask)


#Lastname Firstname" = "2",
#"Firstname Lastname" = "1"


res_temp=supress_unfit_entry(ti_data,au_data,sep=",",max_aut = 1)#permet d'aclimater les do,n?es a la bd (possiblement sortable de la fonction )

ti_data=res_temp[[1]]
au_data=res_temp[[2]]
error_querry<-c()
querry_list=list()
res=c()  
position_togo=rep(1,length(au_data))




au_data=fit_name_position(au_data,position_name,position_togo =position_togo,sep )
au_data=sapply(1:length(au_data),FUN = function(x) paste(au_data[[x]],collapse = ";"))





# pour après !!!!!!
res=c()#variable resultat 

error_querry=c()# table d'erreur  


#on peu monter le pas a dit sans problem 
request<- paste0('{
      "query": {
  "terms":{
    "lens_id": ["017-767-306-508-482", "024-043-983-595-624","038-090-767-345-815","054-385-339-144-45X","054-458-804-952-159","114-286-311-938-179","005-010-192-999-980", "012-581-934-774-370", "025-989-999-947-093"]
  }
},
"include": ["lens_id","title", "lens_id","source","authors","date_published"]
}')

head(test$res$cit)
data <- getScholarlyData(token, request)
data$status_code
result <-jsonlite::fromJSON(txt = httr::content(data, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)
result$total
result$data$source$publisher
result$data$source$issn

result$data$date_published


resdt=test$res



test_cit=lens_get_cit_or_ref(test4$res,type="cit",token)
dim(test_cit$res_cit_ref)













