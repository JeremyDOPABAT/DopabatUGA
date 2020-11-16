# lens api test ____________________________________________________________________________________________________________

token="W7eLvEax6chBRSgFjMSVbUOAPVAQgyFyZDz6o3OeSVdDasBgH9f3"# token 
#5000 requetes par moi =) 
#https://api.lens.org/scholarly/search?token={your-access-token}




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









lens_make_main_request=function(au_data,ti_data){
  part_quest=list()
  
for(i in 1:length(ti_data)){

  part_courant=paste0('
    { 
    "bool": {
      "must": [
        {"match_phrase": {"title":"', gsub("\\","",ti_data[[i]],fixed=TRUE),'"}},
        {"match_phrase": {"author.display_name":"',au_data[[i]],'"}}
                ]
              }
          }')
  
  part_quest=append(part_quest,part_courant)
}

part_quest=paste0(part_quest,collapse = ",")

request<- paste0('{
      "query": {
          "bool": {
              "should": [',part_quest,'
                  
              ]
          }
      }, 
      "include": ["title","scholarly_citations","references","lens_id","source","authors","date_published"]
  }')


return(request)
}


ti_data=list("An ADMM Algorithm for Constrained Material Decomposition in Spectral CT")
au_data=list("Tom Hohweiller")



request=lens_make_main_request(au_data,ti_data)
  




data <- getScholarlyData(token, request)
data$status_code
result <-jsonlite::fromJSON(txt = httr::content(data, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)
result$total



# Pr?non nom--> nom,pr?non  
#pe,ser a séparer les ref des wos 



# cit=result$data$scholarly_citations[[1]]




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
sep_vector_in_data=""
position_vector_in_data=""
id_data=""
pas=3
value_same_min_ask=0.85
value_same_min_accept=0.95



au_data=ths[[au_name]]
ti_data=ths[[ti_name]]



test=lens_get_publi(au_data,ti_data,ths$position_name,pas,value_same_min_ask,value_same_min_accept,token,ths$sep)



#lens_get_publi<-function(au_data,ti_data,position_name,pas,value_same_min_ask,value_same_min_accept,token,sep){
  
  res_temp=supress_unfit_entry(ti_data,au_data,sep,max_aut = 1)#permet d'aclimater les do,n?es a la bd (possiblement sortable de la fonction )
  
  ti_data=res_temp[[1]]
  au_data=res_temp[[2]]
  error_querry<-c()
  querry_list=list()
  res=c()  
  position_togo=rep(1,length(au_data))
  
  
  
  
  au_data=fit_name_position(au_data,position_name,position_togo =position_togo,sep )
  au_data=sapply(1:length(au_data),FUN = function(x) paste(au_data[[x]],collapse = ";"))
  
  
  inter=ceiling(length(au_data)[1]/pas)# nombre d'iteration
  # withProgress(
  #    message='Please wait',
  #    detail='Doing reasearche of publication in ads...',
  #    value=0, {
  res=c()
  for(h in 1:inter){# on parcoure les auteur et les titre par pas et on fait les roquette correspondante 
    first<-(h-1)*pas+1
    last<-h*pas
    # incProgress(1/inter)
    if(last>length(au_data)) last<-length(au_data)
    
    
    
    
    request=lens_make_main_request(gsub(","," ",Unaccent(au_data[first:last])),Unaccent(ti_data[first:last]))
    Sys.sleep(6)  
    data <- getScholarlyData(token, request)
    data$status_code
    result <-jsonlite::fromJSON(txt = httr::content(data, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)
    result$total
    
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
        ref=result$data$references
        cit=result$data$scholarly_citations
        journal=result$data$source$title
        
        
        if(is.null(ref)) ref=NA
        if(is.null(cit)) cit=NA 
        if(is.null(journal)) journal=NA 
        
        
        res<-rbind(res,cbind(h,id,author,title, date,journal,cit,ref))
        
        
      }
      
      # if(value_same_min_ask<1) reject=resdt[resdt$check_title_pct<value_same_min_ask,]# les rejet? sont ceux qui non pas assez de similitudfe pour aire dans les demande 
      # resdt=resdt[resdt$check_title_pct>value_same_min_ask,]
      # #ask est pas accepte car on le garde dans la dataframe pour que ceux ou on a un doute soit quand même treter 
      
      
    }
  }
  res=as.data.frame(res,stringsAsFactors = FALSE)
  
  return(list(res=res, error=error_querry))
}

dim(test$res)

dim(test$error)[1]/length(au_data)
table(test$error$h)
table(test$error$`Status error`)

# pour après !!!!!!
res=c()#variable resultat 

error_querry=c()# table d'erreur  


