# lens api test ____________________________________________________________________________________________________________

token="W7eLvEax6chBRSgFjMSVbUOAPVAQgyFyZDz6o3OeSVdDasBgH9f3"# token a verifier sur le compte lens 
#20000 requetes par mois =) 
#https://api.lens.org/scholarly/search?token={your-access-token}
#3req/S




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
au_data=list("Tom,Hohweiller")




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
sep_vector_in_data="sep"
position_vector_in_data="position_name"
id_data=""
pas=10
value_same_min_ask=0.85
value_same_min_accept=0.95







test_global=extraction_data_api_lens(ths,ti_name,au_name,token,pas=10,value_same_min_accept, value_same_min_ask,type="all",source_name,sep_vector_in_data,position_vector_in_data)

dim(test_global$dataframe_publi_found)
dim(test_global$dataframe_citation_accept)

dim(test_global$dataframe_ref_accept)




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


test4=lens_get_publi(au_data,ti_data,ths$position_name,10,value_same_min_ask,value_same_min_accept,token,ths$sep)
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













