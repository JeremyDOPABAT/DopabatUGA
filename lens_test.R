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

extraction_data_api_lens<-function(data_pub,ti_name,au_name,token,pas=10,value_same_min_accept=0, value_same_min_ask=1,type="all",source_name="",sep_vector_in_data="",position_vector_in_data=""){
  # fonction permetant d'interroger ads sur les reference et les citation d'un corpus de publication placer en entr?e 
  
  
  
  #intput   
  #-data_pub: corpus de publication doit au moin contenir les titre et les auteur en anglais(dans un premier temps )
  # ti_name non de la colonne titre 
  # au name : nom de la colonne auteur
  #token : string, token d'identification 
  # pas: nombre de publication par requete (joue sur l'impact de l'erreur )
  #value_same_min_accept= valeur entre ? et 1 repr?sente le taux minimum de cimilitude a avoir pour accespter la r?ponse de la bd dans l'?tude 
  #value_same_min_ask=valeur minimal pour demander a l'utilisteur si il veul la publication dans l'?tude 
  # type: typ? danalise cit =citation , ref=reference, all=les deux 
  #sep_vector_in_data: vector de separateur par ligne  
  #position_vector_in_data vector of position name bien line 
  
  #outpout res : liste de 12 ?l?m?ents
  #author_vector:vecteur d'auteur retenu pour les requ?tes
  #dataframe_citation :dataframe des resultat(citation)
  #dataframe_citation_ask :dataframe des resultat demander (citation)
  #dataframe_publi_found :dataframe of all the publication found
  #dataframe_ref:dataframe des resultat(reference)
  #dataframe_ref:dataframe des resultat demand?e (reference)
  #error_querry_publi: matrice de rapport d'erreur pour la partie recherche de publication  
  #error_querry_citation matrice d'erreur concertnant la partie citation
  #error_querry_ref matrice d'erreur concertnant la partie reference
  #last_result : resultat brut de la derni?re requ?t 
  #res_accept: dataframe des resultat accespt? (citation ou reference )
  #res_ask: dataframe des resultat demander (citation ou reference )
  #reject_analyse : dataframe(publication rejet? )
  #title_vector : vecteur titre  
  
  # c1273/2ette fonction est la fonction globale ads qui recupere les citation et les reference des publication presente dans le corpus a analyser pour l'api ads.
  #Cette fct est en plusieurs partie, d'une par on cherche si les publis presente dans le corpus sont presente dans la base de donnees.puis on va chercher au besoins les reference et/ou les citationq 
  # on seppare les donnees qui viennent du wos dans le cas des ref car elle sont deja presentes dans le wos. donc on ne les recherche pas, mais ils nous les fauts pour les citations. 
  # l'organisation du programme fait qu'il faut les separer au debut. 
  
  # une fois que l'on as les identifiant des publis on peut les chercher via l'api ads, on prend toutes les infos don't on a besoins et on fait en sorte d'avoir le meme rendu que sur les autres bases.
  
  #NB : a chaque etapes on fait en sorte de pouvoir conserver les erreur et que le programme crach le moins possible. 
  # avant de chercher les publie , on met en formes les titre et les auteur pour eviter que cela cause des bugs.
  
  
  
  au_data<-data_pub[au_name][[1]]#auteur nom colonne 
  ti_data<-data_pub[ti_name][[1]]#titre 
  
  
  res_rest=c() #ini partie non wos 
  res_wos=c() #partoe <os
  err1=err2=c()
  
  reject1=reject2=c()
  
  if(source_name!="" && (type=="ref" || type=="all")){
    #print("passe wos")
    
    data_wos=data_pub[data_pub[source_name]=="WOS",]#separation des source 
    print("rentre dans la partie wos ")
    
    if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_wos[sep_vector_in_data][[1]]
    if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_wos)[1]) else position_name=data_wos[position_vector_in_data][[1]]
    if(dim(data_wos)[1]!=0) {
      res_wos_all=lens_get_publi(data_wos[au_name][[1]],data_wos[ti_name][[1]],position_name[data_pub[source_name]=="WOS"],pas,value_same_min_ask,value_same_min_accept,token,sep)
      res_wos=res_wos_all$res
      err1=res_wos_all$error
      reject1=res_wos_all$reject
      
    }else{#ci type =cit pour eviteez les erreur de non reto
      res_wos=NULL
      err1=NULL
      reject1=NULL
      
    }
    
    
    data_rest=data_pub[data_pub[source_name]!="WOS",]
    print("rentre dans la partie CSV ")
    if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_rest[sep_vector_in_data][[1]]
    if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_rest)[1]) else position_name=data_rest[position_vector_in_data][[1]]
    if(dim(data_rest)[1]!=0) {
      
      res_rest_all=lens_get_publi(data_rest[au_name][[1]],data_rest[ti_name][[1]],position_name[data_pub[source_name]!="WOS"],pas,value_same_min_ask,value_same_min_accept,token,sep)
      print(dim(res_rest_all$res))
      res_rest=res_rest_all$res
      err2=res_rest_all$error
      reject2=res_rest_all$reject
      
    }else{
      res_rest=NULL
      err2=NULL
      reject2=NULL
      
    }
    
    
    res=rbind(res_rest,res_wos)
    error_querry=rbind(err1,err2)
    reject=rbind(reject1,reject2)
    
  }else {
    print("passe par else")
    if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_pub[sep_vector_in_data][[1]]
    if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_pub)[1]) else position_name=data_pub[position_vector_in_data][[1]]
    if(dim(data_pub)[1]!=0) {
      resdt_all=lens_get_publi(au_data,ti_data,position_name,pas,value_same_min_ask,value_same_min_accept,token,sep = sep)
      error_querry=resdt_all$error
      res=resdt_all$res
      reject=resdt_all$reject
    }
  }  
  
  resdt<-as.data.frame(res,stringsAsFactors = FALSE)
  
  
  # source("C:/Users/moroguij/Documents/R_programs/functions_analyses.R")#source nneded function 
  #initialisation des variable __________________________________________  
  
  # Pr?non nom--> nom,pr?non  
  
  
  #ins?r? fct 
  if(type=="all"|| type=="cit"){
    
    
    res_cite=lens_get_cit_or_ref(resdt,type="cit",token)# citation 
    total_res=res_cite$res_cit_ref[res_cite$res_cit_ref$check>=value_same_min_accept,]# permet d'enlever les ask
    total_res_ask=res_cite$res_cit_ref[(res_cite$res_cit_ref$check>=value_same_min_ask) & (res_cite$res_cit_ref$check<value_same_min_accept),]#permet de garder uniquement les ask 
    if(!is.null(total_res)[1]){
      if(dim(total_res)[1]>0) total_res<-total_res[order(unlist(total_res$`citing identifier`)),]
      if(dim(total_res_ask)[1]>0)total_res_ask<-total_res_ask[order(unlist(total_res_ask$`citing identifier`)),]
      if(dim(total_res)[1]>0) row.names(total_res)<-1:dim(total_res)[1]
      if(dim(total_res_ask)[1]>0) row.names(total_res_ask)<-1:dim(total_res_ask)[1]
    }
    
    
    #error_querry_cit=res_cite$error_table
    error_querry=rbind(error_querry,res_cite$error_table)
    
    if(type=="cit"){
      total_res_ref=NULL
      total_res_ref_ask=NULL
      error_querry_ref=NULL
    }
  }
  
  if(type=="all"|| type=="ref"){
    if(source_name==""){
      res_rest=resdt
    }
    
    if(type=="ref"){
      total_res=NULL
      total_res_ask=NULL
      error_querry_cit=NULL
    }
    res_ref=lens_get_cit_or_ref(res_rest,type="ref",token)  #on cherche les ref unkiquement sur la partie concerner 
    total_res_ref=res_ref$res_cit_ref[res_ref$res_cit_ref$check>=value_same_min_accept,]
    total_res_ref_ask=res_ref$res_cit_ref[(res_ref$res_cit_ref$check>=value_same_min_ask) & (res_ref$res_cit_ref$check<value_same_min_accept),]
    if(!is.null(total_res_ref)[1]){
      if(dim(total_res_ref)[1]>0) total_res_ref<-total_res_ref[order(unlist(total_res_ref$`refering identifier`)),]
      if(dim(total_res_ref)[1]>0) row.names(total_res_ref)<-1:dim(total_res_ref)[1]
      if(dim(total_res_ref_ask)[1]>0)total_res_ref_ask<-total_res_ref_ask[order(unlist(total_res_ref_ask$`refering identifier`)),]
      if(dim(total_res_ref_ask)[1]>0) row.names(total_res_ref_ask)<-1:dim(total_res_ref_ask)[1]
      
      
      error_querry=rbind(error_querry,res_ref$error_table)
      
      
      
      
    }
  }
  return(list(dataframe_citation_accept=total_res,error_querry_publi=error_querry,title_vector=ti_data,author_vector=au_data,dataframe_citation_ask=total_res_ask,
              reject_analyse=reject,dataframe_publi_found=resdt,dataframe_ref_accept=total_res_ref,dataframe_ref_ask=total_res_ref_ask))
  
  
}






test_global=extraction_data_api_lens(ths,ti_name,au_name,token,pas=10,value_same_min_accept, value_same_min_ask,type="cit",source_name,sep_vector_in_data,position_vector_in_data)

dim(test_global$dataframe_publi_found)
dim(test_global$dataframe_citation_accept)





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













