#https://inspirehep.net/api/literature?q=dois.value:*
  
  
  
  
  r <- httr::GET(paste0("https://inspirehep.net/api/literature/4328?format=json")
                 )
  
  error=tryCatch({#rep?rage des erreur 
    querry_warning_message(r)
    
  },
  
  warning=function(cond){# mise en forme en cas d'erreur 
    titre_error=as.data.frame(ti_data[first:last])
    names(titre_error)=c("Publication title")
    titre_error["Status error"]=r$status
    titre_error$Message=message_error(r)
    titre_error["Data impact"]="ref & cit"
    titre_error$h=h
    return(titre_error)
  })
  
  result <-jsonlite::fromJSON(txt = httr::content(r, 'text'), simplifyDataFrame = TRUE)
  
  result$id
  
  
  r$all_headers
  r$handle
result$metadata$references

result$metadata$  
    
#t%20A%20First%20Course%20in%20String%20Theory%20and%20
#t%20Geodesics%20of%20the%20hyperbolically%20symmetric%20black%20hole
#get literrature 
#r <- httr::GET(paste0("https://inspirehep.net/api/literature/?&q=a%20witten%20and%20t%20Matrix%20Models%20and%20Deformations%20of%20JT%20Gravity&format=json"))

r <- httr::GET(paste0("https://inspirehep.net/api/literature/?&q=a%28Witten%20OR%20Barton%20Zwiebach%29%20and%20t%20Searching%20for%20a%20Black%20Hole%20in%20the%20Outer%20Solar%20System%20or%20t%20A%20First%20Course%20in%20String%20Theory&format=json"))
r$status_code

result <-jsonlite::fromJSON(txt = httr::content(r, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)
result$hits$hits$id
result$hits$hits$metadata$authors[[1]]$full_name
result$hits$hits$metadata$titles
length(result$hits$hits$metadata$references)
result$hits$hits$metadata$publication_info[[1]]$journal_title


View(result$hits$hits$metadata$references[[1]]$reference)
#get citation 
r <- httr::GET(paste0("https://inspirehep.net/api/literature?q=refersto%20recid%204328&size=100&format=json"))

length(result$hits$hits$metadata$publication_info)







  
#  https://inspirehep.net/api/literature?q=tc conference paper and refersto a E.Witten.1 
result <-jsonlite::fromJSON(txt = httr::content(r, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)
  
  
  
result$hits$hits$metadata$publication_info
View(result$hits$hits$metadata)

names(result$hits$hits)


r <- httr::GET(paste0("https://inspirehep.net/api/literature/1802756"))
r$status_code


result <-jsonlite::fromJSON(txt = httr::content(r, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)
result$metadata$




# r$status_code
r2 <- httr::GET(paste0("https://inspirehep.net/api/literature/1700787?format=json"))
 
# r$status_code

r2$status_code
result2 <-jsonlite::fromJSON(txt = httr::content(r2, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)

result2$hits$hits$
result2$hits$hits$id
length(result$data)

result$data$publicat




path_data=choose.files(caption = "chosse your data file")# choisir le fichier concerner 
au_name <- readline(prompt="Nom de Variable 'Nom_auteur': ")#authFullName_s
ti_name <- readline(prompt="Nom de Variable 'Titre_publication': ")#en_title_s
date_name <- readline(prompt="Nom de colonne 'date_publication': ")#defenseDate_s producedDate_s
ths<-read.csv(path_data, sep = ";",header = TRUE,stringsAsFactors = FALSE)
dim(ths)
if(sum(duplicated(ths[c(au_name,ti_name)]))>0) ths<-ths[-(which(duplicated(ths[c(au_name,ti_name)])==TRUE)),]
dim(ths)


ti_data=ths[[ti_name]]
au_data=ths[[au_name]]

path=choose.files(caption = "chosse your data file")



# 
# data_pub=bib2df::bib2df(path, separate_names = FALSE)
# data_pub=conforme_bibtext(data_pub,data_base = "BIB")
# names(data_pub)
# 
# 
# 
# ti_data=data_pub$TITLE
# au_data=data_pub$AUTHOR


res_temp=supress_unfit_entry(ti_data,au_data,sep,max_aut = 2)






ti_data=res_temp[[1]]
au_data=res_temp[[2]]

position_name=rep(1,length(au_data))
position_togo=rep(1,length(au_data))

au_data=fit_name_position(au_data,position_name,position_togo =position_togo )
au_data=sapply(1:length(au_data),FUN = function(x) paste(au_data[[x]],collapse = ";"))

au_data[[1]]

pas=3
inter=ceiling(length(au_data)[1]/pas)# nombre d'iteration
trouver=c()      
error_querry=c()
nb_res=c()
for(i in 1:inter){
  first<-(i-1)*pas+1
  last<-i*pas
  
  
  
  
  
  
  (au_querry=paste0(gsub("%20%20","%20",gsub("[(]","",gsub("[)]","",gsub(";",'%20AND%20',gsub("[?]","",gsub("\\", "",gsub(" ","%20",Unaccent(au_data[first:last])),fixed =TRUE)))))), collapse = 'OR%20a%20','%20'))
  
  
  
  (ti_querry=paste0(gsub("[(]","%28",gsub("[)]","%29",gsub('[}{$]',"",gsub("\\", "",gsub(":","%3A",gsub("/","%2F",gsub("'","%27",gsub(" ","%20",
                                                                                                                                      gsub("e?","e",gsub("?","",gsub("%","%25",((Unaccent(ti_data[first:last]))))),fixed = TRUE))))),fixed = TRUE)))), collapse = 'OR%20t%20','%20'))
  
  
  adress=paste0("&q=a%28",au_querry,"%29%20and%20t",ti_querry,"&format=json")
  r <- httr::GET(paste0("https://inspirehep.net/api/literature/?",adress))

  error=tryCatch({#rep?rage des erreur 
    querry_warning_message(r)
    
  },
  
  warning=function(cond){# mise en forme en cas d'erreur 
    titre_error=as.data.frame(ti_data[first:last])
    names(titre_error)=c("Publication title")
    titre_error["Status error"]=r$status
    titre_error$Message=message_error(r)
    titre_error["Data impact"]="ref & cit"
    titre_error$h=i
    return(titre_error)
  })
  
  if(length(error)>0){
    error_querry<-rbind(error_querry,error)
    error=c()
    
  }else {  
    # paste0("https://inspirehep.net/api/literature/?&q=a%28Witten%20OR%20Barton%20Zwiebach%29%20and%20t%20Searching%20for%20a%20Black%20Hole%20in%20the%20Outer%20Solar%20System%20or%20t%20A%20First%20Course%20in%20String%20Theory&format=json"
if(r$status_code==426){browser() }
  
  result <-jsonlite::fromJSON(txt = httr::content(r, 'text'), simplifyDataFrame = TRUE,simplifyVector = TRUE)
  

  
  if(result$hits$total>0) {
    print(result$hits$total)
    trouver=c(trouver,i)
    nb_res=c(nb_res,result$hits$total)
    #browser()
  }
  
  print(i)
  Sys.sleep(2.01)
  }
}

if(length(error_querry)>0) print("vive les erreur" )
table(error_querry$`Status error`)
table(error_querry$h)
i=1
error_querry$`Status error`
