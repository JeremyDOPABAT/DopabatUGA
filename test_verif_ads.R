path_data=choose.files(caption = "chosse your data file")# choisir le fichier concerner 
au_name <- readline(prompt="Nom de Variable 'Nom_auteur': ")#authFullName_s
ti_name <- readline(prompt="Nom de Variable 'Titre_publication': ")#en_title_s
date_name <- readline(prompt="Nom de colonne 'date_publication': ")#defenseDate_s producedDate_s

ths<-read.csv(path_data, sep = ";",header = TRUE,stringsAsFactors = FALSE)
guess_encoding(read_lines_raw(path_data))
View(ths)
dim(ths)
if(sum(duplicated(ths[c(au_name,ti_name)]))>0) ths<-ths[-(which(duplicated(ths[c(au_name,ti_name)])==TRUE)),]
dim(ths)

token="SYZW1C1o9mDDfOWXUSh7Jrq0MMqvleL2is0pnTNQ"

source("functions_analyses.R")
library(plyr)


res_data_nasa_ads=extraction_data_api_nasa_ads(data_pub=ths,ti_name=ti_name,au_name=au_name,token=token,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0,type="all")
res_data_nasa_ads$error_querry_publi
dim(res_data_nasa_ads$dataframe_citation_accept)

View(df)
value_same_min_ask=0.85
value_same_min_accept=0.95

pas=1
type="cit"

position_name=rep(2,6)




path_data=choose.files(caption = "chosse your data file")# choisir le fichier concerner 
au_name <- readline(prompt="Nom de Variable 'Nom_auteur': ")#authFullName_s
ti_name <- readline(prompt="Nom de Variable 'Titre_publication': ")#en_title_s
date_name <- readline(prompt="Nom de colonne 'date_publication': ")#defenseDate_s producedDate_s

ths<-read.csv(path_data, sep = ";",header = TRUE,stringsAsFactors = FALSE)

ths$position_name=1


res_arxiv=extraction_data_api_arxiv(data_pub=ths,ti_name=ti_name,au_name=au_name,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0.85,type = "all")
                                                                          View(res_arxiv$res_reject)
  
dim(res_arxiv$res_citation_ask)

extraction_data_api_arxiv<-function(data_pub,ti_name,au_name,pas=8,value_same_min_accept=0, value_same_min_ask=1,type="cit",source_name="",sep=","){
  
  # fonction permetant d'interroger arxiv sur les reference et les citation d'un corpus de publication placer en entr?e 
  #intput   
  #-data_pub: corpus de publication doit au moin contenir les titre et les auteur en anglais(dans un premier temps )
  # ti_name non de la colonne titre 
  # au name : nom de la colonne auteur 
  # pas: nombre de publication par requete (joue sur l'impact de l'erreur )
  #value_same_min_accept= valeur entre ? et 1 repr?sente le taux minimum de cimilitude a avoir pour accespter la r?ponse de la bd dans l'?tude 
  #value_same_min_ask=valeur minimal pour demander a l'utilisteur si il veul la publication dans l'?tude 
  # type: si type vaut "cit il va chercher les sitation, sinon les reference 
  
  
  #outpout res : liste de 10 ?l?m?ents
  #all_author_found : auteur trouv?s par la base de donn?es arxiv 
  #all_title_found : titre retrouver par la base de donn?es 
  # author_vector:vecteur d'auteur retenu pour les requ?tes
  #error_querry: matrice de rapport d'erreur 
  # error_querry_cit matrice d'erreur concertnant la partie citation/reference 
  #last_result : resultat brut de la derni?re requ?t 
  #res_accept: dataframe des resultat accespt? (citation ou reference )
  #res_ask: dataframe des resultat demander (citation ou reference )
  # res_reject : dataframe(publication rejet? )
  #title_vector : vecteur titre 
  #usuful library 
  
  
  library("XML")
  library("httr")
  #source("C:/Users/moroguij/Documents/R_programs/functions_analyses.R")
  
  
  #3
  #initialisation _________________________________________
  #utile pour calculer de temps d'excution 
  start=c()
  end=c()
  # ~_~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res=c() # dataframe resulta
  res_reject=c()# dataframe de rejet
  
  
  # vecteur de suivie po
  trouver=c()#identifiant trouver  
  ti_trouver=c()# titre trouver 
  au_trouver=c()# auteur trouver 
  h_trouver<-c()# conteur trouver 
  
  #__________________________________________*
  # initialisation matrice d'erreur 
  error_querry=c()
  error_querry_cit=c()
  
  
  st<-Sys.time()# temp d?but 
  
  #____________________________________________________________
  
  
  
  
  # if(ti_ask==TRUE){
  ti_data<-data_pub[ti_name][[1]]
  
  au_data<-data_pub[au_name][[1]]
  
  
  if(source_name!="" && (type=="ref" || type=="all")){
    data_wos=data_pub[data_pub[source_name]=="WOS",]
    if(dim(data_wos)[1]!=0) {
      
      res_wos_all=axiv_get_publi(data_wos[au_name][[1]],data_wos[ti_name][[1]],data_wos$position_name,pas,value_same_min_ask,value_same_min_accept,sep)
      res_wos=res_wos_all$res
      err1=res_wos_all$error
      reject1=res_wos_all$reject
      
    }
    data_rest=data_pub[data_pub[source_name]!="WOS",]
    if(dim(data_rest)[1]!=0) {
      res_rest_all=axiv_get_publi(data_rest[au_name][[1]],data_rest[ti_name][[1]],data_rest$position_name,pas,value_same_min_ask,value_same_min_accept,sep)
      res_rest=res_rest_all$res
      err2=res_rest_all[2,]$error
      reject2=res_rest_all$reject
      
    }
    
    res=rbind(res_rest,res_wos)
    error_querry=rbind(err1,err2)
    reject=rbind(reject1,reject2)
  }else {
    resdt_all=axiv_get_publi(au_data,ti_data,data_pub$position_name,pas,value_same_min_ask,value_same_min_accept,sep)
    error_querry=resdt_all$error
    reject=resdt_all$reject
    res=resdt_all$res
    
  }
  if(length(dim(error_querry))>0) print("il y a des erreurs")
  
  if(length(res)>0){
    resdt<-as.data.frame(res,stringsAsFactors = FALSE)   
    names(resdt)=c("h","abs_link","date_pub", "title", "author", "subject","journal","check_pct","check_ind","citation_link","reference_link")
    #voir apr?s _________________________________________
    
    
    
    if(type=="all"|| type=="cit" ){
      list_citation<-get_cit_or_ref_arxiv(resdt,type="cit")
      res_citation=list_citation$resdt_final[list_citation$resdt_final$check>=value_same_min_accept,]
      res_citation_ask=list_citation$resdt_final[(list_citation$resdt_final$check>=value_same_min_ask&list_citation$resdt_final$check<value_same_min_accept),]
      
      error_querry_cit=list_citation$error_querry
      error_querry=rbind(error_querry,list_citation$error_querry)
      if( type=="cit"){
        res_reference=NA
        error_querry_ref=NA
        res_reference_ask=NA
      }
    }
    if(type=="all"|| type=="ref" ){
      if(source_name==""){
        res_rest=resdt
      }
      list_reference<-get_cit_or_ref_arxiv(resdt,type="ref")
      res_reference=list_reference$resdt_final[list_reference$resdt_final$check>=value_same_min_accept,]
      res_reference_ask=list_reference$resdt_final[(list_reference$resdt_final$check>=value_same_min_ask&list_reference$resdt_final$check<value_same_min_accept),]
      error_querry_ref=list_reference$error_querry
      error_querry=rbind(error_querry,list_reference$error_querry)
      
      if( type=="ref"){
        res_citation=NA
        res_citation_ask=NA
        error_querry_cit=NA
      }
    }
    
    
    
    
    
    
    resdt_reject<-as.data.frame(reject)
    
    # colnames(resdt)<-c("Cité","Titre", "Auteur","Domaine","Journal")
    # ft<-Sys.time()
    
    
    return(list(res_publi_foundt=resdt, res_citation_accept=res_citation,res_citation_ask=res_citation_ask,res_reference_accept=res_reference,res_reference_ask=res_reference_ask, res_reject=resdt_reject,error_querry=error_querry,error_querry_cit=error_querry_cit,error_querry_ref=error_querry_ref,title_vector=ti_data,author_vector=au_data,all_title_found=ti_trouver,all_author_found=au_trouver))
  }else{
    print("no data found")
    return(NA)
  }
}

data_pub=ths

position_base=rep(2,length(au_data))
au_data<-data_pub[au_name][[1]]
ti_data=data_pub[ti_name][[1]]

pas=8


axiv_get_publi<-function(au_data,ti_data,position_name,pas,value_same_min_ask,value_same_min_accept,sep=","){
  #fonction interne qui permet de r?cup?r? les publication de l'api d'arxiv avecc un vecteur de titre et d'auteur, on pr?sice aussi le pourcentage de cimilitude a avoir pour le concerver 
  
  
  
  
res_temp=supress_unfit_entry(ti_data,au_data,sep)
  
  ti_data=res_temp[[1]]
  au_data=res_temp[[2]]
  
  
  position_togo=rep(1,length(au_data))
  au_data=fit_name_position(au_data,position_name,position_togo =position_togo )
  au_data=sapply(1:length(au_data),FUN = function(x) paste(au_data[[x]],collapse = ","))
  
  
  ti_data=gsub("[#$%*<=>@^_`{|}\\]","",ti_data)
  au_data<-gsub("\\s+"," ",gsub(","," ",au_data))
  print(au_data[1])

  
  res=c()
  res_reject=c()
  inter=ceiling(length(au_data)/pas)
  error_querry=c()
  # time_min=inter*(5+3)/60
  # print(paste("Le temps d'execution est estimé est  environs ",time_min,"minute(s)"))
  for(h in 1:inter){# boucle principale qui parcour les donn?es 
    print(h)
    start=c(start,Sys.time())
    
    first<-(h-1)*pas+1# premier individu a prendre en compte(ligne)
    last<-h*pas       # dernier ""   "      "       "   "
    if(last>length(au_data)[1]) last<-length(au_data)
    
    # cr?ation de requette espace = + 
    
    
    #querry<-paste0('http://export.arxiv.org/api/query?search_query=(au:( "',(au_querry),'"))&start=0&max_results=2000')#paste0('http://export.arxiv.org/api/query?search_query=(au:( "',au_querry,'"))AND (ti: ("',ti_querry,'"))&start=0&max_results=2000')
    (au_querry<-paste0('%28',gsub("[?]","",gsub(",","%2C",trimws(Unaccent(au_data[first:last])))),'%29', collapse = '+OR+'))
    ti_querry<-paste0('%28',gsub("[?]","",gsub(",",'2C',gsub(":",'%3A',gsub(';','%3B',gsub("+","%2B",trimws(Unaccent(ti_data[first:last])),fixed = TRUE))))),'%29', collapse = '+OR+')
    
    #querry<-paste0('http://export.arxiv.org/api/query?search_query=(au:%20"',au_querry,'")&start=0&max_results=2000')
    querry<-paste0('http://export.arxiv.org/api/query?search_query=%28au:',au_querry,'%29+AND+%28ti:',ti_querry,'%29&start=0&max_results=2000')   #%20AND%20ti:%28"',ti_querry,'"%29
    querry=gsub(" ","+",gsub("\\s+"," ",querry))
    querry=gsub('"',"%22",querry)
    (querry=gsub("++","+", querry,fixed = TRUE))
    r <- GET(querry)
    Sys.sleep(3)#obligation de 3 sec entre chaque roquette term of use de la bd arciv 
    
    error=tryCatch({#permet de recenser les erreur et les cause potentiel 
      querry_warning_message(r)
      
    },
    
    warning=function(cond){
      titre_error=as.data.frame(ti_data[first:last])
      names(titre_error)=c("Publication title")
      titre_error["Status error"]=r$status
      titre_error$Message=message_error(r)
      titre_error["Data impact"]="ref & cit"
      titre_error$h=h
      return(titre_error)
    })
    
    if(length(error)>0){
      error_querry<-rbind(error_querry,error)
      error=c()
      
    }else {# on achemine les resultat trouver dans la data frame 
      #//Extract the text from the response
      xml<-httr::content(r, as="text")
      #//Read as lists
      xml_data<-xmlToList(xml)
      
      (total_results=as.numeric(xml_data$totalResults))
      if(total_results>0){
        # a enlever en fin de d?vloppemen 
        
        (id_index=(which(names(xml_data)=="entry")))# ce sist?me ne nous permet pas de choisisr une ligne convenablement donc on les rep?re toute 
        #entry contient tout d'un coup
        res_temp=sapply(1:length(id_index),FUN=function(x,ty=type){
          abs_link=xml_data[id_index[x]]$entry$id
          cite_link<-gsub("abs","cits",abs_link)
          ref_link<-gsub("abs","refs",abs_link)
          
          titre=xml_data[id_index[x]]$entry$title
          author=unlist(xml_data[id_index[x]]$entry[ which(names(xml_data[id_index[x]]$entry)=="author")]) #affiliation possible 
          names(author)=c()
          category=unlist(xml_data[id_index[x]]$entry[ which(names(xml_data[id_index[x]]$entry)=="category")])[1]
          journal=unlist(xml_data[id_index[x]]$entry[ which(names(xml_data[id_index[x]]$entry)=="journal_ref")])[1]
          date_pub=unlist(xml_data[id_index[x]]$entry[ which(names(xml_data[id_index[x]]$entry)=="published")])[1]
          if(is.null(journal)) journal<-NA
          if(is.null(date_pub)) date_pub<-NA
          return(list(abs_link,cite_link,titre,author,category,journal,date_pub,ref_link))
        })
        
        #on affect les resultat trouver     
        
        abs_link=res_temp[1,]
        cite_link=res_temp[2,]
        tps=res_temp[3,]
        add=res_temp[4,]
        cat=res_temp[5,]
        journal=res_temp[6,]
        date_pub=res_temp[7,]
        ref_link=res_temp[8,]
        #last_good_res=xml_data
        # calcule du pourcentage de similitude par titre 
        indic_compaire_title<-compaire_title(unlist(tps),ti_data[first:last])
        check_title_pct<-unlist(indic_compaire_title[1,])
        check_title_ind<-unlist(indic_compaire_title[2,])+first-1
        #____________________________________________
        
        # trouver<-c(trouver,abs_link)
        # h_trouver<-c(h_trouver,h)
        # ti_trouver<-c(ti_trouver,tps)
        # 
        #au_trouver<-c(au_trouver,add)
        it_ok=(check_title_pct>=value_same_min_ask)# si la valeur n'est pas a rejeter on la garde 
        
        ind=which(it_ok==FALSE)
        if(length(ind>0)) {#si il ya des bon ?l?ment 
          cite_link=cite_link[-ind]
          ref_link=ref_link[-ind]
          abs_link=abs_link[-ind]
          res_reject=rbind(res_reject,cbind(tps[ind],add[ind],check_title_pct[ind]))
          tps<-tps[-ind]
          add<-add[-ind]
          check_title_pct<-check_title_pct[-ind]
          check_title_ind<-check_title_ind[-ind]
        }
        if(length(cite_link)>0){
          for(ar in(1:length(cite_link))){
            cited_aut=list(add[[ar]])
            cited_sub=list(cat[[ar]])
            cited_journal=list(journal[[ar]])
            cited_date_pub=list(date_pub[[ar]])
            cited_title<-tps[[ar]]
            
            
            res<-rbind(res,cbind(h,abs_link[[ar]],cited_date_pub,cited_title,cited_aut,cited_sub,cited_journal,check_title_pct[[ar]],check_title_ind[[ar]],cite_link[[ar]],ref_link[[ar]]))
          }
        }
      }
    }
  }
  return(list(res=res,error=error_querry,reject=res_reject))
}


res_pumed=extract_data_api_pumed(data_pub=ths,ti_name,au_name,pas=8,value_same_min_accept=0.85, value_same_min_ask=0.95,type="all")





