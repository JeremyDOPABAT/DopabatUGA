library(httr)
get_cit_link_arxiv=function(titre_arxiv){
  page_resp <- GET(paste0("https://scholar.google.com/scholar?q=",gsub(" ","%20",titre_arxiv)))
  page_content <- httr::content(page_resp, as = "text")
  

  text_base <- strsplit(page_content, "\n")[[1]]
  
  #read_html(cite_link[[ar]])%>%html_nodes("body")%>%html_text()
  deb<-grep("Cité" ,text_base)
  part_int<- strsplit((text_base[deb]), "<",fixed = TRUE)
  
  
  deb<-grep("Cité" ,part_int[[1]])
  if(length(deb)>1){
    print("supp")
    browse()
  }
  lien=part_int[[1]][deb]
  
  
  
  supdeb<-max(`attributes<-`(gregexpr("/",lien)[[1]],NULL))
  supfin<-max(`attributes<-`(gregexpr(">",lien)[[1]],NULL))
  
  
  
  lien_complet=paste0("https://scholar.google.com/",substr(lien,supdeb+1,supfin-2))
  
  return(lien_complet)
}







page_resp <- GET("https://scholar.google.com/scholar?q=Exploring%20non%20minimal%20Universal%20Extra%20Dimensional%20Model%20at%20the%20LHC.%20arXiv%202018")
page_content <- httr::content(page_resp, as = "text")

# fouille de text pour trouver les info ___________________________________________
text_base <- strsplit(page_content, "\n")[[1]]

#read_html(cite_link[[ar]])%>%html_nodes("body")%>%html_text()
deb<-grep("Cité" ,text_base)
fin<-grep("</dd>",text_base)




part_int<- strsplit((text_base[deb]), "<",fixed = TRUE)
length(part_int[[1]])


deb<-grep("Cité" ,part_int[[1]])
lien=part_int[[1]][deb]



supdeb<-max(`attributes<-`(gregexpr("/",lien)[[1]],NULL))
supfin<-max(`attributes<-`(gregexpr(">",lien)[[1]],NULL))


substr(lien,supdeb+1,supfin-1)

lien_complet=paste0("https://scholar.google.com/",substr(lien,supdeb+1,supfin-2))

page_resp <- GET(lien_complet)
page_content <- httr::content(page_resp, as = "text")
#deb<-grep("A cosmic census of radio pulsars with the SKA" ,part_int[[1]])
supdeb<-max(`attributes<-`(gregexpr("journal",page_content)[[1]],NULL))



test=read_lines(page_content)
contenu<-test[length(test)]

text_base<-unlist(strsplit(contenu,"<div"))
length(contenu_split)
write(text_base,"contenu_split.txt")

#titre _________________________________________
deb<-grep("<h3",text_base)
fin<-grep("</h3>",text_base)
pice_int<-text_base[deb[2]:fin[2]]

sup_balis=gsub("<.*?>","",pice_int)




#r?cup?r? titre de l'article qui cite/est ref 

  
  

  
sup<-max(`attributes<-`(gregexpr(">",sup_balis)[[1]],NULL))
titre<-substr(,supfin+1,nchar(titre))
#pas fiini 



###________________________________________auteur et journal_______________________

deb<-grep('class="gs_a"',text_base)
fin<-grep('class="gs_rs"',text_base)
pice_int<-text_base[deb[1]:(fin[1]-1)]

aut_deb=min(`attributes<-`(gregexpr(">",pice_int)[[1]],NULL))
aut_fin=min(`attributes<-`(gregexpr("-",pice_int)[[1]],NULL))


auteur=substr(pice_int,aut_deb+1,aut_fin-1)


jou_deb=min(`attributes<-`(gregexpr("-",pice_int)[[1]],NULL))
jou_fin=max(`attributes<-`(gregexpr("-",pice_int)[[1]],NULL))


journal=substr(pice_int,jou_deb+2,jou_fin-2)










arxiv_get_publi<-function(au_data,ti_data,position_name,pas,value_same_min_ask,value_same_min_accept,sep,id){
  #fonction interne qui permet de r?cup?r? les publication de l'api d'arxiv avecc un vecteur de titre et d'auteur, on pr?sice aussi le pourcentage de cimilitude a avoir pour le concerver 
  
  
  #' Title
  #'fonction interne pour l'appli, cette fonction r?cup?re les publi arxiv
  #' @param au_data vecteur data d'auteur 
  #' @param ti_data vecteur de titre 
  #' @param position_name entier 1 ou 2 selon la position du nom par rapport au prenom 
  #' @param pas pas d'avancement de la boucle de r?quete 
  #' @param value_same_min_ask r?elle entre 0 et 1 selon le pourcentage de cimilitude des titre que l'on veut appliquer pour demander a l'utilisateur
  #' @param value_same_min_accept r?elle entre 0 et 1 selon le pourcentage de cimilitude des titre que l'on veut appliquer pour accepter le titre  
  #' @param sep carat?re de separation des auteur 
  #' @param id colone des id arxiv 
  #'
  #' @return datafrale des pubLII
  
  
  
  
  res_temp=supress_unfit_entry(ti_data,au_data,sep,max_aut = 1)
  
  ti_data=res_temp[[1]]
  au_data=res_temp[[2]]
  
  
  position_togo=rep(2,length(au_data))
  au_data=fit_name_position(au_data,position_name,position_togo =position_togo,sep = sep)
  au_data=sapply(1:length(au_data),FUN = function(x) paste(au_data[[x]],collapse = ","))
  
  
  ti_data=gsub("[#$%*<=>@^_`{|}\\]","",ti_data)
  au_data<-gsub("\\s+"," ",gsub(","," ",au_data))
  
  
  res=c()
  res_reject=c()
  error_querry=c()
  id=id[!is.na(id)]
  ind=which(!is.null(id) && !id=="")
  id=id[ind]
  #print(id)
  
  if(length(id)!=0){
    #print("on passe par les id" )
    pas_id=15
    inter=ceiling(length(id)/pas_id)
    au_data=au_data[!ind]
    
    for(h in 1:inter){# boucle principale qui parcour les donn?es 
      
      #incProgress(1/inter)
      first<-(h-1)*pas_id+1# premier individu a prendre en compte(ligne)
      last<-h*pas       # dernier ""   "      "       "   "
      if(last>length(id)[1]) last<-length(id)
      
      # cr?ation de requette espace = + 
      
      
      #querry<-paste0('http://export.arxiv.org/api/query?search_query=(au:( "',(au_querry),'"))&start=0&max_results=2000')#paste0('http://export.arxiv.org/api/query?search_query=(au:( "',au_querry,'"))AND (ti: ("',ti_querry,'"))&start=0&max_results=2000')
      id_querry<-paste0(gsub("arXiv:","",id[first:last]), collapse = '+OR+')
      
      #querry<-paste0('http://export.arxiv.org/api/query?search_query=(au:%20"',au_querry,'")&start=0&max_results=2000')
      querry<-paste0('http://export.arxiv.org/api/query?search_query=id:',id_querry,'&start=0&max_results=2000')
      querry=gsub(" ","+",gsub("\\s+"," ",querry))
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
        if(!is.na(xml)){
          #//Read as lists
          xml_data<-xmlToList(xml)
          
          (total_results=as.numeric(xml_data$totalResults))
          if(total_results>0){
            # a enlever en fin de d?vloppemen 
            
            (id_index=(which(names(xml_data)=="entry")))# ce sist?me ne nous permet pas de choisisr une ligne convenablement donc on les rep?re toute 
            #entry contient tout d'un coup
            res_temp=sapply(1:length(id_index),FUN=function(x,ty=type){
              abs_link=xml_data[id_index[x]]$entry$id
              
              titre=xml_data[id_index[x]]$entry$title
              cite_link<-get_cit_link_arxiv(titre)
              ref_link<-gsub("abs","refs",abs_link)
              
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
      
    }
  }
  if(length(au_data)){
    inter=ceiling(length(au_data)/pas)
    
    # time_min=inter*(5+3)/60
    # print(paste("Le temps d'execution est estimé est  environs ",time_min,"minute(s)"))
    
    
    # withProgress(
    #   message='Please wait',
    #   detail="searching for publication in arxiv",
    #   value=0, {
        for(h in 1:inter){# boucle principale qui parcour les donn?es
          
          #print(h)
          #print("On passe title")
          start=c(start,Sys.time())
#          incProgress(1/inter)
          first<-(h-1)*pas+1# premier individu a prendre en compte(ligne)
          last<-h*pas       # dernier ""   "      "       "   "
          if(last>length(au_data)[1]) last<-length(au_data)
          
          # cr?ation de requette espace = + 
          
          
          #querry<-paste0('http://export.arxiv.org/api/query?search_query=(au:( "',(au_querry),'"))&start=0&max_results=2000')#paste0('http://export.arxiv.org/api/query?search_query=(au:( "',au_querry,'"))AND (ti: ("',ti_querry,'"))&start=0&max_results=2000')
          (au_querry<-paste0('%28',gsub("[?]","",gsub(",","%2C",trimws(Unaccent(au_data[first:last])))),'%29', collapse = '+OR+'))
          ti_querry<-paste0('%28',gsub("[?]","",gsub(",",'2C',gsub(":",'%3A',gsub(';','%3B',gsub("+","%2B",trimws(Unaccent(ti_data[first:last])),fixed = TRUE))))),'%29', collapse = '+OR+')
          
          #querry<-paste0('http://export.arxiv.org/api/query?search_query=(au:%20"',au_querry,'")&start=0&max_results=2000')
          querry<-paste0('http://export.arxiv.org/api/query?search_query=%28au:',au_querry,'%29+AND+%28ti:',ti_querry,'%29&start=0&max_results=2000')   #%20AND%20ti:%28"',ti_querry,'"%29
          
          querry=gsub('"',"%22",querry)
          querry=gsub(" ","+",gsub("\\s+"," ",querry))
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
            if(!is.na(xml)){
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
        }
      #})
  }
  return(list(res=res,error=error_querry,reject=res_reject)) 
}










#coté arxiv _______________________________________________________________