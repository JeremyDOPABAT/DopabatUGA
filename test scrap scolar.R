library(httr)





library(httr)


page_resp <- GET("https://scholar.google.com/scholar?q=Exploring%20non%20minimal%20Universal%20Extra%20Dimensional%20Model%20at%20the%20LHC.%20arXiv%202018")
page_content <- httr::content(page_resp, as = "text")

# fouille de text pour trouver les info ___________________________________________
text_base <- strsplit(page_content, "\n")[[1]]

#read_html(cite_link[[ar]])%>%html_nodes("body")%>%html_text()
deb<-grep("Cité" ,text_base)
fin<-grep("</dd>",text_base)




part_int<- strsplit((text_base[deb]), "<",fixed = TRUE)


part_int
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



text_base <- strsplit(page_content, "\n")[[1]]
contenu<-text_base[length(text_base)]

contenu_split<-unlist(strsplit(contenu,"<div"))
length(contenu_split)

#titre _________________________________________
deb<-grep("<h3",contenu_split)
fin<-grep("</h3>",contenu_split)


pice_int<-contenu_split[deb[2]:fin[2]]

sup_balis=gsub("<.*?>","",pice_int)




#r?cup?r? titre de l'article qui cite/est ref 

  
  

  
supfin<-max(`attributes<-`(gregexpr(">",sup_balis)[[1]],NULL))
titre<-substr(sup_balis,supfin+1,nchar(sup_balis))
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




















#coté arxiv _______________________________________________________________



get_cit_or_ref_arxiv<-function(resdt,type){ 
  #Permet de récupré les citation et les reference de arxiv en se basant sur une df d'entré '
  # fonction interne 
  #input dataframe de publication trouver
  error_querry_cit=c()
  if(type=="cit"){  
    link_list<-unlist(resdt$citation_link)
  }#else{
    #link_list<-unlist(resdt$reference_link)
  #}
  
  link_abs<-unlist(resdt$abs_link)
  res_cit<-c()
  # withProgress(
  #    message='Please wait',
  #    detail=paste0("searching for ",type, "in arxiv"),
  #      value=0, {
  
  for(ar in(1:length(link_list))){
    #cited<-abs_link[[ar]]#lien vers l'article trouver 
    
    page_resp <- GET(link_list[[ar]])# code souce de la page inspire
    error=tryCatch({
      querry_warning_message(page_resp)
      
    },
    warning=function(cond){#verification d'erreur 
      titre_error=as.data.frame(resdt$titre[first:last])
      names(titre_error)=c("Publication title")
      titre_error["Status error"]=r$status
      titre_error$Message=message_error(r)
      titre_error["Data impact"]=type
      titre_error$h=ar
    })
    #    incProgress(1/length(link_list))
    if(length(error)>0){
      error_querry_cit<-rbind(error_querry_cit,error)
      error=c()
      
    }else {
      #Sys.sleep(3)
      page_content <- httr::content(page_resp, as = "text")
      
      # fouille de text pour trouver les info ___________________________________________
      text_base <- strsplit(page_content, "\n")[[1]]
      
      #read_html(cite_link[[ar]])%>%html_nodes("body")%>%html_text()
      
      contenu<-text_base[length(text_base)]
      
      contenu_split<-unlist(strsplit(contenu,"<div"))
      
      
      deb<-grep("<h3",contenu_split)
      fin<-grep("</h3>",contenu_split)
      
     
      
      
      if(length(deb)>0){# si il ya as des citation  
       
        for(i in 1:length(deb)){
          #print(deb[i])
          if(is.na(fin[i])) fin[i]=length(text_base) 
          
          
          
          pice_int<-contenu_split[deb[i]:fin[i]]
          
          
          if(length(pice_int)>0){
            sup_balis=gsub("<.*?>","",pice_int)
            
            #r?cup?r? titre de l'article qui cite/est ref 
            
            supfin<-max(`attributes<-`(gregexpr(">",sup_balis)[[1]],NULL))
            titre<-substr(sup_balis,supfin+1,nchar(sup_balis))
            
          }else {titre<-NA}
          deb<-grep('class="gs_a"',text_base)
          fin<-grep('class="gs_rs"',text_base)
          pice_int<-text_base[deb[1]:(fin[1]-1)]
          
          
          
          
          #recupp?ration des auteurs 
          
          if(length(pice_int)>0){
            
            aut_deb=min(`attributes<-`(gregexpr(">",pice_int)[[1]],NULL))
            aut_fin=min(`attributes<-`(gregexpr("-",pice_int)[[1]],NULL))
            
            
            aut=substr(pice_int,aut_deb+1,aut_fin-1)
            
            aut= gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",aut)
            aut=list(aut)
            
            # ind_comment<-grep("Comments:</span>",aut) #detection de commentaire 
            # if(length(ind_comment)>0) aut<-aut[-ind_comment] #supression des commentaires 
            # 
            
          }else{aut<-NA}# en cas de valeur null 
          
          #traitement du subject 
          # suj<-pice_int[grep("Subject",pice_int)]
          # if(length(suj)!=0){
          #   ind1<-max(`attributes<-`(gregexpr("\">",suj)[[1]],NULL))
          #   suj<-substr(suj,ind1+2,nchar(suj))
          #   suj<-gsub("[#$%*,.:;<=>@^_`{|}~.]","",gsub("/span","",suj))
          # } else{suj<-NA}
          # 
          jou_deb=min(`attributes<-`(gregexpr("-",pice_int)[[1]],NULL))
          jou_fin=max(`attributes<-`(gregexpr("-",pice_int)[[1]],NULL))
          
          
          jour=substr(pice_int,jou_deb+2,jou_fin-2)
          
           if(length(journ)==0) journ<-NA
          suj<-NA
          
          
          res_cit<-rbind(res_cit,cbind(ar,link_abs[[ar]],titre,aut,suj,journ))
          
        }
      }
    }
  }
  #})
  
  
  
  resdt_cit<-as.data.frame(res_cit)
  if(length(res_cit)>0){
    names(resdt_cit)<-c("compteur","identifier","title","author","subject","journal")
    
    
    
    
    
    
    ind_id<-sapply(resdt_cit$identifier,function(x){
      return(grep(x,(resdt$abs_link)))
    })
    
    
    
    
    res_link_final=cbind(resdt[ind_id,],res_cit)
    res_link_final<-res_link_final[,-which(names(res_link_final)=="ar")]
    res_link_final<-res_link_final[,-which(names(res_link_final)=="V2")]
    res_link_final<-res_link_final[,-which(names(res_link_final)=="reference_link")]
    res_link_final<-res_link_final[,-which(names(res_link_final)=="citation_link")]
    
    if(type=="cit"){# d?finition des nom de colone attention a respecter l'ordre
      names(res_link_final)<-c("h","cited identifier","cited date","cited title","cited auth", "cited subject","cited journal","check","check ind", "citing title","citing auth","citing subject","citing journal" )
      res_link_final<-res_link_final[order(unlist(res_link_final$`cited identifier`)),]
    # }else{
    #   names(res_link_final)<-c("h","refering identifier","refering date","refering title","refering auth", "refering subject","refering journal","check","check ind", "refered title","refered auth","refered subject","refered journal" )  
    #   res_link_final<-res_link_final[order(unlist(res_link_final$`refering identifier`)),]
    # }
  }else res_link_final=NULL
    if(length(dim(error_querry_cit))>0) print("il y a des erreurs dans les citations")
  
  
  
    if(length(dim(res_link_final)[1])>0) row.names(res_link_final)<-1:dim(res_link_final)[1]
  }else res_link_final=NULL 
  
  return(list(resdt_final=res_link_final,error_querry=error_querry_cit))

}
