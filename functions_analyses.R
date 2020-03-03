library(network)
library(igraph)
library(tidyverse)
library(igraphdata)
require("tm")
require('wordcloud')
require("RColorBrewer")
require("rvest")
require("visNetwork")
library(pdftools)
library(tidyverse)
library(httr)
library(lubridate)


make_input_notwork_graph<-function(keywords) { 
  # fonction interne cr?an les diff?rentes liste pour cr?e le graph
  #intput  keywords: liste de mot cl?s 
  
  #outpout res : liste de 3 ?l?m?ent 
  #            data_eges : table de deux colonnes "from" sommet et "to" sommet d'arriv? 
  #            node_list liste de sommet de graph 
  #            wei poids de chaque sommet (d?termine la taille du sommet )             
  from=c()# liste des mot d?part 
  to=c()# liste des mot "arriver" enssemble ces deux variable crees les arrettes du graph 
  
  wei=table(unlist(keywords))# poid de chaque terme au niveau des noeuds 
  term<-unique(unlist(keywords))# les mot ck? unique pour faire les noeud du graph 
  term<-term[order(factor(term, levels=names(wei)))]# on odonne les nom dans le m?me ordre que l'ordre de poid pour les faire correspondre ensuite 
  
  
  if(is_empty(which(names(wei)==""))==FALSE){ 
    wei<-wei[-(which(names(wei)==""))]
    term<-term[-(which(term==""))]
  }
  
  
  
  for(i in(1:length(keywords))){
    if(length(keywords[[i]])>1){
      for(j in(1:length(keywords[[i]]))){
        if(length(keywords[[i]])>j){
          add_from=rep(keywords[[i]][j],length(keywords[[i]])-j)
          add_to=c(keywords[[i]][(j+1):length(keywords[[i]])])
          from=c(from,add_from)
          to=c(to,add_to)
        }
      }
    }
  }
  
  if(length(term)!=0){  
    edge_list <- tibble(from = from, to = to)
    
    node_list <- tibble(id = term)
  } else{
    edge_list <- tibble(from = "no data as been found", to = "no data as been found")
    
    node_list <- tibble(id = "no data as been found")
    wei=table(c("no data as been found"))
  }
  data_edge <- edge_list %>%  
    group_by(from, to) %>%
    dplyr::summarise(weight = n()) %>% 
    ungroup()
  if(length(which(data_edge["to"]==data_edge["from"])!=0)) data_edge<-data_edge[-(which(data_edge["to"]==data_edge["from"])),]
  
  res_list=list(data_edge,node_list,wei)
  return(res_list)
}





make_input_domain_graph<-function(domainall){
  # fonction interne cr?an les diff?rentes liste pour cr?e le graph du domaine, cette fonction est une varriante assez diff?rente que son
  #homologue du m?me nom pour les graph standard car le traitement est diff?rent 
  #intput  keywords: liste de mot cl?s 
  
  #outpout res : liste de 3 ?l?m?ent 
  #            data_eges : table de deux colonnes "from" sommet et "to" sommet d'arriv? 
  #            node_list liste de sommet de graph 
  #            wei poids de chaque sommet (d?termine la taille du sommet )             
  
  domainall_split<-(strsplit(domainall,","))
  
  
  wei=table(unlist(domainall_split))# poid de chaque terme au niveau des noeuds 
  term<-unique(unlist(domainall_split))# les mot ck? unique pour faire les noeud du graph 
  term<-term[order(factor(term, levels=names(wei)))]# on odonne les nom dans le m?me ordre que l'ordre de poid pour les faire correspondre ensuite 
  
  
  if(is_empty(which(names(wei)==""))==FALSE){ 
    wei<-wei[-(which(names(wei)==""))]
    term<-term[-(which(term==""))]
  }
  
  
  from=c()# liste des mot d?part 
  to=c()# liste des mot "arriver" enssemble ces deux variable crees les arrettes du graph 
  
  
  for(i in(1:length(domainall))){
    if(length(domainall_split[[i]])>1){
      add_from=rep(domainall_split[[i]][1],length(domainall_split[[i]])-1)
      add_to=c(domainall_split[[i]][(2):length(domainall_split[[i]])])
      from=c(from,add_from)
      to=c(to,add_to)
      
    }
  }
  
  
  edge_list <- tibble(from = from, to = to)
  
  node_list <- tibble(id = term)
  
  
  data_edge <- edge_list %>%  
    group_by(from, to) %>%
    dplyr::summarise(weight = n()) %>% 
    ungroup()
  if(length(which(data_edge["to"]==data_edge["from"])!=0)) data_edge<-data_edge[-(which(data_edge["to"]==data_edge["from"])),]
  
  res_list=list(data_edge,node_list,wei)
  return(res_list)
}


make_top_graph<-function(graph_input,wei_table,top_number){
  
  # fonction interne permettant de faire un graphique de type "top" en ne concervant que les sommet les plus pris et leur relation, 
  #concerve toutes les relations entre ces sommets
  
  
  # input graph_unput : graphique de base ? modifier
  # input wei_table : table des poids des sommets 
  # input top number : entier permettant de savoir  quand arreter le top
  
  #output   graph_par graphique objet, le graph r?sultat 
  #NB Not? que cette fonction ne plot pas le graphique
  graph_par=graph_input
    if(length(V(graph_input))>0){
    weitop<-sort(wei_table,decreasing = TRUE)
    print(head(weitop,top_number))
    l<-c()
    
    for(k in(1:length(head(weitop,top_number)))){#on cherche les relation des noeud les plus cit? (les mots cite en paralleles ) 
      temp<-names(unlist(graph_input[[names(head(weitop,top_number))[k],]][[1]]))
      l<-c(l,temp)# on fait une grand liste de mot qui nous fera une nouvelle base de noeud 
      
    }
    
    l<-unique(c(l,names(head(weitop,top_number))))# on ajoute les mot du top et on supprime les doublons 
    graph_par<-induced_subgraph(graph_input, l)# on se sert du travail fait en amons et on ne prend que la partie de graph qui interesse 
    
    #couleur des noeud 
    V(graph_par)$color<-"grey"
    V(graph_par)$group="node in direct relations"
    V(graph_par)[names(head(weitop,top_number))]$color<-"red"
    V(graph_par)[names(head(weitop,top_number))]$group="node in top"
    E(graph_par)$color<-"grey"
    
    }
  #on plot 
  #plot(graph_par, vertex.label.cex=0.7,vertex.size=(V(graph_par)$weight+2),edge.width= E(graph_par)$weight,layout=layout_with_fr)
  return(graph_par)  
}






make_network_graph<-function(keywords,publication_date=0,top_number=0,interval_year=0,sup_ones=TRUE,domain=FALSE,root_weight=FALSE){
  set.seed(1234)
   # fonction principale permettant la r?alisation des graphique de type r?seau 
  
  # input keywords : liste contenant les mot cl? par article (liste de liste) param?tre obligatoire 
  # input publication_date : liste contenant les date de publication des articles ayant fourni les mots cl?s
  # input top number : entier permettant de savoir  quand arreter le top defaut 0(donc pas de top)
  # input interval_year : nombre d'ann?e prit en count pour chaque graphique (donc plusieur graphique effectuer en fct de la p?riode )
  # input sup_ones : bol?een si il est a "TRUE" supprime les sommet pris une seul fois  
  # input domain : bol?en si a true , applis le protovole de gr?ation d'un graph corespondant au domaine de comp?tence(avec les sous domaine(et plus au mot cl?) 
  # input root_weight : bol?en si a "true" remplace les poid des sommmet par leur racine carr? plus 1
  
  
  #output res_graph listte des graphique cr?e permet de sauver les graphique sans avoir a faire tourn? la fonction a chaque fois  
  
  by_year=FALSE
  top=FALSE
  graph_res=list()
  add_title_sup=""
  add_title_top=""
  
  par(mar=c(0.5,0.5,0.5,0.5)+.1)
  
  
  suppressWarnings(if(!is.na(as.numeric(top_number))) top_number<-as.numeric(top_number))
  
  if(is.numeric(top_number) & top_number>0) top=TRUE
  
  suppressWarnings(if(!is.na(as.numeric(interval_year))) interval_year<-as.numeric(interval_year))
  
  if(is.numeric(interval_year) & interval_year>0){
    
    
    year<-publication_date
    
    diff<-max(year)-min(year)# calcule du nombre de graph necessaire 
    
    iter<-ceiling(diff/interval_year)
    if(iter<1) iter=1
    
    for (i in 1:iter){# on cr? chaqun des graphs
      if(i!=iter){# si on est pas sur la derni?re date
        index_year<-((year>=min(year)+(i-1)*interval_year) &(year<min(year)+i*interval_year))
        add_brack="["
      }
      else{
        index_year<-((year>=min(year)+(i-1)*interval_year) &(year<=min(year)+i*interval_year))
        add_brack="]"
      }
      key_c<-keywords[index_year]# on ne prend que les mots clef dans l'interval qui nous interesse 
      
      
      from=c()
      to=c()
      
      
      #on cree les liste 
      if(domain==FALSE){
        res=make_input_notwork_graph(key_c)
        direct=FALSE
        add_title_domaine="keywords"
      }else { 
        res=make_input_domain_graph(key_c)
        direct=TRUE
        add_title_domaine="domains(and association of domains)"
      }
        
    
      data_edge=res[[1]]
      node_list=res[[2]]
      wei=res[[3]]
      
      c_igraph <- graph_from_data_frame(d = data_edge[,c(1,2)], vertices = node_list, directed = direct)%>%
        set_edge_attr("width", value =data_edge$weight)%>%
        set_vertex_attr("weight", value =wei)%>%
        set_vertex_attr("size", value =wei+4)%>%
        set_vertex_attr("label.cex", value =1)
        
      V(c_igraph)$title=names(wei)
      V(c_igraph)$group="node(keyword)"
      
      
      # cr?ation du graphique en assignant ces caract?ristique(permet une utilisation plus simple )
      if(sup_ones==TRUE){ # on supprime les sommet non interessant 
        ind_sup<-V(c_igraph)$weight==1
        c_igraph=delete_vertices(c_igraph,ind_sup)
        add_title_sup="found more than ones,"
      }   
      b=min(year)+(i)*interval_year
      if(b>max(year)) b<-max(year)
      main_title=paste("Graph of ",add_title_domaine,add_title_top,add_title_sup , "year :[",min(year)+(i-1)*interval_year,":",b,add_brack)
      if(root_weight==TRUE){
        V(c_igraph)$size=sqrt(V(c_igraph)$size)  
      }
      
      if(top==TRUE ){# on se focalise sur les sommets les plus pris 
        c_igraph<-make_top_graph(c_igraph,wei,top_number)
        add_title_top=paste("top",top_number,",")
      }
      
      temp=visIgraph(c_igraph, idToLabel = TRUE, layout = "layout_nicely",
                physics = FALSE, smooth = FALSE, type = "square",
                randomSeed = NULL)%>% 
        visInteraction(navigationButtons = TRUE)  %>% visLegend(main = main_title,position = "left",zoom = FALSE) %>%
        visGroups(groupname = "node in top", color = "red") %>%
        visGroups(groupname = "node in direct relations", color = "grey")%>% visExport(type="pdf")
      
      
      
      
      graph_res=append(graph_res,list(temp))
      #plot(c_igraph, vertex.size=(V(c_igraph)$weight+1),edge.arrow.size=0.5, vertex.label.cex=0.55,edge.width=E(c_igraph)$weight,main=main_title)  
      } 
  
 
  } else{# si on n'a pas sp?cifier par date 
    
    
    
    if(domain==FALSE){
      res=make_input_notwork_graph(keywords)
      direct=FALSE
      add_title_domain="keywords"
    }else { 
      #print("passe")
      res=make_input_domain_graph(keywords)
      direct=TRUE
      add_title_domain="domains(and association of domains)"
    }
    
    
    data_edge=res[[1]]
    node_list=res[[2]]
    wei=res[[3]]
    
    
    c_igraph <- graph_from_data_frame(d = data_edge[,c(1,2)], vertices = node_list, directed = direct)%>%
      set_edge_attr("width", value =data_edge$weight)%>%
      set_vertex_attr("weight", value =wei)%>%
      set_vertex_attr("size", value =wei+4)%>%
      set_vertex_attr("label.cex", value =1)
    V(c_igraph)$title=names(wei)
    V(c_igraph)$group="node(keyword)"
    
    #print(vertex_attr(c_igraph,"extra"))
    if(sup_ones==TRUE){
      ind_sup<-V(c_igraph)$weight==1
      c_igraph=delete_vertices(c_igraph,ind_sup)
      add_title_sup="found more than ones"
    }   
    
    
    if(top==TRUE ){
      c_igraph<-make_top_graph(c_igraph,wei,top_number)
      add_title_top=paste("top",top_number)
       
      
    }
    
    if(root_weight==TRUE){
      V(c_igraph)$size=sqrt(V(c_igraph)$size)  
    }
    main_title=paste("Graph of",add_title_domain,add_title_top,add_title_sup)
    
    temp=visIgraph(c_igraph, idToLabel = TRUE, layout = "layout_nicely",
                   physics = FALSE, smooth = FALSE, type = "square",
                   randomSeed = NULL)%>% 
      visInteraction(navigationButtons = TRUE) %>% visLegend(main = main_title,position = "left",zoom = FALSE) %>%
      visGroups(groupname = "node in top", color = "red") %>%
      visGroups(groupname = "node in direct relations", color = "grey")%>% visExport(type="pdf")
    
    
    graph_res=append(graph_res,list(temp))
    
    #plot(c_igraph, vertex.size=(V(c_igraph)$weight+1), vertex.label.cex=0.55,edge.arrow.size=0.5,edge.width=E(c_igraph)$weight,main=main_title)
  }
  
  return(graph_res)
  
}








make_wordcloud<-function(keywords,publication_date=0,interval_year=0,simple_word=FALSE,max_word_print=20,minfreq=2){
  set.seed(1234)
  # fonction principale permettant la r?alisation des graphique de type nuage de mots
  
  # input keywords : liste contenant les mot cl? par article (liste de liste) param?tre obligatoire 
  # input publication_date : liste contenant les date de publication des articles ayant fourni les mots cl?s
  # input max_word_print : entier permettant de savoir  quand arreter le top defaut 0(donc pas de top) defaut a 20
  # input interval_year : nombre d'ann?e prit en count pour chaque graphique (donc plusieur graphique effectuer en fct de la p?riode )
  # input simple word : ce content? des mot simple et pas des grtoupe de mot cl?fs constituer par les chercheurs(s?pare certaine expression clefs)  
  
  
  #output   les nuages de mots sont ploter 
  
  by_year=FALSE
  add_title=""
  year<-publication_date
  p_res=c()
  
  suppressWarnings(if(!is.na(as.numeric(interval_year))) interval_year<-as.numeric(interval_year))
  
  if(is.numeric(interval_year) & interval_year>0){
  
    diff<-max(year)-min(year)
    iter<-ceiling(diff/interval_year)
    for (i in 1:iter){
      if(i!=iter){# si on est pas sur la derni?re date
        index_year<-((year>=min(year)+(i-1)*interval_year) &(year<min(year)+i*interval_year))
        add_brack="["
      }else{
        index_year<-((year>=min(year)+(i-1)*interval_year) &(year<=min(year)+i*interval_year))
        add_brack="]"
      }
      #print(table(year[index_year]))
      key_c<-keywords[index_year]
      
      
      
      vrac<-unlist(key_c)
      if( length(which(vrac=="")!=0)){
        vrac<-vrac[-( which(vrac==""))]
      }
      
      
      
      if(simple_word==TRUE){
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
        b=min(year)+(i)*interval_year
        #print(head(sort(t_freq,decreasing=TRUE),10))
        if(b>max(year)) b<-max(year)
        
        p_res[i]=wordcloud(words = t_word, freq = t_freq, min.freq = minfreq,
                  max.words=max_word_print, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"),scale = c(1.5, 0.3))
        titre<-paste("top",max_word_print,"mots cl?s",add_title,"ann?e(s) [",min(year)+(i-1)*interval_year,":",b,add_brack,"( frequence de mot min:",minfreq,")")
        text(x=0.5, y=0.79, titre)
        
        #print(head(sort(t_freq,decreasing=TRUE),10))
      }
      
      
      
    }
  
  }else{
    vrac<-unlist(keywords)
    if( length(which(vrac=="")!=0)){
      vrac<-vrac[-( which(vrac==""))]
    }
    
    
    
    if(simple_word==TRUE){
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
      p_res[1]=wordcloud(words = t_word, freq = t_freq, min.freq = minfreq,
                max.words=max_word_print, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"),scale = c(1.5, 0.3))
      
      titre<-paste("top",max_word_print,"keywords",add_title,",in,whole dataset ( minimum frequency :",minfreq,")")
      text(x=0.5, y=0.79, titre)
    }
    
  }  
  
return(p_res)
}


pdf_extract_data<-function(path_folder){
  # fonction pertmettant d'importe les m?ta donn?e d'une liste de fichier pdf en se pasant sur un dossier, 
  #dans le dossier le programme ne verra que le fichier pdf 
  
  # input pdf_folder : lien du dossier dans lequel se trouve le fichier pdf 
  
  #output   une dataframe avec les m?tadonner qui nous interesse 
  
  #"C:/Users/moroguij/Documents/R_programs/data/data_raw_txt"
  #exemple de lien 
  file_vector <- list.files(path = path_folder)
  pdf_list <- file_vector[grepl(".pdf",file_vector)]# on ne s'occupe que des fichier pdf 
  #initialisation des variable resultat 
  title=c()
  key=c()
  auth=c()
  dom=c()
  date=list()
  
  #gestion d'erreur quelquonque 
  tryCatch({
    for(i in 1:length(pdf_list)){
      info <- pdf_info(paste0(path_folder,"/",pdf_list[i]))# on parcourt les info de chaque fichier
      title<-c(title,info$keys$Title)
      key<-c(key,info$keys$Keywords)
      auth<-c(auth,info$keys$Author)
      dom<-c(dom,info$keys$Subject)
      date=append(date,as.Date(as.POSIXct(info$created,origin="1970-01-01")))
      
      
      
      
    }
    res_pdf=as.data.frame(cbind(title,auth,key,dom,date),stringsAsFactors = FALSE)
    names(res_pdf)=c("titre","auteur","mot_clefs","domaines","date")
    res_pdf["date"]=date
    return(res_pdf)
},
  
  error=function(cond){
    return(-400)
  })

}

supress_unfit_entry<-function(title_vector,author_vector,sep="",max_aut=8){
  # fonction pertmettant de supprimer les entr? ind?cisrable (ou de les rendre compatible )
  # input pdf_folder : lien du dossier dans lequel se trouve le fichier pdf 
  
  #output   une dataframe avec les m?tadonner qui nous interesse 

  
  ind=which(title_vector=="")
  if(length(ind)>0){
    title_vector=title_vector[-ind]
    author_vector=author_vector[-ind]
    print("Publication without title in the select language can't be analysed")
  } 
  
  if(length(sep)==1) if(sep=="") sep=rep(",",length(author_vector)) 
  au_sep=sapply(1:length(author_vector),FUN=function(x) strsplit(author_vector[x],sep[x]))
  nb_author<-sapply(1:length(author_vector),FUN=function(x) length(au_sep[[x]]))
  
  
  ind=which(nb_author==0)
  if(length(ind)>0){
    title_vector=title_vector[-ind]
    author_vector=author_vector[-ind]
    print("Publication without author can't be analyse")
  } 
  
  ind1=which(nb_author>max_aut)
  
  
  if(length(ind1)>0){
    temp<-sapply(ind1,FUN=function(x){
      return(list((au_sep[[x]])[1:max_aut]))
    })
    au_sep[ind1]<-temp
    author_vector=sapply(1:length(au_sep),FUN=function(x) paste(unlist(au_sep[x]),collapse = ","))
    
    print(paste("the publication with", max_aut, "or more author will be restrected to",max_aut, "authors"))
  }
  
  return(list(title_vector,author_vector))
}







Unaccent <- function(text) {
  #remouve accent of text
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

duplicated2 <- function(x){ 
  #show suplicata 
  if (sum(dup <- duplicated(x))==0) 
    return(dup) 
  if (class(x) %in% c("data.frame","matrix")) 
    duplicated(rbind(x[dup,],x))[-(1:sum(dup))] 
  else duplicated(c(x[dup],x))[-(1:sum(dup))] 
}


compaire_title<-function(ti_trouver,ti_data){
  #Cette fonction compare les titre de ti trouver avec l'autre vecteur, renvoi le score maximal  , elle indique aussi l'indexe du premier titre qui a le score correspondant 
  ##ti_trouver vecteur comparer 
  #ti_data : vecteur comparant 
  res<-sapply(1:length(ti_trouver),FUN=function(x){
  
    courant=ti_trouver[x]
    print(x/length(ti_trouver)*100)
    max_apply<-sapply(1:length(ti_data),FUN=function(y){
      
      
      return(max(nchar(courant),max(nchar(ti_data[y]))))
      })
    tp<-max(1-(adist(Unaccent(ti_trouver[x]),Unaccent(ti_data),ignore.case = TRUE)/max_apply))
    tw<-which((1-(adist(Unaccent(ti_trouver[x]),Unaccent(ti_data),ignore.case = TRUE)/max_apply))==tp)[1]
    return(list(tp,tw))
  return(res)
  })
}



message_error=function(r){
  e=switch(as.character(r$status_code),
           '400' = 'Requ?te incorrecte', '401' = "Unauthorized",
           '403' = 'Forbidden', '404' = 'Not Found',
           '414'='URL too long',
           '500'='Internal Server falure check the query',
           '503'="Service unavailable retry later",
           '502'="bad getway retry",
           "504"="Gateway timeout",
           'Erreur inconnue')
  return(e)
}
#_______________________________________
querry_warning_message<-function(r){
  #prend le statue d'une querry et affiche le message correspondant 
    if (r$status != 200){
     
      warning(
        sprintf("Requ?te en ?chec : %s (%s) \n>>%s",
                message_error(r),
                r$status, r$request$url)
        
     )
      
      return(NULL)
    }
  
  
}






get_cit_or_ref<-function(resdt,type="cit"){# on r?cup?re les infodes citation uniquement 
  #permet de recup?r? les citation et les reference d'ads grace a une base de publie en entr?e, 
  #fonction interne 
  res_cit=c()
  if(type=="cit"){
    if(sum(is.na(resdt$citation)!=length(resdt$citation))) res_temp=resdt[!is.na(resdt$citation),]$citation else res_temp=-666 # si on demande des sitation et il y en a pas 
  } else {
    if(sum(is.na(resdt$reference)!=length(resdt$reference)))res_temp=resdt[!is.na(resdt$reference),]$reference else res_temp=-666
    
  }
  if(typeof(res_temp)=="list"){
    #initialisation
    res_temp=unique(unlist(res_temp))
    error_querry_cit=c()
    pas_cit=30
    count=ceiling(length(res_temp)[1]/pas_cit)
    size_res=c()
    test=c()
    start_RQ=0
    for(j in 1:count){# on parcour tout les ref/cit 
      first<-(j-1)*pas_cit+1
      last<-j*pas_cit
      
      
      if(last>length(res_temp)[1]) last<-length(res_temp)[1]
      citationbib=gsub("&",'%26',unlist(res_temp[first:last]))
      citbib_querry=paste0('%22',citationbib,collapse = 'OR','%22')
      adress=paste0('bibcode%3A%28',citbib_querry,'%29&fl=bibcode%20author%20title%20database%20pubdate%20keyword%20pub&rows=500&start=',start_RQ)
      # adress='author%3A%22Hilico%2C%20Adele%22&fl=citation,bibcode,title,author'
      
      (quest2<-paste0('https://api.adsabs.harvard.edu/v1/search/query?q=', adress))
      #(querry_exemple<-"https://api.adsabs.harvard.edu/v1/search/query?q=author%3A%22huchra%2C%20john%22")
      #year%3A%28',year_querry,'%29%20AND%20author%3A%28',au_querry,'%29AND%20
      #
      #r <- httr::GET(querry_exemple,httr::add_headers( Authorization = paste0('Bearer ', token)))
      
      
      
      r <- httr::GET(paste0("https://api.adsabs.harvard.edu/v1/search/query?q=", adress),
                     httr::add_headers( Authorization = paste0('Bearer ', token))
      )
      error=tryCatch({
        querry_warning_message(r)
        
      },
      
      warning=function(cond){# mise en place de la table erreur 
        titre_error=as.data.frame(ti_data[first:last])
        titre_error$status=r$status
        titre_error$message=message_error(r)
        titre_error$type_error=type
        titre_error$h=h
        return(titre_error)
      })
      
      if(length(error)>0){
        error_querry_cit<-rbind(error_querry_cit,error)
        error=c()
        
      }else {
        result <-jsonlite::fromJSON(txt = httr::content(r, 'text',encoding = "UTF-8"), simplifyDataFrame = TRUE)
        res_header=result$responseHeader
        if(result$response$numFound>0){# si resultat on r?cup?re se qui nous interesse 
          last_good_result=r
          size_res=c(size_res,result$response$numFound)
          bibcode=result$response$docs$bibcode
          aut=result$response$docs$author
          titre=result$response$docs$title
          # doi=result$response$docs$doi# a fair  recr?e ou trouvr identifiant ads
          subj=result$response$docs$database
          keyword=result$response$docs$keyword
          pubdate=result$response$docs$pubdate
          pub=result$response$docs$pub
          #on fait exixter les valeur manquante pour permetre lajoue 
          keyword[which(is.null(keyword))]<-NA
          aut[which(is.null(aut))]<-NA
          titre[which(is.null(titre))]<-NA
          pubdate[which(is.null(pubdate))]<-NA
          subj[which(is.null(subj))]<-NA
          
          res_cit<-rbind(res_cit,cbind(j,bibcode,aut,titre,subj,pubdate,keyword,pub))
        }
        
      }
      
      if(j==count) {#mise en forme finale 
        resdt_cit=as.data.frame(res_cit)
        names(resdt_cit)[2:dim(resdt_cit)[2]]<-paste(names(resdt_cit)[2:dim(resdt_cit)[2]],"citant")
        if(type=="cit"){
          ind_bib<-sapply(resdt_cit$`bibcode citant`,function(x){
            return(grep(x,(resdt$citation)))
          })
        } else{
          ind_bib<-sapply(resdt_cit$`bibcode citant`,function(x){
            return(grep(x,(resdt$reference))) # on fait correspondre les titre de d?part avec leur citation  
          })
        }
        
        total_res=c()
        for(j in 1:length(ind_bib)){
          
          total_res=rbind(total_res,(cbind(resdt$bibcode[(ind_bib[[j]])],resdt$titre[ind_bib[[j]]],  resdt$aut[(ind_bib[[j]])],resdt$pubdate[(ind_bib[[j]])],resdt$subj[ind_bib[[j]]],resdt$pub[ind_bib[[j]]],
                                           resdt_cit$`titre citant`[j],resdt_cit$`aut citant`[j],resdt_cit$`subj citant`[j],resdt_cit$`bibcode citant`[j],
                                           resdt_cit$`pubdate citant`[j],resdt_cit$`pub citant`[j],resdt$check_title_pct[(ind_bib[[j]])],resdt$check_title_ind[(ind_bib[[j]])])))
          
        }
        dim(total_res)
        total_res=as.data.frame(total_res)
        if(type=="cit"){
          names(total_res)<-c("cited identifier","cited title","cited auth","cited date", "cited subject","cited journal", 
                              "citing title","citing auth","citing subject","citing identifier","citing date", "citing journal","check","check ind" )
        }else{
          names(total_res)<-c("refering identifier","refering title","refering auth","refering date", "refering subject","refering journal", 
                              "refered title","refered auth","refered subject","refered identifier","refered date","refered journal","check","check ind" )  
        } 
        result=list(res_cit_ref=total_res,error_table=error_querry_cit)
      }
      if(length(dim(error_querry_cit)) &(j==count)) print("il y a des erreurs lors de l'agr?gation des info des citations") 
    }
    
  } else result=NULL
  
  return(result)
  
}


#________________________________________________________________S











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
      
      res_wos_all=arxiv_get_publi(data_wos[au_name][[1]],data_wos[ti_name][[1]],data_wos$position_name,pas,value_same_min_ask,value_same_min_accept,sep)
      res_wos=res_wos_all$res
      err1=res_wos_all$error
      reject1=res_wos_all$reject
      
    }
    data_rest=data_pub[data_pub[source_name]!="WOS",]
    if(dim(data_rest)[1]!=0) {
      res_rest_all=arxiv_get_publi(data_rest[au_name][[1]],data_rest[ti_name][[1]],data_rest$position_name,pas,value_same_min_ask,value_same_min_accept,sep)
      res_rest=res_rest_all$res
      err2=res_rest_all[2,]$error
      reject2=res_rest_all$reject
      
    }
    
    res=rbind(res_rest,res_wos)
    error_querry=rbind(err1,err2)
    reject=rbind(reject1,reject2)
  }else {
    resdt_all=arxiv_get_publi(au_data,ti_data,data_pub$position_name,pas,value_same_min_ask,value_same_min_accept,sep)
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

arxiv_get_publi<-function(au_data,ti_data,position_name,pas,value_same_min_ask,value_same_min_accept,sep){
  #fonction interne qui permet de r?cup?r? les publication de l'api d'arxiv avecc un vecteur de titre et d'auteur, on pr?sice aussi le pourcentage de cimilitude a avoir pour le concerver 
  
  
  
  
  res_temp=supress_unfit_entry(ti_data,au_data,sep,max_aut = 1)
  
  ti_data=res_temp[[1]]
  au_data=res_temp[[2]]
  
  
  position_togo=rep(2,length(au_data))
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



get_cit_or_ref_arxiv<-function(resdt,type){ 
  #Permet de r?cu^?r? les citation et les reference de arxiv en se basant sur une df d'entr?e '
  error_querry_cit=c()
  if(type=="cit"){  
    link_list<-unlist(resdt$citation_link)
  }else{
    link_list<-unlist(resdt$reference_link)
  }
  
  link_abs<-unlist(resdt$abs_link)
  res_cit<-c()
  for(ar in(1:length(link_list))){
    #cited<-abs_link[[ar]]#lien vers l'article trouver 
    
    page_resp <- GET(link_list[[ar]])# code souce de la page inspire
    error=tryCatch({
      querry_warning_message(page_resp)
      
    },
    warning=function(cond){#verification d'erreur 
      titre_error=as.data.frame(ti_data[first:last])
      names(titre_error)=c("Publication title")
      titre_error["Status error"]=r$status
      titre_error$Message=message_error(r)
      titre_error["Data impact"]=type
      titre_error$h=ar
    })
    
    if(length(error)>0){
      error_querry_cit<-rbind(error_querry_cit,error)
      error=c()
      
    }else {
      Sys.sleep(3)
      page_content <- httr::content(page_resp, as = "text")
      
      # fouille de text pour trouver les info ___________________________________________
      text_base <- strsplit(page_content, "\n")[[1]]
      
      #read_html(cite_link[[ar]])%>%html_nodes("body")%>%html_text()
      deb<-grep("<dd>",text_base)
      fin<-grep("</dd>",text_base)
      
      
      print("voici deb ")
      print(deb)
      
      
      if(length(deb)>0){# si il ya as des citation  
        # cited_title=gsub("\\s+"," ",tps[[ar]])
        # #list permet d'?vitez le changement de nom et la duplication de ligne 
        # cited_aut=list(add[[ar]])
        # cited_sub=list(cat[[ar]])
        # cited_journal=list(journal[[ar]])
        # cited_date_pub=list(date_pub[[ar]])
        # 
        for(i in 1:length(deb)){
          #print(deb[i])
          if(is.na(fin[i])) fin[i]=length(text_base) 
          pice_int<-text_base[deb[i]:fin[i]]
          #r?cup?r? titre de l'article qui cite/est ref 
          titre<-pice_int[grep("Title",pice_int)]
          if(length(titre)>0){
            
            supfin<-max(`attributes<-`(gregexpr("/span>",titre)[[1]],NULL))
            titre<-substr(titre,supfin+1,nchar(titre))
            titre<-gsub("span>","",gsub("</div>","",titre))
          }else {titre<-NA}
          
          #recupp?ration des auteurs 
          aut<-pice_int[grep("<a",pice_int)]
          if(length(aut)>0){
            aut<-sapply(1:length(aut),FUN=function(x){
              (ind1<-min(`attributes<-`(gregexpr(">",aut[x])[[1]],NULL)))
              (ind2<-max(`attributes<-`(gregexpr("<",aut[x])[[1]],NULL)))
              res<-substr(aut[x],ind1+1,ind2-1)
              res<-gsub("/a>","",res)
              res<-gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",res)
              return(res)
              
            })
            aut=list(aut)
            
            ind_comment<-grep("Comments:</span>",aut) #detection de commentaire 
            if(length(ind_comment)>0) aut<-aut[-ind_comment] #supression des commentaires 
            
            
          }else{aut<-NA}# en cas de valeur null 
          
          #traitement du subject 
          suj<-pice_int[grep("Subject",pice_int)]
          if(length(suj)!=0){
            ind1<-max(`attributes<-`(gregexpr("\">",suj)[[1]],NULL))
            suj<-substr(suj,ind1+2,nchar(suj))
            suj<-gsub("[#$%*,.:;<=>@^_`{|}~.]","",gsub("/span","",suj))
          } else{suj<-NA}
          
          journ<-pice_int[grep("Journal-ref",pice_int)]
          if(length(journ)!=0){
            
            (supfin<-max(`attributes<-`(gregexpr(">",journ)[[1]],NULL)))
            (journ<-substr(journ,supfin+2,nchar(journ)))
          }else {journ<-NA}
          
          
          
          res_cit<-rbind(res_cit,cbind(ar,link_abs[[ar]],titre,aut,suj,journ))
          
        }
      }
    }
  }
  
  
  
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
    }else{
      names(res_link_final)<-c("h","refering identifier","refering date","refering title","refering auth", "refering subject","refering journal","check","check ind", "refered title","refered auth","refered subject","refered journal" )  
      res_link_final<-res_link_final[order(unlist(res_link_final$`refering identifier`)),]
    }
  }else res_link_final=NULL
  if(length(dim(error_querry_cit))>0) print("il y a des erreurs dans les citations")
  
  
  
  if(length(dim(res_link_final)[1])>0) row.names(res_link_final)<-1:dim(res_link_final)[1]
  
  return(list(resdt_final=res_link_final,error_querry=error_querry_cit))
}




fit_name_position<-function(au_data,position_base,position_togo,sep="",initial=FALSE){
  if(length(sep)==1) if(sep=="") sep=rep(",",length(au_data)) 
  au_sep=sapply(1:length(au_data),FUN=function(x) strsplit(au_data[x],sep[x]))
  
  
  #togo==2
  test_full=sapply(1:length(au_data),FUN=function(x,position_b=position_base,position_tg=position_togo){
    if(position_b[x]!=position_tg[x]){
      # si le type est ?gale a un, cela signifie pr?nom nom, type deux nom, pr?nom m
      nom=unlist(lapply(strsplit(unlist(au_sep[x]), " "), function(x) {
        
        return(x[length(x)]) }))
      prenom=unlist(lapply(strsplit(unlist(au_sep[x]), " "), function(x) {
        return(paste(x[1:length(x)-1],collapse = " ")) }))
      
      test=sapply(1:length(nom), FUN=function(x) {
        return(paste0(nom[x],",",prenom[x],collapse = " ")) 
        
        return(test)
      })
    }else return(au_data[x])
    
  }) 
  
  return(test_full)
  
}

extraction_data_api_nasa_ads<-function(data_pub,ti_name,au_name,token,pas=8,value_same_min_accept=0, value_same_min_ask=1,type="all",source_name="",sep_vector_in_data="",position_vector_in_data=""){
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
  
  
  
  au_data<-data_pub[au_name][[1]]#auteur nom colonne 
  ti_data<-data_pub[ti_name][[1]]#titre 
  res_rest=c() 
  res_wos=c() 
  err1=c()
  err2=c() 
  reject1=reject2=c()
  if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,length(au_data)) else position_name=data_pub[position_vector_in_data][[1]]
  if(source_name!="" && (type=="ref" || type=="all")){
    data_wos=data_pub[data_pub[source_name]=="WOS",]
    
    
    if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_wos[sep_vector_in_data][[1]]
    if(dim(data_wos)[1]!=0) {
      res_wos_all=ads_get_publi(data_wos[au_name][[1]],data_wos[ti_name][[1]],position_name[data_pub[source_name]=="WOS"],pas,value_same_min_ask,value_same_min_accept,token,sep)
      res_wos=res_wos_all$res
      err1=res_wos_all$error
      reject1=res_wos_all$reject
      lastresult=res_wos_all$lastresult
    }
    data_rest=data_pub[data_pub[source_name]!="WOS",]
    
    if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_rest[sep_vector_in_data][[1]]
    if(dim(data_rest)[1]!=0) {
      res_rest_all=ads_get_publi(data_rest[au_name][[1]],data_rest[ti_name][[1]],position_name[data_pub[source_name]!="WOS"],pas,value_same_min_ask,value_same_min_accept,token)
      res_rest=res_rest_all$res
      err2=res_rest_all[2,]$error
      reject2=res_rest_all$reject
      lastresult=res_rest_all$lastresult
    }
    
    res=rbind(res_rest,res_wos)
    error_querry=rbind(err1,err2)
    reject=rbind(reject1,reject2)
  }else {
    if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_pub[sep_vector_in_data][[1]]
    resdt_all=ads_get_publi(au_data,ti_data,position_name,pas,value_same_min_ask,value_same_min_accept,token,sep = sep)
    error_querry=resdt_all$error
    res=resdt_all$res
    reject=resdt_all$reject
    lastresult=resdt_all$lastresult
  }
  
  
  resdt<-as.data.frame(res,stringsAsFactors = FALSE)
  
  
  # source("C:/Users/moroguij/Documents/R_programs/functions_analyses.R")#source nneded function 
  #initialisation des variable __________________________________________  
  
  # Pr?non nom--> nom,pr?non  
  
  
  #ins?r? fct 
  if(type=="all"|| type=="cit"){
    
    
    res_cite=get_cit_or_ref(resdt,type="cit")# citation 
    total_res=res_cite$res_cit_ref[res_cite$res_cit_ref$check>=value_same_min_accept,]
    total_res_ask=res_cite$res_cit_ref[(res_cite$res_cit_ref$check>=value_same_min_ask) & (res_cite$res_cit_ref$check<value_same_min_accept),]
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
    res_ref=get_cit_or_ref(res_rest,type="ref")  #ref?rence
    total_res_ref=res_ref$res_cit_ref[res_ref$res_cit_ref$check>=value_same_min_accept,]
    total_res_ref_ask=res_ref$res_cit_ref[(res_ref$res_cit_ref$check>=value_same_min_ask) & (res_ref$res_cit_ref$check<value_same_min_accept),]
    if(!is.null(total_res_ref)[1]){
      if(dim(total_res_ref)[1]>0) total_res_ref<-total_res_ref[order(unlist(total_res_ref$`refering identifier`)),]
      if(dim(total_res_ref)[1]>0) row.names(total_res_ref)<-1:dim(total_res_ref)[1]
      if(dim(total_res_ref_ask)[1]>0)total_res_ref_ask<-total_res_ref_ask[order(unlist(total_res_ref_ask$`refering identifier`)),]
      if(dim(total_res_ref_ask)[1]>0) row.names(total_res_ref_ask)<-1:dim(total_res_ref_ask)[1]
      
      
      error_querry=rbind(error_querry,res_cite$error_table)
      
      
      
      if(type=="ref"){
        total_res=NULL
        total_res_ask=NULL
        error_querry_cit=NULL
      }
    }
  }
  return(list(dataframe_citation_accept=total_res,error_querry_publi=error_querry,title_vector=ti_data,author_vector=au_data,dataframe_citation_ask=total_res_ask,
              reject_analyse=reject, last_result=lastresult,dataframe_publi_found=resdt,dataframe_ref_accept=total_res_ref,dataframe_ref_ask=total_res_ref_ask))
}





ads_get_publi<-function(au_data,ti_data,position_name,pas,value_same_min_ask,value_same_min_accept,token,sep){
  #m?me chose que les fonction pr?c?dente du m?me nom mais avec ads, cette fonction permet de recup?r? les m?ta donner de publi sur l'api d'ads en se servant d'un vecteur titre/ auteur 
  
  
  res=c()#variable resultat 
  
  
  
  error_querry=c()# table d'erreur  
  
  
  
  res_temp=supress_unfit_entry(ti_data,au_data,sep)#permet d'aclimater les do,n?es a la bd (possiblement sortable de la fonction )
  
  
  
  
  
  
  # Pr?non nom--> nom,pr?non  
  ti_data=res_temp[[1]]
  au_data=res_temp[[2]]
  
  
  
  position_togo=rep(2,length(au_data))
  
  
  
  
  
  
  au_data=fit_name_position(au_data,position_name,position_togo =position_togo )
  au_data=sapply(1:length(au_data),FUN = function(x) paste(au_data[[x]],collapse = ";"))
  
  
  inter=ceiling(length(au_data)[1]/pas)# nombre d'iteration
  
  for(h in 1:inter){
    first<-(h-1)*pas+1
    last<-h*pas
    if(last>length(au_data)) last<-length(au_data)
    #ti_test="M31 Globular Clusters: Colors and Metallicities"
    #au_test="Huchra,John"
    #adaptation des caract?re sp?ciaux au requette pour les titre et les auteur
    (au_querry=paste0("%22",gsub("&","%26",gsub("[(]","",gsub("[)]","",gsub(";",'%22AND%22',gsub("[?]","",gsub(",","%2C",gsub("\\", "", gsub(":","%3A",gsub("/","%2F",
                                                                                                                                                            gsub(" ","%20",Unaccent((au_data[first:last])))))
                                                                                                                              , fixed =TRUE))))))), collapse = 'OR','%22'))
    
    
    
    (ti_querry=paste0("%22",gsub("[]]","%5D",gsub("`",'%60',gsub("[[]","%5B",gsub("[(]","%28",gsub("[)]","%29",gsub("<","%3C",gsub(">","%3E",gsub("=","%3D",gsub('[}{$]',"",gsub("&","%26",gsub('"','%22',gsub("\\", "",gsub(":","%3A",gsub("/","%2F",gsub("'","%27",gsub(" ","%20",
                                                                                                                                                                                                                                                                          gsub(",","%2c",gsub("e?","e",gsub("?","",gsub("%","%25",(tolower(Unaccent(ti_data[first:last])))),fixed=TRUE),fixed = TRUE)))))), 
                                                                                                                                                                                                               fixed=TRUE)))))))))))), collapse = 'OR','%22'))
    
    
    adress=paste0('author%3A%28',au_querry,'%29AND%20title%3A%28',ti_querry,'%29&fl=reference%20citation%20author%20title%20database%20pubdate%20bibcode%20keyword%20pub%20&sort=date%20desc&rows=500&start=',0)
    (quest2<-paste0('https://api.adsabs.harvard.edu/v1/search/query?q=', adress))
    
    nchar(quest2)
    r <- httr::GET(paste0("https://api.adsabs.harvard.edu/v1/search/query?q=", adress),
                   httr::add_headers( Authorization = paste0('Bearer ', token))
    )
    error=tryCatch({#rep?rage des erreur 
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
      
    }else {#si il n'y a pas d'erreur 
      result <-jsonlite::fromJSON(txt = httr::content(r, 'text',encoding = "UTF-8"), simplifyDataFrame = TRUE)
      if(result$response$numFound>0){
        last_good_result=r
        res_header=result$responseHeader
        aut=result$response$docs$author
        titre=result$response$docs$title
        subj=result$response$docs$database
        pubdate=result$response$docs$pubdate
        keyword=result$response$docs$keyword
        pub=result$response$docs$pub
        
        citation=result$response$docs$citation
        reference=result$response$docs$reference
        
        bibcode=result$response$docs$bibcode
        
        if(is.null(citation)) citation<-NA
        if(is.null(keyword)) keyword<-NA
        if(is.null(reference)) reference<-NA
        
        if(length(unlist(titre))>1){
          titre2<-sapply(1:length(titre),FUN=function(x) return(list(unlist(titre[x])[1])))
          titre=titre2
        }
        res<-rbind(res,cbind(h,bibcode,aut,titre,subj, citation,pubdate,keyword,pub,reference))
      }
      
    }
    
    
    
    if(result$response$numFound>0) print(h) #aide de d?vlopement 
    if(h==inter) {# fin de loop mise en place des data frame 
      resdt=as.data.frame(res)
      if(dim(resdt)[1]>0){
        resdt$aut=sapply(1:dim(resdt)[1], FUN=function(x){paste(unlist(resdt$aut[x]),collapse = ' ; ')})  
        trouver<-unlist(resdt$titre)
        indic_compaire_title<-compaire_title(trouver,ti_data)
        
        resdt["check_title_pct"]<-unlist(indic_compaire_title[1,])
        resdt["check_title_ind"]<-unlist(indic_compaire_title[2,])
        ind_null<-sapply(1:length(resdt$citation), FUN = function(x) is.null(resdt$citation[[x]]))
        resdt$citation[which(ind_null)]<-NA
      }
    }
    if(length(dim(error_querry)) &(h==inter)>0) print("il y a des erreurs")
  }
  
  
  
  
  
  if(value_same_min_ask<1) reject=resdt[resdt$check_title_pct<value_same_min_ask,]# les rejet? sont ceux qui non pas assez de similitudfe pour aire dans les demande 
  resdt=resdt[resdt$check_title_pct>value_same_min_ask,]
  
  return(list(res=resdt,error=error_querry,reject=reject,lastresult=last_good_result))
}


#_________________________







pumed_get_element_id<-function(id_list,type){
  #fonction interne a l'application de requettage pumed permettant d?vit? la r?p?tition de code quand on interoge les identifiant pour les citation et les refrence 
  # input : id_list : list d'identifiant 
  # output: liste de 2 
  # error : erreur treouver 
  # temps : donn?es trouver  
  id_list_string=paste0(id_list,collapse = ",")
  
  querry<-paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=',id_list_string,'&retmode=json')
  
  r <- GET(querry)
  
  
  
  error=tryCatch({#rep?rage des erreur 
    querry_warning_message(r)
    
  },
  
  warning=function(cond){
    titre_error=as.data.frame(ti_data[first:last])
    names(titre_error)=c("Publication title")
    titre_error["Status error"]=r$status
    titre_error$Message=message_error(r)
    titre_error["Data impact"]=type
    titre_error$h=h
    return(titre_error)
  })
  
  if(length(error)==0) {#si il n'y a pas d'erreur  
    Sys.sleep(0.34)
    result <- jsonlite::fromJSON(txt = httr::content(r, 'text'), simplifyDataFrame = TRUE)
    
    
    temp<-sapply(1:length(id_list),FUN=function(x){
      id=result$result[[id_list[x]]]$uid
      au=list(result$result[[id_list[x]]]$authors$name)
      ti=result$result[[id_list[x]]]$title
      da=result$result[[id_list[x]]]$pubdate
      essn=result$result[[id_list[x]]]$essn
      if(is.null(essn)) essn<-NA
      issn=result$result[[id_list[x]]]$issn
      if(is.null(issn)) issn<-NA
      jou=result$result[[id_list[x]]]$fulljournalname
      if(is.null(jou)) jou<-NA
      return(c(id,au,ti,da,essn,issn,jou))
    })
  }
  return(list(error=error,temp=temp))
}



#---------------------------------------------

df_flatten<-function(res){
  
  res_f=res
  #date_name="date"
  
  for(i in names(res_f)){
    ind=which(is.null(res_f[,i]))
    res_f[ind,i]=NA
    if(length(unlist(res_f[,i]))>length(res_f[,i])){
      res_f[,i]=sapply(1:length(res_f[,i]),FUN = function(x) paste(res_f[,i][[x]],collapse = ";"))
    }
    ind=which(is.na(res_f[,i]))
    res_f[ind,i]="NULL"
    if(typeof(i)=="list") res_f[,x]<-unlist(res_f[,x])
    #if(date_name!="") res_f[date_name]=as.character(res_f[date_name])
  }
  
  return(as.data.frame(res_f,stringsAsFactors = FALSE))
}

find_journal_domaine<-function(journal_data,issn,essn,journal_table_ref){
  
  
  
  
  dom=sapply(1:length(journal_data),FUN=function(x){
    if(!is.na(journal_data[x]) && !is.na(issn[x]) && issn[x]!=""){
      ind=which(issn[x]==journal_table_ref$issn) 
      return((journal_table_ref$SubField_English[ind]))
    }
  })
  
  dom2=sapply(1:length(journal_data),FUN=function(x){
    if(!is.na(journal_data[x]) && !is.na(essn[x]) && essn[x]!=""  && is.null(unlist(dom[x]))){
      ind=which(essn[x]==journal_table_ref$essn) 
      return((journal_table_ref$SubField_English[ind]))
    }
  })
  
  dom3=sapply(1:length(journal_data),FUN=function(x){
    #print("first")
    if(!is.na(journal_data[x]) && is.null(unlist(dom2[x]))){ 
      tp=grep(paste0("^",tolower(trimws(gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",gsub("\\s*\\([^\\)]+\\)","",journal_data[x])))),"$"),tolower(trimws(journal_table_ref$Source_title)),perl = TRUE)
      
      if(length(tp)>0){
        return((journal_table_ref$SubField_English[tp]))
      }
    }
    
    
  })
  
  
  
  
  dom4=sapply(1:length(journal_data),FUN=function(x){
    #print("2")
    if(!is.na(journal_data[x]) && is.null(unlist(dom[x]))){
      tp=grep(paste0("^",tolower(trimws(gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",gsub("\\s*\\([^\\)]+\\)","",journal_data[x]))))),tolower(trimws(journal_table_ref$Source_title)),perl = TRUE)
      if(length(tp)>0){
        return((journal_table_ref$SubField_English[tp]))
      }
    }else {
      return(unlist(dom3[x]))
    }
    
    
  })
  
  dom5=sapply(1:length(journal_data),FUN=function(x){
    #print("2")
    if(!is.na(journal_data[x]) && is.null(unlist(dom2[x]))){
      tp=grep(paste0(tolower(trimws(gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",gsub("\\s*\\([^\\)]+\\)","",journal_data[x]))))),tolower(trimws(journal_table_ref$Source_title)),perl = TRUE)
      if(length(tp)>0){
        return((journal_table_ref$SubField_English[tp]))
      }
    }else {
      return(unlist(dom4[x]))
    }
    
    
    
  })
  
  
  #traiiter le pluri danns le if et le esle !!!
  
  
  dom_length<-sapply(1:length(dom3), FUN=function(x) length(unlist(dom3[x])))
  ind<-which(dom_length>1)
  
  length(dom_length)
  table(dom_length)
  
  temp<-sapply(ind, function(x){
    cat<-names(table(dom3[x]))
    if(length(cat)==1) return(cat) else return(NA)
  })
  
  dom3[ind]<-temp
  return(dom3)
  
}


shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
extract_data_api_pumed<-function(data_pub,ti_name,au_name,pas=8,value_same_min_accept=0.85, value_same_min_ask=0.95,type="cit",source_name=""){
  
  # fonction permetant d'interroger pumeed sur les reference et les citation d'un corpus de publication placer en entr?e 
  #intput   
  #-data_pub: corpus de publication doit au moin contenir les titre et les auteur en anglais(dans un premier temps )
  # ti_name non de la colonne titre 
  # au name : nom de la colonne auteur
  #token : string, token d'identification 
  # pas: nombre de publication par requete (joue sur l'impact de l'erreur )
  #value_same_min_accept= valeur entre ? et 1 repr?sente le taux minimum de cimilitude a avoir pour accespter la r?ponse de la bd dans l'?tude 
  #value_same_min_ask=valeur minimal pour demander a l'utilisteur si il veul la publication dans l'?tude 
  # type: typ? danalise cit =citation , ref=reference, all=les deux 
  
  #outpout res : liste de 11 ?l?m?ents
  #author_vector:vecteur d'auteur retenu pour les requ?tes
  #dataframe_citation :dataframe des resultat(citation)
  #dataframe_citation_ask :dataframe des resultat demander (citation)
  #dataframe_publi_found :dataframe of all the publication found
  #dataframe_ref:dataframe des resultat(reference)
  #dataframe_ref:dataframe des resultat demand?e (reference)
  #error_querry_publi: matrice de rapport d'erreur pour la partie recherche de publication  
  #error_querry_citation matrice d'erreur concertnant la partie citation
  #error_querry_ref matrice d'erreur concertnant la partie reference
  #res_accept: dataframe des resultat accespt? (citation ou reference )
  #res_ask: dataframe des resultat demander (citation ou reference )
  #reject_analyse : dataframe(publication rejet? )
  #title_vector : vecteur titre  
  
  
  
  #_____________________________Caract?risqtique a respecter et infos utile ___________________________________
  
  #3request/s
  # support mail (vog.hin.mln.ibcn@seitilitue
  #+ instead of space 
  # retamx max to donlowd 1000but stop to 500 for us 
  #??? fetcj peu ?tre utile a utilis? 
  #pumed=abstract of article 
  #pmc (pumed central =full text of article)
  #???https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=21876726&id=21876761
  #pour les citation(au dessus)
  
  #RECUP2RAtion des noms de colonne 
  au_data<-data_pub[au_name][[1]]
  ti_data<-data_pub[ti_name][[1]]
  res_temp=supress_unfit_entry(ti_data,au_data)#permet d'aclimater les donn?es a la bd (possiblement sortable de la fonction )
  ti_data=res_temp[[1]]
  au_data=res_temp[[2]]
  
  
  
  
  res_rest=c() 
  res_wos=c() 
  err1=c()
  err2=c() 
  reject1=reject2=c()
  if(source_name!="" && (type=="ref" || type=="all")){
    data_wos=data_pub[data_pub[source_name]=="WOS",]
    if(dim(data_wos)[1]!=0) {
      
      res_wos_all=pumed_get_publi(data_wos[au_name][[1]],data_wos[ti_name][[1]],data_wos$position_name,pas,value_same_min_ask,value_same_min_accept)
      res_wos=res_wos_all$res
      err1=res_wos_all$error
      reject1=res_wos_all$reject
      
    }
    data_rest=data_pub[data_pub[source_name]!="WOS",]
    if(dim(data_rest)[1]!=0) {
      res_rest_all=pumed_get_publi(data_rest[au_name][[1]],data_rest[ti_name][[1]],data_rest$position_name,pas,value_same_min_ask,value_same_min_accept)
      res_rest=res_rest_all$res
      err2=res_rest_all[2,]$error
      reject2=res_rest_all$reject
      
    }
    
    res_new=rbind(res_rest,res_wos)
    error_querry=rbind(err1,err2)
    reject=rbind(reject1,reject2)
  }else {
    resdt_all=pumed_get_publi(au_data,ti_data,data_pub$position_name,pas,value_same_min_ask,value_same_min_accept)
    error_querry=resdt_all$error
    res_new=resdt_all$res
    reject=resdt_all$reject
    res_rest=resdt_all$res
    
  }
  
  
  
  
  
  # Pr?non nom--> nom,pr?non 
  
  # interogation par titre et auteur pour retrouver les identifiant_______________________________________________________
  
  
  
  
  
  if(type!="cit"){# on prend soit les citation soit "all soit uniquement les reference 
    
    if(source_name!="") res_new_s=res_rest else  res_new_s=res_new
    
    id_list_ref=as.character(unlist(res_new_s$ref_pmid))# on r?cup?re les ref?rences de notre resultat pr?c?dent 
    if(length(id_list_ref)>0){
      pas=20# le pas peut ?tre plus grand car les identifiant sont petit 
      inter=ceiling(length(id_list_ref)[1]/pas)
      res_ref=c() 
      error_querry_ref=c()#matrice erreur citation 
      
      
      for(h in(1:inter)){
        first<-(h-1)*pas+1
        last<-h*pas
        if(last>length(id_list_ref)) last<-length(id_list_ref)
        
        id_element=pumed_get_element_id(id_list_ref[first:last],type)
        res_ref<-rbind(res_ref,t(id_element$temp))
        
        error_querry_ref<-rbind(error_querry_ref,id_element$error)
        error_querry=c(error_querry,id_element$error)
      }
      res_ref=as.data.frame(res_ref,stringsAsFactors = FALSE)
      
      
      names(res_ref)<-c("id_ref","auteur_ref","titre_ref","date_ref","essn_ref","issn_ref", "journal_ref")
      
      
      
      
      ind_id<-sapply(res_ref$id_ref,function(x){
        return(grep(x,(res_new$ref_pmid)))
      })
      
      
      
      res_ref_final=cbind(res_new[ind_id,],res_ref)
      res_ref_final<-res_ref_final[,-which(names(res_ref_final)=="ref_pmid")]
      names(res_ref_final)<- c("refering identifier","refering auth","refering title","refering date","refering journal","h","refering issn","refering essn","check","check ind","refered identifier", "refered auth","refered title","refered date","refered essn","refered issn","refered journal" )
      #View(res_ref_final)
      if(dim(res_ref)[1]>0) res_ref_final<-res_ref_final[order(unlist(res_ref_final$`refering identifier`)),]
      res_ref_accept=res_ref_final[res_ref_final$check>=value_same_min_accept,]
      print(dim(res_ref_accept))
      if(dim(res_ref_accept)[1]>0) row.names(res_ref_accept)<-1:dim(res_ref_accept)[1]
      
      res_ref_ask=res_ref_final[(res_ref_final$check>=value_same_min_ask) & (res_ref_final$check<value_same_min_accept),]
      print(dim(res_ref_ask))
      if(dim(res_ref_ask)[1]>0) row.names(res_ref_ask)<-1:dim(res_ref_ask)[1]
      #dim(res_ref_accept)
    }else {# si pas de resultat 
      res_ref_accept=res_ref_ask=NULL
      error_querry_ref<-NULL
    }
  } else {#si ref pas demander
    res_ref_accept=res_ref_ask=NULL
    error_querry_ref<-NULL
  }
  
  
  #travaile sur les citation_______________________________________________
  if(type!="ref"){
    #recup?ration des identifiaant de citation ________________________________
    id_list=(unlist(res_new$id))
    
    pas=20
    res_cit=c()
    id_list_citation_final=c()
    id_raw=c()
    inter=ceiling(length(id_list)/pas)
    error_querry_cit=c()
    for(h in(1:inter)){
      first<-(h-1)*pas+1
      last<-h*pas
      if(last>length(id_list)) last<-length(id_list)
      id_querry=paste0(id_list[first:last],collapse="&id=")
      
      querry=paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&id=",id_querry,"&sort=pub+date&retmode=json")
      nchar(querry)
      Sys.sleep(0.34)#obligatoire 
      r <- GET(querry)
      error=tryCatch({#rep?rage des erreur 
        querry_warning_message(r)
        
      },
      
      warning=function(cond){
        titre_error=as.data.frame(ti_data[first:last])
        names(titre_error)=c("Publication title")
        titre_error["Status error"]=r$status
        titre_error$Message=message_error(r)
        titre_error["Data impact"]="cit"
        titre_error$h=h
        return(titre_error)
      })
      
      if(length(error)>0){
        error_querry_cit<-rbind(error_querry_cit,error)
        error_querry<-rbind(error_querry,error)
        error=c()
        
      }else {#si il n'y a pas d'erreur  
        result<- jsonlite::fromJSON(txt = httr::content(r, 'text'), simplifyDataFrame = TRUE)
        list_citation_globale=result$linksets$linksetdbs
        
        #r?cu^?ration des donn?es de cititation globale  
        if(length(list_citation_globale)>0){
          id_list_citation=sapply(1:length(list_citation_globale),FUN=function(x){
            
            if(!is.null(list_citation_globale[[x]])){
              id=unlist(list_citation_globale[[x]]$links)
              raw=list_citation_globale[[x]]$links
            } else {
              id=NULL
              raw=NA
            }
            return(list(id_f=id,raw_f=raw))
          })
          id_list_citation_final<-c(id_list_citation_final,unlist(id_list_citation[1,]))# d'un cot? on ne r?cup?re que les identifiant valide pour les requette 
          id_raw=c(id_raw,(id_list_citation[2,]))}# de l'autre on r?cup?re tout poiur pouvoir faire une seul et meeme table 
        
      }# ?tant donn?e que  nous pouvons avoir plusieur citation pour un seul article nous somme oblig? de faire se finir la boucle ii pour en commencer une autre 
      
    }
    if(length(id_raw)!=dim(res_new)[1] && !is.null(res_new) ) id_raw<-c(id_raw,rep(NA,dim(res_new)[1]-length(id_raw)))
    res_new$citation<-id_raw
    
    #on parcourt les diff?rente sitation (identifiant) pour extraire les info 
    if(length(id_list_citation_final)>0){
      inter=ceiling(length(id_list_citation_final)/pas)
      res_cit=c()
      for(h in(1:inter)){
        first<-(h-1)*pas+1
        last<-h*pas
        if(last>length(id_list_citation_final)) last<-length(id_list_citation_final)
        
        
        
        id_list_citation=id_list_citation_final[first:last]
        id_element=pumed_get_element_id(id_list_citation,type)
        res_cit<-rbind(res_cit,t(id_element$temp))
        
        error_querry_cit<-rbind(error_querry_cit,id_element$error)
        error_querry=c(error_querry,id_element$error)
      }
      
      
      res_cit=as.data.frame(res_cit,stringsAsFactors = FALSE)
      names(res_cit)<-c("id_cit","auteur_cit","titre_cit","date_cit","essn_cit","issn_cit", "journal_cit")
      
      
      ind_id<-sapply(res_cit$id_cit,function(x){
        return(grep(x,(res_new$citation)))
      })
      res_cit_final=cbind(res_new[ind_id,],res_cit)
      res_cit_final<-res_cit_final[,-which(names(res_cit_final)=="ref_pmid")]
      res_cit_final<-res_cit_final[,-which(names(res_cit_final)=="citation")]
      
      names(res_cit_final)<- c("cited identifier","cited auth","cited title","cited date","cited journal","h","cited issn","cited essn","check","check ind","citing identifier", "citing auth","citing title","citing date","citing essn","citing issn","citing journal" )
      if(dim(res_cit_final)[1]>0)res_cit_final<-res_cit_final[order(unlist(res_cit_final$`cited identifier`)),]
      
      res_cit_accept=res_cit_final[res_cit_final$check>=value_same_min_accept,]
      if(dim(res_cit_final)[1]>0) row.names(res_cit_accept)<-1:dim(res_cit_accept)[1]
      res_cit_ask=res_cit_final[(res_cit_final$check>=value_same_min_ask) & (res_cit_final$check<value_same_min_accept),]
      if(dim(res_cit_ask)[1]>0) row.names(res_cit_ask)<-1:dim(res_cit_ask)[1]
      
      
      
      en=Sys.time()
    }else {#si cit pas demander 
      res_cit_accept=res_cit_ask=NULL
      error_querry_cit<-NULL
    } 
  }else {#si cit pas demander 
    res_cit_accept=res_cit_ask=NULL
    error_querry_cit<-NULL
  }
  return(
    list(dataframe_citation_accept=res_cit_accept,error_querry_publi=error_querry,error_querry_citation=error_querry_cit,title_vector=ti_data,author_vector=au_data,dataframe_citation_ask=res_cit_ask,
         reject_analyse=reject,dataframe_publi_found=res_new,dataframe_ref_accept=res_ref_accept,error_querry_ref=error_querry_ref,dataframe_ref_ask=res_ref_ask))
  
  
}


find_journal_domaine_v2<-function(journal_data,issn,essn,journal_table_ref){
  
  
  
  
  dom=sapply(1:length(journal_data),FUN=function(x){
    if(!is.na(journal_data[x])) {
      if(!is.null(issn[x]) && issn[x]!="" && !is.na(issn[x]) ){
        ind=which(issn[x]==journal_table_ref$issn) 
        if(length(ind)==1) return((journal_table_ref$SubField_English[ind]))
      }
      if(!is.null(essn[x]) && essn[x]!=""&& !is.na(essn[x])){
        ind=which(essn[x]==journal_table_ref$essn) 
        if(length(ind)==1) return((journal_table_ref$SubField_English[ind]))
      }
      tp=grep(paste0("^",tolower(trimws(gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",gsub("\\s*\\([^\\)]+\\)","",journal_data[x])))),"$"),tolower(trimws(journal_table_ref$Source_title)),perl = TRUE)
      
      if(length(tp)==1){
        return((journal_table_ref$SubField_English[tp]))
      }else{
        tp=grep(paste0("^",tolower(trimws(gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",gsub("\\s*\\([^\\)]+\\)","",journal_data[x]))))),tolower(trimws(journal_table_ref$Source_title)),perl = TRUE)
        if(length(tp)==1){
          return((journal_table_ref$SubField_English[tp]))
        } else {
          tp=grep(paste0(tolower(trimws(gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",gsub("\\s*\\([^\\)]+\\)","",journal_data[x]))))),tolower(trimws(journal_table_ref$Source_title)),perl = TRUE)
          if(length(tp)>0){
            return((journal_table_ref$SubField_English[tp]))
          }
        }
      }
      
      
      
    }
  })
  dom_length<-sapply(1:length(dom), FUN=function(x) length(unlist(dom[x])))
  ind<-which(dom_length>1)
  
  
  
  temp<-sapply(ind, function(x){
    cat<-names(table(dom[x]))
    if(length(cat)==1) return(cat) else return(NA)
  })
  
  dom[ind]<-temp
  return(dom)
  
}

