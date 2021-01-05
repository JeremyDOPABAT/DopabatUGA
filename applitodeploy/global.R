
library(network)
library(igraph)
library(igraphdata)
require("tm")
require('wordcloud')
require("RColorBrewer")
require("rvest")
require("visNetwork")
library(pdftools)
library(httr)
library(lubridate)
library(tm)
library(plyr)
library(bibliometrix)
library(V8)
library(bib2df)
library(plotly)
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

#library(rlang)
library(tidyverse)
library(readr)
library(ggplot2)

make_input_notwork_graph<-function(keywords) { 
  # fonction interne cr?an les diff?rentes liste pour cr?e le graph
  #intput  keywords: liste de mot cl?s 
  
  #outpout res : liste de 3 ?l?m?ent 
  #            data_eges : table de deux colonnes "from" sommet et "to" sommet d'arriv? 
  #            node_list liste de sommet de graph 
  #            wei poids de chaque sommet (d?termine la taille du sommet )             
  from=c()# liste des mot d?part 
  to=c()# liste des mot "arriver" enssemble ces deux variable crees les arrettes du graph 
  
  
  ind=which(is.na(keywords))
  if(length(ind)!=length(keywords)){
    wei=table(unlist(keywords))# poid de chaque terme au niveau des noeuds 
    term<-unique(unlist(keywords))# les mot ck? unique pour faire les noeud du graph 
    
    
    if(is_empty(which(names(wei)==""))==FALSE){ 
      wei<-wei[-(which(names(wei)==""))]
      term<-term[-(which(term==""))]
    }
    
    term<-term[order(factor(term, levels=names(wei)))]# on odonne les nom dans le m?me ordre que l'ordre de poid pour les faire correspondre ensuite 
    
    
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
    ind=which(from=="")
    if(length(ind)>0) {
      from=from[-ind]
      to=to[-ind]
    }
    ind=which(to=="")
    if(length(ind)>0) {
      from=from[-ind]
      to=to[-ind]
    }
    
  } else term=NULL
  
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
  
  
  ind=which(is.na(domainall))
  if(length(ind)!=length(domainall)){
    
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
    
    
    
  }else term=NULL
  
  
  if(length(term)==0){  
    data_edge <- tibble(from = "no data as been found", to = "no data as been found")
    
    node_list <- tibble(id = "no data as been found")
    wei=table(c("no data as been found"))
  }
  
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
    # print(head(weitop,top_number))
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
        mtext(titre,side=2)
        
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
      mtext(titre,side=2)
    }
    
  }  
  
  return(p_res)
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
    print("passe dans le eslse ")
    
    
    if(domain==FALSE){
      res=make_input_notwork_graph(keywords)
      print("out the input")
      direct=FALSE
      add_title_domain="keywords"
    }else { 
      
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
  #' Title
  #'
  #' @param title_vector vector de titre 
  #' @param author_vector  vector de auteur 
  #' @param sep sep between author 
  #' @param max_aut max auteur autoris
  #'
  #' @return deux vecteur 
  
  
  # fonction pertmettant de supprimer les entr? ind?cisrable (ou de les rendre compatible )
  # input pdf_folder : lien du dossier dans lequel se trouve le fichier pdf 
  
  #output   une dataframe avec les m?tadonner qui nous interesse 
  
  
  ind=which(title_vector=="")
  if(length(ind)>0){
    title_vector=title_vector[-ind]
    author_vector=author_vector[-ind]
    print("Publication without title in the select language can't be analysed")
  } 
  
  if(length(sep)==1) if(sep==""){sep=rep(",",length(author_vector))} else sep=rep(sep,length(author_vector))  
  
  au_sep=sapply(1:length(author_vector),FUN=function(x){ 
    if(length(author_vector[[x]])!=1){
      author_vector[[x]]=paste(author_vector[[x]],collapse = ";")
      sep[x]=";"
      author_vector[[x]]=gsub(",","",author_vector[[x]])
    }
    strsplit(author_vector[[x]],sep[x])
  })
  nb_author<-sapply(1:length(author_vector),FUN=function(x) length(au_sep[[x]]))
  
  
  ind=which(nb_author==0)
  if(length(ind)>0){
    title_vector=title_vector[-ind]
    author_vector=author_vector[-ind]
    sep=sep[-ind]
    print("Publication without author can't be analyse")
  } 
  
  ind1=which(nb_author>max_aut)
  
  
  if(length(ind1)>0){
    temp<-sapply(ind1,FUN=function(x){
      return(list((au_sep[[x]])[1:max_aut]))
    })
    au_sep[ind1]<-temp
    
    author_vector=sapply(1:length(au_sep),FUN=function(x) paste(unlist(au_sep[x]),collapse = ","))
    
    
    print(paste("the publication with", max_aut, "or more author will be restrected to",max_aut, "author(s)"))
  }
  author_vector=sapply(1:length(author_vector),FUN = function(x) gsub("[{}]", "", author_vector[[x]]))
  
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
    tp<-max(1-(adist(Unaccent(ti_trouver[x]),Unaccent(ti_data),ignore.case = TRUE)/max_apply),na.rm = TRUE)
    tw<-which((1-(adist(Unaccent(ti_trouver[x]),Unaccent(ti_data),ignore.case = TRUE)/max_apply))==tp)[1]
    #print("inside indic")
    #print(ti_trouver[x])
    
    # print(adist(Unaccent(ti_trouver[x]),Unaccent(ti_data),ignore.case = TRUE))
    return(list(tp,tw))
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
      sprintf("Requete en echec : %s (%s) \n>>%s",
              message_error(r),
              r$status, r$request$url)
      
    )
    
    return(NULL)
  }
  
  
}






get_cit_or_ref<-function(resdt,type="cit",token){# on r?cup?re les infodes citation uniquement 
  #permet de recup?r? les citation et les reference d'ads grace a une base de publie en entr?e, 
  #fonction interne 
  #print(head(resdt))
  last_good_result=NULL
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
    res_cit=c()
    withProgress(
      message='Please wait',
      detail=paste0("doing ",type, ' search in ads ...'),
      value=0, {
        
        for(j in 1:count){# on parcour tout les ref/cit
          first<-(j-1)*pas_cit+1
          last<-j*pas_cit
          incProgress(1/count)
          
          
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
            print("aie")
            #print(message_error(r))
            #print(paste0("https://api.adsabs.harvard.edu/v1/search/query?q=", adress))
            titre_error=data.frame(titre=NA)
            #names(titre_error)=c("Publication title")
            titre_error$status=r$status
            titre_error$message=message_error(r)
            titre_error$type_error=type
            return(titre_error)
          })
          # print("sortie error")
          if(length(error)>0){
            #print("dans le if ")
            error_querry_cit<-rbind(error_querry_cit,error)
            error=c()
            
          }else {
            #print("dans le eslse")
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
          # print("avant le if ")
          if(j==count && (length(res_cit)[1]>0)) {#mise en forme finale 
            #  print("dans le if" )
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
          } else result=NULL
          print("dernier if" )
          if(length(dim(error_querry_cit)) && (j==count)) print("il y a des erreurs lors de l'agregation des info des citations") 
        }
      })
    
  } else result=NULL
  
  return(result)
  
}


#________________________________________________________________S














get_cit_or_ref_arxiv<-function(resdt,type){ 
  #Permet de recupre les citation et les reference de arxiv en se basant sur une df d'entre '
  # fonction interne 
  #input dataframe de publication trouver
  error_querry_cit=c()
  if(type=="cit"){  
    link_list<-unlist(resdt$citation_link)
  }else{
    link_list<-unlist(resdt$reference_link)
  }
  
  link_abs<-unlist(resdt$abs_link)
  res_cit<-c()
  withProgress(
    message='Please wait',
    detail=paste0("searching for ",type, "in arxiv"),
    value=0, {
      
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
        incProgress(1/length(link_list))
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
    })
  
  
  
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




fit_name_position<-function(au_data,position_base,position_togo,sep=""){
  #' Title
  #'
  #' @param au_data vecteur/list auteur  
  #' @param position_base entier position 1 nom-prenom position 2 prenom nom 
  #' @param position_togo position a aller en fct de la base
  #' @param sep sepatateur entre auteur
  #'
  #' @return vecteur author 
  
  if(length(sep)==1) {if(sep=="") sep=rep(";",length(au_data)) }
  print(sep)
  print(position_base)
  au_sep=sapply(1:length(au_data),FUN=function(x){ 
    if(length(au_data[[x]])!=1){
      au_data[[x]]=paste(au_data[[x]],collapse = ";")
      sep[x]=";"
    }
    return(strsplit(au_data[[x]],sep[x]))
  })
  print("apres s apply")
  
  #togo==2
  test_full=sapply(1:length(au_data),FUN=function(x,position_b=position_base,position_tg=position_togo){
    fin=unlist(lapply(strsplit(unlist(au_sep[x]), split = " "), function(x) {
      
      return(x[length(x)]) }))
    fin=gsub(",","",fin)
    fin=gsub(" and ","",fin)
    
    debut=unlist(lapply(strsplit(unlist(au_sep[x]), " "), function(x) {
      return(paste(x[1:length(x)-1],collapse = " ")) }))
    debut=gsub(",","",debut)
    debut=gsub(" and ","",debut)
    
    if(position_b[x]!=position_tg[x]){
      # si le type est ?gale a un, cela signifie pr?nom nom, type deux nom, pr?nom m
      
      test=sapply(1:length(debut), FUN=function(x) {
        return(paste0(fin[x],",",debut[x],collapse = " ")) 
        
      })
    }else {
      test=sapply(1:length(debut), FUN=function(x) {
        return(paste0(debut[x],",",fin[x],collapse = " ")) 
        
      })
    }
    
    return(test)
  }) 
  
  return(test_full)
  
}





ads_get_publi<-function(au_data,ti_data,position_name,pas,value_same_min_ask,value_same_min_accept,token,sep){
  #m?me chose que les fonction pr?c?dente du m?me nom mais avec ads, cette fonction permet de recup?r? les m?ta donner de publi sur l'api d'ads en se servant d'un vecteur titre/ auteur 
  
  #' Title
  #'
  #' @param au_data vecteur/list  nom auteur
  #' @param ti_data vecteur/list titre publi 
  #' @param position_name interger 1ou 2 selon la place du nom et prenom 
  #' @param pas pas d'avancement de la boucle 
  #' @param value_same_min_ask valeur reel pourcentage similitude pour ne pas valid?e doffice , en dessous de ca c'est non valid?e 
  #' @param value_same_min_accept pourcentage de similitude ou on valide d'office
  #' @param token identifiant ads 
  #' @param sep separateur 
  #'
  #' @return dataframe 
  
  last_good_result=NULL
  
  res=c()#variable resultat 
  
  error_querry=c()# table d'erreur  
  
  
  
  res_temp=supress_unfit_entry(ti_data,au_data,sep)#permet d'aclimater les do,n?es a la bd (possiblement sortable de la fonction )
  
  
  
  
  
  
  # Pr?non nom--> nom,pr?non  
  ti_data=res_temp[[1]]
  au_data=res_temp[[2]]
  
  querry_list=list()
  
  position_togo=rep(2,length(au_data))
  
  
  
  last_result=NA
  
  
  au_data=fit_name_position(au_data,position_name,position_togo =position_togo,sep )
  au_data=sapply(1:length(au_data),FUN = function(x) paste(au_data[[x]],collapse = ";"))
  
  res=c()
  inter=ceiling(length(au_data)[1]/pas)# nombre d'iteration
  withProgress(
    message='Please wait',
    detail='Doing reasearche of publication in ads...',
    value=0, {
      for(h in 1:inter){# on parcoure les auteur et les titre par pas et on fait les roquette correspondante 
        first<-(h-1)*pas+1
        last<-h*pas
        incProgress(1/inter)
        if(last>length(au_data)) last<-length(au_data)
        
        #ti_test="M31 Globular Clusters: Colors and Metallicities"
        #au_test="Huchra,John"
        #adaptation des caract?re sp?ciaux au requette pour les titre et les auteur
        (au_querry=paste0("%22",gsub("&","%26",gsub("[(]","",gsub("[)]","",gsub(";",'%22AND%22',gsub("[?]","",gsub(",","%2C",gsub(", ",",",gsub(" ,",",",gsub('[}{]',"",gsub("\\", "", gsub(":","%3A",gsub("/","%2F",
                                                                                                                                                                                                           gsub(" ","%20",Unaccent((au_data[first:last])))))
                                                                                                                                                                             , fixed =TRUE)))))))))), collapse = 'OR','%22'))
        
        
        
        (ti_querry=paste0("%22",gsub("[]]","%5D",gsub("`",'%60',gsub("[[]","%5B",gsub("[(]","%28",gsub("[)]","%29",gsub("<","%3C",gsub(">","%3E",gsub("=","%3D",gsub('[}{]',"",gsub("&","%26",gsub('"','%22',gsub("\\", "",gsub(":","%3A",gsub("/","%2F",gsub("'","%27",gsub(" ","%20",
                                                                                                                                                                                                                                                                             gsub(",","%2c",gsub("e?","e",gsub("?","",gsub("%","%25",(tolower(Unaccent(ti_data[first:last])))),fixed=TRUE),fixed = TRUE)))))), 
                                                                                                                                                                                                                  fixed=TRUE)))))))))))), collapse = 'OR','%22'))
        
        
        adress=paste0('author%3A%28',au_querry,'%29AND%20title%3A%28',ti_querry,'%29&fl=reference%20citation%20author%20title%20database%20pubdate%20bibcode%20keyword%20pub%20&sort=date%20desc&rows=500&start=',0)
        
        
        r <- httr::GET(paste0("https://api.adsabs.harvard.edu/v1/search/query?q=", adress),
                       httr::add_headers( Authorization = paste0('Bearer ', token))
        )
        querry_list=append(querry_list,paste0("https://api.adsabs.harvard.edu/v1/search/query?q=", adress))
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
        
        if(length(error)>0){
          error_querry<-rbind(error_querry,error)
          error=c()
          
        }else {#si il n'y a pas d'erreur 
          result <-jsonlite::fromJSON(txt = httr::content(r, 'text',encoding = "UTF-8"), simplifyDataFrame = TRUE)
          
          if(result$response$numFound>0){
            print("on est la")
            last_good_result=r
            res_header=result$responseHeader
            aut=result$response$docs$author
            titre=result$response$docs$title
            subj=result$response$docs$database
            pubdate=result$response$docs$pubdate
            keyword=result$response$docs$keyword
            pub=result$response$docs$pub
            
            citation=result$response$docs$citation
            print("cit")
            print(citation)
            reference=result$response$docs$reference
            
            bibcode=result$response$docs$bibcode
            
            if(is.null(citation)) citation<-NA
            if(is.null(keyword)) keyword<-NA
            if(is.null(reference)) reference<-NA
            
            if(length(unlist(titre))>1){# on met en forme le titre en cas de liste(plusieurs titre pour une seul oevre )
              titre2<-sapply(1:length(titre),FUN=function(x) return(list(unlist(titre[x])[1])))
              titre=titre2
            }
            res<-rbind(res,cbind(h,bibcode,aut,titre,subj, citation,pubdate,keyword,pub,reference))
          }
          
        }
        
        
        
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
        if(length(dim(error_querry)) &(h==inter)>0){print("il y a des erreurs")}
      }
    })
  
  
  
  
  if(value_same_min_ask<1) reject=resdt[resdt$check_title_pct<value_same_min_ask,]# les rejet? sont ceux qui non pas assez de similitudfe pour aire dans les demande 
  resdt=resdt[resdt$check_title_pct>value_same_min_ask,]
  #ask est pas accepte car on le garde dans la dataframe pour que ceux ou on a un doute soit quand même treter 
  
  return(list(res=resdt,error=error_querry,reject=reject,lastresult=last_good_result,querry_list=querry_list))
}


#_________________________







pumed_get_element_id<-function(id_list,type){
  #fonction interne a l'application de requettage pumed permettant d?vit? la r?p?tition de code quand on interoge les identifiant pour les citation et les refrence 
  # input : id_list : list d'identifiant 
  # output: liste de 2 
  # error : erreur treouver 
  # temps : donn?es trouver  
  id_list_string=paste0(id_list,collapse = ",")# on s?pare les nombre d'une virgule 
  
  querry<-paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=',id_list_string,'&retmode=json')
  # redaction de la querry 
  r <- GET(querry)# get 
  
  
  
  error=tryCatch({#rep?rage des erreur 
    querry_warning_message(r)
    
  },
  
  warning=function(cond){# si erreur on met en forme les informations 
    titre_error=as.data.frame(ti_data[first:last])
    names(titre_error)=c("Publication title")
    titre_error["Status error"]=r$status
    titre_error$Message=message_error(r)
    titre_error["Data impact"]=type
    titre_error$h=h
    return(titre_error)
  })
  
  if(length(error)==0) {#si il n'y a pas d'erreur  
    Sys.sleep(0.34)#oblogatoire pour respecter les regle de pumbmed 
    result <- jsonlite::fromJSON(txt = httr::content(r, 'text'), simplifyDataFrame = TRUE)# on recup?re les resultat du json 
    
    
    temp<-sapply(1:length(id_list),FUN=function(x){# en fct fes tag on recup?re les info de chaque article trouver car l'id listte ontient tous les article trouver 
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
  return(list(error=error,temp=temp)) # on renvois le tout dans une liste. 
}



#---------------------------------------------

df_flatten<-function(res_f){
  # data tble 
  #fonction qui permet le bonne affichage des tables dans l'interface  
  
  #date_name="date"
  res_f=as.data.frame(res_f)
  for(i in names(res_f)){# on parcourt les colonne 
    
    # ind=which(is.null(res_f[[i]]))
    # if(length(ind)>0 ) res_f[ind,i]=NA
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



shinyInput <- function(FUN, n, id, ...) {
  
  # for each of n, create a new input using the FUN function and convert
  # to a character
  vapply(seq_len(n), function(i){
    as.character(FUN(paste0(id, i), ...))
  }, character(1))
  
}



find_journal_domaine<-function(journal_data,journal_table_ref,issn="",essn="",source=""){
  #Cette fonction permet de retrouver le domaine avec le nom du journal
  
  #input
  #journal_data : vecteur nom de journaux 
  #issn vecteur contenant des issn,
  #essn veteur essn
  #journal_table_ref table de jounaux 
  
  
  
  
  col_journal=source
  journal_table_ref$Abreviation[journal_table_ref$Abreviation==""]=NA
  ab=c()
  cat=c()
  cat_trouver=c()
  pas=ceiling(dim(journal_table_ref)[1]/2)
  inter=ceiling(dim(journal_table_ref)[1]/pas)
  
  # time_min=inter*(5+3)/60
  # print(paste("Le temps d'execution est estime est  environs ",time_min,"minute(s)"))
  
  
  dom=sapply(1:length(journal_data),FUN=function(x){
    #marker
    trouver=FALSE
    # print(x)
    withProgress(
      message='Please wait',
      detail=paste0("matching journal"),
      value=0, {
        for(h in 1:inter){# boucle principale qui parcour les donn?es 
          incProgress(1/inter)
          
          first<-(h-1)*pas+1# premier individu a prendre en compte(ligne)
          last<-h*pas       # dernier ""   "      "       "   "
          if(last>dim(journal_table_ref)[1]) last<-dim(journal_table_ref)[1]
          journal_courant=journal_table_ref[first:last,]
          
          
          #print(x)
          if(!is.na(journal_data[[x]]) && trouver==FALSE  ) {
            if(length(issn)!=0){ if(!is.null(issn[x]) &&issn[x]!="" && !is.na(issn[x]) ){# on trouve l'iissn si il est present 
              ind=which(issn[[x]]==journal_courant$issn) 
              if(length(ind)>0){
                ab=c(ab,journal_courant$Abreviation[ind])
                cat=c(cat,journal_courant$Discipline.Scientifique.OST[ind])
                trouver=TRUE
                cat_trouver=c(cat_trouver,1)
              } 
            }}
            if(length(essn)!=0 && trouver==FALSE){ 
              if(!is.null(essn[x]) && essn[x]!=""&& !is.na(essn[x])){# pareil pour l'essn
                ind=which(essn[x]==journal_courant$essn) 
                if(length(ind)>0) {
                  ab=c(ab,journal_courant$Abreviation[ind])
                  cat=c(cat,journal_courant$Discipline.Scientifique.OST[ind])
                  cat_trouver=c(cat_trouver,1)
                  trouver=TRUE
                }
              }
            }
            #on seach le nom exacte du journal dans la liste de reference 
            if(trouver==FALSE){
              tp=grep(paste0("^",tolower(trimws(gsub("\\[|\\]", "",gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",gsub("\\s*\\([^\\)]+\\)","",journal_data[[x]]))))),"$"),tolower(trimws(journal_courant[[col_journal[[x]]]])))
              
              #}, error = function(e) { return(NA)})
              
              if(length(tp)>0){
                trouver=TRUE
                ab=c(ab,journal_courant$Abreviation[tp])
                cat=c(cat,journal_courant$Discipline.Scientifique.OST[tp])
                cat_trouver=c(cat_trouver,1)
              } 
            }
            #   # on cherche une correspondance de reference qui commence par le debut
            #   tp=grep(paste0("^",tolower(trimws(gsub("\\[|\\]", "",gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",gsub("\\s*\\([^\\)]+\\)","",journal_data[x])))))),tolower(trimws(journal_courant[[source]])))
            #   if(length(tp)>0){
            #     return((journal_courant$Abreviation[tp]))
            #   } else {
            #     #on cherche une reference corespondans partout dans le nom
            #     tp=grep(paste0(tolower(trimws(gsub("\\[|\\]", "",gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",gsub("\\s*\\([^\\)]+\\)","",journal_data[x])))))),tolower(trimws(journal_courant[[source]])))
            #     if(length(tp)==1){
            #       return((journal_courant$Abreviation[tp]))
            #     }
            #     
            #   }
            # }
            
          }
         
        }
        if(trouver==FALSE){
          ab=c(ab,NA)
          cat=c(cat,NA)
          cat_trouver=c(cat_trouver,0)
        }
      })
    return(list(ab=ab,cat=cat,trouver=cat_trouver))
  })
  # dom_length<-sapply(1:length(dom), FUN=function(x) length(unlist(dom[x])))
  # ind<-which(dom_length>1)
  # 
  # 
  # 
  # temp<-sapply(ind, function(x){
  #   cat<-names(table(dom[x]))
  #   if(length(cat)==1) return(cat) else return(NA)
  # })
  
  #dom[ind]<-temp
  return(dom) 
  
}


extract_data_api_pumed<-function(data_pub,ti_name,au_name,pas=8,value_same_min_accept=0.85, value_same_min_ask=0.95,type="cit",source_name="",sep_vector_in_data="",position_vector_in_data=""){
  
  # fonction permetant d'interroger pumeed sur les reference et les citation d'un corpus de publication placer en entree 
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
  
  #l'api change automatiquement les roquette se qui la rend dur a utilisée 
  
  #cette fonction est la meême que poiur ads, juste l'interrogation et la reprise de données sont différente mais l'algo est le même aller voir les commentzaire
  # de la fonction extract_api_ads_nasa
  
  #RECUP2RAtion des noms de colonne 
  au_data<-data_pub[au_name][[1]]
  ti_data<-data_pub[ti_name][[1]]
  
  
  
  res_temp=supress_unfit_entry(ti_data,au_data,sep = data_pub$sep)#permet d'aclimater les donn?es a la bd (possiblement sortable de la fonction )
  ti_data=res_temp[[1]]
  au_data=res_temp[[2]]
  
  #_______________________initialisation ______________________________________
  
  
  res_rest=c() 
  res_wos=c() 
  err1=c()
  err2=c() 
  reject1=reject2=c()
  if(source_name!="" && (type=="ref" || type=="all")){
    data_wos=data_pub[data_pub[source_name]=="WOS",]
    if(dim(data_wos)[1]!=0) {
      if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_wos[sep_vector_in_data][[1]]
      if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_wos)[1]) else position_name=data_wos[position_vector_in_data][[1]]
      res_wos_all=pumed_get_publi(data_wos[au_name][[1]],data_wos[ti_name][[1]],data_wos$position_name,pas,value_same_min_ask,value_same_min_accept,data_wos$sep)
      res_wos=res_wos_all$res# les resulta
      err1=res_wos_all$error# les erreurs 
      reject1=res_wos_all$reject # les titre rejeter 
      #certain titre ressemble a d'autres du corpuse et sont retenu par lapi mais ne doivent pas etre pris en compte.  si leurs titre est trop different alors on les rejects
      
    }else{
      res_wos=NULL
      err1=NULL
      reject1=NULL
      
    }
    
    data_rest=data_pub[data_pub[source_name]!="WOS",]#non wos 
    if(dim(data_rest)[1]!=0) {
      if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_rest[sep_vector_in_data][[1]]
      if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_rest)[1]) else position_name=data_rest[position_vector_in_data][[1]]
      
      res_rest_all=pumed_get_publi(data_rest[au_name][[1]],data_rest[ti_name][[1]],data_rest$position_name,pas,value_same_min_ask,value_same_min_accept,data_rest$sep)
      res_rest=res_rest_all$res
      err2=res_rest_all$error
      reject2=res_rest_all$reject
      
    }else{
      res_rest=NULL
      err2=NULL
      reject2=NULL
      
    }
    
    
    res_new=rbind(res_rest,res_wos)
    error_querry=rbind(err1,err2)
    reject=rbind(reject1,reject2)
  }else {
    if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_pub[sep_vector_in_data][[1]]
    if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_pub)[1]) else position_name=data_pub[position_vector_in_data][[1]]
    
    resdt_all=pumed_get_publi(au_data,ti_data,data_pub$position_name,pas,value_same_min_ask,value_same_min_accept,data_pub$sep)
    error_querry=resdt_all$error
    res_new=resdt_all$res
    reject=resdt_all$reject
    
    
  }
  
  
  
  # interogation par titre et auteur pour retrouver les identifiant_______________________________________________________
  
  
  
  
  
  if(type!="cit"){# on prend soit les citation soit "all soit uniquement les reference 
    
    if(source_name!="") res_new_s=res_rest else  res_new_s=res_new
    
    id_list_ref=as.character(unlist(res_new_s$ref_pmid))# on r?cup?re les ref?rences de notre resultat pr?c?dent 
    if(length(id_list_ref)>0){
      pas=20# le pas peut ?tre plus grand car les identifiant sont petit 
      inter=ceiling(length(id_list_ref)[1]/pas)
      res_ref=c() 
      error_querry_ref=c()#matrice erreur citation 
      
      withProgress(# permet la bare de charchegement dans l'apli, enpeche la fonction de tourner hors shiny mais facile a commenter 
        message='Please wait',
        detail=paste0("doing research reference in pumed"),
        value=0, {
          #
          for(h in(1:inter)){
            first<-(h-1)*pas+1
            last<-h*pas
            incProgress(1/inter)
            if(last>length(id_list_ref)) last<-length(id_list_ref)
            
            id_element=pumed_get_element_id(id_list_ref[first:last],type)
            res_ref<-rbind(res_ref,t(id_element$temp))
            
            error_querry=c(error_querry,id_element$error)
          }
        })
      res_ref=as.data.frame(res_ref,stringsAsFactors = FALSE)
      
      
      names(res_ref)<-c("id_ref","auteur_ref","titre_ref","date_ref","essn_ref","issn_ref", "journal_ref")
      
      
      
      
      ind_id<-sapply(res_ref$id_ref,function(x){# on associe les bon id au bon article pour le mettre avec lmes publis, cela nous sert aussi d'identifiant 
        return(grep(x,(res_new$ref_pmid)))
      })
      
      
      
      res_ref_final=cbind(res_new[ind_id,],res_ref)
      res_ref_final<-res_ref_final[,-which(names(res_ref_final)=="ref_pmid")]
      names(res_ref_final)<- c("refering identifier","refering auth","refering title","refering date","refering journal","h","refering issn","refering essn","check","check ind","refered identifier", "refered auth","refered title","refered date","refered essn","refered issn","refered journal" )
      #View(res_ref_final)
      if(dim(res_ref)[1]>0) res_ref_final<-res_ref_final[order(unlist(res_ref_final$`refering identifier`)),]
      res_ref_accept=res_ref_final[res_ref_final$check>=value_same_min_accept,]
      # print(dim(res_ref_accept))
      if(dim(res_ref_accept)[1]>0) row.names(res_ref_accept)<-1:dim(res_ref_accept)[1]
      
      res_ref_ask=res_ref_final[(res_ref_final$check>=value_same_min_ask) & (res_ref_final$check<value_same_min_accept),]
      # print(dim(res_ref_ask))
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
    withProgress(
      message='Please wait',
      detail=paste0("doing research citation in pumed"),
      value=0, {
        #
        for(h in(1:inter)){
          incProgress(1/inter) # augmentation de la barre de chargement
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
      })
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
  
  if(is.null(dim(error_querry))){ 
    error_querry<-as.data.frame(cbind(NA,NA,NA,NA))
    names(error_querry)=c("Publication title","Status error","Message error", "Data impact")}
  
  if(is.null(dim(res_new))) res_new=read.csv(text=paste0(names(res_new),collapse = ","))
  return(
    list(dataframe_citation_accept=res_cit_accept,error_querry_publi=error_querry,error_querry_citation=error_querry_cit,title_vector=ti_data,author_vector=au_data,dataframe_citation_ask=res_cit_ask,
         reject_analyse=reject,dataframe_publi_found=res_new,dataframe_ref_accept=res_ref_accept,error_querry_ref=error_querry_ref,dataframe_ref_ask=res_ref_ask))
  
  
}





pumed_get_publi<-function(au_data,ti_data,position_name,pas,value_same_min_ask,value_same_min_accept,sep){
  # on get les publication de pu?ed pour un vecteur d'auteur et de titre donn?es , on pr?cise aussi les valeur d'accespatations
  #on renvoie la datazframe utiliser dans la fct ^rincipale et la matrice d'eerreur.
  # initialisation  
  
  inter=ceiling(length(au_data)[1]/pas)
  id_list=c()
  res<-c()
  h_trouver=c()
  error_querry=c()
  reject=c()
  #-------------------------------------------
  position_togo=rep(2,length(au_data))
  au_data=fit_name_position(au_data = au_data,position_base = position_name,position_togo =position_togo,sep = sep)
  
  
  au_data=sapply(1:length(au_data),FUN = function(x) paste(au_data[[x]],collapse = ";"))
  withProgress(
    message='Please wait',
    detail=paste0("doing research in pumed"),
    value=0, {
      
      for(h in 1:inter){
        print(h)
        first<-(h-1)*pas+1
        last<-h*pas
        incProgress(1/inter)
        if(last>length(au_data)) last<-length(au_data)
        
        
        phrase_cut=paste0(unlist(strsplit(paste0(ti_data[first:last],collapse ='[Title]+OR+')," ")),collapse ='+')#on ajoute les balise de titre pour signifier ce que le chercher et d?limiter chaque titree 
        
        
        
        
        
        (ti_querry=paste0(gsub("`",'%60',gsub("[(]","%28",gsub("[)]","%29",gsub("<","%3",gsub(">","%3E",gsub("=","%3D",gsub('[}{$]',"",gsub("&","%26",gsub('"','%22',gsub("\\", "",gsub(":","%3A",gsub("/","%2F",gsub("'","%27",gsub(" ","+",
                                                                                                                                                                                                                                     gsub(",","%2c",gsub("e?","e",gsub("?","%3F",Unaccent(phrase_cut),fixed=TRUE),fixed = TRUE)))))), 
                                                                                                                                                                          fixed=TRUE)))))))))), collapse = '+OR+',"[Title]"))
        
        
        
        phrase_cut_au= paste0(au_data[first:last],collapse ='[Author]+OR+')
        
        
        
        (au_querry=paste0(gsub("[?.]","",gsub(",","%2C",gsub("\\", "", gsub(":","%3A",gsub("/","%2F",
                                                                                           gsub(" ","+",Unaccent(phrase_cut_au))))
                                                             , fixed =TRUE))), collapse = 'OR','[Author]'))
        querry<-paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',au_querry, '+AND+',ti_querry,'&sort=pub+date&retmode=json')
        querry=gsub(" ","+",gsub("\\s+"," ",querry))
        (querry=gsub("++","+", querry,fixed = TRUE))
        
        #"AND",ti_querry,'
        
        r <- GET(querry)
        Sys.sleep(0.34)
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
          
          result <- jsonlite::fromJSON(txt = httr::content(r, 'text'), simplifyDataFrame = TRUE)
          
          id_list<-result$esearchresult$idlist
          
          # retourne les id ____________________________________
          #on requette les id retourn?e pour pouvoir avoir les diff?rente infos de la publie
          
          if(length(id_list)>0){
            h_trouver=c(h_trouver,h)
            print("oooo")
            id_list_string=paste0(id_list,collapse = ",")
            
            #etourn  les id 
            querry2<-paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=',id_list_string,'&retmode=json')
            r2 <- GET(querry2)
            if (r2$status != 200){
              warning(
                sprintf("Requ?te en ?chec : %s (%s) \n>>%s",
                        switch(as.character(r$status_code),
                               '400' = 'Requ?te incorrecte', '401' = "Unauthorized",
                               '403' = 'Forbidden', '404' = 'Not Found','414'='URL too long','500'='Internal Server falure check the query',
                               'Erreur inconnue'),
                        r2$status, r2$request$url)
              )
              return(NULL)
            }
            Sys.sleep(0.34)
            result2 <- jsonlite::fromJSON(txt = httr::content(r2, 'text'), simplifyDataFrame = TRUE)
            
            
            temp<-sapply(1:length(id_list),FUN=function(x){
              id=result2$result[[id_list[x]]]$uid
              au=list(result2$result[[id_list[x]]]$authors$name)
              ti=result2$result[[id_list[x]]]$title
              da=result2$result[[id_list[x]]]$pubdate
              jou=result2$result[[id_list[x]]]$fulljournalname
              pmid=list(result2$result[[id_list[x]]]$references$pmid)
              issn=result2$result[[id_list[x]]]$issn
              essn=result2$result[[id_list[x]]]$essn
              h_trouver=h
              if(is.null(pmid)) pmid<-NA
              return(c(id,au,ti,da,jou,pmid,h_trouver,issn,essn))
              
            })
            
            res<-rbind(res,t(temp))  
            # if(sum(duplicated2(as.data.frame(res,stringsAsFactors = FALSE)[,1]))>0){
            #   print("stttttttttttttttooooooooooooooooooooooooop")
            #   print(h)
            #   res=as.data.frame(res,stringsAsFactors = FALSE)
            #   View(res[duplicated2(as.data.frame(res,stringsAsFactors = FALSE)[,1])==TRUE,])
            #   browser()
            # }
          }
        }
      }
    })
  
  
  
  if(!is.null(res)){
    res=as.data.frame(res,stringsAsFactors = FALSE)
    names(res)<-c("id", "auteur","titre","date", "journal","ref_pmid","h_ref","issn","essn")
    if(sum(duplicated(res$id))>0) res<-res[-(which(duplicated(res$id)==TRUE)),]# supression des doublon causer par la requette translatin 
    
    
    indic_compaire_title<-compaire_title(res$titre,ti_data)
    
    res$check_title_pct=indic_compaire_title[1,]
    res$check_title_ind=indic_compaire_title[2,]
    
    
    if(value_same_min_ask<1) reject=res[res$check_title_pct<value_same_min_ask,]# les rejet? sont ceux qui non pas assez de similitudfe pour aire dans les demande 
  }
  #resdt_ask=resdt[resdt$check_title_pct>value_same_min_ask &resdt$check_title_pct<value_same_min_accept,]
  res_new=res[res$check_title_pct>=value_same_min_ask,]
  
  return(list(res=res_new,error=error_querry,reject=reject))
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
    print(sep_vector_in_data)
    data_wos=data_pub[data_pub[source_name]=="WOS",]#separation des source 
    print("rentre dans la partie wos ")
    
    if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_wos[sep_vector_in_data][[1]]
    if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_wos)[1]) else position_name=data_wos[position_vector_in_data][[1]]
    if(dim(data_wos)[1]!=0) {
      res_wos_all=ads_get_publi(data_wos[au_name][[1]],data_wos[ti_name][[1]],position_name[data_pub[source_name]=="WOS"],pas,value_same_min_ask,value_same_min_accept,token,sep)
      res_wos=res_wos_all$res
      err1=res_wos_all$error
      reject1=res_wos_all$reject
      lastresult=res_wos_all$lastresult
      querry_list1=res_wos_all$querry_list
    }else{#ci type =cit pour eviteez les erreur de non reto
      res_wos=NULL
      err1=NULL
      reject1=NULL
      querry_list1=NULL
    }
    
    
    data_rest=data_pub[data_pub[source_name]!="WOS",]
    print("rentre dans la partie CSV ")
    if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_rest[sep_vector_in_data][[1]]
    if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_rest)[1]) else position_name=data_rest[position_vector_in_data][[1]]
    if(dim(data_rest)[1]!=0) {
      
      res_rest_all=ads_get_publi(data_rest[au_name][[1]],data_rest[ti_name][[1]],position_name[data_pub[source_name]!="WOS"],pas,value_same_min_ask,value_same_min_accept,token,sep)
      print(dim(res_rest_all))
      res_rest=res_rest_all$res
      err2=res_rest_all$error
      reject2=res_rest_all$reject
      lastresult=res_rest_all$lastresult
      querry_list2=res_rest_all$querry_list
    }else{
      res_rest=NULL
      err2=NULL
      reject2=NULL
      querry_list2=NULL
    }
    
    
    res=rbind(res_rest,res_wos)
    error_querry=rbind(err1,err2)
    reject=rbind(reject1,reject2)
    querry_list=c(querry_list1,querry_list2)
  }else {
    
    if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_pub[sep_vector_in_data][[1]]
    if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_pub)[1]) else position_name=data_pub[position_vector_in_data][[1]]
    if(dim(data_pub)[1]!=0) {
      resdt_all=ads_get_publi(au_data,ti_data,position_name,pas,value_same_min_ask,value_same_min_accept,token,sep = sep)
      error_querry=resdt_all$error
      res=resdt_all$res
      reject=resdt_all$reject
      lastresult=resdt_all$lastresult
      querry_list=resdt_all$querry_list
    }
  }  
  
  resdt<-as.data.frame(res,stringsAsFactors = FALSE)
  
  
  # source("C:/Users/moroguij/Documents/R_programs/functions_analyses.R")#source nneded function 
  #initialisation des variable __________________________________________  
  
  # Pr?non nom--> nom,pr?non  
  
  
  #ins?r? fct 
  if(type=="all"|| type=="cit"){
    
    
    res_cite=get_cit_or_ref(resdt,type="cit",token)# citation 
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
    
    if(type=="ref"){
      total_res=NULL
      total_res_ask=NULL
      error_querry_cit=NULL
    }
    res_ref=get_cit_or_ref(res_rest,type="ref",token)  #on cherche les ref unkiquement sur la partie concerner 
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
              reject_analyse=reject, last_result=lastresult,dataframe_publi_found=resdt,dataframe_ref_accept=total_res_ref,dataframe_ref_ask=total_res_ref_ask,querry_list=querry_list))
  
  
}




extraction_data_api_arxiv<-function(data_pub,ti_name,au_name,pas=8,value_same_min_accept=0, value_same_min_ask=1,type="cit",source_name="",sep_vector_in_data="",position_vector_in_data="",id_name=""){
  
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
  
  if(id_name!="") id_data<-data_pub[id_name][[1]] else id_data=NULL 
  if(length(which(!is.na(id_data)))==0) id_data=NULL 
  if(source_name!="" && (type=="ref" || type=="all")){
    data_wos=data_pub[data_pub[source_name]=="WOS",]
    
    if(dim(data_wos)[1]!=0) {
      if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_wos[sep_vector_in_data][[1]]
      if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_wos)[1]) else position_name=data_wos[position_vector_in_data][[1]]
      
      res_wos_all=arxiv_get_publi(data_wos[au_name][[1]],data_wos[ti_name][[1]],position_name,pas,value_same_min_ask,value_same_min_accept,sep,id_data)
      res_wos=res_wos_all$res
      err1=res_wos_all$error
      reject1=res_wos_all$reject
      
    }else{
      res_wos=NULL
      err1=NULL
      reject1=NULL
      
    }
    
    
    data_rest=data_pub[data_pub[source_name]!="WOS",]
    
    if(dim(data_rest)[1]!=0) {
      if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_rest[sep_vector_in_data][[1]]
      if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_rest)[1]) else position_name=data_rest[position_vector_in_data][[1]]
      
      res_rest_all=arxiv_get_publi(data_rest[au_name][[1]],data_rest[ti_name][[1]],position_name,pas,value_same_min_ask,value_same_min_accept,sep,id_data)
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
    if(length(sep_vector_in_data)==1) if(sep_vector_in_data=="") sep="" else sep=data_pub[sep_vector_in_data][[1]]
    if(length(position_vector_in_data)==1) if(position_vector_in_data=="") position_name=rep(1,dim(data_pub)[1]) else position_name=data_pub[position_vector_in_data][[1]]
    
    resdt_all=arxiv_get_publi(au_data = au_data,ti_data,position_name,pas,value_same_min_ask,value_same_min_accept,sep = sep,id_data)
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
      print("laaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
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
    
    # colnames(resdt)<-c("Cite","Titre", "Auteur","Domaine","Journal")
    # ft<-Sys.time()
    
    
    return(list(res_publi_foundt=resdt, res_citation_accept=res_citation,res_citation_ask=res_citation_ask,res_reference_accept=res_reference,res_reference_ask=res_reference_ask, res_reject=resdt_reject,error_querry=error_querry,error_querry_cit=error_querry_cit,error_querry_ref=error_querry_ref,title_vector=ti_data,author_vector=au_data,all_title_found=ti_trouver,all_author_found=au_trouver))
  }else{
    print("no data found")
    return(NA)
  }
}


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
              print(inter)
              cite_link<-get_cit_link_arxiv(titre)
              ref_link<-NA
              
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
    # print(paste("Le temps d'execution est estime est  environs ",time_min,"minute(s)"))
    
    
    withProgress(
      message='Please wait',
      detail="searching for publication in arxiv",
      value=0, {
        for(h in 1:inter){# boucle principale qui parcour les donn?es
          
          #print(h)
          #print("On passe title")
          start=c(start,Sys.time())
          incProgress(1/inter)
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
                  
                  titre=xml_data[id_index[x]]$entry$title
                  cite_link<-get_cit_link_arxiv(titre)
                  ref_link<-NA
                  
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
      })
  }
  return(list(res=res,error=error_querry,reject=res_reject)) 
}




extract_ref_wos<-function(data_wos){
  
  #' Title
  #'fct interne qui met en place les donner wos ou bibtext et la rand accessible pour l'appli
  #' @param data_wos data bibtext 
  #'
  #' @return dataframe 
  
  
  res_wos=data_wos[,c("TI","AU","DE","SC","PY")]
  names(res_wos)<-c('title','author','keywords','domain','date')
  res_wos$idw=paste0("_",1:dim(res_wos)[1],"_")
  
  
  ref_split<-strsplit(data_wos$CR,split = ";")
  
  test_split=sapply(1:dim(data_wos)[1],FUN=function(x) strsplit(ref_split[[x]],split = ","))
  
  res<-c()
  length_data=c()
  for(i in (1:length(test_split))){
    temp<-sapply(1:length(test_split[[i]]), FUN=function(x){
      courant=test_split[[i]][[x]]
      
      auth<-courant[1]
      year<-courant[2]
      journ<-courant[3]
      id=paste0("_",i,"_")
      real_id=paste0("_",paste0(i,x),"_")
      return(list(id,real_id,auth,year,journ))
    })   
    res<-rbind(res, t(temp))
  }
  res_ref<-as.data.frame(res)
  names(res_ref)=c("id_to_sup","id","author","year", "journ")
  
  
  
  #View(res_ref)
  
  
  ind_id<-sapply(res_ref$id_to_sup,function(x){
    return(grep(x,res_wos$idw,fixed = TRUE))
  })
  
  
  
  
  
  
  res_ref_final=cbind(res_wos[unlist(ind_id),],res_ref)
  res_ref_final<-res_ref_final[,-which(names(res_ref_final)=="keywords")]
  res_ref_final<-res_ref_final[,-which(names(res_ref_final)=="id_to_sup")]
  
  #length(names(res_ref_final))
  names(res_ref_final)<- c("refering title",'refering auth',"refering domaine","refering date","refering identifier","refered identifier","refered auth","refered date","refered journal" )
  res_ref_final["source"]="WOS"
  return(res_ref_final)
}





















conforme_bibtext<-function(data_wos,data_base){
  #' Title
  #' mise en place des keyword  et des source si elle y son ou pas 
  #' @param data_wos data bitext   
  #'
  #' @return
  if(data_base=="WOS"){
    error=tryCatch({#reperage des erreur 
      data_wos["SC"]
    },
    
    error=function(cond){
      return(NA)
    })
    
    if(is.null(dim(error)[1])) if(is.na(error)) data_wos["SC"]="NULL"
    
    error=tryCatch({#rep?rage des erreur 
      data_wos["DE"]
    },
    
    error=function(cond){
      return(NA)
    })
    if(is.null(dim(error)[1])) if(is.na(error)) data_wos["DE"]="NULL"
    
    
    error=tryCatch({#reperage des erreur 
      data_wos["TI"]
    },
    
    error=function(cond){
      return(NA)
    })
    
    if(is.null(dim(error)[1])) if(is.na(error)) data_wos["TI"]="NULL"
    
    
    error=tryCatch({#reperage des erreur 
      data_wos["AU"]
    },
    
    error=function(cond){
      return(NA)
    })
    
    if(is.null(dim(error)[1])) if(is.na(error)) data_wos["AU"]="NULL"
    
  } else{
    error=tryCatch({#reperage des erreur 
      data_wos["RESARCH.AREAS"]
    },
    
    error=function(cond){
      return(NA)
    })
    
    if(is.null(dim(error)[1])) if(is.na(error)) data_wos["RESARCH.AREAS"]="NULL"
    
    error=tryCatch({#reperage des erreur 
      data_wos["KEYWORDS"]
    },
    
    error=function(cond){
      return(NA)
    })
    
    if(is.null(dim(error)[1])) if(is.na(error)) data_wos["KEYWORDS"]="NULL"
    
    error=tryCatch({#reperage des erreur 
      data_wos["YEAR"]
    },
    
    error=function(cond){
      return(NA)
    })
    
    if(is.null(dim(error)[1])) if(is.na(error)) data_wos["YEAR"]="NULL"
    
    
    error=tryCatch({#reperage des erreur 
      data_wos["TITLE"]
    },
    
    error=function(cond){
      return(NA)
    })
    
    if(is.null(dim(error)[1])) if(is.na(error)) data_wos["TITLE"]="NULL"
    
    
    
    error=tryCatch({#reperage des erreur 
      data_wos["AUTHOR"]
    },
    
    error=function(cond){
      return(NA)
    })
    
    if(is.null(dim(error)[1])) if(is.na(error)) data_wos["AUTHOR"]="NULL"
    
  }
  
  (test1=grep(pattern = "^TITLE*",names(data_wos)))
  (test2=grep(pattern = "^KEYWORDS*",names(data_wos)))
  (test3=grep(pattern = "^AUTHOR*",names(data_wos)))
  
  if(length(test1)>1|length(test1)>1|length(test1)>1){
    stop("hep hep hep tu vas ou ?")
  }
  return(data_wos) 
}

interdis_matrice_creation_and_calcul<-function(data_gl,table_dist,table_categ_gd,type){
  #' Title
  #' Fonction interne 
  #' @param data_gl dataglobal cit ou ref ou all 
  #' @param table_dist distance cat?gorivow
  #' @param table_categ_gd  table de dysipline 
  #' @param type type of analyse 
  #'
  #' @return list of result et analyse
  
  col_identifier=c()
  col_title=c()
  temp=0
  if(type=="ref"){# chois  du type 
    journal_domaine="refered_journal_domaine"
    identifier="refering identifier"
    title="refering title"
  }else{
    journal_domaine="citing_journal_domaine"
    identifier="cited identifier"
    title="cited title"
  }
  #View(data_gl)
  
  nb_categ=unlist(data_gl[[journal_domaine]]) #on pr?parer les colonne de la table de containgence 
  if(length(nb_categ)>0){
    list_categ<-Unaccent(names(table(nb_categ)))
    matrice_prop=as.data.frame(matrix(0,ncol=length(list_categ)))# initialisation de la matrice 
    matrice_contribution=as.data.frame(matrix(0,ncol=length(list_categ)))
    names(matrice_prop)=list_categ
    names(matrice_contribution)=list_categ
    precedent=""# initialisation , a chaque nouvelle article(id diff?rent) on ce?e une nouvelle ligne.
    for(i in 1:dim(data_gl)[1]){
      if(!is.null(data_gl[[journal_domaine]][[i]])){
        if(precedent!=data_gl[[identifier]][[i]]){
          line=rep(0,length(list_categ))
          matrice_prop=rbind(matrice_prop,line)
          line=rep(list(0),length(list_categ))
          matrice_contribution=rbind(matrice_contribution,line)
          col_identifier=c(col_identifier,data_gl[[identifier]][[i]])
          col_title=c(col_title,paste0(str_sub(string = data_gl[[title]][[i]],start = 1,end = 20),"..."))
        }
        
        col_ind=match(unique(data_gl[[journal_domaine]][[i]]),names(matrice_prop))#donne les bon indice de colonne 
        
        col_ind=col_ind[!is.na(col_ind)]
        
        matrice_prop[dim(matrice_prop)[1],col_ind]= matrice_prop[dim(matrice_prop)[1],col_ind]+1 #mis a jour des compteur 
        temp= sapply(1:length(col_ind),FUN=function(x) list(toString(c(unlist(matrice_contribution[dim(matrice_contribution)[1],col_ind[x]]),i))))
        matrice_contribution[dim(matrice_contribution)[1],col_ind]=temp
        precedent=data_gl[[identifier]][[i]]#avancement du curseur 
        #if(length(col_ind)>1) browser()
      }
    }
    
    
    matrice_prop<-matrice_prop[-1,]# on enl?ve la prermi?re ligne consitituer uniquement de 0
    matrice_contribution=matrice_contribution[-1,]
    matrice_prop<-type.convert(matrice_prop) # chiffre en chiffre 
    sumcol=colSums(matrice_prop)
    matrice_prop=rbind(matrice_prop,sumcol)
    contri_total=sapply(1:dim(matrice_contribution)[2],FUN = function(x){paste0(unlist(matrice_contribution[,x]),collapse = ",")})
    matrice_contribution=rbind(matrice_contribution,contri_total)
    
    sumrow=rowSums(matrice_prop)
    
    
    
    dia=sapply(1:dim(matrice_prop)[1],FUN = function(x){# calvule des dia par article et du total en derni_re ligne 
      
      cal=0
      matrice_prop[x,]=matrice_prop[x,]/sumrow[[x]]# contingence a pourcentage 
      lien<-list()
      
      for(i in 1:dim(matrice_prop)[2]){# il faut adittionner les couple donc parcourir les colonne deux fois 
        
        for(j in 1:dim(matrice_prop)[2]){
          
          if(matrice_prop[x,i]!=0 && matrice_prop[x,j]!=0){
            
            n1=names(matrice_prop)[i]
            n2=names(matrice_prop)[j]
            dij=table_dist[n1,n2]
            lien=append(list(c(n1,n2)),lien)
            cal=cal+matrice_prop[x,i]*matrice_prop[x,j]*dij
          }
        }
      }
      
      
      
      return(list(valeur=cal,couple=lien))
      
      
      #return(list(proportion=matrice_prop,distance=table_dist))
    })
    (DD=unlist(dia["valeur",][length(dia["valeur",])]))
    
    (ID=mean(unlist(dia["valeur",][-length(dia["valeur",])])))
    dia=dia[,-dim(dia)[2]] 
    
    (MD=DD-ID)
    
    # path_gd="data/categ_wos.csv"
    # 
    # table_categ_gd=read.csv(file=data/categ_wos.csv,header = TRUE,stringsAsFactors = FALSE,sep = ";")
    # 
    
    table_conversion=sapply(1:length(names(matrice_prop)),FUN=function(x){#construction de la table de conversion 
      ind=grep(names(matrice_prop)[[x]],table_categ_gd$catcod)
      return(unique(c(table_categ_gd$OST.Category[ind],table_categ_gd$catcod[ind])))
    })#affectation des cat?gorie OST
    table_conversion=t(table_conversion)
    new_col=unique(table_conversion[,1])
    new_matrice_prop=as.data.frame(matrix(0,ncol=length(new_col),nrow = dim(matrice_prop)[1]))
    new_matrice_contribution=as.data.frame(matrix(list(0),ncol=length(new_col),nrow = dim(matrice_contribution)[1]))
    names(new_matrice_prop)=new_col
    names(new_matrice_contribution)=new_col
    
    for(i in 1:dim(matrice_prop)[1]){
      for(j in 1:dim(matrice_prop)[2]){
        if(matrice_prop[i,j]!=0){
          
          ind=grep(names(matrice_prop)[j],table_conversion[,2])
          #if(length(ind)>0) browser()
          new_matrice_prop[i,table_conversion[ind,1]]=new_matrice_prop[i,table_conversion[ind,1]]+matrice_prop[i,j]
          new_matrice_contribution[i,table_conversion[ind,1]]=paste0(unique(strsplit(gsub(" ","",paste0(unlist(new_matrice_contribution[i,table_conversion[ind,1]]),",",unlist(matrice_contribution[i,j]),collapse = ",")),split = ",")[[1]]),collapse = ",")# certaine ligne une fois merger compte pour plusieurs fois car certain journal multidiciplinaire sont dans plusiur colonne donc le unique peret de ne pas avoir a les afficher plusieurs fois 
          
        }
      }
    }
    
    #  View(new_matrice_contribution)
    #View(new_matrice_prop)
    col_identifier=c(col_identifier,"TOTAL")
    col_title=c(col_title,"TOTAL")
    
    #print("coool")
    #print((col_identifier))
    #print(dim(new_matrice_prop))
    matrice_prop=as.data.frame(matrice_prop,stringsAsFactors = FALSE)
    matrice_prop["IDENTIFIANT"]=col_identifier
    matrice_prop["TITLE"]=col_title
    
    #matrice_prop[["CONTRIBUTION"]]=col_list_line
    #print(dim(matrice_prop))
    resultat<-list(dia=dia["valeur",],md=MD,id=ID,dd=DD,prop=matrice_prop,prop_grande_discipline=new_matrice_prop,contribution=new_matrice_contribution)
  }else{
    resultat=NULL
  }
  
  
  return(resultat)
}

merge_result_data_base<-function(ads,arxiv,pumed,wos,col_journal=c(NULL,NULL,NULL,NULL),type){
  
  #' @param ads matrice cit/ref venant d'ads 
  #' @param arxiv matrice cit/ref venant d'arxiv
  #' @param pumed matrice cit/ref venant d'pumed
  #' @param wos matrice cit/ref venant d'un fichier bibtext
  #' @param col_journal vecteur de 4, les colone de journal a utiliser pour les 4 matrice(1 par matrice). apr?viation ou nom plein
  #' @param type type de traitement cit/ref/all (les deux)
  #' @return dataframe fusionner 
  
  
  if(type=="ref"){
    ads_data=ads$dataframe_ref_accept
    arxi_data=arxiv$res_reference_accept
    pumed_data=pumed$dataframe_ref_accept
    if(!is.null(wos)) wos$source=col_journal[4]
  }else {
    ads_data=ads$dataframe_citation_accept
    arxi_data=arxiv$res_citation_accept
    pumed_data=pumed$dataframe_citation_accept
    
  }
  
  
  if(!is.null(ads)) ads_data$source=col_journal[1]
  if(!is.null(arxiv)) arxi_data$source=col_journal[2]
  if(!is.null(pumed)) pumed_data$source=col_journal[3]
  
  
  data_merge=as.data.frame(rbind.fill(ads_data,arxi_data),stringsAsFactors = FALSE)
  
  data_merge=as.data.frame(rbind.fill(data_merge,pumed_data),stringsAsFactors = FALSE)
  if(type=="ref")   data_merge=as.data.frame(rbind.fill(data_merge,wos),stringsAsFactors = FALSE)
  
  return(data_merge) 
}

combine_analyse_data<-function(df_global,journal_table_ref,type){
  #' 
  #'#cette fonction est une fonction interne permettant de retrouuver les journaux a partir des nom, abreviation ou issn.
  #cette fonction s'adapte à la demande : ref cit et a type de donnees 
  
  #' @param df_global table de donn?es   
  #' @param journal_table_ref table de donn?es de journal  
  #' @param type type of analyse, reference or citation  
  #'
  #' @return la table de donnees avec une colone en plus ref ou cit
  
  
  
  
  
  
  if(type=="ref"){# si type=ref 
    # si il y a des donnees wos dans la table de donnees on doit passe par les abreviation
    
    dom<-find_journal_domaine(journal_data = df_global$`refered journal`,journal_table_ref = journal_table_ref,issn = df_global$`refered issn`,source =df_global$source )
    
    df_global$refered_journal_domaine=dom[1,]
    df_global$refered_global_dicipline=dom[2,]
    df_global$refered_indice_pct_found=dom[3,]
    
  }else{#pour les citation il n'y a pas de patricularite dans le wos
    res<-find_journal_domaine(journal_data = df_global$`citing journal`,journal_table_ref = journal_table_ref,issn = df_global$`citing issn`,source =df_global$source )
    df_global$citing_journal_domaine=res[1,]
    df_global$citing_global_dicipline=res[2,]
    df_global$citing_indice_pct_found=res[3,]
  }
  return(df_global)  
}



global_merge_and_cal_interdis<-function(ads=NULL,arxiv=NULL,pumed=NULL,wos=NULL,journal_table_ref,table_dist,table_categ_gd,col_journal=c(NULL,NULL,NULL,NULL),type){
  #' Title fct de rassemblement qui permet de caluler l'interdiciplinarit?  
  #'avec les citations et reference des differente base. cette fonction est une fonction interne n'ayant pas pour but d'?tre appel? en dehor de l'appli
  #' @param ads matrice cit/ref venant d'ads 
  #' @param arxiv matrice cit/ref venant d'arxiv
  #' @param pumed matrice cit/ref venant d'pumed
  #' @param wos matrice cit/ref venant d'un fichier bibtext
  #' @param journal_table_ref table contenant l'information des journaux (abr?viasion, issn, full name )
  #' @param table_dist matrice de distances des cat?gorie wos 
  #' @param table_categ_gd matrice des grandes cat?gorie ost avec les cat?gorie wos 
  #' @param col_journal vecteur de 4, les colone de journal a utiliser pour les 4 matrice(1 par matrice). apr?viation ou nom plein
  #' @param type type de traitement cit/ref/all (les deux)
  #'
  #' @return list de de list resultat 
  
  #wos pumed arxiv et ads sont les matrice de citation ou reference 
  if(type=="all"){# qi on veux les reference et les citation 
    merge_data_ref<-merge_result_data_base(ads,arxiv,pumed,wos,type="ref",col_journal = col_journal)# on merge les dif?rente matrice 
    if(dim(merge_data_ref)[1]>0) {
      merge_data_ref<-combine_analyse_data(merge_data_ref,journal_table_ref,type="ref")# on recup?re leurs nom de journal pour les associ? au cat?gorie wos 
      res_matrice_ref=interdis_matrice_creation_and_calcul(data_gl = merge_data_ref,table_dist,table_categ_gd,type = "ref" ) # on  calcule l'interdiciplinarit? et les diff?rent indicateurs 
    }else {
      res_matrice_ref=NULL
      merge_data_ref=NULL
    }
    
    merge_data_cit<-merge_result_data_base(ads,arxiv,pumed,wos,type="cit",col_journal = col_journal)
    if(dim(merge_data_cit)[1]>0) {
      merge_data_cit<-combine_analyse_data(merge_data_cit,journal_table_ref,type="cit" )
      res_matrice_cit=interdis_matrice_creation_and_calcul(data_gl = merge_data_cit,table_dist,table_categ_gd,type = "cit" )
    }else {
      res_matrice_cit=NULL
      merge_data_cit=NULL
      
    }
    result=list(data_ref=merge_data_ref,data_cit=merge_data_cit,res_ref=res_matrice_ref,res_cit=res_matrice_cit)
  }else{
    
    merge_data<-merge_result_data_base(ads,arxiv,pumed,wos,type,col_journal = col_journal)
    if(dim(merge_data)[1]>0) {
      merge_data<-combine_analyse_data(merge_data,journal_table_ref,type)
      res_matrice=interdis_matrice_creation_and_calcul(data_gl = merge_data,table_dist,table_categ_gd,type=type )
    } else res_matrice=NULL
    
    result=list(data=merge_data,res=res_matrice)
  }
  
  return(result)
  
}

get_cit_link_arxiv=function(titre_arxiv){
  
  
  
  page_resp <- GET(paste0("https://scholar.google.com/scholar?q=",gsub("\n","",fixed = TRUE,gsub(" ","%20",titre_arxiv))))
  
  
  page_content <- httr::content(page_resp, as = "text")
  
  
  text_base <- strsplit(page_content, "\n")[[1]]
  
  #read_html(cite_link[[ar]])%>%html_nodes("body")%>%html_text()
  deb<-grep("Cite" ,text_base)
  part_int<- strsplit((text_base[deb]), "<",fixed = TRUE)
  
  
  deb<-grep("Cite" ,part_int[[1]])
  if(length(deb)>1){
    print("supp")
    # browse()
  }
  lien=part_int[[1]][deb]
  
  
  
  supdeb<-max(`attributes<-`(gregexpr("/",lien)[[1]],NULL))
  supfin<-max(`attributes<-`(gregexpr(">",lien)[[1]],NULL))
  
  
  
  lien_complet=paste0("https://scholar.google.com/",substr(lien,supdeb+1,supfin-2))
  return(lien_complet)
}
