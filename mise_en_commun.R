path_data=choose.files(caption = "chosse your data file")# choisir le fichier concerner 
au_name <- readline(prompt="Nom de Variable 'Nom_auteur': ")#authFullName_s
ti_name <- readline(prompt="Nom de Variable 'Titre_publication': ")#en_title_s
date_name <- readline(prompt="Nom de colonne 'date_publication': ")#defenseDate_s producedDate_s
ths<-read.csv(path_data, sep = ";",header = TRUE,stringsAsFactors = FALSE)
dim(ths)
if(sum(duplicated(ths[c(au_name,ti_name)]))>0) ths<-ths[-(which(duplicated(ths[c(au_name,ti_name)])==TRUE)),]
dim(ths)

token="SYZW1C1o9mDDfOWXUSh7Jrq0MMqvleL2is0pnTNQ"

source("applitodeploy/global.R")
library(plyr)

ths$position_name=1
ths$sep=","

res_data_nasa_ads=extraction_data_api_nasa_ads(data_pub=ths,ti_name=ti_name,au_name=au_name,token=token,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0,type="all",sep_vector_in_data ="sep",position_vector_in_data = "position_name" )




dim(res_data_nasa_ads$error_querry_publi)
View(res_arxiv$res_publi_foundt)
View(res_to_save)


res_arxiv=extraction_data_api_arxiv(data_pub=ths,ti_name=ti_name,au_name=au_name,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0.85,type = "all",sep_vector_in_data ="sep",position_vector_in_data = "position_name")
res_pumed=extract_data_api_pumed(data_pub=ths,ti_name,au_name,pas=8,value_same_min_accept=0.85, value_same_min_ask=0.95,type="all",sep_vector_in_data ="sep",position_vector_in_data = "position_name")
dim(res_data_nasa_ads$dataframe_citation_accept)
dim(res_data_nasa_ads$dataframe_citation_accept)
dim(res_data_nasa_ads$dataframe_publi_found[(res_data_nasa_ads$dataframe_publi_found$check_title_pct<value_same_min_accept) &(res_data_nasa_ads$dataframe_publi_found$check_title_pct>=value_same_min_ask),])

View(as.data.frame(res_data_nasa_ads$dataframe_citation))

names(res_pumed$dataframe_ref_accept)
res_pumed$error_querry_publi
result$response$docs$bibcode

View(result$res_accept)

resdt=res_data_nasa_ads$dataframe_publi_found

ads_data=res_data_nasa_ads$dataframe_citation_accept
arxi_data=res_arxiv$res_citation_accept
pumed_data=res_pumed$dataframe_citation_accept
names(arxi_data)
dim(ads_data)
dim(arxi_data)

data_merge=as.data.frame(rbind.fill(ads_data,arxi_data),stringsAsFactors = FALSE)
test=as.data.frame(rbind.fill(data_merge,pumed_data),stringsAsFactors = FALSE)
#verifier les doublon 


dim(test)
names(test)

#____________________rep?r? les doulons ___________________________________________
dup<-duplicated2(test[,c("cited title","cited auth","citing title","citing auth")])

d1<-duplicated2(arxi_data[,c("cited title","cited auth","citing title","citing auth")])
d2<-duplicated2(pumed_data[,c("cited title","cited auth","citing title","citing auth")])
d3<-duplicated2(ads_data[,c("cited title","cited auth","citing title","citing auth")])

sum((d1))
sum((d2))
sum((d3))
# pour l'instant on les laisse, il faut rajouter la date 
view(ads_data[d3,])
View(duplicated2(ads_data[,c("cited title","cited auth","citing title","citing auth")]))
View(ads_data[ads_data$`cited identifier`=="2018NatMa..17..605Y",])














t=(duplicated2(res_cit[,c("bibcode")]))

res_cit[t,c("bibcode","titre")]  



#____________________________________________________________










path_journal="data/data_journal/table_categ_wos.csv"
journal_table_ref=read.csv(path_journal, sep = ";",header = TRUE,stringsAsFactors = FALSE)
journal_table_ref$Source_title<-gsub("\\s*\\([^\\)]+\\)","",journal_table_ref$Full.Journal.Title)
journal_table_ref$Source_title<-gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",journal_table_ref$Full.Journal.Title)
names(journal_table_ref)


journal_data=test$`citing journal`
#test$`citing journal`
names(journal_table_ref)



names(test)








dom<-find_journal_domaine(test$`cited journal`,test$`cited issn`,journal_table_ref = journal_table_ref)
dom_length<-sapply(1:length(dom), FUN=function(x) length(unlist(dom[x])))
table(dom_length)
unique(dom)

test$cited_journal_domaine=dom

dom2<-find_journal_domaine(test$`citing journal`,test$`citing issn`,test$`citing issn`,journal_table_ref)


test$citing_journal_domaine=dom2
names(test)
names(arxi_data)

is.na(test) <- test == "NULL"
View(test[c("cited_journal_domaine","cited subject","citing_journal_domaine","citing subject")])
tbc<-as.data.frame((table(unlist(test$cited_journal_domaine),unlist(test$citing_journal_domaine))),stringsAsFactors = FALSE)
names(tbc)<-c("cited", "citing", "freq")

View(tbc)

#travaille ref___________________________________________





df_global=test
type="ref"
df_global=test 
prescence_ref_wos=TRUE

combine_analyse_data<-function(df_global,journal_table_ref,prescence_ref_wos,type){
  if(type=="ref"){
    if(prescence_ref_wos==TRUE){
      dom_wos=NULL
      dom_not_wos=NULL
      df_global=df_global[order(df_global$source),]
      df_global_wos=df_global[df_global$source=="WOS",]
      
      if(dim(df_global_wos)[1]!=0) dom_wos<-find_journal_domaine(journal_data = df_global_wos$`refered journal`,journal_table_ref = journal_table_ref,issn = test$`refered issn`,source ="JCR.Abbreviated.Title" )
      df_global_not_wos=df_global[df_global$source!="WOS",]
      if(dim(df_global_not_wos)[1]!=0) dom_not_wos<-find_journal_domaine(journal_data = df_global_not_wos$`refered journal`,journal_table_ref = journal_table_ref,issn = test$`refered issn` )
      dom=c(dom_not_wos,dom_wos)
      
    }else {
      dom<-find_journal_domaine(journal_data = df_global$`refered journal`,journal_table_ref = journal_table_ref,issn = test$`refered issn` )
    }
    df_global$refered_journal_domaine=dom
  }else{#pour les citation il n'y a pas de patricularité dans le wos
    dom<-find_journal_domaine(journal_data = df_global$`citing journal`,journal_table_ref = journal_table_ref,issn = test$`refered issn` )
    df_global$refered_journal_domaine=dom
  }
  return(df_global)  
}
test<-combine_analyse_data(test,journal_table_ref,prescence_ref=TRUE,type="ref" )
# #dom<-find_journal_domaine(journal_data = test$`refering journal`,journal_table_ref = journal_table_ref,issn = test$`refering issn`)
# #.old 11min



# dom<-find_journal_domaine(journal_data =test$`refered journal`[[i]],journal_table_ref = journal_table_ref,issn = test$`refered issn`,source ="JCR.Abbreviated.Title" )


  

# table(dom_length)
# unique(dom2)
#dom_length<-sapply(1:length(dom2), FUN=function(x) length(unlist(dom2[x])))




#----------------------------------------------------------------------------------





#View(test)

















nb_categ=unlist(test$refered_journal_domaine)
list_categ<-names(table(nb_categ))

matrice_prop=as.data.frame(matrix(0,ncol=length(list_categ)))
names(matrice_prop)=c(list_categ)
precedent=""
for(i in 1:dim(test)[1]){
  if(!is.null(test$refered_journal_domaine[[i]])){
    if(precedent!=test$`refering identifier`[[i]]){
      line=rep(0,length(list_categ))
      matrice_prop=rbind(matrice_prop,line)
    }
    col_ind=match(unique(test$refered_journal_domaine[[i]]),names(matrice_prop))
    
    col_ind=col_ind[!is.na(col_ind)]
    matrice_prop[dim(matrice_prop)[1],col_ind]= matrice_prop[dim(matrice_prop)[1],col_ind]+1
    precedent=test$`refering identifier`[[i]]
  }
}
matrice_prop<-matrice_prop[-1,]
matrice_prop<-type.convert(matrice_prop)
sumcol=colSums(matrice_prop)
matrice_prop=rbind(matrice_prop,sumcol)
sumrow=rowSums(matrice_prop)

table_dist<-read.table(file="data/data_journal/category_similarity_matrix.txt",header = TRUE,sep = " ",dec ="." )

dim(matrice_prop)



interdis_calcul<-function(matrice_prop, table_dist){
  dia=sapply(1:dim(matrice_prop)[1],FUN = function(x){
    
    cal=0
    matrice_prop[x,]=matrice_prop[x,]/sumrow[[x]]
    lien<-list()
    for(i in 1:dim(matrice_prop)[2]){
      
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
  }
  )

  (DD=unlist(dia["valeur",][length(dia["valeur",])]))

  (ID=mean(unlist(dia["valeur",][-length(dia["valeur",])])))


  (MD=DD-ID)
  matrice_prop=cbind(matrice_prop,c(unique(test$`refering identifier`),"Total"))
  
  resultat<-list(dia=dia["valeur",],md=MD,id=ID,dd=DD,prop=matrice_prop,couple=dia["couple",])
  return(resultat)
}
#rajouter id dans matrice pop 






resultat=interdis_calcul(matrice_prop, table_dist)
resultat$couple[[1]]










#.premisse du graph ________________________________________________




 
wei<-resultat$couple
  
  
  
ind1=which(is.na(test$refering_journal_domaine))
ind2=which(is.na(test$refered_journal_domaine))
  
ind_tot=unique(c(ind1,ind2))  
data_ss_na=test[-ind_tot,]
part1=unique(unlist(data_ss_na$refering_journal_domaine))
part2=unique(unlist(data_ss_na$refered_journal_domaine))


to=unlist(data_ss_na$refering_journal_domaine)
from=unlist(data_ss_na$refered_journal_domaine)
term=unique(c(from,to))
wei=table(c(from,to))
# for(i in(1:length(keywords))){
#   if(length(keywords[[i]])>1){
#     for(j in(1:length(keywords[[i]]))){
#       if(length(keywords[[i]])>j){
#         add_from=rep(keywords[[i]][j],length(keywords[[i]])-j)
#         add_to=c(keywords[[i]][(j+1):length(keywords[[i]])])
#         from=c(from,add_from)
#         to=c(to,add_to)
#       }
#     }
#   }
# }

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










c_igraph <- graph_from_data_frame(d = data_edge[,c(1,2)], vertices = node_list, directed = TRUE)%>%
                                    set_edge_attr("width", value =sqrt(data_edge$weight))%>%
                                    set_vertex_attr("weight", value =sqrt(wei))%>%
                                    set_vertex_attr("size", value =sqrt(wei))%>%
                                    set_vertex_attr("label.cex", value =1)
V(c_igraph)$group="node(keyword)"


visIgraph(c_igraph, idToLabel = TRUE, layout = "layout_nicely",
          physics = FALSE, smooth = FALSE, type = "square",
          randomSeed = NULL)
V(c_igraph)$group="node(keyword)"
#if(length(which(data_edge["to"]==data_edge["from"])!=0)) data_edge<-data_edge[-(which(data_edge["to"]==data_edge["from"])),]

# t_read=read_lines(path_tester,skip = 1)
# head(t_read,10)
# tap=read.table(path_tester, header = TRUE, sep = "\t", dec = ".")
# t_split=strsplit(t_read,"[\\]")
# write.csv(tap,"tableauUGA.CSV")
# dim(tap)

#citing subject fait plant? 

# tabPanel("ArXiv",actionButton("arxiv_ref_accept", "Show references"),
#          actionButton("arxiv_cit_accept", "Show citations"),
#          dataTableOutput("table_data_ref2")),
# tabPanel("Pubmed",actionButton("pubmed_ref_accept", "Show references"),
#          actionButton("pubmed_cit_accept", "Show citations"),
#          dataTableOutput("table_data_ref3"))
# 


# actionButton("ads_ref_accept", "Show references"),
# actionButton("ads_cit_accept", "Show citations"),
# dataTableOutput("table_data_ref")
# 
# observeEvent(input$contain_ref,{
#     if(input$contain_ref==TRUE) reactive_values$show_ref <- TRUE else reactive_values$show_ref <- FALSE
# })
# 
# observeEvent(input$contain_cit,{
#   if(input$contain_cit==TRUE) reactive_values$show_cit <- TRUE else reactive_values$show_cit <- FALSE
# })
# 


 

domine_test<-find_journal_domaine_v2(test$`cited journal`,test$`cited issn`,test$`cited essn`,journal_table_ref)
 
  
 
 
  
  
  
  
  
  
  #traiiter le pluri danns le if et le esle !!!
  
  


