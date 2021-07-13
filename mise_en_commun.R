path_data=choose.files(caption = "chosse your data file")# choisir le fichier concerner 
au_name <- readline(prompt="Nom de Variable 'Nom_auteur': ")#authFullName_s
ti_name <- readline(prompt="Nom de Variable 'Titre_publication': ")#en_title_s
date_name <- readline(prompt="Nom de colonne 'date_publication': ")#defenseDate_s producedDate_s
ths<-read.csv(path_data, sep = ";",header = TRUE,stringsAsFactors = FALSE)






names(ths)

test=ths[,-which(names(ths)=="en_title_s"|names(ths)=="authFullName_s")]

dim(test)



dim(ths)
if(sum(duplicated(ths[c(au_name,ti_name)]))>0) ths<-ths[-(which(duplicated(ths[c(au_name,ti_name)])==TRUE)),]
dim(ths)



test=data.table::fread(path_data,
                       header = TRUE,
                       sep = ";",
                       quote = '"')

dim(test)

token="SYZW1C1o9mDDfOWXUSh7Jrq0MMqvleL2is0pnTNQ"

source("applitodeploy/global.R")
library(plyr)

ths$position_name=1
ths$sep=","
ths$source="CSV"




View(res_arxiv$res_publi_foundt)
View(res_to_save)

res_data_nasa_ads=extraction_data_api_nasa_ads(data_pub=ths,ti_name=ti_name,au_name=au_name,token=token,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0.85,type="all",sep_vector_in_data ="sep",position_vector_in_data = "position_name",source_name = "source" )


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




# #dom<-find_journal_domaine(journal_data = test$`refering journal`,journal_table_ref = journal_table_ref,issn = test$`refering issn`)
# #.old 11min



# dom<-find_journal_domaine(journal_data =test$`refered journal`[[i]],journal_table_ref = journal_table_ref,issn = test$`refered issn`,source ="JCR.Abbreviated.Title" )


  

# table(dom_length)
# unique(dom2)
#dom_length<-sapply(1:length(dom2), FUN=function(x) length(unlist(dom2[x])))




#----------------------------------------------------------------------------------





#View(test)


















path_journal="applitodeploy/data/table_categ_wos.csv"
journal_table_ref=read.csv("applitodeploy/data/table_categ_wos.csv", sep = ";",header = TRUE,stringsAsFactors = FALSE)
journal_table_ref$Full.Journal.Title<-gsub("\\s*\\([^\\)]+\\)","",journal_table_ref$Full.Journal.Title)
journal_table_ref$Full.Journal.Title<-gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",journal_table_ref$Full.Journal.Title)
names(journal_table_ref)

path=choose.files(caption = "chosse your data file")
data_wos<-convert2df(readFiles(path),dbsource = "wos",format = "bibtex")



voici_un_test<-extract_ref_wos(data_wos)

View(head(voici_un_test))
path=choose.files(caption = "chosse your data file")
data_wos<-convert2df(readFiles(path),dbsource = "wos",format = "bibtex")
voici_un_test2<-extract_ref_wos(data_wos)
test=rbind(voici_un_test,voici_un_test2)
library(bib2df)

#dom_wos<-find_journal_domaine(journal_data = df_global_wos$`refered journal`,journal_table_ref = journal_table_ref,issn =  df_global_wos$`refered issn`,source ="JCR.Abbreviated.Title" )
path <- system.file("extdata", "bib2df_testfile_3.bib", package = "bib2df")
bib <- bib2df(path)
View(bib)
View(data_wos)
str(bib)

# Read from .bib file and separate authors' and editors' names:
bib <- bib2df(path, separate_names = TRUE)
str(bib)

# test<-combine_analyse_data(test,journal_table_ref,type="ref" )
# names(test)
# 
# 
# res_matrice=interdis_matrice_creation_and_calcul(data_gl = test,table_dist = table_dist,table_categ_gd = table_categ_gd,type = "ref" )
# 
# 
# 
# path_gd="data/data_journal/categ_wos.csv"
# 
# table_categ_gd=read.csv(file=path_gd,header = TRUE,stringsAsFactors = FALSE,sep = ";")
# 
# 
# 
# 
# 

# dim(res_data_nasa_ads$dataframe_ref_accept)
# 





error=tryCatch({
  matrice_res<-global_merge_and_cal_interdis(ads=NULL,arxiv=NULL,pumed=NULL,wos=test,journal_table_ref = journal_table_ref,table_dist = table_dist,table_categ_gd = table_categ_gd,col_journal=c("Full.Journal.Title","Full.Journal.Title","Full.Journal.Title","JCR.Abbreviated.Title"),type="ref")
  
  
},
  
error=function(cond){ 
    print("error in the pack")
    return(NULL)
  })



merge_data<-merge_result_data_base(ads,arxiv,pumed,wos,col_journal=c("Full.Journal.Title",NULL,NULL,NULL) ,type = type)







merge_data<-combine_analyse_data(merge_data,journal_table_ref,type )












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
#
  
  
  
  
  
  
  


