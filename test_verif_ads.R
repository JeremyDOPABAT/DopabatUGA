# path_data=choose.files(caption = "chosse your data file")# choisir le fichier concerner 
# au_name <- readline(prompt="Nom de Variable 'Nom_auteur': ")#authFullName_s
# ti_name <- readline(prompt="Nom de Variable 'Titre_publication': ")#en_title_s
# date_name <- readline(prompt="Nom de colonne 'date_publication': ")#defenseDate_s producedDate_s

ths<-read.csv(path_data, sep = ";",header = TRUE,stringsAsFactors = FALSE)
View(ths)
dim(ths)
if(sum(duplicated(ths[c(au_name,ti_name)]))>0) ths<-ths[-(which(duplicated(ths[c(au_name,ti_name)])==TRUE)),]
dim(ths)

token="SYZW1C1o9mDDfOWXUSh7Jrq0MMqvleL2is0pnTNQ"

source("functions_analyses.R")
library(plyr)

ths$position_name=1
# dim(res_data_nasa_ads$dataframe_citation_accept)
# 
# View(df)
value_same_min_ask=0.85
 value_same_min_accept=0.95
# 
pas=8

type="all"
source_name=""
data_pub=ths
ti_data<-data_pub[ti_name][[1]]

au_data<-data_pub[au_name][[1]]
position_name=rep(1,length(ti_data))
sep=rep(",",length(ti_data))


au_data<-data_pub[au_name][[1]]#auteur nom colonne 
ti_data<-data_pub[ti_name][[1]]#titre 






# 
# 
# 
# 
# 
