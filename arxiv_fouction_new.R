path_data=choose.files(caption = "chosse your data file")# choisir le fichier concerner 
au_name <- readline(prompt="Nom de Variable 'Nom_auteur': ")#authFullName_s
ti_name <- readline(prompt="Nom de Variable 'Titre_publication': ")#en_title_s
#date_name <- readline(prompt="Nom de colonne 'date_publication': ")#defenseDate_s producedDate_s
ths<-read.csv(path_data, sep = ";",header = TRUE,stringsAsFactors = FALSE)
dim(ths)
if(sum(duplicated(ths[c(au_name,ti_name)]))>0) ths<-ths[-(which(duplicated(ths[c(au_name,ti_name)])==TRUE)),]
dim(ths)

ths$sep=","
ths$position_name=1
value_same_min_ask=0.85
value_same_min_accept=0.95
pas=8
data_pub=ths
type="all"
source_name=""

result_arxiv=extraction_data_api_arxiv(data_pub=ths,ti_name=ti_name,au_name=au_name,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0.85,type = "cit",sep_vector_in_data = "sep",position_vector_in_data = "position_name")


result_arxiv$error_querry
dim(result_arxiv$res_publi_foundt)


#(((((((((((((((((((((((((((((((((((((((((((((((((((())))))))))))))))))))))))))))))))))))))))))))))))))))

