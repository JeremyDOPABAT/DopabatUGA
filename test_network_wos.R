
#csv_____________________________
path_data=choose.files(caption = "chosse your data file")# choisir le fichier concerner 
au_name <- readline(prompt="Nom de Variable 'Nom_auteur': ")#authFullName_s
ti_name <- readline(prompt="Nom de Variable 'Titre_publication': ")#en_title_s
date_name <- readline(prompt="Nom de colonne 'date_publication': ")#defenseDate_s producedDate_s
ths<-read.csv(path_data, sep = ";",header = TRUE,stringsAsFactors = FALSE)
dim(ths)
if(sum(duplicated(ths[c(au_name,ti_name)]))>0) ths<-ths[-(which(duplicated(ths[c(au_name,ti_name)])==TRUE)),]
dim(ths)

df_global=ths[,c(ti_name,au_name,"en_keyword_s","domainAllCode_s",date_name)]
names(df_global)<-c('titre','auteur','keywords','domain','date')

#wos___________________________________________________________
path=choose.files(caption = "chosse your data file")
data_wos<-convert2df(path,dbsource = "wos",format = "bibtex")
data_wos=data_wos[,c("TI","AU","DE","SC","PY")]
df_global=data_wos
names(df_global)<-c('titre','auteur','keywords','domain','date')
df_global["sep"]=";"
df_global["position_name"]=1
df_global["id_arxiv"]=NA

col_date<-parse_date_time(x = df_global[["date"]],
                          orders = fmts,
                          locale =  Sys.getlocale(category = "LC_TIME"))

df_global["date"]=col_date




if(sum(duplicated( df_global[c("titre","auteur")]))>0)  df_global<- df_global[-(which(duplicated( df_global[c("titre","auteur")])==TRUE)),]


 df_global[["keywords"]]=gsub(";",",", df_global[["keywords"]])
 df_global[["domain"]]=gsub(";",",", df_global[["domain"]])


keywords<- df_global[["keywords"]]




#primary_domaine<- df_csv["primaryDomain_s"]
domainall<- df_global[["domain"]]

dict_lang="en_GB"#on se fixe d'abord en anglais , diff?rent selon dico
lang<-"en"






year<-as.double(format(x = df_global[["date"]],"%Y"))


 df_global[["year"]]<-as.factor(year)





keywords_tok<-strsplit(keywords,",",fixed=TRUE)# mise en forme des mot clef , tokanisation

# lematisation
keywords_lem<-sapply(1:length(keywords_tok),FUN =function(x,l=lang) {stemDocument(keywords_tok[[x]],language = l)})
# tous se qui suis va permete de completer la lematisation par la version la plus courte du mot entier , cela evite les faute d'ortographe dans les graphiques
c1<-unique(unlist(keywords_tok))

c2<-sapply(1:length(c1)[1],FUN =function(x,l=lang) {stemDocument(c1[x],language = l)})


if(is_empty(which(c2==""))==FALSE){
  c2<-c2[-(which(c2==""))]
}

if(is_empty(which(c1==""))==FALSE){
  c1<-c1[-(which(c1==""))]
}

table_lema<-as.data.frame(cbind(c1,c2),stringsAsFactors = FALSE)

keywords_lem_complet<-keywords_lem
ind=which(keywords_lem_complet=="")
if(length(ind)!=0) keywords_lem_complet=keywords_lem_complet[-ind]


for(i in (1:length(keywords_lem_complet))){
  mot=keywords_lem_complet[[i]]
  temp=c()
  for(k in 1:length(mot)){
    trad=""
    if(mot[k]!=""){
      ind=which(mot[k]==table_lema[2])
      trad=table_lema[ind,1]
      
      if(length(trad)>1){
        trad<-trad[nchar(trad)==min(nchar(trad))][1]
      }
    }
    temp=c(temp,trad)
  }
  keywords_lem_complet[[i]]<-temp
  
  
  
}

make_network_graph(keywords_lem_complet,publication_date = year,top_number=0,interval_year=0,sup_ones=FALSE,root_weight = FALSE,domain = FALSE)

keywords=keywords_lem_complet
publication_date=year
sup_ones=TRUE
domain=FALSE
root_weight=FALSE
top_number=1
interval_year=0

