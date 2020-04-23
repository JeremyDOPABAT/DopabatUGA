#api talker with wos 
#support http://support.clarivate.com.
#docummentation for wos servives 
#http://help.incites.clarivate.com/wosWebServicesExpanded/WebServicesExpandedOverviewGroup/Introduction.html
#Last names containing a space should be searched with and without the space to ensure that all relevant records are returned

#library(wosr)
#ls("package:wosr")
#sid<-auth(username = "UGrenoble_HG",password = "Welcome2wos!") # not each time


# uts <- c("WOS:000362312600021", "WOS:000439855300030", "WOS:000294946900020")
# pull_cited_refs(uts, sid)


#library(wosr)
library(bibliometrix)
library(plyr)


#------------------------------------------importation d'un fichier wos _________________

table(data_wos$PY)
max(data_wos$PY,na.rm = TRUE)-min(data_wos$PY,na.rm = TRUE)!=0
#data_wos

source("applitodeploy/global.R")













#AB = abstract
#ti=titre
#Ar article number
#AU autheur 
#keyword=DE
#SC reasearch area (domain)
#SN issn
#PU journal 
#bibtag()
path=choose.files(caption = "chosse your data file")
data_wos<-convert2df(readFiles(path),dbsource = "wos",format = "bibtex")

res_wos=data_wos[,c("TI","AU","DE","SC","PY")]
names(res_wos)<-c('title','author','keywords','domain','date')
res_wos$idw=paste0("_",1:dim(res_wos)[1],"_")



res_wos$position_name=2
res_wos$sep=";"
res_wos$source="WOS"


res_data_nasa_ads=extraction_data_api_nasa_ads(data_pub=res_wos,ti_name=ti_name,au_name=au_name,token=token,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0,type="all",sep_vector_in_data ="sep",position_vector_in_data = "position_name",source_name = 'source' )
res_arxiv=extraction_data_api_arxiv(data_pub=ths,ti_name=ti_name,au_name=au_name,pas=1,value_same_min_accept=0.95,value_same_min_ask = 0.85,type = "all",sep_vector_in_data ="sep",position_vector_in_data = "position_name")

res_arxiv_wos=extraction_data_api_arxiv(data_pub=res_wos,ti_name=ti_name,au_name=au_name,pas=1,value_same_min_accept=0.95,value_same_min_ask = 0.85,type = "all",sep_vector_in_data ="sep",position_vector_in_data = "position_name",source_name = 'source')

View(res_arxiv$res_publi_foundt)


View(res_arxiv_wos$res_publi_foundt)



res_pumed=extract_data_api_pumed(data_pub=res_wos,ti_name,au_name,pas=8,value_same_min_accept=0.85, value_same_min_ask=0.95,type="all",sep_vector_in_data ="sep",position_vector_in_data = "position_name",source_name = 'source')



test<-pdf_extract_data(path_folder)
names(test)<-c('titre','auteur','keywords','domain','date')
tb<-rbind(test[,c('titre','auteur','keywords','domain')],res_wos[,c('titre','auteur','keywords','domain')])


View(head(data_wos))

#______________________________extract ref 


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
   return(list(id,auth,year,journ))
})   
res<-rbind(res, t(temp))
}
res_ref<-as.data.frame(res)
names(res_ref)=c("id","author","year", "journ")



#View(res_ref)


ind_id<-sapply(res_ref$id,function(x){
   return(grep(x,res_wos$idw,fixed = TRUE))
})


table(ind_id)
#pas de domaine dans les citation 




res_ref_final=cbind(res_wos[unlist(ind_id),],res_ref)
res_ref_final<-res_ref_final[,-which(names(res_ref_final)=="keywords")]
res_ref_final<-res_ref_final[,-which(names(res_ref_final)=="position_name")]
res_ref_final<-res_ref_final[,-which(names(res_ref_final)=="sep")]
res_ref_final<-res_ref_final[,-which(names(res_ref_final)=="source")]

#length(names(res_ref_final))
names(res_ref_final)
names(res_ref_final)<- c("refering title",'refering auth',"refering domaine","refering date","refering identifier","refered identifier","refered auth","refered date","refered journal" )
names(res_ref_final)


source("functions_analyses.R")
library(plyr)




test=as.data.frame(res_ref_final,stringsAsFactors = FALSE)
#verifier les doublon 
#verifier les doublon 



###.old 11min




journal_data=test$`refered journal`

path_journal="data/journal_classification.csv"
journal_table_ref=read.csv(path_journal, sep = ";",header = TRUE,stringsAsFactors = FALSE)
journal_table_ref$Source_title<-gsub("\\s*\\([^\\)]+\\)","",journal_table_ref$Source_title)
journal_table_ref$Source_title<-gsub("[(#$%*,.:;<=>@^_`{|}~.)]","",journal_table_ref$Source_title)



domp1<-find_journal_domaine(test$`refered journal`,NULL,NULL,journal_table_ref[1:(dim(journal_table_ref)[1]/2),])
dom_length<-sapply(1:length(domp1), FUN=function(x) length(unlist(domp1[x])))
table(dom_length)
ind=which(dom_length==0)



domp2<-find_journal_domaine(test$`refered journal`[ind],NULL,NULL,journal_table_ref[(dim(journal_table_ref)[1]/2):dim(journal_table_ref)[1],])


dom_length<-sapply(1:length(domp2), FUN=function(x) length(unlist(domp2[x])))
table(dom_length)
unique(dom2)



domp<-find_journal_domaine(test$`refered journal`,NULL,NULL,journal_table_ref)
dom_length<-sapply(1:length(domp), FUN=function(x) length(unlist(domp[x])))
table(dom_length)
unique(dom2)


#Cette fonction permet de retrouver le domaine avec le nom du journal

#input
#journal_data : vecteur nom de journaux 
#issn vecteur contenant des issn,
#essn veteur essn
#journal_table_ref table de jounaux 










test$refered_journal_domaine=dom2
names(test)
names(arxi_data)
# # Save your WoS API username and password in environment variables
# Sys.setenv(WOS_USERNAME = "UGrenoble_HG", WOS_PASSWORD = "Welcome2wos")
# ind=which(is.na(res_ref$V4))
# length(ind)
# # Get session ID
# sid<-auth(username = "UGrenoble_HG",password = "Welcome2wos!")
# 
# # Query WoS to see how many results match your query
# #> Matching records: 548
# path_data=choose.files(caption = "chosse your data file")# choisir le fichier concerner 
# au_name <- readline(prompt="Nom de colonne 'Nom_auteur': ")#authFullName_s
# ti_name <- readline(prompt="Nom de colonne 'Titre_publication': ")#en_title_s
# date_name <- readline(prompt="Nom de colonne 'date_publication': ")#defenseDate_s
# ths<-read.csv(path_data, sep = ";",header = TRUE,stringsAsFactors = FALSE)
# 
# au_ths<-ths[au_name][[1]]
# 
# date<- strftime(strptime(c(ths[[date_name]]), "%d/%m/%Y"),"%d-%m-%Y")
# date2<-as.Date(date,format = "%d-%m-%Y")#101 cat?gories
# 
# year<-as.double(format(date2,"%Y"))
# 
# au_sep=strsplit(au_ths," ")
# 
# 
# au_ths=sapply(1:length(au_sep), FUN=function(x){
#   res=paste0(au_sep[[x]][2],", ",au_sep[[x]][1])
#   return(res)
# })
# 
# 
# query_wos("", sid = sid)
# 
# query <- "UT = WOS:000079530800003"
# total_res=query_wos(query,editions = c("SCI", "ISTP", "ISSHP",
#                              "IC", "CCR"), sid = sid)
# 
# 
# 
# res_pull=pull_wos(query, editions = c("SCI", "ISTP", "ISSHP",
#                              "IC", "CCR"),sid = sid)
# tite=res_pull$publication$title
# cit_numres_pull$publication$tot_cites
# 
# 
# sid<-auth(username = "UGrenoble_HG",password = "Welcome2wos!")
# query <- "PU = WOS:000079530800003"
# total_res=query_wos(query,editions = c("SCI", "ISTP", "ISSHP",
#                                        "IC", "CCR"), sid = sid)
# 
# 
# 
# res_pull=pull_wos(query, editions = c("SCI", "ISTP", "ISSHP",
#                                      "IC", "CCR"),sid = sid)
# 
# 
# res_pull$publication$abstract
# res_pull_cited_ref=pull_cited_refs("WOS:000079530800003",sid=sid)
# res_pull_cited_ref$doc_id #cit?  ref pas ce quon demande 
# 
# res_pull$grant
# 
# 
# 
# 
# #----------------------test------------------------------
# wos_uid="WOS:000270372400005"
# ditions = c("SCI", "ISTP", "ISSHP",
#             "IC", "CCR")
# 
# 
# 
# 
# body <- paste0('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" 
#    xmlns:woksearch="http://woksearch.v3.wokmws.thomsonreuters.com">
#    <soapenv:Header/>
#    <soapenv:Body>
#       <woksearch:citingArticles>
#          <databaseId>WOS</databaseId> 
#          <uid>',wos_uid,'</uid>  
#          <editions>
#             <collection>WOS</collection>
#             <edition>ISTP</edition>
#          </editions>
#          <editions>
#             <collection>WOS</collection>
#             <edition>CCR</edition>
#          </editions>
#          <editions>
#             <collection>WOS</collection>
#             <edition>ISSHP</edition>
#          </editions>
#          <editions>
#             <collection>WOS</collection>
#             <edition>IC</edition>
#          </editions>
#           <editions>
#             <collection>WOS</collection>
#             <edition>SCI</edition>
#          </editions>
#          <timeSpan>
#             <begin>1899-01-01</begin>
#             <end>2009-12-31</end>
#          </timeSpan>
#          <queryLanguage>en</queryLanguage>
#          <retrieveParameters>
#             <firstRecord>1</firstRecord>
#             <count>100</count>
#             <option>
#                <key>RecordIDs</key>
#                <value>On</value>
#             </option>
#             <option>            
#                <key>targetNamespace</key>
#                <value>http://scientific.thomsonreuters.com/schema/wok5.4/public/FullRecord</value>
#             </option>           
#         </retrieveParameters>
#       </woksearch:citingArticles>
#    </soapenv:Body>
# </soapenv:Envelope>')
# 
# 
# response <- httr::POST("http://search.webofknowledge.com/esti/wokmws/ws/WokSearch", 
#                        body = body, httr::add_headers(cookie = sprintf("SID=%s", 
#                                                                        sid)))
# 
# 
# 
# response$status_code
# 
# response$headers
# 
# 
# 
# result <- httr::content(response, 'text',encoding = "ISO-8859-1")
# xml_data<-xmlToList(result)
# xml_data$Body
# (xml_data$Body$citingArticlesResponse$return$queryId)
# 
# xmlToDataFrame(xml_data$Body$citingArticlesResponse$return$queryId)
# xmlToDataFrame(xml_data$Body$citingArticlesResponse$return$parent)
# xml_data$Body$citingArticlesResponse$return$optionValue$label
# xml_data$Body$citingArticlesResponse$return$optionValue$value
# test=xmlToDataFrame(xml_data$Body$citingArticlesResponse$return$records)
# 
# test$static_data
# 
# xml_data$Body$citingArticlesResponse$return$recordsFound



## Numeric to integer
class(rivers)
x <- type.convert(rivers)
class(x)

## Convert many columns
auto <- type.convert(mtcars)
str(mtcars)
str(auto)

## Convert matrix
phones <- type.convert(WorldPhones)
storage.mode(WorldPhones)
storage.mode(phones)

## Factor or character
chr <- c("A", "B", "B", "A")
fac <- factor(c("A", "B", "B", "A"))
type.convert(chr)               # -> factor
type.convert(fac)               # -> factor
type.convert(chr, as.is = TRUE) # -> character
type.convert(fac, as.is = TRUE) # -> character


#                