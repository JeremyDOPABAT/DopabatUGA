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
library(bibtex)
library(bib2df)

#------------------------------------------importation d'un fichier wos _________________


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

res_data_nasa_ads=extraction_data_api_nasa_ads(data_pub=res_wos,ti_name=ti_name,au_name=au_name,token=token,pas=8,value_same_min_accept=0.95,value_same_min_ask = 0,type="all",sep_vector_in_data ="sep",position_vector_in_data = "position_name",source_name = 'source' )
res_arxiv=extraction_data_api_arxiv(data_pub=ths,ti_name=ti_name,au_name=au_name,pas=1,value_same_min_accept=0.95,value_same_min_ask = 0.85,type = "all",sep_vector_in_data ="sep",position_vector_in_data = "position_name")

res_arxiv_wos=extraction_data_api_arxiv(data_pub=res_wos,ti_name=ti_name,au_name=au_name,pas=1,value_same_min_accept=0.95,value_same_min_ask = 0.85,type = "all",sep_vector_in_data ="sep",position_vector_in_data = "position_name",source_name = 'source')

View(res_arxiv$res_publi_foundt)


View(res_arxiv_wos$res_publi_foundt)



res_pumed=extract_data_api_pumed(data_pub=res_wos,ti_name,au_name,pas=8,value_same_min_accept=0.85, value_same_min_ask=0.95,type="all",sep_vector_in_data ="sep",position_vector_in_data = "position_name",source_name = 'source')



# test<-pdf_extract_data(path_folder)
# names(test)<-c('titre','auteur','keywords','domain','date')
# tb<-rbind(test[,c('titre','auteur','keywords','domain')],res_wos[,c('titre','auteur','keywords','domain')])
# 
# 
# View(head(data_wos))

#______________________________extract ref 






data_wos$AU[1]






test_T=bib2df::bib2df(path, separate_names = TRUE)
View(test_T)



View(test)




tryCatch({
  test_F=bib2df::bib2df(path, separate_names = FALSE)
  
    

},

error=function(cond){
  return(-400)
  print("error")
},
warning=function(cond){
  return(-400)
  print("war")
})







View(test_F)


View(data_wos)


test_F=bib2df::bib2df(path, separate_names = FALSE)
View(test)

test3=df_flatten(test)
ind=which(is.na(test[[i]]))
if(length(ind)>0 )
  test[ind,i]="NULL"#na ne peu pas ?tre afficher dans la table donc on le remplace par "null" 


res_wos$position_name=2
res_wos$sep=";"
res_wos$source="WOS"


View(test3)




# data tble 
#fonction qui permet le bonne affichage des tables dans l'interface  

#date_name="date"


  # ind=which(is.null(res_f[[i]]))
  # if(length(ind)>0 ) res_f[ind,i]=NA
#----------------------------------------------paralel
path=choose.files(caption = "chosse your data file")
data_wos <-convert2df(path,dbsource = "wos",format = "bibtex")
readbib=do_read_bib(path)
df <- bib2df(path)
dim(df)

test=conforme_bibtext(test_F,data_base = "BIB")

voici_un_test<-extract_ref_wos(data_wos)

test=as.data.frame(voici_un_test,stringsAsFactors = FALSE)

data_bib <-(bib2df::bib2df(path, separate_names = FALSE))

data_wos <-convert2df(path,format = "bibtex")
dim(data_wos)


data_bib$TITLE[dim(data_bib)[1]]


View(data_bib$TITLE)
duplicated(data_bib$TITLE)

try_table=table(data_bib$TITLE)
max(try_table)
not_corret=which(try_table!=1)
length(not_corret)
not_corret[1]

sum(try_table[not_corret])

View(not_corret)
dim(test2)
View(test2)
which(is.null(data_bib$AUTHOR))

(dim(data_bib)[1]-length(which(duplicated(data_bib$DOI)==TRUE )))

data_bib_new<-data_bib[-which(duplicated(data_bib$DOI)==TRUE ),]

dim(data_bib_new)[1]-length(which(duplicated(data_bib_new$AUTHOR)==TRUE))






















test=(bib2df::bib2df(path, separate_names = TRUE))
res_f=test

class(test$TITLE)
test2=df_flatten(test)

test$TITLE[[1]]
test2$TITLE[[2]]

for(i in names(res_f)){# on parcourt les colonne 
  print(i)
  ind=which(is.null(res_f[[i]]))
  if(length(ind)>0 ) res_f[ind,i]=NA
  ind=which(is.na(res_f[[i]]))
  if(length(ind)>0 )res_f[ind,i]="NULL"#na ne peu pas ?tre afficher dans la table donc on le remplace par "null" 
  if(class(res_f[[i]])!="character" && class(res_f[[i]])!="numeric" ){
    if(length(unlist(res_f[[i]]))>length(res_f[[i]])){
      if(tolower(i)=="author"){
        for(j in 1:dim(res_f)[1]){
          res_f$AUTHOR[[j]]=res_f$AUTHOR[[j]]$full_name
        }
      }else {
        res_f[,i]=as.character(res_f[,i])#sapply(1:length(res_f[j,i]),FUN = function(x) paste(res_f[j,i][[x]],collapse = ";"))# si il y a plusieur element sur une m?me ligne
      }
    }
  }
}
  
class(res_f[[i]])
  
one_try=sapply(1:length(res_f$AUTHOR),FUN = function(x) gsub("[}{}\\]", "", res_f$AUTHOR[[x]]))



  
  
length(res_f$AUTHOR)

View(res_f)
test_with_no_na=test[ , sapply(test, function(x) all(is.na(x)) ) ] <- NULL
View(test_with_no_na)
#verifier les doublon 
#verifier les doublon 



###.old 11min













path2=choose.files(caption = "chosse your data file")



test_F=bib2df::bib2df(path2, separate_names = FALSE)




(test1=grep(pattern = "^TITLE*",names(test_F)))
(test2=grep(pattern = "^KEYWORDS*",names(test_F)))
(test3=grep(pattern = "^AUTHOR*",names(test_F)))
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