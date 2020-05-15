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




res_wos$position_name=2
res_wos$sep=";"
res_wos$source="WOS"


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








voici_un_test<-extract_ref_wos(data_wos)

test=as.data.frame(voici_un_test,stringsAsFactors = FALSE)
#verifier les doublon 
#verifier les doublon 



###.old 11min




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