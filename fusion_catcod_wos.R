path_journal="data/DOPABAT_Titres_AvecWC.csv"
journal_table_ref=read.csv(path_journal, sep = ";",header = TRUE,stringsAsFactors = FALSE)
catco=read.csv("data/data_journal/categ_wos.csv", sep = ";",header = TRUE,stringsAsFactors = FALSE)
dim(catco)
if(sum(duplicated(catco[c("catcod","Category.Name")]))>0) catco<-catco[-(which(duplicated(catco[c("catcod","Category.Name")])==TRUE)),]
dim(catco)



col_abrv=sapply(1:dim(journal_table_ref)[1],FUN = function(x){
  tp=grep(paste0("^",journal_table_ref$Subject.Category[x],"$"),catco$Category.Name)  
  
  return(catco$catcod[tp])
  
})







length((col_abrv))




length_col=sapply(1:length(col_abrv),FUN=function(x) length(col_abrv[[x]]))
table(length_col)








col_abrv=sapply(1:length(col_abrv),FUN = function(x) paste(col_abrv[[x]],collapse = ";"))


table_merge=cbind(journal_table_ref,col_abrv)
dim(table_merge)
names(table_merge)=c("JCR.Abbreviated.Title", "Full.Journal.Title","Issn","Subject.Category","Discipline.Scientifique.OST", "Origine","Abreviation" )                

write.csv2(table_merge, file = "data/data_journal/table_categ_wos.csv")






table_categ<-read.csv("table_categ_wos.csv",header = TRUE,sep = ";")
dim(table_categ)
grand_dis<-read.csv("grandes_diciplines.csv",header = TRUE,sep = ";")
dim(grand_dis)




col_abrv=sapply(1:dim(grand_dis)[1],FUN = function(x){
  tp=grep(paste0("^",grand_dis$Discipline[x],"$"),table_categ$Discipline.Scientifique.OST)  
  
  return(catco$catcod[tp])
  
})
