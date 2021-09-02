#parallelisation ------------------------



#import des données 

find_journal_domaine<-function(journal_data,journal_table_ref,issn="",essn="",source=""){
  #Cette fonction permet de retrouver le domaine avec le nom du journal
  
  #input
  #journal_data : vecteur nom de journaux 
  #issn vecteur contenant des issn,
  #essn veteur essn
  #journal_table_ref table de jounaux 
  
  
  
  
  col_journal=source# ici ce qu'on apelle source est bien la colonne du journal 
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
    
    # withProgress(
    #   message='Please wait',
    #   detail=paste0("matching journal"),
    #   value=0, {
        for(h in 1:inter){# boucle principale qui parcour les donn?es
          
          first<-(h-1)*pas+1# premier individu a prendre en compte(ligne)
          last<-h*pas       # dernier ""   "      "       "   "
          if(last>dim(journal_table_ref)[1]) last<-dim(journal_table_ref)[1]
          journal_courant=journal_table_ref[first:last,]
          
          
          #print(x)
          if(!(is.na(journal_data[[x]]) ||is.null(journal_data[[x]]))  && trouver==FALSE  ) {
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
          #   incProgress(1/inter)
          
        }
        if(trouver==FALSE){
          ab=c(ab,NA)
          cat=c(cat,NA)
          cat_trouver=c(cat_trouver,0)
        }
      #})
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







path=choose.files(caption = "chosse your data file")
data_wos <-convert2df(path,dbsource = "wos",format = "bibtex")

test=conforme_bibtext(data_wos,data_base = "WOS")

voici_un_test<-extract_ref_wos(data_wos)

test=as.data.frame(voici_un_test,stringsAsFactors = FALSE)
test$`refered identifier`
journal_table_ref= journal_table_ref[order( journal_table_ref[["JCR.Abbreviated.Title"]], journal_table_ref[["Full.Journal.Title"]],na.last = TRUE,decreasing = TRUE),]#on tri par le doi, utile pour l'utilisation des api 


dom<-find_journal_domaine(journal_data = test$`refered journal`,journal_table_ref = journal_table_ref,issn = test$`refered issn`,source =rep("JCR.Abbreviated.Title",dim(test)[1]) )

dom_para<-find_journal_domaine_para(journal_data = test$`refered journal`,journal_table_ref = journal_table_ref,issn = test$`refered issn`,source =rep("JCR.Abbreviated.Title",dim(test)[1]) )


dom[1,]

















#---------------------------------------------






dom=foreach(x=1:length(journal_data),.combine="c") %dopar% {}

parallel::stopCluster(cl)
  
mb <- microbenchmark(find_journal_domaine(journal_data = test$`refered journal`,journal_table_ref = journal_table_ref,issn = test$`refered issn`,source =rep("JCR.Abbreviated.Title",dim(test)[1]),nbcpu = 8 ), 
                     find_journal_domaine(journal_data = test$`refered journal`,journal_table_ref = journal_table_ref,issn = test$`refered issn`,source =rep("JCR.Abbreviated.Title",dim(test)[1]),nbcpu = 3 ),
                     find_journal_domaine(journal_data = test$`refered journal`,journal_table_ref = journal_table_ref,issn = test$`refered issn`,source =rep("JCR.Abbreviated.Title",dim(test)[1]),nbcpu = 2 ), times=120)
plot(mb)
View(mb)









compaire_title_para<-function(ti_trouver,ti_data){
  #' compaire_title
  #'Cette fonction compare les titre trouvé et les titre demandés, renvoi le score maximal  , elle indique aussi l'indexe du premier titre qui a le score correspondant 
  #
  #' @param ti_trouver vecteur comparer  
  #' @param ti_data vecteur comparant 
  #'
  #' @return liste avec le taux de omparaison et le numéro du plus proche element trouver pour chaque element du vecteur 
  
  Unaccent <- function(text) {
    #remouve accent of text
    text <- gsub("['`^~\"]", " ", text)
    text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
    text <- gsub("['`^~\"]", "", text)
    return(text)
  }
  
  Ncpus <- (parallel::detectCores() - 1)/2
  Ncpus<-floor(Ncpus)
  if(Ncpus>1){
    print("passage in para")
    cl <- parallel::makeCluster(Ncpus)
    doParallel::registerDoParallel(cl)
  
    res<-foreach(x=1:length(ti_trouver),.combine="c") %dopar% {
      
      
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
    }
    parallel::stopCluster(cl)
  }
}


