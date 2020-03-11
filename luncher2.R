
print(Sys.time())
dompV1<-find_journal_domaine(test$`refered journal`,test$`refering issn`,test$`refering essn`,journal_table_ref)
print(Sys.time())
domp<-find_journal_domaine_v3(test$`refered journal`,test$`refering issn`,test$`refering essn`,journal_table_ref)
print(Sys.time())
