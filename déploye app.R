library(rsconnect)


rsconnect::setAccountInfo(name='uga-projet',# name of projet 
                          token='9C39839E2B72E23120E5F29AD738CFA8',#token on projet see shinyapps 
                          secret='qcq7vfWVfaRDodLf6emp4ra5UNRu3TTJpG9RUlpA')

rsconnect::deployApp('C:/Users/moroguij/Documents/R_programs/DopabatUGA/applitodeploy' ) # link for the api dopabat 
#link of the folder containing the app and global files 



#----------------- app test for the dev team 
rsconnect::setAccountInfo(name='uga-projet',
                          token='9C39839E2B72E23120E5F29AD738CFA8',
                          secret='qcq7vfWVfaRDodLf6emp4ra5UNRu3TTJpG9RUlpA')
rsconnect::deployApp('C:/Users/moroguij/Documents/R_programs/devapp' )

#####appp 2 # other projet now hold by Didier Verceuil 
rsconnect::setAccountInfo(name='uga-projet',
                          token='9C39839E2B72E23120E5F29AD738CFA8',
                          secret='qcq7vfWVfaRDodLf6emp4ra5UNRu3TTJpG9RUlpA')
rsconnect::deployApp('C:/Users/moroguij/Documents/R_programs/DopabatUGA/lab_data_visualisation' )


