#twitter

bearer_token="AAAAAAAAAAAAAAAAAAAAAIbONgEAAAAA9nT3tLKcjqTXUTNp%2FN46tDoCPEw%3DPkFd5WaCL8wP5nPOfdVjdugx8eih9JIRdc0DDC9N3QbwVw0z9F"

#acces_token="1359496622330310658-XU50abr71vvgUvOzIh99ZkRRBnvvGa"
#secret_token="WVrSysmRmPiLOYverfomkFublUVw2stEx2YXoT1QuOGCk"



library(rtweet)
vignette("auth")


token <- create_token(
  app = "strem_APP_dopabat",
  consumer_key = "doik54TUCTzH5Vvf5yAhDoYkI",
  consumer_secret = "bMkRahqrHQi2Lq5aQhhyRQOBSnPBrILexqtuDyqZpcXRyT4leG",
  access_token = "1359496622330310658-XU50abr71vvgUvOzIh99ZkRRBnvvGa",
  access_secret = "WVrSysmRmPiLOYverfomkFublUVw2stEx2YXoT1QuOGCk"
  
  )




st2 <- search_tweets2(c("\"data science\"", "rstats OR python"),n = 500,token=token)
size=object.size(st2)

write(x=st2,file = "twitts_sample")

stuser=search_users("52696182",10,token = token)





dim(st2)
View(st2)
st2$location

dim(stuser)
rstats <- search_users(q = "rstats", n=10)
dim(rstats)
names(rstats)

rstats$location


twitt_data=tweets_data(rstats)
twitt_data$status_id
