#twitter

bearer_token="AAAAAAAAAAAAAAAAAAAAAIbONgEAAAAA9nT3tLKcjqTXUTNp%2FN46tDoCPEw%3DPkFd5WaCL8wP5nPOfdVjdugx8eih9JIRdc0DDC9N3QbwVw0z9F"

#acces_token="1359496622330310658-XU50abr71vvgUvOzIh99ZkRRBnvvGa"
#secret_token="WVrSysmRmPiLOYverfomkFublUVw2stEx2YXoT1QuOGCk"



library(rtweet)
#vignette("auth")


token <- create_token(
  app = "strem_APP_dopabat",
  consumer_key = "doik54TUCTzH5Vvf5yAhDoYkI",
  consumer_secret = "bMkRahqrHQi2Lq5aQhhyRQOBSnPBrILexqtuDyqZpcXRyT4leG",
  access_token = "1359496622330310658-XU50abr71vvgUvOzIh99ZkRRBnvvGa",
  access_secret = "WVrSysmRmPiLOYverfomkFublUVw2stEx2YXoT1QuOGCk"
  
  )




st2 <- search_tweets(c("Test of lepton universality in beauty-quark decays"),n = 10,include_rts=TRUE)

dim(st2)
View(st2)



size=object.size(st2)




write(x=st2,file = "twitts_sample")



limite=rate_limit(token)
View(limite)


stuser=search_users("52696182",10,token = token)

tweet=search_tweets("tweeter",n =10 ) #un ou=, 


dim(tweet)
view(tweet2)

names(tweet)==names(tweet2)
tweet$quoted_friends_count

(file_name=gsub(" ", "_",gsub(":", "_",paste0("file_test_",Sys.Date(),".json"),fixed = TRUE)))
tweet2=stream_tweets("covid vaccin",timeout = 30,file_name = file_name) #un ou=,
## get user IDs of people who mentioned trump

dim(tweet2)
view(tweet2)


tweet2$

  tweet2=parse_stream(tweet2)
usrs <- users_data(tweet)

## lookup users data
usrdat <- lookup_users(unique(usrs$user_id))

View(usrdat)






ts_plot(tweet, by = "mins")










tweet2=stream_tweets2(q="tweeter OR covid",token = token)


dim(tweet2)
view(tweet2)

sum(names(tweet)==names(tweet2))

st2$location

dim(stuser)
rstats <- search_users(q = "rstats", n=10)
dim(rstats)
names(rstats)

rstats$location


twitt_data=tweets_data(rstats)
twitt_data$status_id

tweet_and=stream_tweets("tweeter AND covid") #un ou=, 

dim(tweet_and)
#ref 

#' article{Repellin:2020qmf,
#'   author = "Repellin, C. and Leonard, J. and Goldman, N.",
#'   title = "{Hall drift of fractional Chern insulators in few-boson systems}",
#'   eprint = "2005.09689",
#'   archivePrefix = "arXiv",
#'   primaryClass = "cond-mat.quant-gas",
#'   doi = "10.1103/PhysRevA.102.063316",
#'   journal = "Phys. Rev. A",
#'   volume = "102",
#'   pages = "063316",
#'   year = "2020"
#' }
#' 
#' @article{Aad:2021egl,
#'   author = "Aad, Georges and others",
#'   collaboration = "ATLAS",
#'   title = "{Search for new phenomena in events with an energetic jet and missing transverse momentum in $pp$ collisions at $\sqrt{s} = 13$ TeV with the ATLAS detector}",
#'   eprint = "2102.10874",
#'   archivePrefix = "arXiv",
#'   primaryClass = "hep-ex",
#'   reportNumber = "CERN-EP-2020-238",
#'   month = "2",
#'   year = "2021"
#' }
#' 
