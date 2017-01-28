# Similarity

# Jaccard Similarity

# set function

set1 <- c("a", "b", "c")
set2 <- c("c", "d", "e")

intersect(set1, set2) 
union(set1, set2)

jaccard.sim <- function(x, y)
{
  return(length(intersect(x, y)) / length(union(x, y))) 
}

jaccard.sim(set1, set2)

# Cosine Sim

v1 <- c(1, 5, 3)
v2 <- c(3, 7, 9)

sum(v1 * v2)

cosine.sim <- function(x, y)
{
  return(sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y))))   
}
  
cosine.sim(v1, v2)

setwd("//home//cursos//Descargas")
getwd()  


music.usage <- read.csv('lastfm-matrix-germany.csv')
summary(music.usage)
str(music.usage)

music.usage.nouser <- music.usage[-1]
music.usage.nouser[1, 1:10]

ncol(music.usage[1,1])

music.usage[1,1:10]

colnames(music.usage[1,])

selected.artists(music.usage[1,]) 

selected.artists  <- function(df)
{
  df <- df[-1]
  artists.names <- colnames(df)
  artists <- NULL
  for (i in 1:ncol(df))
  {
    if (df[,i] == 1)
       artists <- c(artists, artists.names[i])
  }
  artists
}

!names(music.usage) %in% c("user")
  
music.usage.nu <- music.usage[,!(names(music.usage) %in% c("user")) ]
music.usage.nu[1,1:10] 

music.usage.sim <- matrix(NA, nrow=ncol(music.usage.nu), 
                      ncol= ncol(music.usage.nu), 
                      dimnames=list(colnames(music.usage.nu), colnames(music.usage.nu) ))

music.usage.sim[1:5,1:5] 

as.matrix(music.usage.nu[1,])

for (i in 1:ncol(music.usage.nu)) {
  for (j in 1:ncol(music.usage.nu) )
    music.usage.sim[i,j] <- cosine.sim(as.matrix(music.usage.nu[i]),
                                       as.matrix(music.usage.nu[j]))
}



music.usage.sim <- data.frame(music.usage.sim)
class(music.usage.sim)
music.usage.similar.artists <- matrix(NA, nrow=ncol(music.usage.sim), ncol=11, 
                                      dimnames=list(colnames(music.usage.sim)))

head(n=11, rownames(music.usage.sim[order(music.usage.sim[,1],decreasing=T),][1]))

music.usage.similar.artists[]

order(music.usage.sim[,1],decreasing=T) 

for (i in 1:ncol(music.usage.sim))
{
  
  music.usage.similar.artists[i,] <- t(head(n=11, 
      rownames(music.usage.sim[order(music.usage.sim[,i],decreasing=T),][i])))
  
}

music.usage.similar.artists[1,1:10]

# similar artists to the beatles???
music.usage.similar.artists

music.usage.similar.artists <- data.frame(music.usage.similar.artists)
music.usage.similar.artists <- music.usage.similar.artists[-1]

music.usage.similar.artists["the.beatles",]

## by user

# 2 
user.data <- matrix(NA, nrow=nrow(music.usage), ncol=ncol(music.usage)-1, 
       dimnames=list(music.usage$user, colnames(music.usage[-1])))

user.data 

# 3
user.data[1:5,1:5]
# 4
rownames(user.data)[2]
colnames(user.data)[5]

#5
music.usage[music.usage$user=="1","the.killers"]

#6
top.artists <- head(n=6, music.usage.sim[order(music.usage.sim[,"the.killers"],
                                               decreasing=T),]["the.killers"])
top.artists 
killers.sim <- rownames(top.artists)[-1]
killer.sim <- top.artists[,1][-1]
killer.sim
auxnames
killers.sim 
# 7 historial de gustos
top.artists.history <-

  music.usage[music.usage$user=="153",c("user", auxnames)]  
  
user1.history <-  head(music.usage[music.usage$user=="51",c("user", auxnames)])[-1]
user1.history
user51.history <-  head(music.usage[music.usage$user=="51",c("user", auxnames)])[-1]
user51.history
music.usage[1,1:5]

#8 SUM(H*S)/SUM(S).

killers.sim

recom.to.user1.the.killers <- score.recommendation(similarities=killer.sim, history=user51.history )
recom.to.user1.the.killers 

music.usage[music.usage$arctic.monkeys==1,][1,1:5]

score.recommendation <- function(history, similarities)
{
  return(sum(history*similarities)/sum(similarities))
}
 


for (i in 1:nrow(user.data)) {  # users
  for (j in 1:ncol(user.data))     # artists
  {
    user <- rownames(user.data)[i]
    artist <- colnames(user.data)[j]
    
    # if exist --> empty string
    if ( music.usage[music.usage$user==user,artist] == 1)
    {
      user.data[i,j] <- ""
    }
    else
    {
      top.artists <- head(n=6, music.usage.sim[order(music.usage.sim[,artist],decreasing=T),][artist])
      top.artists.names <- rownames(top.artists)
      top.artists.sim <- top.artists[,1]
      top.artists.names <- top.artists.names[-1]
      top.artists.sim <- top.artists.sim[-1]
      
      # history of user
      top.artists.history <- music.usage[music.usage$user==user,c("user", top.artists.names)][-1]
      
      user.data[i,j] <- score.recommendation(similarities=top.artists.sim, history=top.artists.history)
      
      
      
    }
    
    
  }
}

