##**********************************************************
## ITESO - Maestría en Sistemas Computacionales
## Manejo y Análisis de Información Masiva
## Práctica 2. Similarity
##
## Mario Contreras (705080)
##
##*********************************************************
# Change working directory to this file's folder (running file)
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)
# Change working directory to this file's folder (IDE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Similarity

set1 <- c("a", "b", "c")
set2 <- c("c", "d", "e")

intersect(set1, set2)
union(set1, set2)

# No es sensible a la magnitud de la unión
jaccard.sim <- function(x, y) {
  return(length(intersect(x, y)) / length(union(x, y)))
}

jaccard.sim(set1, set2)

v1 <- c(1, 5, 3)
v2 <- c(3, 7, 9)

# Es sensible a la magnitud de la unión
cosine.sim <- function(x, y) {
  return(sum(x * y) / (sqrt(sum(x * x)) * sqrt(sum(y * y))))
}

cosine.sim(v1, v2)

music.usage <- read.csv("lastfm-matrix-germany.csv")
summary(music.usage)
structure(music.usage)

music.usage[1,]

selected.artists <- function(df) {
  df <- df[-1]
  artists.names <- colnames(df)
  artists <- NULL
  for (i in 1:ncol(df)) {
    if(df[,i] == 1) {
      artists <- c(artists, artists.names[i])
    }
  }
  artists
}

selected.artists(music.usage[1,])

music.usage.nu <- music.usage[,!(names(music.usage) %in% c("user"))]

music.usage.sim <- matrix(NA, nrow = ncol(music.usage.nu), ncol = ncol(music.usage.nu), dimnames = list(colnames(music.usage.nu), colnames(music.usage.nu)))
music.usage.sim[1:5, 1:5]

for (i in 1:ncol(music.usage.nu)) {
  for (j in 1:ncol(music.usage.nu)) {
    music.usage.sim[i, j] <- cosine.sim(as.matrix(music.usage.nu[i]), as.matrix(music.usage.nu[j]))
  }
}

utils::View(music.usage.sim)

music.usage.sim <- data.frame(music.usage.sim)

music.usage.similar.artists <- matrix(NA, nrow = ncol(music.usage.sim), ncol = 11, dimnames = list(colnames(music.usage.sim)))
music.usage.similar.artists

for (i in 1:ncol(music.usage.sim)) {
  music.usage.similar.artists[i,] <- t(head(n=11, rownames(music.usage.sim[order(music.usage.sim[,i], decreasing = TRUE),][i])))
}

music.usage.similar.artists[1, 1:10]

music.usage.similar.artists["the.beatles",]

music.usage.similar.artists <- data.frame(music.usage.similar.artists)
music.usage.similar.artists <- music.usage.similar.artists[-1]

music.usage.similar.artists["the.beatles",]


# 2

user.data <- matrix(NA, nrow = nrow(music.usage), ncol = ncol(music.usage)-1, dimnames = list(music.usage$user, colnames(music.usage[-1])))
utils::View(user.data)

# 8
score.recommendation <- function(history, similarities) {
  return(sum(history * similarities) / sum(similarities))
}

# 3, 4, 5, 6

for (i in 1:nrow(user.data)) {        #users
  for (j in 1:ncol(user.data)) {      #artists
    user <- rownames(user.data)[i]
    artist <- colnames(user.data)[j]
    
    # if exists -> emtpy string
    if(music.usage[music.usage$user == user, artist] == 1) {
      user.data[i,j] <- ""
    }
    else {
      top.artists <- head(n=6, music.usage.sim[order(music.usage.sim[,artist],decreasing=T),][artist])
      top.artists.names <- rownames(top.artists)
      top.artists.sim <- top.artists[,1]
      top.artists.names <- top.artists.names[-1]
      top.artists.sim <- top.artists.sim[-1]
      
      # History of a user
      top.artists.history <- music.usage[music.usage$user==user,c("user", top.artists.names)][-1]
      user.data[i,j] <- score.recommendation(similarities=top.artists.sim, history=top.artists.history)
    }
  }
}

utils::View(user.data)

# EOF
