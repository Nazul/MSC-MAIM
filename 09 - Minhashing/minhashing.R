# Minhashing
install.packages('microbenchmark')
library(microbenchmark)
library(magrittr)
microbenchmark(2 + 2)

jaccard <- function(x, y) {
  set_int <- length(intersect(x,y))
  set_uni <- length(union(x,y))
  return(set_int / set_uni)
}

set.a <- c(1,2,3,4,5)
set.b <- c(4,5,6,7,8)

jaccard(set.a, set.b)
jaccard(set.a, set.a)

jaccard(set.a, set.a + 15)

# generate random sample
letters
paste(sample(letters, 3), collapse='')
vector <- 1:1e5
length(vector)
set.seed(12345)
lastnames <-  Map(function(x) paste(sample(letters, 3), collapse=''),  1:1e5) %>% unique
head(lastnames)
length(lastnames)
length(letters)

p.repetition <- (1/26 * 1/26 * 1/26)
p.repetition

# two sets
set.seed(123)
friends.one <- sample(lastnames, 100, replace=F)
# two sets
set.seed(234)
friends.two <- sample(lastnames, 100, replace=F)

microbenchmark(jaccard(friends.one, friends.two))

# time 1e5 persons
hours <-((50 * 1e-6) * 1e5 * 1e5) / 3600
hours

# Section 2

set1 <- c("JOSE", "CARLOS", "ANA", "RAUL", "PEDRO", "JUAN")
set2 <- c("MARCOS", "LUCAS", "MATEO", "JUAN")
set3 <- c("ANGELICA", "MARIA", "ALBERTO", "DANIEL", "JUANA")
set4 <- c("MARIA", "CARLOS", "RAUL", "LUCAS", "MARCOS", "JOSE", "DANIEL")

set_list <- list(set1, set2, set3, set4)
set_list
set_list[[1]]

# create a function
names <- Reduce(union, set_list)  
names
is.element(names, set1)
mat <- cbind(is.element(names, set1), is.element(names, set2), is.element(names, set3), is.element(names, set4))
mat

?matrix
typeof(mat)
mat.num <- as.numeric(mat)
mat.num.mtx <- matrix(mat.num, ncol=4) 
mat.num.mtx

sets_dict <- unlist(set_list) %>% unique
sets_dict
names == sets_dict
microbenchmark(Reduce(union, set_list))
# ms 28.8
microbenchmark(unlist(set_list) %>% unique)

1 %in% c(1,2,3)

microbenchmark(matrix(as.numeric(cbind(is.element(names, set1), 
                is.element(names, set2), is.element(names, set3), 
                is.element(names, set4))), ncol=4))

mat.num.2 <-   Map(f = function(set, dict) as.integer(dict %in% set), 
          set_list, MoreArgs = list(dict=sets_dict)) %>%
           do.call(what = cbind, .) 

mat.num.2

mat.num.mtx ==  mat.num.2





#mseg 12.4

microbenchmark( Map(f = function(set, dict) as.integer(dict %in% set), 
                    set_list, MoreArgs = list(dict=sets_dict)) %>%
                  do.call(what = cbind, .) )


dimnames(mat.num.2) <- list(sets_dict, paste('set', 1:length(set_list), sep = '_'))
mat.num.2["DANIEL", "set_4"]
mat.num.2


mat.num.2[which(mat.num.2[,3] | mat.num.2[,4])]

column_jaccard <- function(c1, c2) {
  non_zero <- which(c1 | c2)
  column_int <- sum(c1[non_zero] & c2[non_zero])
  column_uni <- length(non_zero)
  return (column_int / column_uni)
  
}

jaccard(set1, set2)
column_jaccard(mat.num.2[,1] , mat.num.2[,2]) 

nrow(mat.num.2)

perm <- sample(nrow(mat.num.2))
perm
mat.num.2[perm,]

n.signatures <- 5
sm <- matrix(data = NA_integer_, nrow=n.signatures, ncol = ncol(mat.num.2))
sm
set.seed(1234)

non.zero.idx <- apply(mat.num.2, MARGIN = 2, FUN = function(x) which(x != 0))

non.zero.idx 


for (i in 1 : n.signatures)
{
  perm <- sample(nrow(mat.num.2))
  mat.aux <- mat.num.2[perm,]
  non.zero.idx <- apply(mat.aux, MARGIN = 2, FUN = function(x) which(x != 0))
  for (j in 1:ncol(mat.aux))
    sm[i,j] <- min(non.zero.idx[[j]])
}

sm

jaccard_signatures <- function(c1, c2) {
  col_int <- sum(c1==c2)
  col_un  <- length(c1)
  return (col_int / col_un)
}

# comparison
jaccard(set1, set2) 

set1
mat.num.2[,1]
sm[,1]

jaccard(set1, set2) 
jaccard_signatures(sm[,1], sm[,2])

jaccard(set1, set3) 
jaccard_signatures(sm[,1], sm[,3])

jaccard(set1, set4) 
jaccard_signatures(sm[,1], sm[,4])



