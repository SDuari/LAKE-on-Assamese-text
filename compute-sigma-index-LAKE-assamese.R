#############################################################################
#                               Sigma index                                 #
# (Creates a data frame of words with their respective sigma index values.) #
#############################################################################


library(NLP)
library(tm)
library(openNLP)
library(foreign)
library(tools)


# -------------Functions 

SplitText <- function(Phrase) { 
  unlist(strsplit(Phrase," "))
}

SigmaIndex <- function(df){
  words = df$words
  l = length(df$words)
  sigma = rep(0, each=l)
  
  for(j in 1:l){
    pos = df$positions[[j]]
    n = length(pos)-1
    if(n >= 3){
      N = as.numeric(pos[n+1]-1)
      avg = (N + 1)/(n + 1)
      sum = (pos[1] - avg)^2
      for(i in 1:n){
        sum = sum + ((pos[i+1] - pos[i]) - avg)^2
      }
      s = sqrt(sum/(n-1))
      sigma[j] = s/avg
    }
  }
  return(sigma)
}



# ---------- Main Code
path = "path/to/file"
setwd(path)
myFile = "assamese-sample1.txt" # original text document
positions_file = "Positions/assamese-sample1.Rda" # the corresponding positions file
file_id = as.character(file_path_sans_ext(myFile))
file_ids = unlist(strsplit(file_id,"/"))
file_id = file_ids[length(file_ids)]

df3 = readRDS(positions_file)

######### Calculating sigma-index
sigma_index = SigmaIndex(df3)
sigma_index = sigma_index/max(sigma_index) #nrmalizing
  
######### Creating data frames
df_w <- data.frame(df3$words,sigma_index,file_id)
names(df_w) <- c("word", "sigma", "fileName")

df_sig <- df_w[order(df_w$sigma, decreasing = TRUE),] # sort in descending order of sigma-index values
  
write.table(df_sig,file = "Positions/candidates_sigma_1.csv",sep=",",row.names=F,col.names=!file.exists("Positions/candidates_sigma_1.csv"),append=T)
