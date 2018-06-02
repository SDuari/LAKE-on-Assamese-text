#############################################################################
#                                                                           #
#       Incorporate Positional Weight of Nodes to the Computed Wscore       #
#                                                                           #
#############################################################################


library(foreign)
library(stringi)
library(tools)

# --------------- Functions

invertNum <- function(x){
  return(as.numeric(1/x))
}

getNodeWeight <- function(w){
  w_weight = 0
 
  pos_w = subset(pos_df, pos_df$words == w)
  
  
  if(length(pos_w$words) > 0){
    posi = pos_w$positions[[1]]
    posi = posi[-length(posi)]
    if(length(posi) > 0){
      w_weight = sum(invertNum(posi))
    }
  }
  return(w_weight)
}




# ------------- MAIN CODE
path = "path/to/file" # input fle path here
setwd(path)
f_txt = "assamese-sample1.txt" # original Text Document
file_id = as.character(file_path_sans_ext(f_txt))
f_wordScore = paste0(path, "Results/assamese-sample1.sortedranked.IF.txt") # replace <algo> sCAKE or LAKE
pos_df = readRDS(paste0(path, "Positions/assamese-sample1.Rda"))#,file_id,".Rda"))

  
t <- readChar(f_wordScore, file.info(f_wordScore)$size)
t = unlist(strsplit(as.character(t),"\n"))
t = t[-1]
  
  
  
w = sapply(strsplit(t, split=',', fixed=TRUE), function(x) (x[1]))
w = sapply(stri_sub(w, 4, stri_length(w)-3), function(x) (x[1]))
  
wscore = sapply(strsplit(t, split=',', fixed=TRUE), function(x) (x[2]))
wscore = as.numeric(wscore)
  
node_weight = NULL
  
for(myindex in 1:length(w)){
  node_weight[myindex] = getNodeWeight(w[myindex])
}
node_weight = replace(node_weight, is.na(node_weight), 0)
  
newScore = wscore * node_weight
  
new_df = data.frame(w,wscore,node_weight,newScore)
names(new_df) = c("Words", "Old_WScore","Position_Weight","SCScore")
  
new_df = new_df[order(new_df$SCScore, decreasing = TRUE),]
  
write.table(new_df,file = paste0(path,"SCScore res",file_id,"_ranked_list.csv"),sep=",",row.names=F,col.names=!file.exists(paste0(path,"SCScore res",file_id,"_ranked_list.csv")),append=F)
