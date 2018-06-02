##########################################
#     Create edge list
##########################################


library(NLP)
library(tm)
library(openNLP)
library(graph)
library(foreign)
library(igraph)
library(tools)

path = "path/to/file"
setwd("/path/to/file/Graphs/LAKE/")
files = list.files(pattern = ".*.Rda")
i = 1
for(f in files){
  file_id = as.character(file_path_sans_ext(f))
  #file_id = unlist(strsplit(file_id, "_"))

  
  df1 = readRDS(f)   #### read .rda file, loads data frame
  gr = graph.adjacency(df1, mode = "undirected", weighted = TRUE)   
  
  edgel = get.data.frame(gr, "edges")
  
  f_n = paste0(path,"Graphs/edgelists/",file_id,sep = "")
  f_n = paste(f_n, "csv", sep = ".")
  
  write.table(edgel,file = f_n,sep="\t",row.names=F, col.names = F)
  i = i+1
}
print("Done with edgelist conversion!!!")