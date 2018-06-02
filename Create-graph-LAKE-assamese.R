###########################################################
#                                                         #
#         Create Context-Aware Graphs for LAKE            #
#                                                         #
###########################################################

library(NLP)
library(tm)
library(openNLP)
library(graph)
library(foreign)
library(igraph)
library(tools)
library(BiocGenerics)


text_clean <- function (x) {
  # Function to clean and pre-process the input text document
  # required packages: "tm", "openNLP"
  
  # convert to lower case
  x = tolower(x)
  
  # remove extra white space
  x = gsub("\\s+"," ",x)
  
  # replace tabs with white space
  x = gsub("[/\t]"," ",x)
  
  # remove dashes
  x = gsub("- ", " ", x, perl = TRUE) 
  x = gsub(" -", " ", x, perl = TRUE)
  
  # remove parentheses
  x = gsub("\\(", " ", x, perl = TRUE) 
  x = gsub("\\)", " ", x, perl = TRUE)
  
  # remove punctuations
  x = removePunctuation(x)
  
  # remove plus and star signs
  x = gsub("+", " ", x, fixed = TRUE)
  x = gsub("*", " ", x, fixed = TRUE)
  
  # remove apostrophes that are not intra-word
  x = gsub("' ", " ", x, perl = TRUE)
  x = gsub(" '", " ", x, perl = TRUE)
  
  # remove numbers (integers and floats) but not dates like 2015
  x = gsub("\\b(?!(?:18|19|20)\\d{2}\\b(?!\\.\\d))\\d*\\.?\\d+\\b"," ", x, perl=T)
  
  # remove "e.g." and "i.e."
  x = gsub("\\b(?:e\\.g\\.|i\\.e\\.)", " \\1 ", x, perl=T)
  
  # replace "...." by "..."
  x = gsub("(\\.\\.\\.\\.)", " \\.\\.\\. ", x, perl=T)
  
  # replace ".." by "."
  x = gsub("(\\.\\.\\.)(*SKIP)(*F)|(\\.\\.)", " \\. ", x, perl=T)
  
  
  # remove leading and trailing white space
  x = str_trim(x,"both")
  
  # tokenize
  x = unlist(strsplit(x,split=" "))
  
  # make a copy of tokens without further preprocessing
  xx = x
  
  # remove stopwords
  index = which(x %in% my_stopwords)
  if (length(index)>0){
    x = x[-index]
  }
  
  # remove blank elements
  index = which(x=="")
  if (length(index)>0){
    x = x[-index]
  }
  
  index = which(xx=="")
  if (length(index)>0){
    xx = xx[-index]
  }
  output = list(unprocessed=xx, processed=x, sentences=sentences)
}


# ------------- functions

convert_text_to_sentences <- function(text) {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator()
  
  # Convert text to class String from package NLP
  text <- as.String(text)
  
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  # Extract sentences
  sentences <- text[sentence.boundaries]
  
  # return sentences
  return(sentences)
}

convert_text_to_sentences1 <- function(text){
  return(unlist(strsplit(text, "[|?!]", perl = TRUE)))
}




# -------------- MAIN CODE
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
path = "path/to/file"
setwd(path)
df_sigma = read.csv("Positions/candidates_sigma_1.csv")
f <- "assamese-sample1.txt"
file_id = as.character(file_path_sans_ext(f))
file_ids = unlist(strsplit(file_id,"/"))
file_id = file_ids[length(file_ids)]

texts = read.delim(f, header = F, sep = "\t", quote = "", stringsAsFactors = FALSE)#, fileEncoding = "UTF-8") 
texts = as.character(texts)

texts = gsub("\\।", ".", texts, perl = T)

sent1 = convert_text_to_sentences(texts)
sent = NULL
# join two consecutive sentences as one - s1s2  s2s3  s3s4 ...
if(length(sent1) < 2){
  sent = as.String(sent1)
}else{
  for(i in 1:(length(sent1)-1)){
    s = paste(sent1[i], ". ", sent1[i+1])
    sent = c(sent, as.String(s))
  }
}
doc<-c(sent)

my_stopwords <- readLines("assamese-stopwords.txt") # reads the list of stopwords


df = as.data.frame(doc) 
# Create the corpus (using VectorSource), taking two consecutive sentences as a document
corp = Corpus(VectorSource(df$doc))
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, my_stopwords)
  


# select top 33% words as candidates
df_sample = subset(df_sigma, df_sigma$fileName == file_id)
thres = round(length(df_sample$word) * 0.33) # defining threshold for top-33% words
thres = ifelse(length(which(df_sample$sigma > 0)) > thres, thres, length(which(df_sample$sigma > 0)))
selected_words <- df_sample$word[1:thres]
index = which(selected_words== "")
if(length(index) > 0){
  selected_words = selected_words[-index]
}
  
  
# create document-term matrix
dtm = DocumentTermMatrix(corp)
dtm = dtm[,colnames(dtm)%in%selected_words] # keep only candidates as terms
dtm = as.matrix(dtm)
dtm[dtm > 1] <- 1
ttm = t(dtm) %*% dtm # create term-term matrix
diag(ttm) <- 0

# Following code is a sample to create graphs from ttm
# g = graph.adjacency(ttm, mode = "undirected", weighted = TRUE) # create graph from ttm
# g<- simplify(g, remove.multiple = T, remove.loops = T) # make g a simple graph without self loops and multiple edges
# g_data = as_data_frame(g, what = "edges")
  
c = "Graphs/LAKE/"
sfile1 = paste(c, file_id, sep = "")
sfile1 = paste(sfile1,"Rda",sep=".")
saveRDS(ttm, file = sfile1)