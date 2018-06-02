##############################################################################
#               Create data frame for positions of word occurrence           #
#                               For LAKE method                              #
#                     (Taken help from online repositories)                  #
##############################################################################



library(NLP)
library(tm)
library(openNLP)
library(foreign)
library(stringr)
library(tools)



# ------------- FUNCTIONS

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
  
  # remove english-characters
  x = gsub("[a-zA-Z]+", " ", x, perl = T)
  
  # remove "e.g." and "i.e."
  x = gsub("\\b(?:e\\.g\\.|i\\.e\\.)", " \\1 ", x, perl=T)
  
  # replace "...." by "..."
  x = gsub("(\\.\\.\\.\\.)", " \\.\\.\\. ", x, perl=T)
  
  # replace ".." by "."
  x = gsub("(\\.\\.\\.)(*SKIP)(*F)|(\\.\\.)", " \\. ", x, perl=T)
  
  
  # remove leading and trailing white space
  x = str_trim(x,"both")
  
  # tokenize
  x = unlist(strsplit(x,split="[ ???]"))
  
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
  output = list(unprocessed=xx, processed=x)
}



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



# ------------ Main Code
path = "path/to/file"
setwd(path)
my_stopwords <- readLines("assamese-stopwords.txt") # reads the list of stopwords

f <- "assamese-sample1.txt" # the text document from where keywords are to be extracted

file_id <- as.character(file_path_sans_ext(f))
file_ids = unlist(strsplit(file_id,"/"))
file_id = file_ids[length(file_ids)]
  
# reads contents of the text document
texts = read.delim(f, header = F, sep = "\t", quote = "", stringsAsFactors = FALSE)#, fileEncoding = "UTF-8") 
texts <- as.character(texts)

texts = gsub("\\???", ".", texts, perl = T) # convert sentence terminating marks to .



output <- text_clean(texts)
words <- output$unprocessed

selected_words <- output$processed
selected_words <- unique(selected_words)
  
t = NULL
posi = list()
tf = NULL
i = 1
pos_w = NULL
N = length(words)
  
for (w in selected_words) {
  posw <- which(w==words)
  w_freq <- length(posw)
  posw = c(posw,N)
    
  t = c(t, w)
  tf = c(tf, w_freq)
  posi[[i]] = posw
  i=i+1
}
  
pos_w = as.array(posi)
  
term_freq = data.frame(t,tf,pos_w)
names(term_freq) = c("words","tf","positions")
  
 
c = paste0(path, "Positions/")
sfile1 = paste(c, file_id, sep = "")
sfile1 = paste(sfile1,"Rda",sep=".")
  
saveRDS(term_freq,file = sfile1)
 