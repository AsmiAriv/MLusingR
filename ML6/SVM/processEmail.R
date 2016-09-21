processEmail <- function(email_contents){
  
  #PROCESSEMAIL preprocesses a the body of an email and
  #returns a list of word_indices 
  #   word_indices = PROCESSEMAIL(email_contents) preprocesses 
  #   the body of an email and returns a list of indices of the 
  #   words contained in the email. 
  
  #Library required
  library(tm)
  #Initialize a vector
  word_indices <- vector(mode = "list")
  
  
  
  # Load Vocabulary
  # Read the fixed vocabulary list
  vocabList = read.table('vocab.txt',row.names = NULL)
  
  #Preprocess Email
  sentence <- email_contents
  
  sentence <- iconv(sentence, "latin1", "UTF-8") 
  sentence <- gsub("\\$","dollar",sentence, perl=TRUE)
  sentence <- gsub("[^\\s]+@[^\\s]+", "emailaddr", sentence, perl=TRUE)
  sentence = gsub('[[:punct:]]', '', sentence, perl=TRUE)
  sentence = gsub('[[:cntrl:]]', '', sentence, perl=TRUE)
  sentence = gsub('\\d+', 'number', sentence, perl=TRUE)
  sentence <- gsub("(RT|via)((?:\\b\\w+)+)", "", sentence, perl=TRUE)
  sentence <- gsub("[[:digit:]]", "number", sentence, perl=TRUE)
  sentence <- gsub("http\\w+", "httpaddr", sentence, perl=TRUE)
  sentence <- gsub("[ \t]{2,}", "", sentence, perl=TRUE)
  sentence <- gsub("^\\s+|", "", sentence, perl=TRUE)
  sentence = tolower(sentence)
  
  #stemming words
  corpdata <- Corpus(VectorSource(sentence))
  corpdata <- tm_map(corpdata, stemDocument)
  #Converting to PlainTextDocument
  corpdata <- tm_map(corpdata, PlainTextDocument)
  #Converting back to text file
  text = sapply(corpdata, as.character)
  
  #Splitting the sentences into words
  word.list = strsplit(text, '\\s+')
  words = unlist(word.list)
  
  for(i in 1:length(words)){
    word_indices[[i]] <- grep(paste0('^',words[i],'$'), vocabList[,2])
  }
  word_indices <- unlist(word_indices)
  word_indices
}