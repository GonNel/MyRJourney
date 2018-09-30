#Text Mining in R
#Nursery image link #https://www.nurseryrhymes.org/ten-in-a-bed.html#
shh<-suppressMessages
shh(library(tidyverse))
shh(library(tm))
shh(library(SnowballC))#Implementation of C Porter word stemming algorithm
shh(library(wordcloud))#Visual representation of words
shh(library(tidytext))
shh(library(magick))
shh(library(tesseract))
library(openNLP)
#Now let's start studying
sometxt<-readLines("https://opensource.com/article/16/12/best-of-science")
#Remove html strings
nohtml<-function(xstring){
  return(gsub("<.*?>","",xstring))
        
}
doctxt<-Corpus(VectorSource(sometxt))
doctxt<-nohtml(doctxt)
doctxt<-Corpus(VectorSource(doctxt))
doctxt<-doctxt %>% 
 tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords,stopwords(kind="en")) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(stemDocument)
docmat<-TermDocumentMatrix(doctxt)
docmatr<-as.matrix(docmat)
docvec<-sort(rowSums(docmatr),decreasing = F)
df<-data.frame(word=names(docvec),freq=docvec)
head(df,10)
#generating the word cloud
par(bg="gray5")
wordcloud(df$word,df$freq,col=terrain.colors(length(df$word),alpha=0.3),random.order = F)
#Using some ocr
textimg<-image_read("nursery.png")
text<-image_ocr(textimg)
text<-text %>% 
  str_replace_all("[[:punct:]]","") %>% 
  str_replace_all("\\n","") %>% 
  str_replace_all("Ten.Bed","") %>% 
  str_replace_all("Lyrics{1,}","") %>% 
  str_replace_all("^Ten","") 
#Text is now read using OCR
#Create text dataframe
text<-Corpus(VectorSource(text))
text<-text %>% 
  tm_map(removeWords,stopwords("en")) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(stemDocument)
#Create document matrix
textmat<-as.matrix(TermDocumentMatrix(text))
textvec<-sort(rowSums(textmat),decreasing=F)
textdf<-data_frame(word=names(textvec),freq=textvec)
head(textdf,5)
wordcloud(textdf$word,textdf$freq,random.order = F,col=terrain.colors(length(textdf$word),alpha = 0.5))
#Open NLP
tagPOS <- function(x, thisPOSregex) {
  x <- as.String(x)
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  thisPOSindex <- grep(thisPOSregex, tags)
  tokenizedAndTagged <- sprintf("%s/%s", x[POSwords][thisPOSindex], tags[thisPOSindex])
  untokenizedAndTagged <- paste(tokenizedAndTagged, collapse = " ")
  untokenizedAndTagged
}
#Return all nouns
lapply(text,tagPOS,"NN")
