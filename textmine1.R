library(tm)
library(magick)
library(SnowballC)
library(wordcloud)
library(tidyverse)
mytext<-readLines("https://www.lifehack.org/articles/lifestyle/20-awesome-diy-science-projects-with-your-kids.html")
#Process text
nohtml<-function(xhtml){
  gsub("<.*?>","",xhtml)
}
#no punctuation
nopunc<-function(xhtml){
  gsub("[[:punct:]]","",xhtml)
}
noperma<-function(xhtml){
  gsub("permalink","",xhtml)
}
noqot<-function(xhtml){
  gsub("quot{1,}","",xhtml)
}
noad<-function(xhtml){
  gsub("google{1,}","",xhtml)
}

#remove html and others
mytext<-nohtml(mytext)
mytext<-nopunc(mytext)
mytext<-noperma(mytext)
mytext<-noqot(mytext)
mytext<-noad(mytext)

#.....
mytext<-Corpus(VectorSource(mytext))
mytext<-mytext %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords,stopwords("en")) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(removeNumbers) 
mytext<-Corpus(VectorSource(mytext))
mytextmat<-as.matrix(TermDocumentMatrix(mytext),control(list(wordLengths=c(0,Inf))))
mytextvec<-sort(rowSums(mytextmat),decreasing = T)
mytextdf<-data_frame(word=names(mytextvec),freq=mytextvec)
mytextdf
mytextdf<-mytextdf %>% 
  filter(!word=="var"&!word=="itrsquos"&!word=="yoursquoll")
par(bg="gray23")
wordcloud(mytextdf$word,mytextdf$freq,random.order=F,col=brewer.pal(8,"Set3"),
          rot.per=0.21,max.words = 112)
#...........................
myimg<-image_read("http://www.proteinatlas.org/images_dictionary/intermediate_filament__1__35915__1_blue_green.jpg")
myimg1<-image_read("http://i3.mirror.co.uk/incoming/article91738.ece/ALTERNATES/s615/wonderkid-image-2-284865704.jpg")
myimg2<-image_read("myimg.png")
img<-c(myimg1,myimg)
myimg3<-myimg%>% 
  image_scale("500") %>% 
  image_composite(image_scale(myimg2,"200"),offset="+0") %>% 
  image_modulate(90,53,876)
myimg3<-myimg3%>% 
  image_scale("500") %>% 
  image_composite(image_scale(myimg1,"200"),offset="+0") %>% 
  image_modulate(90,80,8768)
yep<-myimg3 %>% 
  image_composite(image_scale(myimg2,"215"),offset="+290+359") %>% 
  image_modulate(100,80,9867) %>% 
  image_colorize(14,"skyblue3") %>% 
  image_annotate("Nelson",location = "+256+316",size="30",color="papayawhip") %>% 
  image_annotate("RInside!",location="+267+35",size=25,color="papayawhip",degrees = 56)
image_write(yep,"me.png")
