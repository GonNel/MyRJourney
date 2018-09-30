#Text mining on a famous poem
library(wordcloud)
library(tidytext)
library(tidyverse)
library(tm)
library(SnowballC)
library(magick)
#..
cambridge<-c("Softly I am leaving,
        Just as softly as I came;
        I softly wave goodbye
        To the clouds in the western sky.-",
        
        "The golden willows by the riverside
        Are young brides in the setting sun;
        Their glittering reflections on the shimmering river
        Keep undulating in my heart.-",
        
        "The green tape grass rooted in the soft mud
        Sways leisurely in the water;
        I am willing to be such a waterweed
        In the gentle flow of the River Cam.-",
        
        "That pool in the shade of elm trees
        Holds not clear spring water, but a rainbow
        Crumpled in the midst of duckweeds,
        Where rainbow-like dreams settle.-",
        
        "To seek a dream? Go punting with a long pole,
        Upstream to where green grass is greener,
        With the punt laden with starlight,
        And sing out loud in its radiance.-",
        
        "Yet now I cannot sing out loud,
        Peace is my farewell music;
        Even crickets are now silent for me,
        For Cambridge this evening is silent.-",
        
        "Quietly I am leaving,
        Just as quietly as I came;
        Gently waving my sleeve,
        I am not taking away a single cloud.
        ")
cambridge<-cambridge %>% 
  str_replace_all("Cam.","Cambridge")
#word count
cambridge<-cambridge %>% 
  str_remove_all("\\\n")
cambridge1<-Corpus(VectorSource(cambridge))
cambridge1<-cambridge1 %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords,stopwords(kind="en")) 
#Create matrix
cambridge1<-as.matrix(TermDocumentMatrix(cambridge1))
cambrvec<-sort(rowSums(cambridge1),decreasing=F)
cambrdf<-data_frame(word=names(cambrvec),freq=cambrvec)
par(bg="gray16")
wordcloud(cambrdf$word,cambrdf$freq,random.order = F,
          random.color=F,colors=brewer.pal(7,"Set1"),min.freq = 1,rot.per =0.1,
          max.words = 100)



