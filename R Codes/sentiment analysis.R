library(tidyverse)
library(tidytext)


text<-c("A chill ran up Lorkin's spine, and his heart missed a beat. Father made this! He turned it over and over, the stone catching the light. Did Father know stone-making? Surely not. The answer was suddenly clear to him. It must be a blood gem. The implications of that hit him like a slap. 'You were in communication with him all along!'")

text<-as.data.frame(text)

tidy_text <- text %>% ungroup() %>% unnest_tokens(word, text)

bing_text<-tidy_text %>% inner_join(get_sentiments("bing"))

bing_text %>% group_by(sentiment) %>% count()

afinn_text<-tidy_text %>% inner_join(get_sentiments("afinn"))

nrc_text<-tidy_text %>% inner_join(get_sentiments("nrc"))

loughran_text<-tidy_text %>% inner_join(get_sentiments("loughran"))
