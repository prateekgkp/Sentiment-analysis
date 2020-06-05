library(tidytext) #tidytext package contains sentiments lexicons
get_sentiments("bing")
library(janeaustenr)
library(stringr) #stringr package lets us use the pipe operator(%>%) 
tidy_data<- austen_books()%>%group_by(book)%>% #austen_books() to gets a tibble of  books
  mutate(linenumber=row_number(),
         chapter=cumsum(str_detect(text,regex("^chapter [\\divxcl]", #\\d includes all decimal digits
                                              ignore_case = T))))%>% #ignorecase considers both small and big case
ungroup()%>%
  unnest_tokens(word,text)
positive_senti<- get_sentiments("bing")%>%filter(sentiment=="positive") #creating a tibble of all the 
#positive words from bing sentiment
positivetidy_data<- tidy_data%>%filter(book=="Sense & Sensibility")%>%semi_join(positive_senti)%>%count(word,sort=T) #filter the 
# tidy data created by selecting only positive words from chapter sense and sensibility
library(tidyr)
bing <- get_sentiments("bing")
Senseandsensibility_sentiment<- tidy_data %>% inner_join(bing)%>%
count(book = "Sense & Sensibility" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
library(ggplot2)
ggplot(Senseandsensibility_sentiment, aes(index, sentiment, fill = book)) #visualize the words
#present in the book “Sense & Sensibility” based on their corrosponding positive and negative scores.
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
  
  counting_words <- tidy_data %>% #counting the most common 
    #positive and negative words that are present in the novel.
    inner_join(bing) %>%
    count(word, sentiment, sort = TRUE)
  head(counting_words)
  
  counting_words %>%  #plot the scores along the 
    #axis that is labeled with both positive as well as negative words
    filter(n > 150) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment))+
    geom_col() +
    coord_flip() +
    labs(y = "Sentiment Score")  
  
  
  