library(tidytext) #tidytext package contains sentiments lexicons
get_sentiments("bing")
library(janeaustenr)
library(stringr) #stringr package lets us use the pipe operator(%>%) 
tidy_data<- austen_books()%>%group_by(book)%>% #austen_books() gets a tibble of  books
  mutate(linenumber=row_number(),
         chapter=cumsum(str_detect(text,regex("^chapter [\\divxcl]", #\\d includes all decimal digits
                                              ignore_case = T))))%>% #ignorecase considers both small and big case
ungroup()%>%
  unnest_tokens(word,text)