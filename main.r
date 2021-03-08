#library(tidytext);
#library(janeaustenr);

library(janeaustenr)
library(stringr)
library(tidyr)
library(tibble)
library(dplyr)
library(ggplot2)


ID <- c(1:50)

## Creating sex variable (25 males/25 females)
Sex <- rep(c("male", "female"), 25) # rep stands for replicate

## Creating age variable (20-39 year olds)
Age <- c(26, 25, 39, 37, 31, 34, 34, 30, 26, 33, 
         39, 28, 26, 29, 33, 22, 35, 23, 26, 36, 
         21, 20, 31, 21, 35, 39, 36, 22, 22, 25, 
         27, 30, 26, 34, 38, 39, 30, 29, 26, 25, 
         26, 36, 23, 21, 21, 39, 26, 26, 27, 21) 

## Creating a dependent variable called Score
Score <- c(0.010, 0.418, 0.014, 0.090, 0.061, 0.328, 0.656, 0.002, 0.639, 0.173, 
           0.076, 0.152, 0.467, 0.186, 0.520, 0.493, 0.388, 0.501, 0.800, 0.482, 
           0.384, 0.046, 0.920, 0.865, 0.625, 0.035, 0.501, 0.851, 0.285, 0.752, 
           0.686, 0.339, 0.710, 0.665, 0.214, 0.560, 0.287, 0.665, 0.630, 0.567, 
           0.812, 0.637, 0.772, 0.905, 0.405, 0.363, 0.773, 0.410, 0.535, 0.449)

## Creating a unified dataset that puts together all variables
data <- tibble(ID, Sex, Age, Score);
# print(data)
# group_by(data)


# library(janeaustenr)
# library(dplyr)
# library(stringr)



# original_books <- austen_books() %>%
#   group_by(book) %>%
#   mutate(line = row_number(),
#          chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
#                                                  ignore_case = TRUE)))) %>%
#   ungroup()

# print(original_books)

# print(colnames(original_books))

# print(group_by(austen_books(),text))

tidy_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>% 
  unnest_tokens(word, text)

#print(tidy_data)

possentiment<-get_sentiments("bing") %>% filter(sentiment=="positive");
#print(tidy_data %>% filter(book=='Emma'))

#print(tidy_data %>% filter(book=='Emma') %>% semi_join(possentiment) %>% count(word,sort = TRUE))


bing<-get_sentiments(lexicon = "bing");
emmasent<-tidy_data %>% inner_join(bing) %>% count(book="Emma",index=linenumber %/% 80,sentiment) %>% 
  spread(sentiment,n,fill=0) %>% mutate(sentiment=positive-negative);


#print(emmasent);


barplt<-ggplot(emmasent,aes(index,sentiment,fill=book))+geom_bar(stat = "identity",show.legend = TRUE)+
  facet_wrap(~book,ncol=2,scales = "free_x");

countoword<-tidy_data %>% inner_join(bing) %>% count(word,sentiment,sort = TRUE);
print(head(countoword))





split<-countoword %>% filter(n>150) %>% mutate(n=ifelse(sentiment=="negative",-n,n)) %>% mutate(word = reorder(word, n))

grapsplit<-ggplot(split,aes(word,n,fill=sentiment))+geom_col()+coord_flip()+labs(y="sentiment scr");
print(split);
print(grapsplit);






# data %>% group_by(Sex, Age) %>%     # grouped by Sex and Age
#   summarize(m = mean(Score),
#             s = sd(Score),   
#             n = n())

#print(austen_books())

