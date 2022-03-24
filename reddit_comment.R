# IMPORTING NECESSARY MODULES
install.packages('tidyverse')
library(tidytext)
library(tidyverse)
library(tibble)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(reshape2)
library(wordcloud)
library(igraph)
library(widyr)
library(ggraph)
library(ngram)
library(wordcloud2)
library(stringr)
library(ggplot2)
library(tm)
library(wordcloud)
library(knitr)
library(kableExtra)
library(data.table)
library(lubridate)

install.packages("wordcloud")
library(wordcloud)


#setwd("C:\\CIS8392\\reddit")
setwd("E:/Aparnasree/courses/Rcourse_topics_3personone/Project/worldnews")

# READING THE FILE

df <- fread("worldnews_test_3.csv")
glimpse(df)

# CLEANING

# Dropping Link, domain, URL and structure as these columns does not play any role in analyzing comments and further predicting upvotes
df$link <- NULL
df$domain <- NULL
df$URL <- NULL
df$structure <- NULL


# Format time
df$post_date <- dmy(df$post_date)


# Creating seperate columns for POST's Year, Months and Date which will be used later in visualizations
df$post_day <- wday(df$post_date,label = T)

df$post_month <- month(df$post_date,abbr = T,label = T)

df$post_year <- year(df$post_date)

head(df)

# Converting upvote_prop to percentage by multiplying the whole column by 100
df$upvote_prop <- df$upvote_prop*100

# Changing the upvote_prop to a more descriptive name like "upvote_pct"
names(df)[names(df) == 'upvote_prop'] <- 'upvote_pct'

# Lets check for any Null values
colSums(is.na(df))

# We don't have any NULL values but we have inappropriate data in comment column i.e [deleted] which is not serving any purpose, let us delete those columns 
df <- df[df$comment != "[deleted]"]
head(df)

summary(df)



# VISUALIZATIONS

# Post by Year
ggplot(df[,.N,by=post_year],aes(x = post_year,y = N,fill=N, label=round(N,2)))+
  geom_bar(stat = "identity")+labs(title="Posts by year",subtitle="Number of Posts")+xlab("Year")+ylab(NULL)+geom_text(size=5, vjust=1, color="white")

# It can be seen, the highest number of posts are in the year 2020 followed by 2016. In contrast to 2013 which has seen the lowest number of posts

# Post by Month

ggplot(df[,.N,by=post_month],aes(x = post_month,y = N,fill=N, label=round(N,2)))+
  geom_bar(stat = "identity")+labs(title="Posts by month",subtitle="Number of Posts")+xlab("Month")+ylab(NULL)+geom_text(size=5, vjust=1, color="white")

# As is evident, January month dominates the number of posts 
# Its interesting to see the pattern i.e Number of posts is maintained ~950 for the months of February, April, May and June while for the rest of the months its ~500
# This will be really useful from business perspective, as now companies knows that the first month of new year i.e January is the month for highest number of posts, hence companies can launch and advertise their products during this peak time  

# Posts by weekdays

ggplot(df[,.N,by=post_day],aes(x = post_day,y = N,fill=N,label=round(N,2)))+
  geom_bar(stat = "identity")+labs(title="Posts by day",subtitle="Number of Posts")+xlab("Day")+ylab(NULL)+geom_text(size=5, vjust=1, color="white")

# Its interesting to see the highest number of posts that has been posted is on a weekday i.e Tuesday followed by Monday and then Sunday 


# Top 20 authors

# TOP 20 Authors - By number of posts
df[,.N,by=author][order(-N)][1:20]

# TOP 20 Authors - By post score
df[,.("Post_Score"=sum(post_score,na.rm=T)),by=author][order(-Post_Score)][1:20]



#Replacing the URLS
# df_data <- df %>% mutate(comment = str_replace_all(comment, "https?://[A-Za-z0-9./]+", ""))

df_data <- df %>% mutate(comment = str_replace_all(comment, "//^(?:http(?:s)?:\\//\\//)?(?:[^\\.]+\\.)?example\\.com(\\//.*)?$", ""))


#Replacing HTML tags
df_data <- df %>% mutate(comment = str_replace_all(comment, "(<br />)+", ""))


#Remove # in hashtags
df_data <- df %>% mutate(comment = str_replace_all(comment, "#([^\\s]+)", "\1"))

#Remove punctuations,numbers and special characters
df_data <- df %>% mutate(comment = str_replace_all(comment, "[^a-zA-Z#]", ""))

#Remove punctuations,numbers and special characters
df_data <- df %>% mutate(comment = str_replace_all(comment, "^[a-zA-Z0-9]*$", ""))

#df_data_orginal <- df_data

get_cleaned_tokens <- function(df_data,redditname) {
  if (redditname == 'all') {
    df_data <- df_data
  } else {
    df_data <- subset(df_data,subreddit == redditname)
  }

  tokens <- df_data %>% unnest_tokens(output = word, input = comment)
  tokens %>%  count(word,sort = TRUE)
  
  #get_stopwords()
  #get stop words
  sw = get_stopwords()
  #cleaned_tokens <- tokens %>%  anti_join(get_stopwords())
  cleaned_tokens <- tokens %>%  filter(!word %in% sw$word)

    nums <- cleaned_tokens %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
  #head(nums)
  cleaned_tokens <- cleaned_tokens %>%   anti_join(nums, by = "word")
  #head(cleaned_tokens)

  cleaned_tokens %>%
    count(word, sort = T) %>%
    rename(word_freq = n) %>%
    ggplot(aes(x=word_freq)) +
    geom_histogram(aes(y=..count..), color="black", fill="blue", alpha=0.3) +
    scale_x_continuous(breaks=c(0:5,10,100,500,10e3), trans="log1p", expand=c(0,0)) +
    scale_y_continuous(breaks=c(0,100,1000,5e3,10e3,5e4,10e4,4e4), expand=c(0,0)) +
    theme_bw()
  
  rare <- cleaned_tokens %>%   count(word) %>%  filter(n<10) %>%  select(word) %>% unique()
  head(rare)
  
  rare <- cleaned_tokens %>%
    count(word) %>%
    filter(n<10) %>%
    select(word) %>% unique()

  alpha_remove <- cleaned_tokens %>% filter(str_detect(word, "^[â|s|t|r|gt|http]$")) %>%   select(word) %>% unique()
  
  
  cleaned_tokens <- cleaned_tokens %>%
    filter(!word %in% rare$word)
  length(unique(cleaned_tokens$word))
  
  cleaned_tokens <- cleaned_tokens %>% filter(!word %in% alpha_remove$word)
  return(cleaned_tokens)
  }

#Word cloud for world news
cleaned_tokens = get_cleaned_tokens(df_data,'worldnews')
# define a nice color palette
pal <- brewer.pal(8,"Dark2")
# plot the 100 most common words
cleaned_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))

#Word cloud for AskReddit
cleaned_tokens = get_cleaned_tokens(df_data,'AskReddit')
# define a nice color palette
pal <- brewer.pal(8,"Dark2")
# plot the 100 most common words
cleaned_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))

#Word cloud for bangtan
cleaned_tokens = get_cleaned_tokens(df_data,'bangtan')
# define a nice color palette
pal <- brewer.pal(8,"Dark2")
# plot the 100 most common words
cleaned_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))

#Word cloud for NintendoSwitch
cleaned_tokens = get_cleaned_tokens(df_data,'NintendoSwitch')
# define a nice color palette
pal <- brewer.pal(8,"Dark2")
# plot the 100 most common words
cleaned_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))

# getting word cloud for 'all' so that sentiment analysis will work on everything
cleaned_tokens = get_cleaned_tokens(df_data,'all')
# define a nice color palette
pal <- brewer.pal(8,"Dark2")
# plot the 100 most common words
cleaned_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))


#Sentiment analysis 

get_sentiments("nrc")
get_sentiments("afinn")


sent_reviews = cleaned_tokens %>%   
  left_join(get_sentiments("nrc")) %>%  
  rename(nrc = sentiment) %>%  
  left_join(get_sentiments("bing")) %>%  
  rename(bing = sentiment) %>%  
  left_join(get_sentiments("afinn")) %>%  
  rename(afinn = value)
head(sent_reviews,5)

#Most common positive and negative words
bing_word_counts <- sent_reviews %>%  
  filter(!is.na(bing)) %>%  
  count(word, bing, sort = TRUE)
head(bing_word_counts,5)


bing_word_counts %>%  
  filter(n > 700) %>% 
  mutate(n = ifelse(bing == "negative", -n, n)) %>%  
  mutate(word = reorder(word, n)) %>%  
  ggplot(aes(word, n, fill = bing)) +  geom_col() +  coord_flip() +  labs(y = "Contribution to sentiment")

head(df_data,5)
head(df,5)


####
#From the above graph we see that the words like good, like, love, sweet, great are positive words and the words like bitter, dark, nasty, horrible and hate are negative words.
#Tokenizing by n-grams 



df_data$comment<-as.character(df_data$comment)

bigrams <- df_data %>%  unnest_tokens(bigram, comment, token = "ngrams", n = 2)



#Most common bigrams

bigrams %>%  count(bigram, sort = TRUE)


#Filtering by n-grams


bigrams_separated <- bigrams %>%  
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%  
  filter(!word1 %in% stop_words$word) %>%  
  filter(!word2 %in% stop_words$word)

bigrams_filtered %>%   count(word1, word2, sort = TRUE)


bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")



bigram_counts <- bigram_united %>% 
  count(bigram, sort = TRUE)


# Visualizing top 10 bigram words

bigram_counts %>% arrange(desc(n))%>% head(10)%>%ggplot(aes(x=factor(bigram,levels=bigram),y=n))+geom_bar(stat="identity",fill="#003E45")+labs(title="Top 10 bigram words")+coord_flip()

# Document term matrix

word_counts_by_doc_id <- cleaned_tokens %>%  
  group_by(id) %>%  
  count(word, sort = TRUE)

review_dtm <- word_counts_by_doc_id %>%  
  cast_dtm(id, word, n)

review_dtm

inspect(review_dtm)


# tf-idf


tfidf <- word_counts_by_doc_id %>%  
  bind_tf_idf(word, id, n) 

tfidf



#Looking at  tf-idf we can understand the words that are important for the particular document but not in the corpus

top_tfidf = tfidf %>%  
  group_by(id) %>%  
  arrange(desc(tf_idf)) %>%  
  top_n(3) %>% ungroup() %>%  
  arrange(id, -tf_idf)

top_tfidf

# Word correlation 

uncommon <- cleaned_tokens %>%   
  count(word) %>%  
  filter(n<5000) %>%   
  select(word) %>% unique()



word_cor = cleaned_tokens %>%   
  anti_join(uncommon, by = "word") %>%  
  widyr::pairwise_cor(word, id) %>%  
  filter(!is.na(correlation),         
         correlation > .25)

head(word_cor)


# Visulization of corelated words

word_cor %>%  
  graph_from_data_frame() %>%  
  ggraph(layout = "fr") +  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) + 
  geom_node_point(color = "green") +  geom_node_text(aes(label = name), repel = TRUE) + 
  theme_void()










