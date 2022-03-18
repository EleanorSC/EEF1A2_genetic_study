## ---------------------------
##
## Notes: Initial exploratory analysis of EEF1A2 interview transcript data
##        Takes transcript data and manipulates with tidy tools
##        
##        
##   
##
## Packages: 
##           
##   
##   
##           
## ---------------------------

install.packages("tidytext")
install.packages("tidyverse")
install.packages("ggwordcloud")

# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

library(tidytext)
library(tidyverse)


# Convert transcribed interviews into  a text corpus 
# n.b a corpus is a collection of text document(s) to apply text mining or NLP routines on
# Then cleaning and stemming the data before performing analysis

RawTranscriptData <- read.table("Transcript1.txt", header = FALSE, fill = TRUE, encoding = "UTF-8")
print(head(RawTranscriptData), 1)

#How many unique words do we have?

tidy_transcript <- gather(RawTranscriptData, key, word) %>% 
  select(word)

#checks how many unique words there are in total
# output [1] 1225

unique(tidy_transcript$word) %>%
  length()

# Tidying the data
# unnest_tokens(word, word) transforms our data to be one word per row. 
# Also strips punctuation. count(word, sort = TRUE) counts up the total amount of each word,
# sort = TRUE then places them in order. ungroup() count has grouped the data by word,
# we have now ungrouped so tokens is not grouped by word.

tokens <- tidy_transcript %>% 
  unnest_tokens(word, word) %>% 
  count(word, sort = TRUE) %>% 
  ungroup()

# Exploring what words are most frequent
top_10 <- tokens %>% 
  head(10)

knitr::kable(top_10, caption = "Top ten all words table")


# removing stop words with built in tidytext package
data("stop_words")
tokens_clean <- tokens %>%
  anti_join(stop_words)

# removing numbers
nums <- tokens_clean %>% 
  filter(str_detect(word, "^[0-9]")) %>% 
  select(word) %>% 
  unique()

tokens_clean <- tokens_clean %>% 
  anti_join(nums, by = "word")

unique_stopwords <- data.frame(word = c("erm", "err", "Err", "bugggy",
                                        "Yes", "yes", "and", "thing", "Thing", "day", "Day", "get", "Get", 
                                        "interviewer", "P16", "p16", "emma", "bit",
                                        "something", "Something", "sometimes", "Sometimes", "can", 
                                        "Can", "And", "Yeah", "yeah", "Emma's", "emma's",
                                        "Emma", "Sarah", "Andrew", "Interviewer", 
                                        "P16", "Um", "um", "okay", "Okay", "transcript", "sarah",
                                        "Erm", "like", "what", "Just", "just"))

tokens_clean <- tokens_clean %>% 
  anti_join(unique_stopwords, by = "word")

top_10_clean <- tokens_clean %>% 
  head(10) 

knitr::kable(top_10_clean, caption = "Top ten clean table")

# wordcloud plot
# need ggwordcloud
library(ggwordcloud)

#set.seed(42)
wordcloudplot <- print(head(tokens_clean, 50) %>%
        ggplot(aes(label = word, 
                   size = n, 
                   color = word)) +
        geom_text_wordcloud_area(shape = "diamond") +
        scale_size_area(max_size = 15) +
        theme_minimal() +
        ggtitle("Interview 1 wordcloud"))

wordcloudplot


# Sentiment matching
install.packages("textdata")
library(textdata)

nrc_words <- tokens_clean %>%
  inner_join(get_sentiments("nrc"), by = "word")

nrc_words 

nrc_words %>%
  group_by(sentiment) %>%
  tally %>%
  arrange(desc(n))

nrc_words %>%
  count %>%
  pull


cloud <- tokens_clean %>%
  inner_join(get_sentiments("bing"), by = "word")  %>%
count(word, sentiment) %>% 
  reshape2::acast(word~sentiment, 
                  value.var = "n", 
                  fill = 0) %>% 
  comparison.cloud(scale=c(0.8,
                           0.5),
                   color = c("#1b2a49", "#00909e"),
                   max.words = 100)
cloud


####
stem <- tokens_clean %>%
  mutate(stem = hunspell::hunspell_stem(word)) %>%
  unnest(stem) %>%
  count(stem, 
        sort = TRUE)

stem <- rename(stem, 
       word = stem)

# Graph of negative and positive sentiments
negative_positive <- 
  stem %>%
  # add sentiment scores to words
  left_join(get_sentiments("bing"), 
            by = "word") %>%
  arrange(sentiment)
##

# Graph of all sentiments
negative_positive <-
  stem %>%
  # add sentiment scores to words
  left_join(get_sentiments("afinn"),
            by = "word") %>%
  arrange(value) %>%
  left_join(get_sentiments("bing"),
            by = "word")

##


# PLOT of sentiments
afinn_words <- tokens_clean %>%
  inner_join(get_sentiments("afinn"), by = "word")

bing_words <- tokens_clean %>%
  inner_join(get_sentiments("bing"), 
             by = "word") %>%
  group_by(sentiment)

##
data <- 
  bing_words %>%
  #top_n(20) %>%
  group_by(sentiment)  %>%
  arrange(desc(n)) %>%
  arrange(sentiment)%>%
  ggplot(aes(reorder(word, n), 
             n, 
             fill=sentiment
             )
         ) +
  geom_bar(stat="identity", 
           show.legend = FALSE) +
  
  facet_wrap(~sentiment, 
             scales="free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()
data

######## postiive negative circle
Words_sentiment_plot <- 
  bing_words %>%
  group_by(sentiment)  %>%
  arrange(desc(n)) %>%
  arrange(sentiment) %>%
  ungroup() %>%
  ggplot(aes(word, 
             1, 
             label = word, 
             fill = sentiment,
             colour = factor(sentiment),
             alpha = n)) +
  coord_polar(theta = "x") +
  geom_point(size =2) +
             #color = "transparent") +
  scale_color_discrete(name = "sentiment")
  
Words_sentiment_plot



##
Words_sentiment_plot <- 
  bing_words %>%
  group_by(sentiment)  %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  ggplot(aes(word, 1, label = word, 
             fill = sentiment,
             alpha = n)) +
  coord_polar(theta = "x") +
  geom_point(color = "transparent") +
  #geom_jitter() +
  ggrepel::geom_label_repel(force = 1,
                            nudge_y = 1.5,
                            nudge_x = 2.5,
                            direction = "both",
                            box.padding = 0.04,
                            segment.color = "transparent",
                            size = 3) +
  #facet_grid(~sentiment) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Interview 1 sentiment towards EEFA12 diagnosis") +
  coord_flip()
Words_sentiment_plot
##

##
Words_sentiment_plot <- 
 # nrc_words %>%
 # affin_words %>%
  bing_words %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  #slice(seq_len(8)) %>% #consider top_n() from dplyr also
  ungroup()

Words_sentiment_plot  %>%
  #Set `y = 1` to just plot one variable and use word as the label
  ggplot(aes(word, 1, label = word, 
             fill = sentiment,
             alpha = n
             )) +
  #You want the words, not the points
  geom_point(color = "transparent") +
  #Make sure the labels don't overlap
  ggrepel::geom_label_repel(force = 1,nudge_y = .7,  
                   direction = "y",
                   box.padding = 0.04,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(~sentiment) +
  #theme_lyrics() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Interview 1 sentiment towards EEFA12 diagnosis") +
  coord_flip()
##


negative_positive <- 
  tokens_clean %>%
  # add sentiment scores to words
  left_join(get_sentiments("bing"), by = "word") %>%
  # count number of negative and positive words
  count(sentiment) %>%
  spread(key = sentiment, value = n) %>%
  ungroup %>%
  # create centered score
  mutate(sentiment = positive - negative - 
           mean(positive - negative)) 

==

# visualise
install.packages("ggridges")
library(ggridges)

sentiment_test <- read.csv("sentiment_test.csv")

sentiment_test  %>%
  count %>%
  pull











sentiment_test$sentiment<- as.factor(sentiment_test$sentiment)

sentiment_test$diagnosis <- as.factor(sentiment_test$diagnosis)

sentiment_test %>%
  group_by(sentiment) %>%
  tally %>%
  arrange(desc(n))

ggplot(sentiment_test) +
  geom_joy(aes(
    x = reorder(sentiment, n),
    y = diagnosis, 
    fill = reorder(sentiment, n)),
    rel_min_height = 0.01,
    alpha = 0.7,
    scale = 3) +
  theme_joy() +
  labs(title = "#rstats sentiment analysis",
       x = "Years since diagnosis of EEF1A2",
       y = "Sentiment") 


#
#for (shape in c("diamond"
#)) {
#  set.seed(42)
#  print(head(tokens_clean, 50) %>%
#    ggplot(aes(label = word, 
#               size = n, 
#               color = word)) +
#          geom_text_wordcloud_area(shape = "diamond") +
#          scale_size_area(max_size = 15) +
#          theme_minimal() 
#    + ggtitle("Interview 1 wordcloud"))
#}
#
#
tokens <- dtm_d %>% 
  unnest_tokens(word, word) %>% 
  count(word, sort = TRUE) %>% 
  ungroup()

top_10 <- tokens %>% 
  head(10)

knitr::kable(top_10, caption = "Top ten all words table")



lotr %>%
  # split text into words
  unnest_tokens(word, text) %>%
  # remove stop words
  anti_join(stop_words, by = "word") %>%
  # add sentiment scores to words
  left_join(get_sentiments("bing"), by = "word") %>%
  # count number of negative and positive words
  count(chapter, book, sentiment) %>%
  spread(key = sentiment, value = n) %>%
  ungroup %>%
  # create centered score
  mutate(sentiment = positive - negative - 
           mean(positive - negative)) %>%
  select(book, chapter, sentiment) %>%
  # reorder chapter levels
  mutate(chapter = factor(as.character(chapter), 
                          levels = levels(chapter)[61:1])) %>%
  # plot
  ggplot(aes(x = chapter, y = sentiment)) + 
  geom_bar(stat = "identity", aes(fill = book)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  coord_flip() + 
  ylim(-250, 250) +
  ggtitle("Centered sentiment scores", 
          subtitle = "Transcript interviews")



############ Approach 1
# https://www.red-gate.com/simple-talk/databases/sql-server/bi-sql-server/text-mining-and-sentiment-analysis-with-r/

# Read the text file from local machine , choose file interactively
text <- readLines(file.choose())

# Load the data as a corpus
TextDoc <- Corpus(VectorSource(text))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("erm", "Yes", "yes", "and", "thing", "Thing", "day", "Day", "get", "Get", 
                                          "something", "Something", "sometimes", "Sometimes", "can", 
                                          "Can", "And", "Yeah", "yeah", 
                                          "Emma", "Sarah", "Andrew", "Interviewer", 
                                          "P16", "Um", "um", "okay", "Okay", "transcript", "sarah",
                                          "Erm", "like", "what", "Just", "just"
                                        )) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)

# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

# Display the top 5 most frequent words
head(dtm_d, 5)

##generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))
