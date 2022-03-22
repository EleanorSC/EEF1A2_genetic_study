# Sentiment analysis by questions
# Convert transcribed interviews into  a text corpus 
# n.b a corpus is a collection of text document(s) to apply text mining or NLP routines on
# Then cleaning and stemming the data before performing analysis


# For Interview 1, Years since diagnosis = 1 year 

RawTranscriptData <- read.table("T1_Q1.txt", header = FALSE, fill = TRUE, encoding = "UTF-8")
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

#knitr::kable(top_10, caption = "Top ten all words table")

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

unique_stopwords <- data.frame(word = c("erm", "err", "Err", "buggy", "noises", "noise", "funny", "dust", "cold", "leak",
                                        "Yes", "yes", "and", "thing", "Thing", "day", "Day", "get", "Get", 
                                        "interviewer", "P16", "p16", "emma", "bit",
                                        "something", "Something", "sometimes", "Sometimes", "can", 
                                        "Can", "And", "Yeah", "yeah", "Emma's", "emma's",
                                        "Emma", "Sarah", "Andrew", "Interviewer", 
                                        "P16", "Um", "um", "okay", "Okay", "transcript", "sarah",
                                        "Erm", "like", "what", "Just", "just",
                                        "wow"))

tokens_clean <- tokens_clean %>% 
  anti_join(unique_stopwords, by = "word")

# add additional sentiments not found
additional_sentiment <- tibble(word=c("hope","acceptance", "alleviate", "answers"),
                               n=c(5,3,4,3),
                               sentiment=c("positive","positive", "positive", "positive"))

# get TOP 10 negative vs positive sentiments


bing_words_interview1 <- tokens_clean %>%
  inner_join(get_sentiments("bing"), 
             by = "word") %>%
  rbind(additional_sentiment) %>%
  arrange(desc(n)) %>%
  group_by(sentiment) %>%
  do(head(.,10))

# Make negatives negative
# try slicing 
bing_words_interview1$n[bing_words_interview1$sentiment == 'negative'] = 
  bing_words_interview1$n[bing_words_interview1$sentiment == 'negative']*-1


# add a column to demarcate which interview this is
bing_words_interview1$Interview <- 1
bing_words_interview1$Question <- "Impact of EEF1A2 diagnosis"


#### 2nd interview
RawTranscriptData <- read.table("T2_Q1.txt", header = FALSE, fill = TRUE, encoding = "UTF-8")
print(head(RawTranscriptData), 1)

#How many unique words do we have?

tidy_transcript <- gather(RawTranscriptData, key, word) %>% 
  select(word)

#checks how many unique words there are in total
# output [1] 1523

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

#knitr::kable(top_10, caption = "Top ten all words table")

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

unique_stopwords <- data.frame(word = c("erm", "err", "Err", "buggy", "noises", "noise", "funny", "dust", "cold", "leak",
                                        "Yes", "yes", "and", "thing", "Thing", "day", "Day", "get", "Get", 
                                        "interviewer", "p05", "jack", "bit",
                                        "something", "Something", "sometimes", "Sometimes", "can", 
                                        "Can", "And", "Yeah", "yeah", "Jack's", "jack's",
                                        "Jack", "sniffs", "Andrew",
                                        "gruffalo", "Interviewer", 
                                        "P05", "Um", "um", "okay", "Okay", "transcript", "sarah",
                                        "Erm", "like", "what", "Just", "just"))

tokens_clean <- tokens_clean %>% 
  anti_join(unique_stopwords, by = "word")

# get negative vs positive sentiments

bing_words_interview2 <- tokens_clean %>%
  inner_join(get_sentiments("bing"), 
             by = "word") %>%
  rbind(additional_sentiment) %>%
  arrange(desc(n)) %>%
  group_by(sentiment) %>%
  do(head(.,10))


# Make negatives negative
# try slicing 
bing_words_interview2$n[bing_words_interview2$sentiment == 'negative'] = 
  bing_words_interview2$n[bing_words_interview2$sentiment == 'negative']*-1


# add a column to demarcate which interview this is
bing_words_interview2$Interview <- 2

##3rd interview
#### 2nd interview
RawTranscriptData <- read.table("T3_Q1.txt", header = FALSE, fill = TRUE, encoding = "UTF-8")
print(head(RawTranscriptData), 1)

#How many unique words do we have?
tidy_transcript <- gather(RawTranscriptData, key, word) %>% 
  select(word)

#checks how many unique words there are in total
# output [1] 1022
unique(tidy_transcript$word) %>%
  length()

# Tidying the data
tokens <- tidy_transcript %>% 
  unnest_tokens(word, word) %>% 
  count(word, sort = TRUE) %>% 
  ungroup()

# Exploring what words are most frequent
top_10 <- tokens %>% 
  head(10)

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

unique_stopwords <- data.frame(word = c("erm", "err", "Err", "buggy", "noises", "noise", "funny", "dust", "cold", "leak",
                                        "Yes", "yes", "and", "thing", "Thing", "day", "Day", "get", "Get", 
                                        "interviewer", "p09", "neul", "bit",
                                        "something", "Something", "sometimes", "Sometimes", "can", 
                                        "Can", "And", "Yeah", "yeah", "Neil's", "neil's", "neil",
                                        "Neil", "sniffs", "spain",
                                        "Interviewer", 
                                        "P09", "Um", "um", "okay", "Okay", "transcript", "sarah",
                                        "Erm", "like", "what", "Just", "just"))

tokens_clean <- tokens_clean %>% 
  anti_join(unique_stopwords, by = "word")

# add additional sentiments not found
additional_sentiment <- tibble(word=c("emotional","alleviate", "acceptance", "answers"),
                               n=c(5,4,4,6),
                               sentiment=c("negative","positive", "positive", "positive"))

bing_words_interview3 <- tokens_clean %>%
  inner_join(get_sentiments("bing"), 
             by = "word") %>%
  rbind(additional_sentiment) %>%
  arrange(desc(n)) %>%
  group_by(sentiment) %>%
  do(head(.,10))


# Make negatives negative
# try slicing 
bing_words_interview3$n[bing_words_interview3$sentiment == 'negative'] = 
  bing_words_interview3$n[bing_words_interview3$sentiment == 'negative']*-1


# add a column to demarcate which interview this is
bing_words_interview3$Interview <- 3
####

test <- rbind(bing_words_interview1, 
              bing_words_interview2,
              bing_words_interview3)

test <- test %>%
  unite(group, sentiment:Interview, remove = FALSE)

data <- 
  test %>%
  group_by(sentiment)  %>%
  arrange(desc(n)) %>%
  arrange(sentiment)%>%
  ggplot(aes(reorder(word, n, sum), 
             n, 
             fill=group)) +
  geom_bar(stat="identity",
          # alpha =0.8,
           colour="white",
           show.legend = TRUE) +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  #scale_fill_brewer(palette = "Spectral")
  scale_fill_manual(values = c(
    "indianred2",
    "cornflowerblue",
    "#7D1D67",#negative3
    "indianred2",
    "cornflowerblue",
    "#7D1D67"#positive3
  )) +
  geom_vline(xintercept = 17.5) +

  theme_classic() +
  theme(
    axis.title.x = element_text(
      size = 11,
      face = "bold",
      colour = "black"),
    axis.text.x= element_text(),
    axis.title.y = element_text(
      size = 11,
      face = "bold",
      colour = "black"))
    
data

