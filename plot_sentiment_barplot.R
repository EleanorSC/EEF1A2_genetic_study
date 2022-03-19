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

unique_stopwords <- data.frame(word = c("erm", "err", "Err", "buggy", "noises", "dust", "cold", "leak",
                                        "Yes", "yes", "and", "thing", "Thing", "day", "Day", "get", "Get", 
                                        "interviewer", "P16", "p16", "emma", "bit",
                                        "something", "Something", "sometimes", "Sometimes", "can", 
                                        "Can", "And", "Yeah", "yeah", "Emma's", "emma's",
                                        "Emma", "Sarah", "Andrew", "Interviewer", 
                                        "P16", "Um", "um", "okay", "Okay", "transcript", "sarah",
                                        "Erm", "like", "what", "Just", "just"))

tokens_clean <- tokens_clean %>% 
  anti_join(unique_stopwords, by = "word")

# get negative vs positive sentiments

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