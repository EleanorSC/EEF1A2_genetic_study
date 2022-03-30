# Convert transcribed interviews into  a text corpus 
# n.b a corpus is a collection of text document(s) to apply text mining or NLP routines on
# Then cleaning and stemming the data before performing analysis

install.packages("tidyverse")
library(tidyverse)

install.packages("tidytext")
library(tidytext)

# For Interview 1, Years since diagnosis = 1 year 

RawTranscriptData <- read.table("Transcript1.txt", 
                                header = FALSE, 
                                fill = TRUE, 
                                encoding = "UTF-8")

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
                                        "Erm", "like", "what", "Just", "just"))

tokens_clean <- tokens_clean %>% 
  anti_join(unique_stopwords, by = "word")


Interview1_tidytext <- tokens_clean %>%
  count(word, sort = TRUE)

##
#### 2nd interview
RawTranscriptData <- read.table("Transcript2.txt", header = FALSE, fill = TRUE, encoding = "UTF-8")
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

Interview2_tidytext <- tokens_clean %>%
  count(word, sort = TRUE)

####### interview 3
##3rd interview
#### 2nd interview
RawTranscriptData <- read.table("Transcript3.txt", header = FALSE, fill = TRUE, encoding = "UTF-8")
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

Interview3_tidytext <- tokens_clean %>%
  count(word, sort = TRUE)

##


library(tidyr)

frequency <- bind_rows(mutate(Interview1_tidytext, author = "Interview1"),
                       mutate(Interview2_tidytext, author = "Interview2"),
                       mutate(Interview3_tidytext, author = "Interview3")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, 
              values_from = proportion) %>%
  pivot_longer("Interview2":"Interview3",
               names_to = "author", 
               values_to = "proportion")

### plot

library(scales)


frequency <- read.csv("word_freq.csv")
ggplot(frequency, 
       aes(x = proportion, y = Interview1, 
                      color = abs(Interview1 - proportion)
                      )) +
  
  geom_abline(color = "gray40", lty = 2) +
  
  geom_point(alpha = 0.2, 
             size = 2.5) +
  
  geom_jitter(alpha = 0.2, 
              size = 2.5, 
              width = 0.3, 
              height = 0.3) +
  
  geom_text(aes(label = word), 
            #check_overlap = TRUE, 
            vjust = 2.5, 
            size = 2.8) +
  
  
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  
  scale_color_gradient(
                      limits = c(0, 0.001), 
                       low = "darkslategray4", 
                       high = "gray75") +
  
  facet_wrap(~author, 
             ncol=3
             #nrow=1
             ) +
  theme(legend.position="none") +
  labs(y = "Interview1", 
       x = NULL)






