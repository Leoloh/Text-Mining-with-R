---
title: "04 Relationships between Words"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(gutenbergr)
library(stringr)
library(devtools)
library(widyr)
```

So far we’ve considered words as individual units, and considered their relationships to sentiments or to documents. However, many interesting text analyses are based on the relationships between words, whether examining which words tend to follow others immediately, or that tend to co-occur within the same documents.

In this project, I will explore some of the methods tidytext offers for calculating and visualizing relationships between words in text dataset. 

## 04_01 Tokenizing by n-gram

I have been using the unnest_tokens function to tokenize by word, or sometimes by sentence, which is useful for the kinds of sentiment and frequency analyses I have been doing so far. But I can also use the function to tokenize into consecutive sequences of words, called n-grams. By seeing how often word X is followed by word Y, I can then build a model of the relationships between them.

```{r}
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams
```

### 04_01_01 Counting and Filtering n-grams

```{r}
austen_bigrams %>%
  count(bigram, sort = TRUE)
```

In the next step, I split a column into multiple based on a delimiter. This lets me separate it into two columns, “word1” and “word2”, at which point I can remove cases where either is a stop-word.

```{r}
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts
```

I see that names (whether first and last or with a salutation) are the most common pairs in Jane Austen books.

For other analyses, I recombine the columns into one. Thus, “separate/filter/count/unite” let us find the most common bigrams not containing stop-words.

```{r}
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united
```

In other analyses the most common trigrams, which are consecutive sequences of 3 words, can be also interesting.

```{r}
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
```

### 04_01_02 Analyzing Bigrams

This one-bigram-per-row format is helpful for exploratory analyses of the text. As a simple example, the most common “streets” mentioned in each book can be interesting:

```{r}
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)
```

A bigram can also be treated as a term in a document in the same way that individual words are treated.

```{r}
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf
```

Much as I discovered in earlier, the units that distinguish each Austen book are almost exclusively names. I also notice some pairings of a common verb and a name, such as “replied elizabeth” in Pride & Prejudice, or “cried emma” in Emma.

There are advantages and disadvantages to examining the tf-idf of bigrams rather than individual words. Pairs of consecutive words might capture structure that isn’t present when one is just counting single words, and may provide context that makes tokens more understandable (for example, “pulteney street”, in Northanger Abbey, is more informative than “pulteney”).

### 04_01_03 Using Bigrams to provide Context in Setiment Analysis

One of the problems with this approach is that a word’s context can matter nearly as much as its presence. For example, the words “happy” and “like” will be counted as positive, even in a sentence like “I’m not happy and I don’t like it!”Now that we have the data organized into bigrams, it’s easy to tell how often words are preceded by a word like “not”:

```{r}
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)
```

By performing sentiment analysis on the bigram data, I can examine how often sentiment-associated words are preceded by “not” or other negating words. I could use this to ignore or even reverse their contribution to the sentiment score.

I use the AFINN lexicon for sentiment analysis with positive or negative numbers indicating the direction of the sentiment.

```{r}
AFINN <- get_sentiments("afinn")

AFINN
```

I can then examine the most frequent words that were preceded by “not” and were associated with a sentiment.

```{r}
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words
```

For example, the most common sentiment-associated word to follow “not” was “like”, which would normally have a (positive) score of 2.

It’s worth asking which words contributed the most in the “wrong” direction. To compute that, I can multiply their score by the number of times they appear (so that a word with a score of +3 occurring 10 times has as much impact as a word with a sentiment score of +1 occurring 30 times). I visualize the result with a bar plot.

```{r}
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()
```

The bigrams “not like” and “not help” were overwhelmingly the largest causes of misidentification, making the text seem much more positive than it is. But I can see phrases like “not afraid” and “not fail” sometimes suggest text is more negative than it is.

I could pick four common words (or more) that negate the subsequent term, and use the same joining and counting approach to examine all of them at once. So I can then visualize what the most common words to follow each particular negation are.

```{r}
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()
```

```{r}
negated_words %>%
  mutate(contribution = n * score,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  top_n(12, abs(contribution)) %>%
  ggplot(aes(word2, contribution, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment score * # of occurrences") +
  coord_flip()
```

While “not like” and “not help” are still the two most common examples, we can also see pairings such as “no great” and “never loved.” We could combine this with the approaches in Chapter 2 to reverse the AFINN scores of each word that follows a negation. These are just a few examples of how finding consecutive words can give context to text mining methods.

### 04_01_04 Visualizing a Network of Bigrams with ggraph

The igraph package has many powerful functions for manipulating and analyzing networks. One way to create an igraph object from tidy data is the graph_from_data_frame() function, which takes a data frame of edges with columns for “from”, “to”, and edge attributes (in this case n):

```{r}
# original counts
bigram_counts
```

```{r}
# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph
```

I can convert an igraph object into a ggraph with the ggraph function, after which I add layers to it, much like layers are added in ggplot2. For example, for a basic graph I need to add three layers: nodes, edges, and text.

```{r}
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
```

I can visualize some details of the text structure. For example, I see that salutations such as “miss”, “lady”, “sir”, “and''colonel” form common centers of nodes, which are often followed by names. I also see pairs or triplets along the outside that form common short phrases (“half hour”, “thousand pounds”, or “short time/pause”).

With a few polishing operations I want to make a better looking graph:

```{r}
set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
```

### 04_01_05 Visualizing Bigrams in other Texts

I went to a good amount of work in cleaning and visualizing bigrams on a text dataset, so I want to collect it into a function so that I easily perform it on other text datasets.

```{r}
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}
```

At this point, I can visualize bigrams in other works, such as the King James Version of the Bible:

```{r}
# the King James version is book 10 on Project Gutenberg:
kjv <- gutenberg_download(10)
```

```{r}
kjv_bigrams <- kjv %>%
  count_bigrams()

# filter out rare combinations, as well as digits
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()
```

## 04_02 Counting and Correlating Pairs of Words with the Widyr Package

Tokenizing by n-gram is a useful way to explore pairs of adjacent words. However, I may also be interested in words that tend to co-occur within particular documents or particular chapters, even if they don’t occur next to each other.

Tidy data is a useful structure for comparing between variables or grouping by rows, but it can be challenging to compare between rows: for example, to count the number of times that two words appear within the same document, or to see how correlated they are. Most operations for finding pairwise counts or correlations need to turn the data into a wide matrix first.

### 04_02_01 Counting and Correlating among Sections

Consider the book “Pride and Prejudice” divided into 10-line sections. I am interested in what words tend to appear within the same section.

```{r}
austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words
```

```{r}
# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs
```

For example, I can see that the most common pair of words in a section is “Elizabeth” and “Darcy” (the two main characters). I can easily find the words that most often occur with Darcy:

```{r}
word_pairs %>%
  filter(item1 == "darcy")
```

### 04_02_02 Pairwise Correlation

Pairs like “Elizabeth” and “Darcy” are the most common co-occurring words, but that’s not particularly meaningful since they’re also the most common individual words. I may instead want to examine correlation among words, which indicates how often they appear together relative to how often they appear separately.

The pairwise_cor() function in widyr lets us find the phi coefficient between words based on how often they appear in the same section. Its syntax is similar to pairwise_count().

```{r}
# we need to filter for at least relatively common words first
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors
```

This lets me pick particular interesting words and find the other words most associated with them.

```{r}
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()
```

Just as I used ggraph to visualize bigrams, I can use it to visualize the correlations and clusters of words that were found by the widyr package.

```{r}
set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
```

Unlike the bigram analysis, the relationships here are symmetrical, rather than directional (there are no arrows). I can also see that while pairings of names and titles that dominated bigram pairings are common, such as “colonel/fitzwilliam”, I can also see pairings of words that appear close to each other, such as “walk” and “park”, or “dance” and “ball”.

## Summary

This project showed how the tidy text approach is useful not only for analyzing individual words, but also for exploring the relationships and connections between words. Such relationships can involve n-grams, which enable me to see what words tend to appear after others, or co-occurences and correlations, for words that appear in proximity to each other. Further, this project also demonstrated the ggraph package for visualizing both of these types of relationships as networks. These network visualizations are a flexible tool for exploring relationships.
