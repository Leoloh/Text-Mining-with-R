---
title: "06 Topic Modeling"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gutenbergr)
library(stringr)
library(ggplot2)
library(mallet)
library(scales)
```

In text mining, we often have collections of documents, such as blog posts or news articles, that we’d like to divide into natural groups so that we can understand them separately. Topic modeling is a method for unsupervised classification of such documents, similar to clustering on numeric data, which finds natural groups of items even when we’re not sure what we’re looking for.

In this project, I will learn to work with Latent Dirichlet allocation (LDA) objects from the topicmodels package, particularly tidying such models so that they can be manipulated with ggplot2 and dplyr. Further, I will also explore an example of clustering chapters from several books, where I can see that a topic model “learns” to tell the difference between the four books based on the text content.

## 06_01 Latent Dirich Allacation

LDA is a mathematical method for estimating both of these at the same time: finding the mixture of words that is associated with each topic, while also determining the mixture of topics that describes each document. There are a number of existing implementations of this algorithm.

The AssociatedPress dataset provided by the topicmodels package, as an example of a DocumentTermMatrix. This is a collection of 2246 news articles from an American news agency, mostly published around 1988.

```{r}
data("AssociatedPress")
AssociatedPress
```

This function returns an object containing the full details of the model fit, such as how words are associated with topics and how topics are associated with documents.

```{r}
# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda
```

### 06_01_01 Word-Topic Probabilities

The tidy() method, originally from the broom package (Robinson 2017), for tidying model objects. The tidytext package provides this method for extracting the per-topic-per-word probabilities, called "beta", from the model.

```{r}
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics
```

Notice that this has turned the model into a one-topic-per-term-per-row format. For each combination, the model computes the probability of that term being generated from that topic. For example, the term “aaron” has a 1.686917×10-12 probability of being generated from topic 1, but a  3.8959408×10-5 probability of being generated from topic 2.

I want to find the 10 terms that are most common within each topic. As a tidy data frame, this lends itself well to a ggplot2 visualization.

```{r}
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
```

This visualization lets me understand the two topics that were extracted from the articles. The most common words in topic 1 include “percent”, “million”, “billion”, and “company”, which suggests it may represent business or financial news. Those most common in topic 2 include “president”, “government”, and “soviet”, suggeting that this topic represents political news. One important observation about the words in each topic is that some words, such as “new” and “people”, are common within both topics. This is an advantage of topic modeling as opposed to “hard clustering” methods: topics used in natural language could have some overlap in terms of words.

As an alternative, I could consider the terms that had the greatest difference in beta between topic 1 and topic 2. 

```{r}
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread
```

Next, I visualize the words with the greatest differences between the two topics.

```{r}
beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()
```

I can see that the words more common in topic 2 include political parties such as "democratic" and "republican", as well as politician's names such as "dukakis" and "gorbachev". Topic 1 was more characterized by currencies like "yen" and "dollar", as well as financial terms such as "index", "prices" and "rates". This helps confirm that the two topics the algorithm identified were political and financial news.

### 06_01_02 Document-Topic Probabilities

Besides estimating each topic as a mixture of words, LDA also models each document as a mixture of topics. I can examine the per-document-per-topic probabilities, called “gamma”.

```{r}
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents
```

Each of these values is an estimated proportion of words from that document that are generated from that topic. For example, the model estimates that only about 24.8% of the words in document 1 were generated from topic 1.

I can see that many of these documents were drawn from a mix of the two topics, but that document 6 was drawn almost entirely from topic 2, having a gamma from topic 1 close to zero. To check this answer, I could tidy() the document-term matrix and check what the most common words in that document were.

```{r}
tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))
```

Based on the most common words, this appears to be an article about the relationship between the American government and Panamanian dictator Manuel Noriega, which means the algorithm was right to place it in topic 2 (as political/national news).

## 06_02 Example: The Great Library Heist

When examining a statistical method, it can be useful to try it on a very simple case where you know the “right answer”. For example, we could collect a set of documents that definitely relate to four separate topics, then perform topic modeling to see whether the algorithm can correctly distinguish the four groups. This lets us double-check that the method is useful, and gain a sense of how and when it can go wrong. I will try this with some data from classic literature.

Suppose a vandal has broken into your study and torn apart four of your books:

- Great Expectations by Charles Dickens
- The War of the Worlds by H.G. Wells
- Twenty Thousand Leagues Under the Sea by Jules Verne
- Pride and Prejudice by Jane Austen

I will retrieve the text of these four books using the gutenbergr package.

```{r}
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")
```

```{r}
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")
```

```{r}
# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts
```

### 06_02_01 LDA on Chapters

Right now my data frame word_counts is in a tidy form, with one-term-per-document-per-row, but the topicmodels package requires a DocumentTermMatrix. I can cast a one-token-per-row table into a DocumentTermMatrix with tidytext’s cast_dtm().

```{r}
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm
```

I can then use the LDA() function to create a four-topic model. In this case I know I am looking for four topics because there are four books. In other problems I may need to try a few different values of k.

```{r}
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda
```

Much as I did on the Associated Press data, I can examine per-topic-per-word probabilities.

```{r}
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics
```

Notice that this has turned the model into a one-topic-per-term-per-row format. For each combination, the model computes the probability of that term being generated from that topic. For example, the term “joe” has an almost zero probability of being generated from topics 1, 2, or 3, but it makes up 1.45% of topic 4.

I want to find the top 5 terms within each topic.

```{r}
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms
```

This tidy output lends itself well to a ggplot2 visualization.

```{r}
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
```

These topics are pretty clearly associated with the four books! There’s no question that the topic of “captain”, “nautilus”, “sea”, and “nemo” belongs to Twenty Thousand Leagues Under the Sea, and that “jane”, “darcy”, and “elizabeth” belongs to Pride and Prejudice. I see “pip” and “joe” from Great Expectations and “martians”, “black”, and “night” from The War of the Worlds. I also notice that, in line with LDA being a “fuzzy clustering” method, there can be words in common between multiple topics, such as “miss” in topics 1 and 4, and “time” in topics 3 and 4.

### 06_02_02 Per-Document Classification

```{r}
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma
```

Each of these values is an estimated proportion of words from that document that are generated from that topic. For example, the model estimates that each word in the Great Expectations_57 document has only a 0.00135% probability of coming from topic 1 (Pride and Prejudice).

Now that I have these topic probabilities, I can see how well our unsupervised learning did at distinguishing the four books. I would expect that chapters within a book would be found to be mostly (or entirely), generated from the corresponding topic.

First I re-separate the document name into title and chapter, after which I can visualize the per-document-per-topic probability for each.

```{r}
chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma
```

```{r}
# reorder titles in order of topic 1, topic 2, etc before plotting
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)
```

I notice that almost all of the chapters from Pride and Prejudice, War of the Worlds, and Twenty Thousand Leagues Under the Sea were uniquely identified as a single topic each.

It does look like some chapters from Great Expectations (which should be topic 4) were somewhat associated with other topics. Are there any cases where the topic most associated with a chapter belonged to another book? First I would find the topic that was most associated with each chapter using top_n(), which is effectively the “classification” of that chapter.

```{r}
chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications
```

I can then compare each to the “consensus” topic for each book (the most common topic among its chapters), and see which were most often misidentified.

```{r}
book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)
```

I see that only two chapters from Great Expectations were misclassified, as LDA described one as coming from the “Pride and Prejudice” topic (topic 1) and one from The War of the Worlds (topic 3). That’s not bad for unsupervised clustering!

### 06_02_03 By word assignments: augment

I may want to take the original document-word pairs and find which words in each document were assigned to which topic. 

```{r}
assignments <- augment(chapters_lda, data = chapters_dtm)
assignments
```

This returns a tidy data frame of book-term counts, but adds an extra column: .topic, with the topic each term was assigned to within each document. (Extra columns added by augment always start with ., to prevent overwriting existing columns). I can combine this assignments table with the consensus book titles to find which words were incorrectly classified.

```{r}
assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments
```

This combination of the true book (title) and the book assigned to it (consensus) is useful for further exploration. We can, for example, visualize a confusion matrix, showing how often words from one book were assigned to another.

```{r}
assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")
```

I notice that almost all the words for Pride and Prejudice, Twenty Thousand Leagues Under the Sea, and War of the Worlds were correctly assigned, while Great Expectations had a fair number of misassigned words (which, as we saw above, led to two chapters getting misclassified).

Next, I analyze the most commonly mistaken words.

```{r}
wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words
```

```{r}
wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))
```

I can see that a number of words were often assigned to the Pride and Prejudice or War of the Worlds cluster even when they appeared in Great Expectations. For some of these words, such as “love” and “lady”, that’s because they’re more common in Pride and Prejudice (we could confirm that by examining the counts).

On the other hand, there are a few wrongly classified words that never appeared in the novel they were misassigned to. For example, I can confirm “flopson” appears only in Great Expectations, even though it’s assigned to the “Pride and Prejudice” cluster.

```{r}
word_counts %>%
  filter(word == "flopson")
```

The LDA algorithm is stochastic, and it can accidentally land on a topic that spans multiple books.

## 06_03 Alternative LDA Implementations

The LDA() function in the topicmodels package is only one implementation of the latent Dirichlet allocation algorithm. For example, the mallet package (Mimno 2013) implements a wrapper around the MALLET Java package for text classification tools, and the tidytext package provides tidiers for this model output as well.

The mallet package takes a somewhat different approach to the input format. For instance, it takes non-tokenized documents and performs the tokenization itself, and requires a separate file of stopwords. This means I have to collapse the text into one string for each document before performing LDA.

```{r}
# create a vector with one string per chapter
collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)
```

Once the model is created, however, I can use the tidy() and augment() functions described in the rest of the chapter in an almost identical way. This includes extracting the probabilities of words within each topic or topics within each document.

```{r}
# word-topic pairs
tidy(mallet_model)

# document-topic pairs
tidy(mallet_model, matrix = "gamma")

# column needs to be named "term" for "augment"
term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)
```

## Summary

This project introduces topic modeling for finding clusters of words that characterize a set of documents, and shows how the tidy() verb lets us explore and understand these models using dplyr and ggplot2. This is one of the advantages of the tidy approach to model exploration: the challenges of different output formats are handled by the tidying functions, and I can explore model results using a standard set of tools. In particular, I saw that topic modeling is able to separate and distinguish chapters from four separate books, and explored the limitations of the model by finding words and chapters that it assigned incorrectly.
