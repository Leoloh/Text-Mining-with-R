---
title: "03 Analyzing Word and Document Frequency"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)
library(gutenbergr)
library(stringr)
```

As I mentioned earlier, there are words in a document, however, that occur many times but may not be important. In English, these are probably words like “the”, “is”, “of”, and so forth. I might take the approach of adding words like these to a list of stop words and removing them before analysis, but it is possible that some of these words might be more important in some documents than others. A list of stop words is not a very sophisticated approach to adjusting term frequency for commonly used words.

My approach for this project is to look at a term’s inverse document frequency (idf), which decreases the weight for commonly used words and increases the weight for words that are not used very much in a collection of documents. This can be combined with term frequency to calculate a term’s tf-idf (the two quantities multiplied together), the frequency of a term adjusted for how rarely it is used.

## 03_00 Background Information

The statistic tf-idf is intended to measure how important a word is to a document in a collection (or corpus) of documents, for example, to one novel in a collection of novels or to one website in a collection of websites.

It is a rule-of-thumb or heuristic quantity. While it has proved useful in text mining, search engines, etc., its theoretical foundations are considered less than firm by information theory experts. The inverse document frequency for any given term is defined as

idf(term) = ln((n of documents)*(n of documents containing term))

## 03_01 Term Frequency in Jane Austen's Novels

Let’s start by looking at the published novels of Jane Austen and examine first term frequency, then tf-idf.

```{r}
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words
```

There is one row in this book_words data frame for each word-book combination; n is the number of times that word is used in that book and total is the total words in that book. The usual suspects are here with the highest n, “the”, “and”, “to”, and so forth.

Next, I want to look at the distribution of n/total for each novel, the number of times a word appears in a novel divided by the total number of terms (words) in that novel. This is exactly what term frequency is.

```{r}
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")
```

There are very long tails to the right for these novels (those extremely common words!) that I have not shown in these plots. These plots exhibit similar distributions for all the novels, with many words that occur rarely and fewer words that occur frequently.

## 03_02 Zipf's Law

Since I have the data frame I used to plot term frequency, I can examine Zipf’s law for Jane Austen’s novels with just a few lines of dplyr functions. Zipf’s law states that the frequency that a word appears is inversely proportional to its rank.

```{r}
freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank
```

The rank column here tells us the rank of each word within the frequency table; the table was already ordered by n so we could use row_number() to find the rank. Then, we can calculate the term frequency in the same way we did before.

The rank column here tells us the rank of each word within the frequency table; the table was already ordered by n so we could use row_number() to find the rank. Then, we can calculate the term frequency in the same way we did before.

```{r}
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_line(size = 1.2, alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10()
```

I see that all six of Jane Austen’s novels are similar to each other, and that the relationship between rank and frequency does have negative slope. It is not quite constant, though. Perhaps I could view this as a broken power law with, say, three sections. Let’s see what the exponent of the power law is for the middle section of the rank range.

```{r}
rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
```

I want to plot this fitted power law with the data.

```{r}
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.2, alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10()
```

I have found a result close to the classic version of Zipf’s law for the corpus of Jane Austen’s novels. The deviations I see here at high rank are not uncommon for many kinds of language. A corpus of language often contains fewer rare words than predicted by a single power law. The deviations at low rank are more unusual.

## 03_03 The bind_tf_idf Function

The idea of tf-idf is to find the important words for the content of each document by decreasing the weight for commonly used words and increasing the weight for words that are not used very much in a collection or corpus of documents, in this case, the group of Jane Austen’s novels as a whole. Calculating tf-idf attempts to find the words that are important (i.e., common) in a text, but not too common.

```{r}
book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words
```

Notice that idf and thus tf-idf are zero for these extremely common words. These are all words that appear in all six of Jane Austen’s novels, so the idf term (which will then be the natural log of 1) is zero. The inverse document frequency (and thus tf-idf) is very low (near zero) for words that occur in many of the documents in a collection. This is how this approach decreases the weight for common words. The inverse document frequency will be a higher number for words that occur in fewer of the documents in the collection.

I look at terms with high tf-idf in Jane Austen’s works.

```{r}
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))
```

I see all proper nouns, names that are in fact important in these novels. None of them occur in all of novels, and they are important, characteristic words for each text within the corpus of Jane Austen’s novels.

Next, I want to look at a visualization for these high tf-idf words.

```{r}
plot_austen <- book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

ggplot(plot_austen[1:20,], aes(word, tf_idf, fill = book)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()
```

And I look at the novels individually.

```{r}
plot_austen <- plot_austen %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup

ggplot(plot_austen, aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 3, scales = "free") +
  coord_flip()
```

These words are, as measured by tf-idf, the most important to each novel and most readers would likely agree. What measuring tf-idf has done here is show us that Jane Austen used similar language across her six novels, and what distinguishes one novel from the rest within the collection of her works are the proper nouns, the names of people and places. This is the point of tf-idf. It identifies words that are important to one document within a collection of documents.

## 03_04 A Carpus of Physics Texts

Further, I want to work with another corpus of documents, to see what terms are important in a different set of works. Leave the world of fiction and narrative entirely and download some classic physics texts from [Project Gutenberg](http://www.gutenberg.org/ebooks/5001) and see what terms are important in these works, as measured by tf-idf.

```{r}
physics <- gutenberg_download(c(37729, 14725, 13476, 5001),
                              meta_fields = "author")
```

After I have the texts, I want to find out how many times each word was used in each text.

```{r}
physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

physics_words
```

I see just the raw counts. I need to remember that these documents are all different lengths. Let’s go ahead and calculate tf-idf, then visualize the high tf-id words.

```{r}
physics_words <- physics_words %>%
  bind_tf_idf(word, author, n)

plot_physics <- physics_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics[1:20,], aes(word, tf_idf, fill = author)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()
```

 Next, I want to look at each text individually.
 
 
```{r}
plot_physics <- plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  mutate(word = reorder(word, tf_idf))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()
```

I notice the word "eq" in the Einstein text, which is interesting. So I want to analyze this in detail.

```{r}
physics %>%
  filter(str_detect(text, "eq\\.")) %>%
  select(text)
```

Some cleaning up of the text may be in order. “K1” is the name of a coordinate system for Einstein:

```{r}
physics %>%
  filter(str_detect(text, "K1")) %>%
  select(text)
```

Maybe it makes sense to keep this one. Also notice that in this line I have “co-ordinate”, which explains why there are separate “co” and “ordinate” items in the high tf-idf words for the Einstein text.

“AB”, “RC”, and so forth are names of rays, circles, angles, and so forth for Huygens.

```{r}
physics %>%
  filter(str_detect(text, "AK")) %>%
  select(text)
```

I want to remove some of these less meaningful words to make a better, more meaningful plot. I will need to go back a few steps since I am removing words from the tidy data frame.

```{r}
mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn",
                                   "fig", "file", "cg", "cb", "cm"))
physics_words <- anti_join(physics_words, mystopwords, by = "word")
plot_physics <- physics_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()
```

## Summary

Using term frequency and inverse document frequency allowed me to find words that are characteristic for one document within a collection of documents, whether that document is a novel or physics text or webpage. Exploring term frequency on its own can give insight into how language is used in a collection of natural language, and dplyr verbs like count() and rank() give me tools to reason about term frequency. The tidytext package uses an implementation of tf-idf consistent with tidy data principles that enabled me to see how different words are important in documents within a collection or corpus of documents.
