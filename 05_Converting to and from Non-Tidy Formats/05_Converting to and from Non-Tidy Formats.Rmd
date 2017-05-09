---
title: "05 Converting to and from Non-Tidy Formats"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(knitr)
opts_chunk$set(message = FALSE, warning = FALSE, cache = TRUE)
options(width = 100, dplyr.width = 150)
library(ggplot2)
library(methods)
theme_set(theme_light())
library(purrr)
library(tm)
library(dplyr)
library(tidytext)
library(tidyr)
library(janeaustenr)
library(tm)
```

This project will focus on the process of tidying document-term matrices, as well as casting a tidy data frame into a sparse matrix. I will also expore how to tidy Corpus objects, which combine raw text with document metadata, into text data frames, leading to a case study of ingesting and analyzing financial articles.

## 05_01 Tidying a Document-Term Matrix

One of the most common structures that text mining packages work with is the document-term matrix (or DTM). This is a matrix where:

- each row represents one document (such as a book or article),

- each column represents one term, and

- each value (typically) contains the number of appearances of that term in that document.

### 05_01_01 Tidying DocumentTermMatrix Objects

Perhaps the most widely used implementation of DTMs in R is the DocumentTermMatrix class in the tm package. Many available text mining datasets are provided in this format. For example, consider the collection of Associated Press newspaper articles included in the topicmodels package.

```{r}
data("AssociatedPress", package = "topicmodels")
AssociatedPress
```

I see that this dataset contains documents (each of them an AP article) and terms (distinct words). Notice that this DTM is 99% sparse (99% of document-word pairs are zero). I could access the terms in the document with the Terms() function.

```{r}
terms <- Terms(AssociatedPress)
head(terms)
```

If I wanted to analyze this data with tidy tools, I would first need to turn it into a data frame with one-token-per-document-per-row. The broom package introduced the tidy() verb, which takes a non-tidy object and turns it into a tidy data frame. The tidytext package implements this method for DocumentTermMatrix objects.

```{r}
ap_td <- tidy(AssociatedPress)
ap_td
```

I now have a tidy three-column tbl_df, with variables document, term, and count.

```{r}
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments
```

This would let me visualize which words from the AP articles most often contributed to positive or negative sentiment. I can see that the most common positive words include "like", "work", "support", and "good", while the most negative words include "killed", "death", and "vice". (The inclusion of "vice" as a negative term is probably a mistake on the algorithm's part, since it likely usually refers to "vice president").

```{r}
ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()
```

### 05_01_02 Tidying DocumentTermMatrix Objects

Just as some existing text mining packages provide document-term matrices as sample data or output, some algorithms expect such matrices as input. Therefore, tidytext provides cast_ verbs for converting from a tidy form to these matrices.

For example, I could take the tidied AP dataset and cast it back into a document-term matrix using the cast_dtm() function.

```{r}
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)
```

```{r}
inaug_dfm
```

The tidy method works on these document-feature matrices as well, turning them into a one-token-per-document-per-row table:

```{r}
inaug_td <- tidy(inaug_dfm)
inaug_td
```

I am interested in finding the words most specific to each of the inaugural speeches.

```{r}
inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf
```

As another example of a visualization possible with tidy data, I could extract the year from each document’s name, and compute the total number of words within each year.

```{r}
year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))
```

Further, I can see that over time, American presidents became less likely to refer to the country as the “Union” and more likely to refer to “America”. They also became less likely to talk about the “constitution” and foreign" countries, and more likely to mention “freedom” and “God”.

```{r}
year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")
```

These examples show how I can use tidytext, and the related suite of tidy tools, to analyze sources even if their origin was not in a tidy format.

## 05_02 Casting tidy text data into a matrix

Just as some existing text mining packages provide document-term matrices as sample data or output, some algorithms expect such matrices as input. Therefore, tidytext provides cast_ verbs for converting from a tidy form to these matrices.

For example, I could take the tidied AP dataset and cast it back into a document-term matrix using the cast_dtm() function.

```{r}
ap_td %>%
  cast_dtm(document, term, count)
```

Similarly, I could cast the table into a dfm object from quanteda’s dfm with cast_dfm().

```{r}
ap_td %>%
  cast_dfm(term, document, count)
```

Some tools simply require a sparse matrix:

```{r}
# cast into a Matrix object
m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)
```

```{r}
dim(m)
```

This kind of conversion could easily be done from any of the tidy text structures I have used so far in this project. For example, I could create a DTM of Jane Austen’s books in just a few lines of code.

```{r}
austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm
```

This casting process allows for reading, filtering, and processing to be done using dplyr and other tidy tools, after which the data can be converted into a document-term matrix for machine learning applications.

## 05_03 Tidying Corpus Objects with Metadata

Some data structures are designed to store document collections before tokenization, often called a “corpus”. One common example is Corpus objects from the tm package. These store text alongside metadata, which may include an ID, date/time, title, or language for each document.

For example, the tm package comes with the acq corpus, containing 50 articles from the news service Reuters.

```{r}
data("acq")
acq
```

```{r}
# first document
acq[[1]]
```

A corpus object is structured like a list, with each item containing both text and metadata (see the tm documentation for more on working with Corpus documents). This is a flexible storage method for documents, but doesn’t lend itself to processing with tidy tools.

Next, I construct a table with one row per document, including the metadata (such as id and datetimestamp) as columns alongside the text.

```{r}
acq_td <- tidy(acq)
acq_td
```

So, for example, I can find the most common words across the 50 Reuters articles, or the ones most specific to each article.

```{r}
acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# most common words
acq_tokens %>%
  count(word, sort = TRUE)
```

```{r}
# tf-idf
acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))
```

## Summary

Text analysis requires working with a variety of tools, many of which have inputs and outputs that aren’t in a tidy form. This project showed how to convert between a tidy text data frame and sparse document-term matrices, as well as how to tidy a Corpus object containing document metadata.
