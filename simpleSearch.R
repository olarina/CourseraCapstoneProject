library(tidytext)
library(tidyr)
data = cleanData(text_data)
## see gettingData

test_num = sample(1:length(data), length(data)*0.2)
test = data[test_num]
train = data[-test_num]

min_count = 4
word_data = data.table(text = train)
word_data = word_data %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  filter(n > min_count)

bigram_data = data.table(text = train)
bigram_data = bigram_data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  filter(n > min_count)

trigram_data = data.table(text = train)
trigram_data = trigram_data %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE) %>%
  filter(n > min_count)

fourgram_data = data.table(text = train)
fourgram_data = fourgram_data %>%
  unnest_tokens(fourgram, text, token = "ngrams", n = 4) %>%
  count(fourgram, sort = TRUE) %>%
  filter(n > min_count)

fourgram_data = fourgram_data %>%
  separate(fourgram, c("w1","w2","w3", "last")) %>%
  unite(first, w1,w2,w3, sep = " ")

trigram_data = trigram_data %>%
  separate(trigram, c("w1","w2", "last")) %>%
  unite(first, w1,w2, sep = " ")

bigram_data = bigram_data %>%
  separate(bigram, c("first", "last"))

## must get 2-4 words
findInNGram = function(n, words)
{
  if (n == 4)
    ngram = data.table(fourgram_data)
  else if (n == 3)
    ngram = data.table(trigram_data)
  else if (n == 2)
    ngram = data.table(bigram_data)
  else return("n must a number 2 to 4")
  result = ngram[grep(words, ngram[,first], perl = T, useBytes = T),]
  if (nrow(result) > 0)
    return(result$last[1])
  else return(NULL)
}

## must get cleaned phrase
## only letters
## 1 word min
findInAllNgrams = function(phrase)
{
  result = NULL
  words = str_split(phrase, " ")[[1]]
  if (length(words) > 3)
    words = words[(length(words) - 2):length(words)]
  if (length(words) == 3)
  {
    data_string = paste(words,  collapse = " ")
    result = findInNGram(4, data_string)
    if (is.null(result))
      words = words[2:3]
  }
  if (length(words) == 2)
  {
    data_string = paste(words,  collapse = " ")
    result = findInNGram(3, data_string)
    if (is.null(result))
      words = words[2]
  }
  if (length(words) == 1)
    result = findInNGram(2, words)
  result
}

Next_word = function(phrase)
{
  phrase = cleanPhrase(phrase)
  findInAllNgrams(phrase)
}















