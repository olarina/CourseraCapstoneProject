library(readtext)
library(data.table)
library(lexicon)
library(dplyr)
library(quanteda)
library(stringi)
data(profanity_arr_bad)
data(profanity_banned)
data(profanity_racist)
data(profanity_alvarez)
pa = gsub("\\*", "", profanity_alvarez)
pa = gsub("\\$", "\\\\$", pa)
pa = gsub("\\+", "\\\\+", pa)
pa = gsub("\\!", "\\\\!", pa)
pa = gsub("\\.", "\\\\.", pa)
pa = gsub("\\-", "\\\\-", pa)
pa = gsub("\\;", "\\\\;", pa)
pa = gsub("\\(", "\\\\(", pa)
if (!dir.exists("./data"))
  dir.create("./data")
if (!dir.exists("./data/final"))
{
  url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(url, destfile = "./data/Coursera-SwiftKey.zip", method = "curl")
  unzip(zipfile = "./data/Coursera-SwiftKey.zip", exdir = "./data")
}
sample_size = 0.8
news = stri_split_lines1(readtext("data/final/en_US/en_US.news.txt"))
news = sample(news, size=length(news)*sample_size)
blogs = stri_split_lines1(readtext("data/final/en_US/en_US.blogs.txt"))
blogs = sample(blogs, size=length(blogs)*sample_size)
twitter = stri_split_lines1(readtext("data/final/en_US/en_US.twitter.txt"))
twitter = sample(twitter, size=length(twitter)*sample_size)
text_data = c(news, blogs, twitter)

test_num = sample(1:length(text_data), length(text_data)*0.1)
test = text_data[test_num]
train = text_data[-test_num]

rm(text_data, twitter, news, blogs)
gc()

## Using tidytext
cleanData = function(phrase)
{
  library(stringr)
  library(tidytext)
  library(tm)
  library(textclean)
  
##  phrase = replace_internet_slang(phrase) takes 2/3 of total time and > 90% memory
  phrase = tolower(phrase)
  phrase = removeWords(phrase, profanity_arr_bad)
  phrase = removeWords(phrase, profanity_banned)
  phrase = removeWords(phrase, profanity_racist)
  phrase = removeWords(phrase, pa)
##  phrase = removeWords(phrase, stop_words[['word']]) changed to make faster
  phrase = removeWords(phrase, stopwords("en"))
  phrase = replace_contraction(phrase)
  phrase = tolower(phrase) ##must be used with replace_contraction as it conwerts to capital letter
  phrase = replace_hash(phrase, replacement = '$3')
  phrase = replace_tag(phrase, replacement = '$3')
  phrase = replace_symbol(phrase)
##  phrase = stemDocument(phrase)
  phrase = str_replace_all(phrase, "[^a-zA-Z\\s]", " ")
  phrase
}

# cleanData() doesn't remove extra whitespaces because unnest_tokens does it in
# any case. But we need to do it with the single phrase.
cleanPhrase = function(phrase)
{
  phrase = cleanData(phrase)
  phrase = gsub("\\s+"," ",phrase)
  trimws(phrase)
}

data = cleanData(train)

UniFreq = data.table(text = data)
UniFreq = UniFreq %>%
  unnest_tokens(term, text) %>%
  count(term, sort = TRUE, name = "c")
UniFreq = data.table(term = UniFreq$term, c = UniFreq$c)

BiFreq = data.table(text = data)
BiFreq = BiFreq %>%
  unnest_tokens(term, text, token = "ngrams", n = 2) %>%
  count(term, sort = TRUE, name = "c")
BiFreq = data.table(term = BiFreq$term, c = BiFreq$c)

TriFreq = data.table(text = data)
TriFreq = TriFreq %>%
  unnest_tokens(term, text, token = "ngrams", n = 3) %>%
  count(term, sort = TRUE, name = "c")
TriFreq = data.table(term = TriFreq$term, c = TriFreq$c)

min_count = 4
UniFreq = UniFreq[c>min_count,]
BiFreq = BiFreq[c>min_count,]
TriFreq = TriFreq[c>min_count,]
BiFreq$term = gsub(" ", "_", BiFreq$term)
TriFreq$term = gsub(" ", "_", TriFreq$term)

## Using quanteda

cleanDataQ = function(textData)
{
  text.tokens = textData %>%
    tokens(remove_numbers=T, remove_punct=T, remove_symbols=T, remove_hyphens=T,
           remove_twitter=T, remove_url=T) %>%
    tokens_remove(stopwords("en")) %>%
    tokens_remove(profanity_arr_bad) %>%
    tokens_remove(profanity_banned) %>%
    tokens_remove(profanity_racist) %>%
    tokens_remove(profanity_racist) %>%
    tokens_remove(pa)
  return(text.tokens)
}

tokensData = cleanDataQ(train)
CountNGramFreq = function(NGrDfm) {
  FreqV = colSums(NGrDfm)
  return(data.table(term=names(FreqV), c=FreqV))
}
min_count = 4

getFrequrency = function(ngramType)
{
  dfMatrix = tokensData %>%
    tokens_ngrams(n=ngramType) %>%
    dfm()
  frequencyMatrix = CountNGramFreq(dfMatrix)
  frequencyMatrix = frequencyMatrix[c>min_count,]
  rm(dfMatrix)
  frequencyMatrix
}

UniFreq = getFrequrency(1)
BiFreq = getFrequrency(2)
TriFreq = getFrequrency(3)
write.csv(train, file = "data/train.csv")
write.csv(test, file = "data/test.csv")

rm(train, test, tokensData)
gc()

## Calculate the "frequency of frequency r" (N_r)
CountNC = function(FreqVec) {
  CountTbl = table(FreqVec[,.(c)])
  return(data.table(cbind(c=as.integer(names(CountTbl)), Nr=as.integer(CountTbl))))
}

UniBins = CountNC(UniFreq)
BiBins = CountNC(BiFreq)
TriBins = CountNC(TriFreq)

avg.zr = function(Bins) {
  max = dim(Bins)[1]
  r=2:(max-1)
  Bins[1, Zr:=2*Nr/Bins[2,c]]  # r=1, q=0, Zr=Nr/(0.5t)
  Bins[r, Zr:=2*Nr/(Bins[r+1,c]-Bins[r-1,c])]  # else, Zr=Nr/(0.5(t-q))
  Bins[max, Zr:=Nr/(c-Bins[(max-1),c])]  # r=max, t=2r-q, Zr=Nr/(r-q)
}
avg.zr(UniBins)
avg.zr(BiBins)
avg.zr(TriBins)

FitLM = function(CountTbl) {
  return(lm(log(Zr) ~ log(c), data = CountTbl))
}
UniLM = FitLM(UniBins)
BiLM = FitLM(BiBins)
TriLM = FitLM(TriBins)

## Only perform the discounting to small count (c) n-grams, where c <= k, using Katz's formula
k=5
Cal_GTDiscount = function(cnt, N) {
  if (N==1) {
    model = UniLM
  } else if (N==2) {
    model = BiLM
  } else if (N==3) {
    model = TriLM
  }
  Z1 = exp(predict(model, newdata=data.frame(c=1)))
  Zr = exp(predict(model, newdata=data.frame(c=cnt)))
  Zrp1 = exp(predict(model, newdata=data.frame(c=(cnt+1))))
  Zkp1 = exp(predict(model, newdata=data.frame(c=(k+1))))
  
  sub = ((k+1)*Zkp1)/(Z1)
  new_r = ((cnt+1)*(Zrp1)/(Zr)-cnt*sub)/(1-sub)
  return(new_r)
}

# UpdateCount = function(FreqTbl, N) {
#   FreqTbl[c>k ,cDis:=as.numeric(c)]
#   FreqTbl[c<=k, cDis:=Cal_GTDiscount(c, N)]
# }

UpdateCount = function(FreqTbl, N) {
  FreqTbl[c>k ,cDis:=as.numeric(c)]
  FreqTbl[c<=k, cDis:=Cal_GTDiscount(c, N)]
  FreqTbl
}

UniFreq = UpdateCount(UniFreq, 1)
BiFreq = UpdateCount(BiFreq, 2)
TriFreq = UpdateCount(TriFreq, 3)
write.csv(UniFreq, file = "data/UniFreq.csv", row.names = FALSE)
write.csv(BiFreq, file = "data/BiFreq.csv", row.names = FALSE)
write.csv(TriFreq, file = "data/TriFreq.csv", row.names = FALSE)
setkey(UniFreq, term)
setkey(BiFreq, term)
setkey(TriFreq, term)
