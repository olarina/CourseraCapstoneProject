library(data.table)
library(stringr)
library(lexicon)
library(quanteda)
library(tidyr)
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
test = data.table(read.csv("data/test.csv"))
testStrings = as.character(test$x)
UniFreq = data.table(read.csv("data/UniFreq.csv"))
BiFreq = data.table(read.csv("data/BiFreq.csv"))
TriFreq = data.table(read.csv("data/TriFreq.csv"))

cleanedTest = cleanDataQ(testStrings)

dfMatrix = cleanedTest %>%
  tokens_ngrams(n=3) %>%
  dfm()
frequencyMatrix = CountNGramFreq(dfMatrix)
rm(dfMatrix)

smallTestNum = sample(1:frequencyMatrix[,.N], size = frequencyMatrix[,.N]*0.0001)
testDT = frequencyMatrix[smallTestNum,]
testDT = testDT[!is.na(term),]

getBegin = function(myString)
{
  words = unlist(str_split(myString, "_"))
  paste(words[1], words[2], collapse = " ")
}
getEnd = function(myString)
{
  words = unlist(str_split(myString, "_"))
  words[3]
}
testDT[ ,begin := sapply(term, getBegin)]
testDT[ ,end := sapply(term, getEnd)]

getPrediction = function(myString)
{
  result = Find_Next_word(xy=myString, words_num = 20)
  paste(result$term, collapse = " ")
}

system.time(testDT[, prediction := sapply(begin, getPrediction)])

for (i in 1:nrow(testDT))
{
  temp = grep(testDT$end[i], testDT$prediction[i])
  if (length(temp) > 0)
    testDT$correct[i] = 1
  else testDT$correct[i] = 0
}

triGNum = testDT[,sum(c)]
correct = sum(testDT$c*testDT$correct)
correct/triGNum