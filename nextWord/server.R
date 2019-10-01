library(shiny)
library(data.table)
library(stringr)
library(lexicon)
library(quanteda)
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
UniFreq = data.table(read.csv("UniFreq.csv"))
BiFreq = data.table(read.csv("BiFreq.csv"))
TriFreq = data.table(read.csv("TriFreq.csv"))
setkey(UniFreq, term)
setkey(BiFreq, term)
setkey(TriFreq, term)

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

## Return all the observed N-grams given the previous (N-1)-gram
##
## - wordseq: character vector of (N-1)-gram separated by underscore, e.g. "x1_x2_..._x(N-1)"
## - NgramFreq: datatable of N-grams
get.obs.NGrams.by.pre = function(wordseq, NgramFreq) {
    PreTxt = sprintf("%s%s%s", "^", wordseq, "_")
    NgramFreq[grep(PreTxt, NgramFreq[,term], perl=T, useBytes=T),]
}
## Return all the unigrams that end unobserved Ngrams
get.unobs.Ngram.tails = function(ObsNgrams, N) {
    ObsTails = str_split_fixed(ObsNgrams[,term], "_", N)[,N]
    return(data.table(term=UniFreq[!ObsTails,term,on="term"]))
}

## Compute the probabilities of observed N-gram.
## We need the counts from (N-1)-gram table since corpus doesn't include <EOS> explicitly,
## therefore the denominator will be smaller if only summing up all the terms
## from N-gram table
cal.obs.prob = function(ObsNgrams, Nm1Grams, wordseq) {
    PreCount = Nm1Grams[wordseq, c, on=.(term)]
    ObsNgrams[,Prob:=ObsNgrams[,cDis]/PreCount]  # c_dis/c
}
## Return the normalization factor Alpha
##
## - ObsNgrams: datatable contains all observed ngrams starting with wordseq
## - Nm1Grams: datatable of (N-1)-grams containing count of wordseq
## - wordseq: an observed history: w_{i-N+1}^{i-1}
cal.alpha = function(ObsNGrams, Nm1Grams, wordseq) {
    if (dim(ObsNGrams)[1] != 0) {
        # return(1-sum(ObsNGrams[,.(Qbo)]))  # We don't use this formular because End Of Sentence is not counted
        return(sum(ObsNGrams[,c-cDis]/Nm1Grams[wordseq, c, on=.(term)]))
    } else {
        return(1)
    }
}
## Find next word
## Return a list of predicted next words according to previous 2 user input words
##
## - xy: character vector containing user-input bigram, separated by a space
## - words_num: number of candidates of next words returned
Find_Next_word = function(xy, words_num) {
    xy = gsub(" ", "_", xy)
    if (length(which(BiFreq$term == xy)) > 0) {  # C(x,y) > 0
        ## N-grams preparation
        # Retrieve all observed trigrams beginning with xy: OT
        ObsTriG = get.obs.NGrams.by.pre(xy, TriFreq)
        y = str_split_fixed(xy,"_", 2)[,2]
        # Retrieve all observed bigrams beginning with y: OB
        ObsBiG = get.obs.NGrams.by.pre(y, BiFreq)
        # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0, UOB in UOT
        UnObsBiTails = get.unobs.Ngram.tails(ObsBiG, 2)
        # Exclude observed bigrams that also appear in observed trigrams: OB in UOT
        ObsBiG = ObsBiG[!str_split_fixed(ObsTriG[,term], "_", 2)[,2], on="term"]
        
        ## Calculation part
        # Calculate probabilities of all observed trigrams: P^*(z|x,y)
        ObsTriG = cal.obs.prob(ObsTriG, BiFreq, xy)
        # Calculate Alpha(x,y)
        Alpha_xy = cal.alpha(ObsTriG, BiFreq, xy)
        # Calculate probabilities of all observed bigrams: P^*(z|y), (y,z) in UOT
        ObsBiG = cal.obs.prob(ObsBiG, UniFreq, y)
        # Calculate Alpha(y)
        Alpha_y = cal.alpha(ObsBiG, UniFreq, y)
        # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
        UnObsBiTails[, Prob:=UniFreq[UnObsBiTails, c, on=.(term)]/UniFreq[UnObsBiTails, sum(c), on=.(term)]]
        UnObsBiTails[, Prob:=Alpha_xy*Alpha_y*Prob]
        # Remove unused column in ObsTriG and ObsBiG
        ObsTriG[, c("c", "cDis"):=NULL]
        ObsTriG[, term:=str_remove(ObsTriG[, term], "([^_]+_)+")]
        ObsBiG[, c("c", "cDis"):=NULL]
        ObsBiG[, term:=str_remove(ObsBiG[, term], "([^_]+_)+")]
        # Compare OT, Alpha_xy * P_{Katz}(z|y)
        # P_{Katz}(z|y) = 1. P^*(z|y), 2. Alpha_y * P_{ML}(z)
        ObsBiG[,Prob:=Alpha_xy*Prob]
        AllTriG = setorder(rbind(ObsTriG, ObsBiG, UnObsBiTails), -Prob)
        return(AllTriG[Prob!=0][1:min(dim(AllTriG[Prob!=0])[1], words_num)])
    } else {  # C(x,y) = 0
        y = str_split_fixed(xy,"_", 2)[,2]
        # c(y>0)
        if (length(which(UniFreq$term == y)) > 0) {
            # Retrieve all observed bigrams beginning with y: OB
            ObsBiG = get.obs.NGrams.by.pre(y, BiFreq)
            # Calculate probabilities of all observed bigrams: P^*(z|y)
            ObsBiG = cal.obs.prob(ObsBiG, UniFreq, y)
            # Calculate Alpha(y)
            Alpha_y = cal.alpha(ObsBiG, UniFreq, y)
            # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0
            UnObsBiTails = get.unobs.Ngram.tails(ObsBiG, 2)
            # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
            UnObsBiTails[, Prob:=UniFreq[UnObsBiTails, c, on=.(term)]/UniFreq[UnObsBiTails, sum(c), on=.(term)]]
            UnObsBiTails[, Prob:=Alpha_y*Prob]
            # Remove unused column in ObsBiG
            ObsBiG[, c("c", "cDis"):=NULL]
            ObsBiG[, term:=str_remove(ObsBiG[, term], "([^_]+_)+")]
            AllBiG = setorder(rbind(ObsBiG, UnObsBiTails), -Prob)
            return(AllBiG[Prob!=0][1:words_num])
        } else {  # c(y=0)
            # P^*z
            return(setorder(UniFreq, -cDis)[1:words_num,.(term, Prob=cDis/UniFreq[,sum(c)])])  
        }
    }
}
Next_word = function(prephrase, words_num=10) {
    prephraseTokens = cleanDataQ(prephrase) %>%
        tokens_tolower()
    bigr = paste(tail(prephraseTokens[[1]], 2), collapse = " ")
    result = Find_Next_word(bigr, words_num)
    names(result) = c("Next word", "Probability")
    if (dim(result)[1] == 0) {
        rbind(result, list("<Please input more text>", 1))
    }
    return(result)
}

shinyServer(function(input, output) {
    
    pred = reactive({
        Next_word(input$userPhrase)
    })
    output$result <- renderDataTable({
        if (input$userPhrase == "")
            "Enter some text"
        else pred()
    })
    
})
