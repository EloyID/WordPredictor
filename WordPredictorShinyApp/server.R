#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
set.seed(1234)
library(tm)
library(tokenizers)
library(dplyr)
library(cld2)
library(qdap)
library(wordcloud)
library(tidyr)
library(shiny)

# Data charging
con <-
  file("./en_US.all_0001.txt", open = "r")
raw_data <- readLines(con, encoding = "UTF-8")
close(con)

# filter foreign sentences
# if the function is not sure about the language (NA), it is included
english_raw_data = raw_data[detect_language(raw_data) %in% c("en", NA)]

# training-validation-testing proportions
training_raw_data <- english_raw_data

raw_training_corpus <- SimpleCorpus(VectorSource(training_raw_data))

# preprocessing

# profanity words pre-work
profanityWordsFile = "bad-words.txt"
profanityWordsUrl = "https://raw.githubusercontent.com/coffee-and-fun/google-profanity-words/main/data/list.txt"
if (!file.exists(profanityWordsFile)) {
  download.file(profanityWordsUrl, destfile = profanityWordsFile)
}
profanityTokens = tokenize_lines(readLines(profanityWordsFile))


# Takes a rawCropus as SimpleCorpus object as input and applies the following preprocessing steps :
# 1. Remove profanity words
# 2. Remove numbers
# 3. Replace word contractions
# 4. Remove punctuation
# 5. To lowercase
# 6. Remove not character elements
# 7. Remove not standard whitespaces

# raw_corpus: SimpleCorpus object to preprocess
# returns the SimpleCorpus object after having followed the indicated steps
preprocess_raw_corpus = function(raw_corpus) {
  tranformed_raw_corpus <- raw_corpus
  tranformed_raw_corpus <-
    tm_map(tranformed_raw_corpus,
           removeWords,
           unlist(profanityTokens))
  tranformed_raw_corpus <-
    tm_map(tranformed_raw_corpus, removeNumbers)
  tranformed_raw_corpus <-
    tm_map(tranformed_raw_corpus,
           content_transformer(replace_contraction))
  tranformed_raw_corpus <-
    tm_map(tranformed_raw_corpus, removePunctuation)
  tranformed_raw_corpus <-
    tm_map(tranformed_raw_corpus, content_transformer(tolower))

  symbolsToSpace <-
    content_transformer(function(x, pattern)
      gsub(pattern, " ", x))
  tranformed_raw_corpus <-
    tm_map(tranformed_raw_corpus, symbolsToSpace, "[^a-zA-Z]")
  tranformed_raw_corpus <-
    tm_map(tranformed_raw_corpus, stripWhitespace)
  tranformed_raw_corpus
}

preprocessed_training_corpus = preprocess_raw_corpus(raw_training_corpus)

# Tokenizing

# list of all words
unigrams = tokenize_words(paste(preprocessed_training_corpus$content, collapse = " "))
# data frame with
# Freq: freq of each word
# unigrams: word
unigrams_freq = as.data.frame(table(unigrams))
unigrams_freq = arrange(unigrams_freq, desc(Freq))

# list of all bigrams
bigrams = unlist(tokenize_ngrams(preprocessed_training_corpus$content, n = 2))
# data frame with
# Freq: freq of each bigram
# bigrams: bigram
bigrams_freq = as.data.frame(table(bigrams))
bigrams_freq = arrange(bigrams_freq, desc(Freq))
bigrams_freq$bigrams = factor(bigrams_freq$bigrams, levels = bigrams_freq$bigrams)
bigrams_freq <- separate(
  bigrams_freq,
  bigrams,
  c("last_word_1", "last_word"),
  sep = " ",
  remove = FALSE
)

# list of all trigrams
trigrams = unlist(tokenize_ngrams(preprocessed_training_corpus$content, n = 3))
# data frame with
# Freq: freq of each trigram
# trigrams: trigram
trigrams_freq = as.data.frame(table(trigrams))
trigrams_freq = arrange(trigrams_freq, desc(Freq))
trigrams_freq$trigrams = factor(trigrams_freq$trigrams, levels = trigrams_freq$trigrams)
trigrams_freq <-
  separate(
    trigrams_freq,
    trigrams,
    c("last_word_2", "last_word_1", "last_word"),
    sep = " ",
    remove = FALSE
  )

# model

word_threshold = 5
# two data frames with the unigrams separated by the threshold
unigrams_high_freq = filter(unigrams_freq, Freq > word_threshold)
unigrams_low_freq = filter(unigrams_freq, Freq <= word_threshold)

unknown_word_token = "__UNK__"

unigrams_high_freq = unigrams_high_freq %>%
  mutate(logprobability = log(Freq / sum(Freq)))

# subsitute in new data frames the uncommon words by unknown
bigrams_freq_with_unk = data.frame(bigrams_freq)
trigrams_freq_with_unk = data.frame(trigrams_freq)

# change in an array of word the low frequency words into unknown token
# words_array: the array of words
# returns an array of words modified
substitute_low_freq_words = function(words_array) {
  words_array[!words_array %in% unigrams_high_freq$unigrams] =
    unknown_word_token
  words_array
}

bigrams_freq_with_unk$last_word_1 =
  substitute_low_freq_words(bigrams_freq_with_unk$last_word_1)
bigrams_freq_with_unk$last_word =
  substitute_low_freq_words(bigrams_freq_with_unk$last_word)
bigrams_freq_with_unk = subset(bigrams_freq_with_unk, select = -bigrams)
bigrams_freq_with_unk = bigrams_freq_with_unk %>%
  group_by(last_word, last_word_1) %>%
  summarise(Freq = sum(Freq))

bigrams_freq_with_unk <- bigrams_freq_with_unk %>%
  group_by(last_word_1) %>%
  mutate(logprobability = log(Freq / sum(Freq)))

trigrams_freq_with_unk$last_word_2 =
  substitute_low_freq_words(trigrams_freq_with_unk$last_word_2)
trigrams_freq_with_unk$last_word_1 =
  substitute_low_freq_words(trigrams_freq_with_unk$last_word_1)
trigrams_freq_with_unk$last_word =
  substitute_low_freq_words(trigrams_freq_with_unk$last_word)
trigrams_freq_with_unk =
  subset(trigrams_freq_with_unk, select = -trigrams)
trigrams_freq_with_unk = trigrams_freq_with_unk %>%
  group_by(last_word, last_word_1, last_word_2) %>%
  summarise(Freq = sum(Freq))

trigrams_freq_with_unk <- trigrams_freq_with_unk %>%
  group_by(last_word_1, last_word_2) %>%
  mutate(logprobability = log(Freq / sum(Freq)))

# tests if the word is uknown (not in the high frequency words)
# word: the word to test
# returns boolean with the answer
is_word_unkown = function(word) {
  !word %in% unigrams_high_freq$unigrams
}

most_common_unigram = unigrams_freq[1, ]$unigrams

# takes a text and using the last two words, tries to predict the next word
# text: the text input
# trigrams_coef: weight given to the trigrams prediction
# bigrams_coef: weight given to the bigrams prediction
# unigrams_coef: weight given to the unigrams prediction
# returns a dataframe with the columns words and probability
predict_next_word = function(text,
                             trigrams_coef = 0.6,
                             bigrams_coef = 0.3,
                             unigrams_coef = 0.1) {
  words = strsplit(text, " ")[[1]]
  words_count = length(words)

  # break the text words
  if (words_count == 0) {
    return("")
  }
  if (words_count >= 1) {
    last_word_1_input = words[words_count]
    if (is_word_unkown(last_word_1_input))
      last_word_1_input = unknown_word_token
  }
  if (words_count >= 2) {
    last_word_2_input = words[words_count - 1]
    if (is_word_unkown(last_word_2_input))
      last_word_2_input = unknown_word_token
  } else {
    last_word_2_input = NA
  }

  # check the trigrams and bigrams for the words
  if (trigrams_coef != 0) {
    trigrams_result = trigrams_freq_with_unk[trigrams_freq_with_unk$last_word_2 == last_word_2_input &
                                               trigrams_freq_with_unk$last_word_1 == last_word_1_input &
                                               trigrams_freq_with_unk$last_word != unknown_word_token
                                             , ]
  } else {
    trigrams_result = data.frame(last_word = c(), logprobability = c())
  }

  if (bigrams_coef != 0) {
    bigrams_result = bigrams_freq_with_unk[bigrams_freq_with_unk$last_word_1 == last_word_1_input &
                                             bigrams_freq_with_unk$last_word != unknown_word_token
                                           , ]
  } else {
    bigrams_result = data.frame(last_word = c(), logprobability = c())
  }

  # if no result return the most common unigram
  if (NROW(trigrams_result) == 0 & NROW(bigrams_result) == 0)
    return(data.frame(
      words = c(most_common_unigram),
      probability = 1
    ))

  # get the probability of the candidates as unigrams
  if (unigrams_coef != 0) {
    candidate_words = c(trigrams_result$last_word, bigrams_result$last_word)
    unigrams_result =  unigrams_high_freq[unigrams_high_freq$unigrams %in% candidate_words, ]
  } else {
    unigrams_result = data.frame(unigrams = c(), logprobability = c())
  }

  log_trigrams_coef = ifelse(trigrams_coef == 0, 0, log(trigrams_coef))
  log_bigrams_coef = ifelse(bigrams_coef == 0, 0, log(bigrams_coef))
  log_unigrams_coef = ifelse(unigrams_coef == 0, 0, log(unigrams_coef))

  # join the scores for each word as tri, bi or unigram
  probabilities_result = data.frame(
    words = c(
      trigrams_result$last_word,
      bigrams_result$last_word,
      as.character(unigrams_result$unigrams)
    ),
    score = c(
      log_trigrams_coef + trigrams_result$logprobability,
      log_bigrams_coef + bigrams_result$logprobability,
      log_unigrams_coef + unigrams_result$logprobability
    )
  )

    probabilities_result = probabilities_result %>%
    # add the scores of the same words as tri, bi or unigram
    group_by(words) %>%
    summarize(score = sum(exp(score))) %>%
    # calculate the probability as standardizing the score
    mutate(probability = score / sum(score)) %>%
    arrange(desc(score))


  probabilities_result
}


predict_next_word_with_preprocess = function(text,
                                             trigrams_coef = 0.6,
                                             bigrams_coef = 0.3,
                                             unigrams_coef = 0.1) {
  text_corpus = SimpleCorpus(VectorSource(text))
  preprocessed_text_corpus = preprocess_raw_corpus(text_corpus)
  preprocessed_text = preprocessed_text_corpus[[1]]$content
  predict_next_word(
    preprocessed_text,
    trigrams_coef = trigrams_coef,
    bigrams_coef = bigrams_coef,
    unigrams_coef = unigrams_coef
  )
}

needed_variables <- c(
  "predict_next_word",
  "analyse_sentence_perplexity",
  "default_probability",
  "is_word_unkown",
  "unigrams_high_freq",
  "trigrams_freq_with_unk",
  "bigrams_freq_with_unk",
  "unknown_word_token",
  "most_common_unigram",
  "exported_variables",
  "predict_next_word_with_preprocess",
  "preprocess_raw_corpus",
  "profanityTokens"
)

rm(list = setdiff(ls(), needed_variables))

# Define server logic required to draw a histogram
function(input, output, session) {

    output$word_table = renderTable({
      prediction = predict_next_word_with_preprocess(
        input$text_input
      )
      if(is.character(prediction)) {return(NA)}
      head(select(prediction, c(words, probability)),input$words_number)
    })
    output$wordcloud = renderPlot({
      prediction = predict_next_word_with_preprocess(
        input$text_input
      )
      if(is.character(prediction)) {return(NA)}
      wordcloud(
        prediction$words,
        prediction$probability,
        max.words = 100,
        random.order = FALSE
      )
    })
}
