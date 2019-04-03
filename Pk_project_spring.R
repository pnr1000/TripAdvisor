setwd("/Users/Phoebe/Desktop/R scripts")

library(rJava)
library(qdap)
library(tm)
library(text2vec)
library(wordcloud)
library(RWeka)
library(tokenizers)
library(sentimentr)
library(lexicon)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(sentimentr)

mytext <- read.csv("b.csv")
mytext$Review <- iconv(mytext$Review, from = "UTF-8", to = "ASCII", sub = "")

#######
#Cleaning reviews

mytext_tokens <- mytext$Review %>%
  tolower %>% 
  removePunctuation %>%
  removeNumbers %>%
  stripWhitespace %>%
  word_tokenizer 

stop_words <- stopwords("english")
it <- itoken(mytext_tokens)
bi_it <- itoken(mytext_tokens)
tri_it <- itoken(mytext_tokens)

vocab <- create_vocabulary(it,  stopwords = stop_words) 

bi_vocab <- create_vocabulary(bi_it,  stopwords = stop_words, ngram = c(ngram_min=2, ngram_max=2), sep_ngram = " ")

tri_vocab <- create_vocabulary(tri_it,  stopwords = stop_words, ngram = c(ngram_min=3, ngram_max=3), sep_ngram = " ")

############Word Cloud
# Create a wordcloud for the values in vocab
wordcloud(vocab$term, vocab$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"))

# Create a wordcloud for the values in bi_vocab
wordcloud(bi_vocab$term, bi_vocab$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"))

# Create a wordcloud for the values in tri_vocab
wordcloud(tri_vocab$term, tri_vocab$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"))



############
#Sentiment Stuff
tidyvocab <- tbl_df(vocab)

bing_lex <- get_sentiments("bing")
mytext_bing <- inner_join(tidyvocab, bing_lex, by = c("term" = "word"))

table(mytext_bing$sentiment)

############
#Positive and Negative Word Clouds
##this is a mess and I'm sorry
#subsetting the positive/negative words
postext <- mytext_bing %>%
  filter(sentiment == 'positive')

negtext <- mytext_bing %>%
  filter(sentiment == 'negative')


postext <- data.frame(postext$term, postext$term_count)
negtext <- data.frame(negtext$term, negtext$term_count)
words <- data.frame(mytext_bing$term)

colnames(words) <- "term"
colnames(postext)[1] <- "term"
colnames(postext)[2] <- "term_count"
colnames(negtext)[1] <- "term"
colnames(negtext)[2] <- "term_count"

test <- merge(x = words, y = postext, by = "term", all = TRUE)
test2 <- merge(x = test, y = negtext, by = "term", all = TRUE)
test2[is.na(test2)] <- 0

colnames(test2)[2] <- "positive"
colnames(test2)[3] <- "negative"

test3 <- unique(test2)

rownames(test3) <- test3$term
test4 <- within(test3, rm('term'))

compare <- as.matrix(test4)
comparison.cloud(compare,colors=brewer.pal(8, "Dark2"),max.words = 200, random.order=FALSE)

########
#Sentiment Stuff

afinn_lex <- get_sentiments("afinn")
mytext_afinn <- inner_join(tidyvocab, afinn_lex, by = c("term" = "word"))

table(afinn_lex$score)

mytext_bing$sentiment_n <- ifelse(mytext_bing$sentiment=="negative", -1, 1)
mytext_bing$sentiment_score <- mytext_bing$term_count*mytext_bing$sentiment_n

sentiment_summary <- mytext_bing %>%
  group_by(term) %>%
  summarize(review_sentiment = sum(sentiment_score)) %>%
  arrange(desc(review_sentiment))

hist(afinn_lex$score)

###Radar chart
###Emotions
library(radarchart)
nrc_lex <- get_sentiments("nrc")
mytext_nrc <- inner_join(tidyvocab, nrc_lex, by = c("term" = "word"))
mytext_nrc_noposneg <- mytext_nrc[!(mytext_nrc$sentiment %in% c("positive","negative")),]
emotion_summary <- mytext_nrc_noposneg %>%
  group_by(term,sentiment) %>%
  summarize(review_sentiment = sum(term_count)) %>%
  arrange(desc(review_sentiment))

emotion_overall_summary <- mytext_nrc_noposneg %>%
  group_by(sentiment) %>%
  summarize(review_sentiment = sum(term_count)) %>%
  arrange(desc(review_sentiment))

chartJSRadar(emotion_overall_summary)


############
##Shiny
library(shiny)
#install.packages('shinydashboard')
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("ReadMe", tabName = "ReadMe", icon = icon("book")),
      menuItem("WordCloud", tabName = "WordCloud", icon = icon("cloud")),
      menuItem("Bigram", tabName = "Bigram", icon = icon("cloud")),
      menuItem("Trigram", tabName = "Trigram", icon = icon("cloud")),
      menuItem("Histogram", tabName = "Histogram", icon = icon("bar-chart-o")),
      menuItem("RadarChart", tabName = "RadarChart", icon = icon("list-alt")),
      menuItem("Comparison", tabName = "Comparison", icon = icon("balance-scale"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "ReadMe",
              h2("ReadMe"),
              textOutput("readme")),
      tabItem(tabName = 'WordCloud',
              fluidRow(
                box(title = "Word Cloud", plotOutput('wordcloud', height = 500, width = 500)))),
      tabItem(tabName = "Bigram",
              fluidRow(
                box(title = "Bigram", plotOutput('bigram', height = 700, width = 700)))),
      tabItem(tabName = "Trigram",
              fluidRow(
                box(title = "Trigram", plotOutput('trigram', height = 700, width = 700)))),
      tabItem(tabName = "Histogram",
              fluidRow(
                box(title = "Histogram", plotOutput('hist', height = 500, width = 500)))),
      tabItem(tabName = "RadarChart",
              fluidRow(
                box(title = "RadarChart", chartJSRadarOutput('rc', height = 700, width = 700)))),
      tabItem(tabName = "Comparison",
              fluidRow(
                box(title = "Comparison", plotOutput('cc', height = 500, width = 500))))
  )
)
)

server <- function(input, output) {
  output$wordcloud <- renderPlot(
    wordcloud(vocab$term, vocab$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"))
    
  )
  output$bigram <- renderPlot(
    wordcloud(bi_vocab$term, bi_vocab$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"))
    
  )
  output$trigram <- renderPlot(
    wordcloud(tri_vocab$term, tri_vocab$term_count,min.freq=5,max.words=200,colors=brewer.pal(8, "Paired"))
    
  )
  output$hist <- renderPlot(
    hist(afinn_lex$score)
  )
  output$rc <- renderChartJSRadar(
    chartJSRadar(emotion_overall_summary)
  )
  output$cc <- renderPlot(
    comparison.cloud(compare,colors=brewer.pal(8, "Dark2"),max.words = 200, random.order=FALSE)
  )
  output$readme <- renderText(
    "In the past, TripAdvisor and Yelp have come under fire for the fake reviews
    present on their sites. Companies were either hiring individuals to write
    extremely positive reviews for their business, or someone creates bots to 
    spam the TripAdvisor page with negative reviews. A 2018 research study found
    that around 1 in 3 reviews on these travel sites are fake. TripAdvisor denied
    these claims, saying 'The claims about fake reviews are based on entirely
    flawed techniques. Their methods are unreliable for one simple reason:
    they have no access to the technical data you would need to determine whether a 
    review is fake. We do - and we have been using this data for over
    a decade to track millions of reviews.' My goal for this project is to see
    if I can indentify the presence of fake reviews using sentiment analysis and
    wordcloud techniques."
  )
}

shinyApp(ui, server)
