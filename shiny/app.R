##################
# https://jiayin-qu.shinyapps.io/projectA_Qu/
# The APP is deployed here. However, there's an error stating that it is unable to connect after 60.00 seconds. 
# It seems like data retrieves from twitter is too big with the 1GB limit of the datastorage in shinyapp
##################
  
# Libraries
library(tidyverse)
library(twitteR)
library(shiny)
library(DT)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(wordcloud)

# Data import
api <- 'e0y2pHHrYtT320wP8WSsPa0i7'
apiSecret <- 'Ewv63M79SR6ocpWGbW5lpOgiX3z8aMJddwqR5Fx70tjxo4lb4r'
access <- '1195948151179501569-THuTApWDbYUrbooDA0iSldWpgY63WT'
accessSecret <- 'OY5ESKILi1GgVivsbM1JeM3UmWLISO7ToADW8V9ZlZL4v'
setup_twitter_oauth(api, apiSecret, access, accessSecret)

COVID <- searchTwitter("#COVID", 500)
COVID_tbl <- twListToDF(COVID)
COVID_tbl$text <- COVID_tbl$text %>% 
  iconv("UTF-8", "ASCII", sub = "")

COVID19 <- searchTwitter("#COVID19", 500)
COVID19_tbl <- twListToDF(COVID19)
COVID19_tbl$text <- COVID19_tbl$text %>% 
  iconv("UTF-8", "ASCII", sub = "")

COVIDhyphen19 <- searchTwitter("#COVID-19", 500)
COVIDhyphen19_tbl <- twListToDF(COVIDhyphen19)
COVIDhyphen19_tbl$text <- COVIDhyphen19_tbl$text %>% 
  iconv("UTF-8", "ASCII", sub = "")

COVIDdash19 <- searchTwitter("#COVID_19", 500)
COVIDdash19_tbl <- twListToDF(COVIDdash19)
COVIDdash19_tbl$text <- COVIDdash19_tbl$text %>% 
  iconv("UTF-8", "ASCII", sub = "")

dataset <- bind_rows(COVID_tbl, COVID19_tbl, COVIDhyphen19_tbl, COVIDdash19_tbl)

saveRDS(COVID_tbl, "COVID.rds")
saveRDS(COVID19_tbl, "COVID19.rds")
saveRDS(COVIDhyphen19_tbl, "COVIDhyphen19.rds")
saveRDS(COVIDdash19_tbl, "COVIDdash19.rds")
saveRDS(dataset, "dataset.rds")

# Data cleaning
cleanData <- function(dbl){
  dbl$text <- gsub("\\@[a-zA-Z]+", "", dbl$text)
  twitter_cp <- VCorpus(VectorSource(dbl$text))
  twitter_cp <- tm_map(twitter_cp, PlainTextDocument)
  twitter_cp <- tm_map(twitter_cp, removeNumbers)
  twitter_cp <- tm_map(twitter_cp, content_transformer(replace_abbreviation))
  twitter_cp <- tm_map(twitter_cp, content_transformer(replace_contraction))
  twitter_cp <- tm_map(twitter_cp, content_transformer(str_to_lower))
  twitter_cp <- tm_map(twitter_cp, removePunctuation)
  twitter_cp <- tm_map(twitter_cp, removeWords, c(stopwords(kind = "en"), stopwords(kind = "fr"), stopwords(kind = "es")))
  twitter_cp <- tm_map(twitter_cp, stripWhitespace)
  
  twitter_cp <- tm_map(twitter_cp, content_transformer(lemmatize_strings))
  
  twitter_dtm <- DocumentTermMatrix(twitter_cp)
  twitter_dtm <- removeSparseTerms(twitter_dtm, .99)
  tokenCounts <- apply(twitter_dtm, 1, sum)
  twitter_dtm <- twitter_dtm[tokenCounts>0, ]
  
  return(twitter_dtm)
}

COVID_tbl <- readRDS("COVID.rds")
COVID_dtm <- cleanData(COVID_tbl)
COVID_dtm_m <- as.matrix(COVID_dtm)
wordCounts_COVID <- colSums(COVID_dtm_m)
wordNames_COVID <- names(wordCounts_COVID)
tbl_COVID <- tibble(wordNames_COVID, wordCounts_COVID) %>%
  arrange(desc(wordCounts_COVID)) %>%
  filter(wordCounts_COVID > 5) %>%
  mutate(wordNames_COVID = reorder(wordNames_COVID, wordCounts_COVID))

COVID19_tbl <- readRDS("COVID19.rds")
COVID19_dtm <- cleanData(COVID19_tbl)
COVID19_dtm_m <- as.matrix(COVID19_dtm)
wordCounts_COVID19 <- colSums(COVID19_dtm_m)
wordNames_COVID19 <- names(wordCounts_COVID19)
tbl_COVID19 <- tibble(wordNames_COVID19, wordCounts_COVID19) %>%
  arrange(desc(wordCounts_COVID19)) %>%
  filter(wordCounts_COVID19 > 5) %>%
  mutate(wordNames_COVID19 = reorder(wordNames_COVID19, wordCounts_COVID19))

COVIDhyphen19_tbl <- readRDS("COVIDhyphen19.rds")
COVIDhyphen19_dtm <- cleanData(COVIDhyphen19_tbl)
COVIDhyphen19_dtm_m <- as.matrix(COVIDhyphen19_dtm)
wordCounts_COVIDhyphen19 <- colSums(COVIDhyphen19_dtm_m)
wordNames_COVIDhyphen19 <- names(wordCounts_COVIDhyphen19)
tbl_COVIDhyphen19 <- tibble(wordNames_COVIDhyphen19, wordCounts_COVIDhyphen19) %>%
  arrange(desc(wordCounts_COVIDhyphen19)) %>%
  filter(wordCounts_COVIDhyphen19 > 5) %>%
  mutate(wordNames_COVIDhyphen19 = reorder(wordNames_COVIDhyphen19, wordCounts_COVIDhyphen19))

COVIDdash19_tbl <- readRDS("COVIDdash19.rds")
COVIDdash19_dtm <- cleanData(COVIDdash19_tbl)
COVIDdash19_dtm_m <- as.matrix(COVIDdash19_dtm)
wordCounts_COVIDdash19 <- colSums(COVIDdash19_dtm_m)
wordNames_COVIDdash19 <- names(wordCounts_COVIDdash19)
tbl_COVIDdash19 <- tibble(wordNames_COVIDdash19, wordCounts_COVIDdash19) %>%
  arrange(desc(wordCounts_COVIDdash19)) %>%
  filter(wordCounts_COVIDdash19 > 5) %>%
  mutate(wordNames_COVIDdash19 = reorder(wordNames_COVIDdash19, wordCounts_COVIDdash19))

# Calculate the summary table that displays how many tokens with over 5 mentions overlap between each pair of hashtags
COVID_COVID19 <- full_join(tbl_COVID, tbl_COVID19, by = c("wordNames_COVID" = "wordNames_COVID19")) %>%
  filter(!is.na(wordCounts_COVID) & !is.na(wordCounts_COVID19))
COVID_COVID19_num <- nrow(COVID_COVID19)

COVID_COVIDhyphen19 <- full_join(tbl_COVID, tbl_COVIDhyphen19, by = c("wordNames_COVID" = "wordNames_COVIDhyphen19")) %>%
  filter(!is.na(wordCounts_COVID) & !is.na(wordCounts_COVIDhyphen19))
COVID_COVIDhyphen19_num <- nrow(COVID_COVIDhyphen19)

COVID_COVIDdash19 <- full_join(tbl_COVID, tbl_COVIDdash19, by = c("wordNames_COVID" = "wordNames_COVIDdash19")) %>%
  filter(!is.na(wordCounts_COVID) & !is.na(wordCounts_COVIDdash19))
COVID_COVIDdash19_num <- nrow(COVID_COVIDdash19)

COVID19_COVIDhyphen19 <- full_join(tbl_COVID19, tbl_COVIDhyphen19, by = c("wordNames_COVID19" = "wordNames_COVIDhyphen19")) %>%
  filter(!is.na(wordCounts_COVID19) & !is.na(wordCounts_COVIDhyphen19))
COVID19_COVIDhyphen19_num <- nrow(COVID19_COVIDhyphen19)

COVID19_COVIDdash19 <- full_join(tbl_COVID19, tbl_COVIDdash19, by = c("wordNames_COVID19" = "wordNames_COVIDdash19")) %>%
  filter(!is.na(wordCounts_COVID19) & !is.na(wordCounts_COVIDdash19))
COVID19_COVIDdash19_num <- nrow(COVID19_COVIDdash19)

COVIDhyphen19_COVIDdash19 <- full_join(tbl_COVIDhyphen19, tbl_COVIDdash19, by = c("wordNames_COVIDhyphen19" = "wordNames_COVIDdash19")) %>%
  filter(!is.na(wordCounts_COVIDhyphen19) & !is.na(wordCounts_COVIDdash19))
COVIDhyphen19_COVIDdash19_num <- nrow(COVIDhyphen19_COVIDdash19)

summary_table <- bind_cols(comparison = c("COVID_COVID19", "COVID_COVID-19", "COVID_COVID_19", "COVID19_COVID-19", "COVID19_COVID_19", "COVID-19_COVID_19"), 
                           num_of_overlaps = c(COVID_COVID19_num, COVID_COVIDhyphen19_num, COVID_COVIDdash19_num, COVID19_COVIDhyphen19_num, COVID19_COVIDdash19_num, COVIDhyphen19_COVIDdash19_num))


# Prepare dataset for a summary bar chart
dataset <- readRDS("dataset.rds")
dataset_dtm <- cleanData(dataset)
dataset_dtm_m <- as.matrix(dataset_dtm)
wordCounts_total <- colSums(dataset_dtm_m)
wordNames_total <- names(wordCounts_total)

tbl <- tibble(wordNames_total, wordCounts_total) %>%
  arrange(desc(wordCounts_total)) %>%
  top_n(20) %>%
  mutate(wordNames_total = reorder(wordNames_total, wordCounts_total))


# APP interface
ui <- fluidPage(
  titlePanel("projectA_shiny"), 
  
  sidebarLayout(
    sidebarPanel(
      selectInput("hashtag", 
                  "Select hashtag", 
                  c("COVID", "COVID19", "COVID-19", "COVID_19")
                  )
      
    ), 
    mainPanel(plotOutput("wordcloud"), 
              plotOutput("barchart"),
              DT::dataTableOutput("table")
    )
  )
)

server <- function(input, output){
  shiny.maxRequestSize=50*1024^2
  
  filtered_data <- reactive({
    if(input$hashtag == "COVID"){
      twitter_dtm_m <- COVID_dtm_m
    }else if(input$hashtag == "COVID19"){
      twitter_dtm_m <- COVID19_dtm_m
    }else if(input$hashtag == "COVID-19"){
      twitter_dtm_m <- COVIDhyphen19_dtm_m
    }else if(input$hashtag == "COVID_19"){
      twitter_dtm_m <- COVIDdash19_dtm_m
    }
    twitter_dtm_m
  })
  
  output$wordcloud <- renderPlot({
    twitter_dtm_m <- filtered_data()
    wordCounts <- colSums(twitter_dtm_m)
    wordNames <- names(wordCounts)
    
    wordcloud(wordNames, wordCounts, max.words = 50, colors = "blue")
  })

  output$barchart <- renderPlot({
    ggplot(tbl, aes(x = wordNames_total, y = wordCounts_total)) + geom_col() + coord_flip() + xlab("word counts") + ylab("words")
  })
  
  output$table <- DT::renderDataTable({
    summary_table
  })
}
  


# Run the application
shinyApp(ui = ui, server = server)