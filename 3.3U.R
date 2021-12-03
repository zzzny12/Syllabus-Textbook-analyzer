library("shinycustomloader")
library("shiny")
library("plotly")
library("rsconnect")

library("dplyr")
library("tidytext")
library("text2vec")
library("readr")
library("stringr")
library("tidyverse")
library("shiny")
library("DT")
library("writexl")
library("dplyr")
library("text2vec")
library("readr")
library("stringr")
library("stopwords")
library("textstem")
library("tm")
library("NLP")
library("quanteda")
library("corpus")
library("tibble")
library("gsubfn")
library("plotly")



# Define UI ----
ui <- fluidPage(
  titlePanel("Syllabus Textbook analyzer"),
  sidebarLayout(
    sidebarPanel(
      p("*Upload the source .txt text file and target .csv table using the input buttons below."),
      p("*Please noticed:The first row of target .csv should be 'title' and 'description';"),
      p("*When finished, upload your files and click Analyze Data."),
      fileInput('syllabus', 'Upload syllabus File (.txt)', multiple = FALSE),
      fileInput(
        'textbook', 'Choose textbook File (.csv)', multiple = FALSE, accept = c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      
      actionButton("go",label="Analyze data", 
                   icon("bolt"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    ),
    mainPanel(
      downloadButton("download", "Download Similirity Scoring Result"),
      tabsetPanel(
        tabPanel("ScoringResults", 
                 dataTableOutput("case_syllabus_result")
        )
        
      )
      
    )
  ))


scoreResult = function(input1, input2) {
  case <- read.csv(input1, stringsAsFactors = F)
  resume_f <- read_file(input2)
  
  # make resume content a dataframe
  resume_fdf <- tibble(title = "User", description = resume_f, ISNB10 = "1538728621", 
                       ISNB13 = "978-1538728628", publisher="max")
  
  # combine resume and job description
  case_resume <- rbind(resume_fdf, case)
  
  # data cleaning function
  prep_fun = function(x) {
    x %>%
      # make text lower case
      str_to_lower() %>%
      # remove non-alphanumeric symbols
      str_replace_all("[^[:alnum:]]", " ") %>%
      # remove numbers
      {
        gsub(patter = "\\d", replace = " ", .)
      } %>%
      # remove stopwords
      removeWords(stopwords()) %>%
      # remove single character
      {
        gsub(patter = "\\b[A-z]\\b{1}", replace = " ", .)
      } %>%
      # collapse multiple spaces
      str_replace_all("\\s+", " ") %>%
      # lemmatization
      lemmatize_strings()
  }
  
  # clean the job description data and create a new column
  case_resume$description_clean = lemmatize_words(prep_fun(case_resume$description))
  
  # use vocabulary_based vectorization
  it_resume <-
    itoken(case_resume$description_clean, progressbar = FALSE)
  v_resume <- create_vocabulary(it_resume)
  
  # eliminate very frequent and very infrequent terms
  v_resume <- prune_vocabulary(v_resume)
  vectorizer_resume <- vocab_vectorizer(v_resume)
  
  # apply TF-IDF transformation
  dtm_resume <- create_dtm(it_resume, vectorizer_resume)
  tfidf <- TfIdf$new()
  dtm_tfidf_resume <- fit_transform(dtm_resume, tfidf)
  
  # compute similarity-score against each row
  resume_tfidf_cos_sim = sim2(x = dtm_tfidf_resume, method = "cosine", norm = "l2")
  
  # create a new column for similarity_score of dataframe
  case_resume <- case_resume %>%
    mutate(similarity_score = resume_tfidf_cos_sim[1:nrow(resume_tfidf_cos_sim)])
  
  # sort the dataframe by similarity score from the lowest to the highest
  case_syllabus_result <- case_resume %>%
    arrange(similarity_score %>% desc())%>%
    filter(title != "User") %>%
    select(c(title, similarity_score, ISNB10, ISNB13, publisher))
  
  return(case_syllabus_result)
  
}

# Server logic ----

server<-function(input , output, session) {
  restab <- eventReactive(input$go, {
    req(input$syllabus)
    req(input$textbook)
    scoreResult(input$textbook$datapath,
                input$syllabus$datapath)
  })
  output$case_syllabus_result <- renderDataTable({
    restab()# %>%
  },
  server = TRUE)
  output$download <- downloadHandler(
    filename = "Scoring_Result.xlsx",
    content = function(file1) {
      write_xlsx(restab(), file1)
    }
  )
} 

# Run the app ----

shinyApp(ui = ui, server = server)



