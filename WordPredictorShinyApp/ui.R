#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(tabsetPanel(
  type = "tabs",
  tabPanel(
    "Word prediction",

    p("Author: Eloy Insunza"),
    titlePanel("Word predictor"),
    p(
      "Insert some text in the input and select how many predictions you want to see."
    ),
    p(
      "First time, it takes more time since Shiny App is installing dependencies. The data used for the model has been reduced due to small Shiny capacity."
    ),
    p("You have more info about this predictor in the tab 'About the model'"),
    
    sidebarLayout(
      sidebarPanel(
        textInput("text_input",
                  "Please enter some text"),
        sliderInput(
          "words_number",
          "Number of alternatives",
          min = 1,
          max = 20,
          value = 3
        ),
        tableOutput("word_table")
      ),
      
      mainPanel(plotOutput("wordcloud"))
    )
  ),
  tabPanel(
    "About the model",
    titlePanel("About the model"),
    p(
      "The objective of this project is creating a Language Model to predict the next word taking as input the last words written by an user. The data consists of three data sets of sentences from blogs, twitter and news. The raw data is available", a("here.", href="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"),"In total +2M sentences are available."
    ),
    h2("Preprocessing"),
    p("The following preprocessing steps have been applied to the data :"),
    p(
      "- Remove the profanity words to avoid the predictor to propose this kind of words"
    ),
    p("- Remove numbers and punctuation to simplify the task"),
    p("- Replace word contractions (i.e., ‘d to would)"),
    p("- Set all the words to lowercase"),
    p("- Remove non character or standard white spaces elements"),
    p(
      "Stemming and removing stop words have not been applied. The objective of the predictor is giving the next word and it can be a stop word (to, with) or a conjugated verb."
    ),
    h2("The model"),
    p(
      "The text has been tokenized into uni-, bi- and trigrams and the least common words have been substituted for an unknown token to modelize the out of vocabulary inputs. The candidate words are chosen among the matching bi- and trigrams and the probability is obtained as an interpolation of the probabilities of each candidate in the different n-grams. For better performance, parallelization and vectorial filtering have been applied."
    ),
    h2("Improvements"),
    p(
      "The testing and perplexity analysis show some consistency but also some steps for improvement:"
    ),
    p(
      "- It cannot predict the use of numbers, punctuation or even uppercase."
    ),
    p(
      "- The prediction is limited to news, twitter or blogs. Other data sources can be used for bigger accuracy in other fields."
    ),
    p(
      "- The performance and size of the objects could be optimized to be applied in a mobile environment."
    ),
    p(
      "- Language detection can be used with more data sets to apply in other languages."
    ),
    h2("More information"),
    p("To know more about this project:"),
    p(a("- Slides", href = "https://rpubs.com/EloyID/1037248")),
    p(a("- RPubs", href = "https://rpubs.com/EloyID/1036329")),
    p("About me:"),
    p(a("- LinkedIn", href = "https://www.linkedin.com/in/eloy-insunza/?locale=en_US")),
      p(a("- GitHub", href = "https://github.com/EloyID"))
    
    
  )
))
