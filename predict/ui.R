library(shiny)


shinyUI(fluidPage(
  
  
  titlePanel("What's the lucky word?"),
  
   
  sidebarLayout(
    sidebarPanel(
      
      p(" Please enter in a phrase. Please feel free to use uppercase characters,
         numbers, punctuation and whitespaces. Once you enter the phrase, please
         click the SUBMIT button. The function will return a word in all lowercase
         letters with no numbers, punctuation nor whitespaces."),  
      
       textInput("phrase", "Please enter in a phrase"),
       submitButton("SUBMIT")
    ),
    
    
    mainPanel(
      
      h1("DRUMROLL PLEASE... AND THE LUCKY WORD IS..."),
      
       textOutput("nextWord")
    )
  )
))
