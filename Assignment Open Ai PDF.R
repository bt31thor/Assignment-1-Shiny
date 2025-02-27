# Load necessary libraries
# Load necessary libraries
library(shiny)
library(pdftools)
library(httr)
library(stringr)
library(shinythemes)

# Define UI for the app
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Times New Roman', Times, serif;
      }
      .response-box {
        border: 1px solid #ddd;
        padding: 10px;
        border-radius: 5px;
        background-color: #f9f9f9;
        white-space: pre-wrap; /* Ensures new lines are preserved */
      }
    "))
  ),
  titlePanel("PDF Text Processing and AI Interaction"),
  sidebarLayout(
    sidebarPanel(
      fileInput("pdf_file", "Choose PDF File", accept = ".pdf"),
      actionButton("process_summary", "Generate Summary"),
      actionButton("process_keypoints", "Extract Key Points"),
      textInput("custom_prompt", "Ask a Question", value = ""),
      actionButton("ask_question", "Submit Question")
    ),
    mainPanel(
      h3("AI Response"),
      div(class = "response-box", textOutput("response"))
    )
  ),
  theme = shinytheme("journal")
)

# Define server logic for the app
server <- function(input, output) {
  pdf_text_data <- reactiveVal(NULL)
  
  observeEvent(input$pdf_file, {
    req(input$pdf_file)
    
    # Read PDF text
    file_path <- input$pdf_file$datapath
    text <- pdf_text(file_path)
    
    # Save text data
    pdf_text_data(text)
  })
  
  observeEvent(input$process_summary, {
    req(pdf_text_data())
    
    # Process text
    text_processed <- str_c(pdf_text_data(), collapse = "\n")
    text_processed <- str_sub(text_processed, 1, 20000)
    
    # Get API key from environment variable
    api_key <- read.table("C:/Users/mjsta/OneDrive - mjstaffingservice.com/Desktop/School/GBUS650/Rstudio/API Key", header=FALSE,stringsAsFactors=FALSE)[1,1]
  
    
    # Get AI response for summary
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(.headers = c(Authorization = paste("Bearer", api_key))),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-3.5-turbo",
        messages = list(
          list(role = "system", content = "You are a helpful assistant."),
          list(role = "user", content = str_c("Summarize this paper: ", text_processed))
        )
      )
    )
    
    content <- content(response, "parsed")
    text_chatgpt_response <- content$choices[[1]]$message$content
    
    # Display response
    output$response <- renderText({
      text_chatgpt_response
    })
  })
  
  observeEvent(input$process_keypoints, {
    req(pdf_text_data())
    
    # Process text
    text_processed <- str_c(pdf_text_data(), collapse = "\n")
    text_processed <- str_sub(text_processed, 1, 20000)
    
    # Get API key from environment variable
    api_key <- Sys.getenv("OPENAI_API_KEY")
    
    # Get AI response for key points
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(.headers = c(Authorization = paste("Bearer", api_key))),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-3.5-turbo",
        messages = list(
          list(role = "system", content = "You are a helpful assistant."),
          list(role = "user", content = str_c("What are the key points of this paper: ", text_processed))
        )
      )
    )
    
    content <- content(response, "parsed")
    text_chatgpt_response <- content$choices[[1]]$message$content
    
    # Display response
    output$response <- renderText({
      text_chatgpt_response
    })
  })
  
  observeEvent(input$ask_question, {
    req(pdf_text_data(), input$custom_prompt)
    
    # Process text
    text_processed <- str_c(pdf_text_data(), collapse = "\n")
    text_processed <- str_sub(text_processed, 1, 20000)
    
    # Get API key from environment variable
    api_key <- Sys.getenv("OPENAI_API_KEY")
    
    # Get AI response for custom question
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(.headers = c(Authorization = paste("Bearer", api_key))),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-3.5-turbo",
        messages = list(
          list(role = "system", content = "You are a helpful assistant."),
          list(role = "user", content = str_c(input$custom_prompt, "\n\n", text_processed))
        )
      )
    )
    
    content <- content(response, "parsed")
    text_chatgpt_response <- content$choices[[1]]$message$content
    
    # Display response
    output$response <- renderText({
      text_chatgpt_response
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
