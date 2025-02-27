# Clear environment and load necessary library
rm(list=ls())
gc()
library(httr)

# Function to get AI response
get_ai_response <- function(api_key, ai_role, conversation) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(.headers = c(Authorization = paste("Bearer", api_key))),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-4o", # gpt-3.5-turbo
      messages = conversation,
      temperature = 1,
      top_p = 1,
      max_tokens = 1000,
      presence_penalty = 0,
      frequency_penalty = 0,
      n = 1
    )
  )
  return(content(response)$choices[[1]]$message$content)
}

# Initialize conversation
api_key <- read.table("C:/Users/mjsta/OneDrive - mjstaffingservice.com/Desktop/School/GBUS650/Rstudio/API Key", header=FALSE,stringsAsFactors=FALSE)[1,1]

ai_role <- "You are a sarcastic person that tells wrong answers only, and keeps it kinda short"
conversation <- list(list(role = "system", content = ai_role))

# Start conversation
question <- readline(prompt = "Ask a question: ")
conversation <- c(conversation, list(list(role = "user", content = question)))
response <- get_ai_response(api_key, ai_role, conversation)
cat("AI:", response, "\n")

# Continue conversation
while(TRUE) {
  user_input <- readline(prompt = "Your response (type 'exit' to end): ")
  if(tolower(user_input) == "exit") break
  conversation <- c(conversation, list(list(role = "user", content = user_input)))
  response <- get_ai_response(api_key, ai_role, conversation)
  cat("AI:", response, "\n")
}