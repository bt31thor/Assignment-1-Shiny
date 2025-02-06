---
output: html_document
runtime: shiny
---
library(shiny)
library(ggplot2)
library(bslib)

ui <- page_sidebar(
  sidebar = sidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel("EDA Tool"),
      fileInput("file", "Choose CSV File", accept = ".csv"),
      uiOutput("num_var_select"),
      uiOutput("qual_var_select"),
      uiOutput("plot_type_select")
    ),
    card(
          tableOutput("contents"),
          plotOutput("plot")
        ),
  theme =  bs_theme(preset = "superhero"),
  input_switch("theme_switch", "Switch to light theme", value = FALSE)
  
)


server <- function(input, output, session) {
  observe({
    if (input$theme_switch) {
      session$setCurrentTheme(bs_theme(preset = "minty"))
    } else {
      session$setCurrentTheme(bs_theme(preset = "superhero"))
    }
  })
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    # Convert character columns to factors
    df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
    df
  })
  
  output$contents <- renderTable({
    head(data(), 6)
  })
  
  output$num_var_select <- renderUI({
    req(data())
    selectInput("num_var", "Select Numeric Variables", choices = names(data())[sapply(data(), is.numeric)], multiple = TRUE)
  })
  
  output$qual_var_select <- renderUI({
    req(data())
    selectInput("qual_var", "Select Qualitative Variable", choices = names(data())[sapply(data(), is.factor)])
  })
  
  output$plot_type_select <- renderUI({
    req(data())
    selectInput("plot_type", "Select Plot Type", choices = c("Histogram", "Bar Plot", "Scatterplot", "Mosaic Plot", "Boxplot"))
  })
  
  output$plot <- renderPlot({
    req(input$plot_type)
    plot_data <- data()
    
    if (input$plot_type == "Histogram") {
      req(input$num_var)
      ggplot(plot_data, aes_string(x = input$num_var[1])) + geom_histogram()
    } else if (input$plot_type == "Bar Plot") {
      req(input$qual_var)
      ggplot(plot_data, aes_string(x = input$qual_var)) + geom_bar()
    } else if (input$plot_type == "Scatterplot") {
      req(input$num_var)
      if (length(input$num_var) >= 2) {
        ggplot(plot_data, aes_string(x = input$num_var[1], y = input$num_var[2])) + geom_point() + geom_smooth(method = "lm")
      }
    } else if (input$plot_type == "Mosaic Plot") {
      req(input$num_var, input$qual_var)
      ggplot(plot_data, aes_string(x = input$qual_var, fill = input$num_var[1])) + geom_bar(position = "fill")
    } else if (input$plot_type == "Boxplot") {
      req(input$num_var, input$qual_var)
      ggplot(plot_data, aes_string(x = input$qual_var, y = input$num_var[1])) + geom_boxplot()
    }
  })
}

shinyApp(ui = ui, server = server)