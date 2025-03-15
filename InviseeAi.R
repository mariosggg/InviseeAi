# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
if (!requireNamespace("RPostgres", quietly = TRUE)) install.packages("RPostgres")
# Ensure shinydashboard is installed and loaded
if (!requireNamespace("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")
library(shinydashboard)
# Load required libraries
library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(caret)
library(plotly)
library(RPostgres)
library(shinythemes)

# Database connection
con <- dbConnect(RPostgres::Postgres(), dbname = 'Invisee', host = 'localhost', port = 5433, user = 'postgres', password = '12345')

# Function to fetch health data from PostgreSQL
fetch_health_data <- function(con) {
  dbGetQuery(con, 'SELECT * FROM health_data')
}

# Function to apply K-anonymity
apply_k_anonymity <- function(con, k) {
  query <- "
    WITH anon_data AS (
      SELECT
        CASE
          WHEN age BETWEEN 20 AND 30 THEN '20-30'
          WHEN age BETWEEN 30 AND 40 THEN '30-40'
          WHEN age BETWEEN 40 AND 50 THEN '40-50'
          WHEN age BETWEEN 50 AND 60 THEN '50-60'
          WHEN age BETWEEN 60 AND 70 THEN '60-70'
          WHEN age BETWEEN 70 AND 80 THEN '70-80'
        END AS age_group,
        gender,
        CASE
          WHEN bmi BETWEEN 15 AND 20 THEN 'Underweight'
          WHEN bmi BETWEEN 20 AND 25 THEN 'Normal'
          WHEN bmi BETWEEN 25 AND 30 THEN 'Overweight'
          WHEN bmi BETWEEN 30 AND 35 THEN 'Obese'
          WHEN bmi BETWEEN 35 AND 40 THEN 'Severely Obese'
        END AS bmi_group
      FROM health_data
    )
    SELECT age_group, gender, bmi_group, COUNT(*) as count
    FROM anon_data
    GROUP BY age_group, gender, bmi_group
    HAVING COUNT(*) >= $1;
  "
  dbGetQuery(con, query, params = list(k))
}

# Function to apply I-diversity
apply_i_diversity <- function(con, i) {
  query <- "
    WITH anon_data AS (
      SELECT
        CASE
          WHEN age BETWEEN 20 AND 30 THEN '20-30'
          WHEN age BETWEEN 30 AND 40 THEN '30-40'
          WHEN age BETWEEN 40 AND 50 THEN '40-50'
          WHEN age BETWEEN 50 AND 60 THEN '50-60'
          WHEN age BETWEEN 60 AND 70 THEN '60-70'
          WHEN age BETWEEN 70 AND 80 THEN '70-80'
        END AS age_group,
        gender,
        CASE
          WHEN bmi BETWEEN 15 AND 20 THEN 'Underweight'
          WHEN bmi BETWEEN 20 AND 25 THEN 'Normal'
          WHEN bmi BETWEEN 25 AND 30 THEN 'Overweight'
          WHEN bmi BETWEEN 30 AND 35 THEN 'Obese'
          WHEN bmi BETWEEN 35 AND 40 THEN 'Severely Obese'
        END AS bmi_group,
        diabetes
      FROM health_data
    ),
    diversity_check AS (
      SELECT age_group, gender, bmi_group, COUNT(DISTINCT diabetes) as diversity
      FROM anon_data
      GROUP BY age_group, gender, bmi_group
    )
    SELECT anon_data.*
    FROM anon_data
    JOIN diversity_check
    ON anon_data.age_group = diversity_check.age_group
    AND anon_data.gender = diversity_check.gender
    AND anon_data.bmi_group = diversity_check.bmi_group
    WHERE diversity_check.diversity >= $1;
  "
  dbGetQuery(con, query, params = list(i))
}

# Function to apply T-closeness
apply_t_closeness <- function(con, t) {
  query <- "
    WITH anon_data AS (
      SELECT
        CASE
          WHEN age BETWEEN 20 AND 30 THEN '20-30'
          WHEN age BETWEEN 30 AND 40 THEN '30-40'
          WHEN age BETWEEN 40 AND 50 THEN '40-50'
          WHEN age BETWEEN 50 AND 60 THEN '50-60'
          WHEN age BETWEEN 60 AND 70 THEN '60-70'
          WHEN age BETWEEN 70 AND 80 THEN '70-80'
        END AS age_group,
        gender,
        CASE
          WHEN bmi BETWEEN 15 AND 20 THEN 'Underweight'
          WHEN bmi BETWEEN 20 AND 25 THEN 'Normal'
          WHEN bmi BETWEEN 25 AND 30 THEN 'Overweight'
          WHEN bmi BETWEEN 30 AND 35 THEN 'Obese'
          WHEN bmi BETWEEN 35 AND 40 THEN 'Severely Obese'
        END AS bmi_group,
        diabetes
      FROM health_data
    ),
    diabetes_distribution AS (
      SELECT age_group, gender, bmi_group, diabetes, COUNT(*) as count
      FROM anon_data
      GROUP BY age_group, gender, bmi_group, diabetes
    ),
    total_counts AS (
      SELECT age_group, gender, bmi_group, SUM(count) as total
      FROM diabetes_distribution
      GROUP BY age_group, gender, bmi_group
    ),
    closeness_check AS (
      SELECT d.age_group, d.gender, d.bmi_group,
             MAX(ABS(d.count::float / t.total - 0.5)) as distance
      FROM diabetes_distribution d
      JOIN total_counts t
      ON d.age_group = t.age_group AND d.gender = t.gender AND d.bmi_group = t.bmi_group
      GROUP BY d.age_group, d.gender, d.bmi_group
    )
    SELECT anon_data.*
    FROM anon_data
    JOIN closeness_check
    ON anon_data.age_group = closeness_check.age_group
    AND anon_data.gender = closeness_check.gender
    AND anon_data.bmi_group = closeness_check.bmi_group
    WHERE closeness_check.distance <= $1;
  "
  dbGetQuery(con, query, params = list(t))
}

# Function to apply Differential Privacy
apply_differential_privacy <- function(con, epsilon) {
  add_laplace_noise <- function(x, epsilon) {
    sensitivity <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
    noise <- rlaplace(length(x), 0, sensitivity / epsilon)
    return(x + noise)
  }
  
  data <- fetch_health_data(con)
  
  data <- data %>%
    mutate(
      age = add_laplace_noise(age, epsilon),
      bmi = add_laplace_noise(bmi, epsilon),
      bloodpressure = add_laplace_noise(bloodpressure, epsilon),
      cholesterol = add_laplace_noise(cholesterol, epsilon)
    )
  
  return(data)
}

# Laplace noise generator function
rlaplace <- function(n, mu, b) {
  u <- runif(n, min = -0.5, max = 0.5)
  return(mu - b * sign(u) * log(1 - 2 * abs(u)))
}

# Function to simulate federated learning
simulate_federated_learning <- function(data, num_clients = 5, rounds = 10) {
  # Ensure heartdisease column is present
  if (!"heartdisease" %in% colnames(data)) {
    stop("Column 'heartdisease' not found in the dataset.")
  }
  
  # Split data into clients
  client_data <- split(data, rep(1:num_clients, length.out = nrow(data)))
  
  # Initialize global model (using logistic regression for simplicity)
  global_model <- train(heartdisease ~ age + gender + bmi + bloodpressure + cholesterol + diabetes, 
                        data = data, method = "glm", family = "binomial")
  
  for (round in 1:rounds) {
    client_models <- lapply(client_data, function(client) {
      train(heartdisease ~ age + gender + bmi + bloodpressure + cholesterol + diabetes, 
            data = client, method = "glm", family = "binomial")
    })
    
    # Aggregate client models into the global model (simple averaging for demonstration)
    global_model$finalModel$coefficients <- Reduce("+", 
                                                   lapply(client_models, 
                                                          function(model) model$finalModel$coefficients)) / num_clients
  }
  
  return(global_model)
}

# Function to simulate secure multiparty computation
simulate_secure_computation <- function(data, num_parties = 3) {
  # Split data into parties
  party_data <- split(data, rep(1:num_parties, length.out = nrow(data)))
  
  # Each party computes some computations on its data (for demonstration, let's compute the mean of 'age')
  party_means <- lapply(party_data, function(party) {
    mean(party$age, na.rm = TRUE)
  })
  
  # Simulate secure aggregation (e.g., averaging the results of all parties)
  secure_mean <- mean(unlist(party_means))
  return(secure_mean)
}

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("InviseeAi"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("k_value", "Select k-value (K-Anonymity):", min = 2, max = 10, value = 3, step = 1),
      actionButton("anonymize", "Apply K-Anonymity"),
      sliderInput("i_value", "Select i-value (I-Diversity):", min = 2, max = 10, value = 2, step = 1),
      actionButton("diversify", "Apply I-Diversity"),
      sliderInput("t_value", "Select t-value (T-Closeness):", min = 0.01, max = 0.5, value = 0.1, step = 0.01),
      actionButton("t_closeness", "Apply T-Closeness"),
      sliderInput("epsilon_value", "Select epsilon-value (Differential Privacy):", min = 0.01, max = 1, value = 0.1, step = 0.01),
      actionButton("apply_dp", "Apply Differential Privacy"),
      numericInput("num_clients", "Number of Clients (Federated Learning):", value = 5, min = 2, max = 10, step = 1),
      numericInput("rounds", "Number of Rounds (Federated Learning):", value = 10, min = 1, max = 100, step = 1),
      actionButton("federated_learning", "Simulate Federated Learning"),
      numericInput("num_parties", "Number of Parties (Secure Computation):", value = 3, min = 2, max = 10, step = 1),
      actionButton("secure_computation", "Simulate Secure Computation")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Original Data", DTOutput("original_table")),
        tabPanel("K-Anonymized Data", DTOutput("anonymized_table")),
        tabPanel("I-Diversity Data", DTOutput("diversified_table")),
        tabPanel("T-Closeness Data", DTOutput("t_closeness_table")),
        tabPanel("Differential Privacy Data", DTOutput("dp_table")),
        tabPanel("Federated Learning Model", verbatimTextOutput("federated_model")),
        tabPanel("Secure Computation", verbatimTextOutput("secure_computation_result")),
        tabPanel("Data Visualization",
                 fluidRow(
                   valueBoxOutput("total_patients", width = 4),
                   valueBoxOutput("avg_age", width = 4),
                   valueBoxOutput("avg_bmi", width = 4)
                 ),
                 fluidRow(
                   column(6,
                          sliderInput("age_range", "Select Age Range:", min = 20, max = 80, value = c(20, 80), step = 1),
                          plotlyOutput("age_histogram")
                   ),
                   column(6,
                          sliderInput("bmi_range", "Select BMI Range:", min = 15, max = 40, value = c(15, 40), step = 1),
                          plotlyOutput("bmi_bar_chart")
                   )
                 ),
                 fluidRow(
                   column(6, plotlyOutput("gender_pie_chart")),
                   column(6, plotlyOutput("diabetes_stacked_bar_chart"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("cholesterol_histogram")),
                   column(6, plotlyOutput("cholesterol_box_plot"))
                 )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Fetch health data
  health_data <- reactive({
    fetch_health_data(con)
  })
  
  # Value boxes
  output$total_patients <- renderValueBox({
    valueBox(
      value = nrow(health_data()),
      subtitle = "Total Patients",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_age <- renderValueBox({
    valueBox(
      value = round(mean(health_data()$age, na.rm = TRUE), 1),
      subtitle = "Average Age",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  
  output$bmi_bar_chart <- renderPlotly({
    # Example BMI categories by gender
    bmi_gender_data <- data.frame(
      BMICategory = rep(c("Underweight", "Normal", "Overweight", "Obese", "Severely Obese"), 2),
      Gender = rep(c("Male", "Female"), each = 5),
      Count = c(5, 30, 25, 20, 10, 8, 35, 30, 18, 9)
    )
    
    # Create stacked bar chart
    plot_ly(bmi_gender_data, x = ~BMICategory, y = ~Count, color = ~Gender, type = "bar") %>%
      layout(title = "BMI Distribution by Gender", barmode = "stack", yaxis = list(title = "Count"))
  })
  
  
  
  
  # Display 100 rows of original data
  output$original_table <- renderDT({
    datatable(health_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Apply K-anonymity when button is clicked
  anonymized_data <- eventReactive(input$anonymize, {
    data <- apply_k_anonymity(con, input$k_value)
    return(data)
  })
  
  # Display 100 rows of K-anonymized data
  output$anonymized_table <- renderDT({
    req(anonymized_data())
    datatable(anonymized_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Apply I-diversity when button is clicked
  diversified_data <- eventReactive(input$diversify, {
    data <- apply_i_diversity(con, input$i_value)
    return(data)
  })
  
  # Display 100 rows of I-diversity data
  output$diversified_table <- renderDT({
    req(diversified_data())
    datatable(diversified_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Apply T-closeness when button is clicked
  t_closeness_data <- eventReactive(input$t_closeness, {
    data <- apply_t_closeness(con, input$t_value)
    return(data)
  })
  
  # Display 100 rows of T-closeness data
  output$t_closeness_table <- renderDT({
    req(t_closeness_data())
    datatable(t_closeness_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Apply Differential Privacy when button is clicked
  dp_data <- eventReactive(input$apply_dp, {
    data <- apply_differential_privacy(con, input$epsilon_value)
    return(data)
  })
  
  # Display 100 rows of Differential Privacy data
  output$dp_table <- renderDT({
    req(dp_data())
    datatable(dp_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Simulate Federated Learning when button is clicked
  federated_model <- eventReactive(input$federated_learning, {
    data <- health_data()
    model <- simulate_federated_learning(data, input$num_clients, input$rounds)
    return(model)
  })
  
  # Display Federated Learning model summary
  output$federated_model <- renderPrint({
    req(federated_model())
    summary(federated_model()$finalModel)
  })
  
  # Simulate Secure Multiparty Computation when button is clicked
  secure_computation_result <- eventReactive(input$secure_computation, {
    data <- health_data()
    result <- simulate_secure_computation(data, input$num_parties)
    return(result)
  })
  
  # Display Secure Computation result
  output$secure_computation_result <- renderPrint({
    req(secure_computation_result())
    print(secure_computation_result())
  })
  
  # Age histogram
  output$age_histogram <- renderPlotly({
    req(health_data())
    data <- health_data() %>%
      filter(age >= input$age_range[1] & age <= input$age_range[2])
    
    plot_ly(data, x = ~age, type = "histogram") %>%
      layout(title = "Age Distribution", xaxis = list(title = "Age"), yaxis = list(title = "Count"))
  })
  
  # BMI heatmap
  output$bmi_heatmap <- renderPlotly({
    req(health_data())
    data <- health_data() %>%
      filter(bmi >= input$bmi_range[1] & bmi <= input$bmi_range[2])
    
    plot_ly(data, x = ~age, y = ~bmi, type = "heatmap", colors = colorRamp(c("blue", "red"))) %>%
      layout(title = "BMI Distribution", xaxis = list(title = "Age"), yaxis = list(title = "BMI"))
  })
  
  # Gender pie chart
  output$gender_pie_chart <- renderPlotly({
    req(health_data())
    data <- health_data() %>%
      count(gender)
    
    plot_ly(data, labels = ~gender, values = ~n, type = "pie") %>%
      layout(title = "Gender Distribution")
  })
  
  # Diabetes stacked bar chart
  output$diabetes_stacked_bar_chart <- renderPlotly({
    req(health_data())
    data <- health_data() %>%
      count(gender, diabetes)
    
    plot_ly(data, x = ~gender, y = ~n, color = ~diabetes, type = "bar") %>%
      layout(title = "Diabetes Distribution", barmode = "stack", xaxis = list(title = "Gender"), yaxis = list(title = "Count"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)