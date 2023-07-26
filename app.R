library(shiny)
#library(plotly)
#library(lubridate)
#library(purrr)
#library(prophet)
library(bslib)
library(tidyverse)
#library(shinydashboard)
library(bsicons)

cards <- list(
  card(
    full_screen = TRUE,
    card_header("Data Science Activities"),
    plotOutput("plot_ds")
  ))

value_boxes <- list(
  value_box(
    value = textOutput("total_hours_ds_box"),
    title = "Data Science Hours",
    showcase = bs_icon("activity"),
    theme_color = "success"
  ),
  value_box(
    value = textOutput("total_hours_pm_box"),
    title = "Project Management Hours",
    showcase = bs_icon("kanban"),
    theme_color = "secondary"
  )
  )



ui <- page_sidebar(
  title = "Time Tracking Dashboard",
  sidebar = sidebar(
    div(style = "border: 1px solid #ccc; padding: 10px; margin-top: 10px;",
        p("Welcome to the Rock Lobster Predation Behaviour Dashboard!"),
        #p("Use the sidebar menu to navigate between different sections."),
        p("Select a tank from the radio buttons and an event ID from the dropdown."),
        #p("Explore the data and visualizations for each section.")
        ),
  ),
  page_fillable(
    layout_column_wrap(
      width="10px",
      fill = FALSE,
      value_boxes[[1]], value_boxes[[2]]
  )
  ),
  layout_columns(cards[[1]])
)

# Define the server code
server <- function(input, output) {
  # Reactive function that reads the dataframes from the directory www/data.csv
  data <- reactive({
    data <- read.csv("www/data.csv") %>%
      mutate(type = ifelse(tags == "AI / Deep Learning", "Data Science and Analytics", tags),
            type = ifelse(tags == "Data Move/Store", "Data Science and Analytics", type),
            type = ifelse(tags == "Data Explore/Visualisation", "Data Science and Analytics", type),
            type = ifelse(tags == "UI Design", "Data Science and Analytics", type),
            type = ifelse(tags == "Comms/Report and BI", "Business Intelligence and Reporting", type),
            type = ifelse(tags == "Sci Writing", "Business Intelligence and Reporting", type),
            type = ifelse(tags == "Proj Manage", "Project Management", type),
            type = ifelse(tags == "Prod Manage", "Product Management", type))
  return(data)
  })

  # Render text with the total hours in data science type
  output$total_hours_ds_box <- renderText({
    data() %>% filter(type == "Data Science and Analytics") %>% summarise(total_hours=round(sum(Duration)/60)) %>% pull() %>% as.character()
  })

  # Render text with the total hours in project management type
  output$total_hours_pm_box <- renderText({
    data() %>% filter(type == "Project Management") %>% summarise(total_hours=round(sum(Duration)/60)) %>% pull() %>% as.character()
  })

  # Render ggplot bar graph for Data Science and Analytics
  output$plot_ds <- renderPlot({
    data() %>%
      filter(type == "Data Science and Analytics") %>%
      group_by(tags) %>%
      summarise(Duration = sum(Duration)/60) %>%
      ggplot(aes(x = tags, y = Duration)) +
      geom_bar(stat = "identity", fill = "#00BFC4") +
      labs(title = "Data Science and Analytics", x = "Activity", y = "Duration (hours)") +
      theme_minimal()
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
