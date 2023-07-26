# Author: Sebastian Lopez-Marcano
# Date: 2023-07-26

# Load required packages
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(bsicons))

# Define the UI cards that contain the visualizations
cards <- list(
  card(
    full_screen = TRUE,
    card_header("Data Science Activities"),
    plotOutput("plot_ds")
  ))

# Define the UI value boxes that contain the total hours per type
value_boxes <- list(
  value_box(
    value = textOutput("total_hours_ds_box"),
    title = "Data Science Hours",
    showcase = bs_icon("activity"),
    theme_color = "success",
    p('January to July 2023')
  ),
  value_box(
    value = textOutput("total_hours_pm_box"),
    title = "Project Management Hours",
    showcase = bs_icon("kanban"),
    theme_color = "success",
    p('January to July 2023')
  ),
  value_box(
    value = textOutput("total_hours_product_box"),
    title = "Product Management Hours",
    showcase = bs_icon("cash"),
    theme_color = "info",
    p('January to July 2023')
  ),
  value_box(
    value = textOutput("total_hours_bi_box"),
    title = "Business Intelligence Hours",
    showcase = bs_icon("briefcase"),
    theme_color = "info",
    p('January to July 2023')
  )
  )
  
# Define the UI code
ui <- page_sidebar(
  title = "💡Time Tracking Dashboard",
  theme = bs_theme(bootswatch = "minty"),
  sidebar = sidebar(
    div(style = "border: 1px solid #ccc; padding: 10px; margin-top: 10px;",
        p("Welcome to my time tracking dashboard!"),
        p("This dashboard will show a snippet of the activities I have been doing in the last 6 months."),
        p("The data is collected from my time tracking app and it is updated every 6 months."),
        p("Check out TogglTrack for more information about the app")
        #p("Explore the data and visualizations for each section.")
        ),
    div(style = "border: 1px solid #ccc; padding: 10px; margin-top: 10px;",
      p("Dr Sebastian Lopez-Marcano"),
      p("Environmental Data Scientist for FishID"),
      p("Griffith University"),
      p("Brisbane, Australia"))),
  layout_columns(value_boxes[[1]], value_boxes[[2]]),
  layout_columns(value_boxes[[3]], value_boxes[[4]]),
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

  # Render text with the total hours in product management type
  output$total_hours_product_box <- renderText({
    data() %>% filter(type == "Product Management") %>% summarise(total_hours=round(sum(Duration)/60)) %>% pull() %>% as.character()
  })

  # Render text with the total hours in business intelligence type
  output$total_hours_bi_box <- renderText({
    data() %>% filter(type == "Business Intelligence and Reporting") %>% summarise(total_hours=round(sum(Duration)/60)) %>% pull() %>% as.character()
  })

  # Render ggplot bar graph for the percentage of time spent on each tags for data science type
  output$plot_ds <- renderPlot({
      data() %>%
      filter(type == "Data Science and Analytics") %>%
      group_by(tags) %>%
      summarise(total_hours = sum(Duration) / 60) %>%
      mutate(percentage_time = (total_hours / sum(total_hours)) * 100) %>%
      ggplot(aes(x = reorder(tags, percentage_time), y = percentage_time)) +
      geom_bar(stat = "identity", fill = "#ee7979c6") +
      labs(x = "Activity", y = "Percentage Time", subtitle = "January to July 2023") +
      theme_minimal() +
      #theme(axis.text.x = element_text(angle = 45, hjust = 1)) %>%
      coord_flip()
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)

