# Libraries
library(bslib)
library(DT)
library(janitor)
library(reactable)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(thematic)
library(tidyverse)

###############################################################################
# Setup

df <- readRDS("data/final/A201912.RDS")
df_labels <- readRDS("data/final/A201912_labels.RDS")
labels_vector <- df %>%
  colnames() %>%
  as_tibble() %>%
  left_join(df_labels, by = c("value" = "variable")) %>%
  pull(label)
factors_vector <- df %>%
  select_if(is.factor) %>%
  colnames() %>%
  as_tibble() %>%
  left_join(df_labels, by = c("value" = "variable")) %>%
  pull(label)
totals_vector <- c("None", "Row", "Column", "Both")
percentages_style <- c("Row", "Column", "Cell")

# Functions
light_theme <- bs_theme(bootswatch = "simplex", base_font = "Montserrat", version = 4)
dark_theme <- bs_theme(bootswatch = "darkly", base_font = "Montserrat", version = 4)
###############################################################################


###############################################################################
# UI

ui <- navbarPage("DARES",
  theme = light_theme,
  
  # View data
  tabPanel("Data",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "view_variable_choice",
          "Select variables to show:",
          choices = labels_vector,
          multiple = TRUE
        ),
        textOutput("variable_choice_length"),
        prettySwitch("dark_mode", "Dark mode", fill = TRUE)
      ),
      mainPanel(
        DT::dataTableOutput("view_table")
      )
    )
  ),

  # Tables
  tabPanel("Xtabs",
    tabsetPanel(
      
      ## ONEWAY TABLES
      ### SIDEBAR
      tabPanel("One-way",
        sidebarPanel(
          selectInput(
            "oneway_variable_choice",
            "Select variable to show:",
            choices = labels_vector,
            multiple = FALSE
          ),
          uiOutput("oneway_variable_category_drop_ui"),
          checkboxInput(
            "oneway_total",
            "Show total",
            value = FALSE
          ) 
        ),
        ### MAIN PANEL
        mainPanel(
          DT::dataTableOutput("oneway_table")
        )
      ),
      
      ## TWOWAY TABLES
      tabPanel("Two-way",
        ### SIDEBAR
        fluidRow(
          column(4,
            h4("Variables"),
            h5("Selection"),
            selectInput(
              "twoway_variable_choice_column",
              "Select column variable:",
              choices = factors_vector,
              selected = factors_vector[1],
              multiple = FALSE
            ),
            selectInput(
              "twoway_variable_choice_row",
              "Select row variable:",
              choices = labels_vector,
              selected = labels_vector[3],
              multiple = FALSE
            )
          ),
          column(4,
            h4("Categorical variables options"),
            radioButtons(
              "twoway_totals",
              "Totals",
              choices = totals_vector
            ),
            checkboxInput(
              "twoway_percent",
              "Show percentages",
              value = TRUE
            ),
            div(style = "margin-top: -20px"),
            conditionalPanel(
              condition = "input.twoway_percent == true",
              radioButtons(
                "twoway_percent_style",
                "Percentage style",
                choices = percentages_style
              ),
              checkboxInput(
                "twoway_percent_show_n",
                "Show counts",
                value = FALSE
              )
            )
          ),
          column(4,
            h4("Numerical variables options")
          )
        ),
        ### MAIN PANEL
        span(
          textOutput("twoway_warning"),
          class = "text-danger"
        ),
        DT::dataTableOutput("twoway_table")
      )
    )
  )
)


###############################################################################


###############################################################################
# SERVER

server <- function(input, output, session) {

  # Dataset
  dataset <- reactive({
    df
  })

  #Change theme
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark_theme else light_theme
  ))

  # VISUALIZE DATA

  # Columns to keep
  view_variable_choice <- reactive({
    input$view_variable_choice
  })

  # For DT customization
  # https://stackoverflow.com/questions/35624413/remove-search-option-but-leave-search-columns-option
  # Or use reactable: https://glin.github.io/reactable/articles/examples.html#shiny-1

  output$view_variable_choice_length <- renderText({
    paste(length(input$view_variable_choice), "variables were used")
  })

  output$view_table <- renderDataTable(
    {
      if (length(input$view_variable_choice) == 0) {
        dataset() %>%
            rename_with(~labels_vector)
      } else {
        dataset() %>%
            rename_with(~labels_vector) %>%
            select(input$view_variable_choice)
      }
    },
    options = list(
      pageLength = 5,
      sDom  = '<"top">lrt<"bottom">ip'
    ),
    filter = "top"
  )

  # CROSS TABULATIONS
  ## ONE WAY

  oneway_variable_choice <- reactive({
    input$oneway_variable_choice
  })

  # Could create a reactive function with the column pulled?
  
  ## CATEGORY DROP (UI)

  output$oneway_variable_category_drop_ui <- renderUI(
    {
      selectInput(
          "oneway_variable_category_drop",
          "Drop categories:",
          choices = dataset() %>%
            rename_with(~labels_vector) %>%
            pull(oneway_variable_choice()) %>%
            levels(),
          multiple = TRUE
      )
    }
  )
  
  ### TABLE
  output$oneway_table <- renderDataTable(
    {
      # Two different tables depending on variable types
      # If the variable is a factor, you get a oneway table
      # If it's a numerical variable, you get a summary
      if (
        (
          dataset() %>%
            rename_with(~labels_vector) %>%
            pull(oneway_variable_choice()) %>%
            is.factor()
        ) | (
          dataset() %>%
            rename_with(~labels_vector) %>%
            pull(oneway_variable_choice()) %>%
            is.character()
        )
      ) {
        # ONEWAY TABLE
        oneway_table_display <- dataset() %>%
          rename_with(~labels_vector) %>%
          # Filter unwanted categories
          filter(!.data[[oneway_variable_choice()]] %in% input$oneway_variable_category_drop) %>%
          # Drop unused categories (otherwise they appear with N = 0 on the table)
          # Mutate syntax:
          # https://stackoverflow.com/questions/57906128/r-shiny-reactive-column-name-with-mutate
          mutate(!!oneway_variable_choice() := fct_drop(.data[[oneway_variable_choice()]])) %>%
          tabyl(oneway_variable_choice())
      
          
        if (input$oneway_total == TRUE) {
          oneway_table_display <- bind_rows(
            oneway_table_display %>%
              adorn_totals() %>%
              slice_tail(),
            oneway_table_display
          )
        }
      
        oneway_table_display <- oneway_table_display %>%
          mutate(percent = (percent * 100) %>% round(2)) %>%
          rename(N = n, `%` = percent)
        
        oneway_table_display
        
      } else {
        # SUMMARY TABLE
        dataset() %>%
          rename_with(~labels_vector) %>%
          drop_na(oneway_variable_choice()) %>%
          summarise(
            N = n(),
            Mean = mean(get(oneway_variable_choice())),
            SD = sd(get(oneway_variable_choice())),
            Minimum = min(get(oneway_variable_choice())),
            `1%` = quantile(get(oneway_variable_choice()), 0.01),
            `5%` = quantile(get(oneway_variable_choice()), 0.05),
            `25%` = quantile(get(oneway_variable_choice()), 0.25),
            `50%` = quantile(get(oneway_variable_choice()), 0.50),
            `75%` = quantile(get(oneway_variable_choice()), 0.75),
            `95%` = quantile(get(oneway_variable_choice()), 0.95),
            `99%` = quantile(get(oneway_variable_choice()), 0.99),
            Maximum = max(get(oneway_variable_choice()))
          ) %>%
          round(2)
      }
    },
    options = list(
      pageLength = 25
    )
  )

  ## TWO-WAY
  ### REACTIVE VAR
  twoway_variable_choice_column <- reactive({
    input$twoway_variable_choice_column
  })
  twoway_variable_choice_row <- reactive({
    input$twoway_variable_choice_row
  })

  ### ROWVAR FACTOR CHECK
  twoway_rowvar_is_factor <- reactive({
    (
      dataset() %>%
        rename_with(~labels_vector) %>%
        pull(twoway_variable_choice_row()) %>%
        is.factor()
    ) | (
      dataset() %>%
        rename_with(~labels_vector) %>%
        pull(twoway_variable_choice_row()) %>%
        is.character()
    )
  })

  ### SAME VARIABLE WARNING
  output$twoway_warning <- renderText(
    {
      if (twoway_variable_choice_column() == twoway_variable_choice_row()) {
        "Row and column variables cannot be identical"
      }
    }
  )

  ### TABLE
  output$twoway_table <- renderDataTable(
    {
      # CHECK 1: variables are different
      if (twoway_variable_choice_column() != twoway_variable_choice_row()) {

        # CHECK 2: factor variables or numerical variables
        if (twoway_rowvar_is_factor()) {
          # MAIN TABLE (N)
          twoway_table_display <- dataset() %>%
            rename_with(~labels_vector) %>%
            tabyl(.data[[input$twoway_variable_choice_row]], .data[[input$twoway_variable_choice_column]])

          # ADD TOTALS
          if (input$twoway_totals != "None") {
            totals_var <- switch(
              input$twoway_totals,
              "Row" = "row",
              "Column" = "col",
              "Both" = c("row", "col")
            )
            twoway_table_display <- twoway_table_display %>%
              adorn_totals(totals_var)
          }

          # ADD PERCENTAGES
          if (input$twoway_percent == TRUE) {

            percentages_style <- switch(
              input$twoway_percent_style,
              "Row" = "row",
              "Column" = "col",
              "Cell" = "all"
            )

            twoway_table_display <- twoway_table_display %>%
              adorn_percentages(percentages_style) %>%
              adorn_pct_formatting(digits = 2)

            ## SHOW COUNTS
            if (input$twoway_percent_show_n == TRUE) {
              twoway_table_display <- twoway_table_display %>%
                adorn_ns()
            }
          }
        } else {
          twoway_table_display <- dataset() %>%
          rename_with(~labels_vector) %>%
          drop_na(twoway_variable_choice_column(), twoway_variable_choice_row()) %>%
          group_by(.data[[twoway_variable_choice_column()]]) %>%
          summarise(
            N = n(),
            Mean = mean(get(twoway_variable_choice_row())) %>% round(2),
            SD = sd(get(twoway_variable_choice_row())) %>% round(2),
            Minimum = min(get(twoway_variable_choice_row())) %>% round(2),
            `1%` = quantile(get(twoway_variable_choice_row()), 0.01) %>% round(2),
            `5%` = quantile(get(twoway_variable_choice_row()), 0.05) %>% round(2),
            `25%` = quantile(get(twoway_variable_choice_row()), 0.25) %>% round(2),
            `50%` = quantile(get(twoway_variable_choice_row()), 0.50) %>% round(2),
            `75%` = quantile(get(twoway_variable_choice_row()), 0.75) %>% round(2),
            `95%` = quantile(get(twoway_variable_choice_row()), 0.95) %>% round(2),
            `99%` = quantile(get(twoway_variable_choice_row()), 0.99) %>% round(2),
            Maximum = max(get(twoway_variable_choice_row())) %>% round(2)
          )
        }
        # Display
        twoway_table_display
      }
    },
    options = list(
      pageLength = 25
    )
  )
}
###############################################################################

shinyApp(ui, server)
