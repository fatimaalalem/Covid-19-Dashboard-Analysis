# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(DT)
library(tidyr)
library(scales)
library(ggrepel)
library(corrplot)


covidData <- read.csv("owid-covid-data.csv", stringsAsFactors = FALSE)
covidData$date <- as.Date(covidData$date)


jordan_csv_exists <- file.exists("jordan_data.csv")
if (jordan_csv_exists) {
  jordan_csv <- read.csv("jordan_data.csv", stringsAsFactors = FALSE)
  if ("date" %in% names(jordan_csv)) jordan_csv$date <- as.Date(jordan_csv$date)
} else {
  jordan_csv <- NULL
}


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      :root{
        --covid-red:#b91c1c;
        --covid-blue:#1E88E5;
        --covid-green:#00CD66;
        --covid-dark:#111827;
      }
      body{background:#f7f8fb;}
      .tabbable > .nav > li > a { font-weight:600; }
      h2{margin-top:10px;}
      .section-card{
        background:white;
        border:1px solid #e5e7eb;
        border-radius:14px;
        padding:14px 16px;
        margin:12px 0;
        box-shadow:0 8px 20px rgba(17,24,39,0.06);
        transition:transform .18s ease, box-shadow .18s ease;
      }
      .section-card:hover{
        transform:translateY(-2px);
        box-shadow:0 12px 28px rgba(17,24,39,0.10);
      }
      .kpi-row{display:flex; gap:12px; flex-wrap:wrap; margin:8px 0 14px 0;}
      .kpi{
        flex:1 1 210px;
        background:linear-gradient(135deg, rgba(30,136,229,.10), rgba(185,28,28,.08));
        border:1px solid #e5e7eb;
        border-radius:14px;
        padding:12px 14px;
      }
      .kpi .label{font-size:12px; color:#374151; font-weight:700; letter-spacing:.2px;}
      .kpi .value{font-size:22px; font-weight:800; color:#111827; margin-top:4px;}
      .kpi .sub{font-size:12px; color:#6b7280; margin-top:2px;}
    "))
  ),

  titlePanel("COVID-19 Global Analysis Dashboard"),

  tabsetPanel(
    # ===== TAB 1 =====
    tabPanel("Global Overview",
      div(class="section-card",
          div(class="kpi-row",
              div(class="kpi",
                  div(class="label", "Total Cases (Worldwide)"),
                  div(class="value", textOutput("kpi_cases", inline=TRUE)),
                  div(class="sub", "Latest available snapshot")
              ),
              div(class="kpi",
                  div(class="label", "Total Deaths (Worldwide)"),
                  div(class="value", textOutput("kpi_deaths", inline=TRUE)),
                  div(class="sub", "Latest available snapshot")
              ),
              div(class="kpi",
                  div(class="label", "Fully Vaccinated (Avg %)"),
                  div(class="value", textOutput("kpi_vax", inline=TRUE)),
                  div(class="sub", "Across countries (latest)")
              )
          )
      ),

      div(class="section-card",
          h2("1. Vaccination vs Deaths Analysis"),
          hr(),
          fluidRow(
            column(4,
                   selectInput("country", "Select Country:", choices = NULL)
            ),
            column(8, uiOutput("date_ui"))
          ),
          plotOutput("vacc_death_plot", height = "400px")
      ),

      div(class="section-card",
          h2("2. Global Correlation Heatmap"),
          hr(),
          fluidRow(
            column(12,
                   checkboxGroupInput("vars", "Select Variables for Correlation:",
                                      choices = NULL,
                                      selected = NULL,
                                      inline = TRUE)
            )
          ),
          plotOutput("heatmap", height = "550px")
      ),

      div(class="section-card",
          h2("3. Interactive Global Map"),
          hr(),
          fluidRow(
            column(4,
                   selectInput("metric", "Select Metric:",
                               choices = c(
                                 "Cases per Million" = "total_cases_per_million",
                                 "Deaths per Million" = "total_deaths_per_million",
                                 "Vaccination (%)" = "people_fully_vaccinated_per_hundred"
                               ),
                               selected = "total_cases_per_million")
            )
          ),
          plotlyOutput("world_map", height = "650px")
      ),

      div(class="section-card",
          h2("4. Climate & Seasonality Analysis"),
          hr(),
          fluidRow(
            column(4,
                   dateRangeInput("date_range", "Select Date Range:",
                                  start = "2020-06-01",
                                  end = Sys.Date(),
                                  min = "2020-01-01",
                                  max = Sys.Date()),
                   checkboxGroupInput("continents", "Select Continents:",
                                      choices = c("Africa", "Asia", "Europe",
                                                  "North America", "South America", "Oceania"),
                                      selected = c("Africa", "Asia", "Europe",
                                                   "North America", "South America", "Oceania")),
                   radioButtons("metric_season", "Metric to Display:",
                                choices = c("Average" = "avg", "Median" = "median"),
                                selected = "avg")
            ),
            column(8, plotOutput("seasonal_bar", height = "500px"))
          ),
          hr(),
          h4("Monthly Patterns by Hemisphere"),
          fluidRow(
            column(6, plotOutput("monthly_north", height = "400px")),
            column(6, plotOutput("monthly_south", height = "400px"))
          ),
          fluidRow(
            column(6, h5("Northern Hemisphere Stats"), verbatimTextOutput("stats_north")),
            column(6, h5("Southern Hemisphere Stats"), verbatimTextOutput("stats_south"))
          )
      )
    ),
    
    # ===== TAB 3 =====
    tabPanel("Continental Performance",
             div(class="section-card",
                 h2("1. Number of Weekly COVID-19 Cases by Continent"),
                 hr(),
                 fluidRow(
                   column(3,
                          radioButtons("cases_viz_type", "Visualization Type:",
                                       choices = c("Individual Panels" = "facet",
                                                   "Stacked Area" = "stacked"),
                                       selected = "facet"),
                          checkboxInput("show_labels", "Show Latest Values", value = FALSE)
                   ),
                   column(9, plotOutput("cases_timeseries", height = "600px"))
                 )
             ),
             
             div(class="section-card",
                 h2("2. Total COVID-19 Cases by Continent"),
                 hr(),
                 plotOutput("total_cases_bar", height = "450px")
             ),
             
             div(class="section-card",
                 h2("3. Vaccination Progress by Continent"),
                 hr(),
                 plotlyOutput("vaccination_boxplot", height = "500px")
             ),
             
             div(class="section-card",
                 h2("4. Best vs Worst Performing Countries per Continent"),
                 hr(),
                 plotOutput("best_worst_plot", height = "500px")
             )
    ),

    # ===== TAB 2 =====
    tabPanel("Socioeconomic Analysis",
      div(class="section-card",
          h2("1. Expected vs Observed Deaths"),
          hr(),
          fluidRow(
            column(3,
                   selectInput("deaths_continent", "Select Continent:", choices = NULL),
                   sliderInput("topN", "Number of Top Outliers:", min = 1, max = 20, value = 10)
            ),
            column(9, plotlyOutput("deaths_scatter", height = "450px"))
          )
      ),

      div(class="section-card",
          h2("2. Population Density vs Cases/Deaths"),
          hr(),
          fluidRow(
            column(3,
                   selectInput("density_metric", "Y-axis Indicator:",
                               choices = c(
                                 "Total Cases per Million" = "total_cases_per_million",
                                 "Total Deaths per Million" = "total_deaths_per_million"
                               ),
                               selected = "total_cases_per_million"),
                   selectInput("density_continent", "Select Continent:", choices = NULL),
                   selectizeInput("density_countries", "Pick up to 3 countries:",
                                  choices = NULL, multiple = TRUE,
                                  options = list(maxItems = 3, placeholder = "Select countries")),
                   checkboxInput("log_x", "Use Log scale", value = TRUE),
                   checkboxInput("show_trend_density", "Show Trend Line", value = FALSE)
            ),
            column(9, plotlyOutput("density_scatter", height = "500px"))
          )
      ),

      div(class="section-card",
          h2("3. Socioeconomic Correlations + Country Comparison"),
          hr(),
          fluidRow(
            column(3,
                   checkboxGroupInput("corr_vars", "Pick variables (min 2):",
                                      choices = c(
                                        "GDP per Capita" = "gdp_per_capita",
                                        "Median Age" = "median_age",
                                        "Population Density" = "population_density",
                                        "Cases per Million" = "total_cases_per_million",
                                        "Deaths per Million" = "total_deaths_per_million",
                                        "Vaccination %" = "people_fully_vaccinated_per_hundred"
                                      ),
                                      selected = c("gdp_per_capita", "median_age", "total_deaths_per_million")),
                   selectInput("corr_continent", "Continent filter:", choices = NULL),
                   selectizeInput("corr_countries", "Compare countries (3-5):",
                                  choices = NULL,
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(maxItems = 5))
            ),
            column(9,
                   h4("Correlation Heatmap"),
                   plotlyOutput("corr_heat", height = "400px"),
                   br(),
                   h4("Parallel Coordinates"),
                   plotlyOutput("parallel", height = "400px")
            )
          )
      ),

      div(class="section-card",
          h2("4. Income Tier Comparison"),
          hr(),
          fluidRow(
            column(3,
                   selectInput("income_continent", "Filter by Continent:", choices = NULL),
                   selectInput("income_metric", "Metric:",
                               choices = c(
                                 "Cases per Million" = "total_cases_per_million",
                                 "Deaths per Million" = "total_deaths_per_million",
                                 "Vaccination (%)" = "people_fully_vaccinated_per_hundred"
                               ),
                               selected = "total_cases_per_million"),
                   selectizeInput("income_countries", "Compare countries (3-5):",
                                  choices = NULL,
                                  selected = NULL,
                                  multiple = TRUE,
                                  options = list(maxItems = 5))
            ),
            column(9,
                   plotlyOutput("income_boxplot", height = "400px"),
                   br(),
                   h4("Selected Countries Ranking"),
                   plotlyOutput("income_rank", height = "300px")
            )
          )
      )
    ),

    

    # ===== TAB 4 =====
    tabPanel("Policy Response Analysis",
      div(class="section-card",
          h2("1. Stringency Index vs COVID-19 Case Rate"),
          hr(),
          fluidRow(
            column(3,
                   selectInput("treemap_continent", "Select Continent:", choices = NULL, selected = "Europe"),
                   sliderInput("top_n", "Number of Countries:",
                               min = 5, max = 30, value = 15, step = 1)
            ),
            column(9, plotlyOutput("treemap_plot", height = "600px"))
          )
      ),

      div(class="section-card",
          h2("2. Early vs Late Response Impact"),
          hr(),
          fluidRow(
            column(3,
                   numericInput("threshold", "Early Response Threshold (days):",
                                value = 14, min = 7, max = 30, step = 1),
                   selectInput("response_continent", "Select Continent:", choices = NULL, selected = "Europe"),
                   selectInput("response_metric", "Display Metric:",
                               choices = c("Deaths per 100k" = "deaths",
                                           "Cases per 100k" = "cases"),
                               selected = "deaths"),
                   sliderInput("num_countries", "Number of Countries to Display:",
                               min = 5, max = 20, value = 10, step = 1)
            ),
            column(9, plotlyOutput("diverging_plot", height = "700px"))
          )
      ),

      div(class="section-card",
          h2("3. Test Positivity Rate vs Case Trends"),
          hr(),
          
          fluidRow(
            column(3,
                   selectInput(
                     "positivity_country",
                     "Select Country:",
                     choices = NULL,
                     selected = "United States"
                   ),
                   dateRangeInput(
                     "positivity_date_range",
                     "Date Range:",
                     start = "2020-06-01",
                     end = Sys.Date()
                   ),
                   hr(),
                   h4("Chart Lines:"),
                   tags$div(style="border-left:4px solid #DC2626; padding-left:8px;",
                            strong("RED:"), " Test Positivity %"),
                   tags$div(style="border-left:4px solid #2563EB; padding-left:8px;",
                            strong("BLUE:"), " Cases per 100k"),
                   tags$div(style="border-left:4px dashed #10B981; padding-left:8px;",
                            strong("GREEN:"), " Safe (5%)"),
                   tags$div(style="border-left:4px dashed #F59E0B; padding-left:8px;",
                            strong("YELLOW:"), " Danger (10%)")
            ),
            
            column(9,
                   plotlyOutput("positivity_plot", height = "600px")
            )
          )
      ),
            
            
    

      div(class="section-card",
          h2("4. Vaccination Impact: Death Rate Comparison"),
          hr(),
          fluidRow(
            column(3,
                   sliderInput("vax_threshold", "Vaccination Milestone (%):",
                               min = 30, max = 70, value = 50, step = 10),
                   selectInput("vax_continent", "Select Continent:", choices = NULL, selected = "Europe"),
                   sliderInput("vax_num_countries", "Number of Countries:",
                               min = 3, max = 15, value = 5, step = 1)
            ),
            column(9, plotOutput("vax_impact_plot", height = "700px"))
          )
      )
    ),

    # ===== TAB 5 =====
    tabPanel("Jordan Analysis",
      div(class="section-card",
          h2("1. Jordan COVID-19 Waves: Cases vs Deaths"),
          hr(),
          fluidRow(
            column(3,
                   checkboxGroupInput(
                     inputId = "jordan_metrics",
                     label = "Select Metrics:",
                     choices = c("Cases", "Deaths"),
                     selected = c("Cases", "Deaths")
                   )
            ),
            column(9, plotOutput("jordan_waves_plot", height = "500px"))
          )
      ),

      div(class="section-card",
          h2("2. Jordan and Neighboring Countries Case Trends"),
          hr(),
          plotOutput("regional_comparison", height = "700px")
      ),

      div(class="section-card",
          h2("3. Policy, Vaccination, and COVID-19 Outcomes in Jordan"),
          hr(),
          fluidRow(
            column(3,
                   radioButtons(
                     inputId = "jordan_outcome",
                     label = "Select Outcome:",
                     choices = c("COVID-19 Cases" = "cases",
                                 "COVID-19 Deaths" = "deaths"),
                     selected = "cases"
                   ),
                   sliderInput("y_max_limit", "Set Y-axis Upper Limit:",
                               min = 1000, max = 25000, value = 20000, step = 1000)
            ),
            column(9, plotOutput("jordan_bubble_plot", height = "550px"))
          )
      ),

      div(class="section-card",
          h2("4. Interactive Correlation Analysis for Jordan"),
          hr(),
          fluidRow(
            column(3,
                   checkboxGroupInput(
                     "corr_vars_jordan",
                     "Select Variables (min 2):",
                     choices = c(
                       "Cases per Million" = "new_cases_smoothed_per_million",
                       "Deaths per Million" = "new_deaths_smoothed_per_million",
                       "Stringency Index" = "stringency_index",
                       "Reproduction Rate" = "reproduction_rate",
                       "Positive Rate" = "positive_rate",
                       "Tests per Thousand" = "new_tests_smoothed_per_thousand"
                     ),
                     selected = c("new_cases_smoothed_per_million",
                                  "new_deaths_smoothed_per_million",
                                  "stringency_index")
                   ),
                   selectInput("corr_method", "Correlation Method:",
                               choices = c("Pearson" = "pearson",
                                           "Spearman" = "spearman"),
                               selected = "pearson")
            ),
            column(9, plotOutput("jordan_corrplot", height = "600px"))
          )
      )
    )
  )
)


# SERVER
server <- function(input, output, session) {

  # ------------------------------------------------------
  # TAB 1 preprocessing (local)
  tab1_latest <- reactive({
    covidData %>%
      filter(!is.na(continent), continent != "") %>%
      group_by(location) %>%
      slice_max(date, n = 1, with_ties = FALSE) %>%
      ungroup()
  })

  tab1_country_list <- reactive({
    covidData %>%
      filter(!continent %in% c("", NA)) %>%
      pull(location) %>%
      unique() %>%
      sort()
  })

  tab1_var_choices <- reactive({
    c(
      "Median Age" = "median_age",
      "Population Density" = "population_density",
      "Fully Vaccinated (%)" = "people_fully_vaccinated_per_hundred",
      "Total Cases / Million" = "total_cases_per_million",
      "Total Deaths / Million" = "total_deaths_per_million",
      "Stringency Index" = "stringency_index"
    )
  })

  observeEvent(tab1_country_list(), {
    updateSelectInput(session, "country", choices = tab1_country_list(), selected = "Jordan")
  }, ignoreInit = FALSE)

  observeEvent(tab1_var_choices(), {
    updateCheckboxGroupInput(
      session, "vars",
      choices = tab1_var_choices(),
      selected = c("median_age", "people_fully_vaccinated_per_hundred", "total_deaths_per_million")
    )
  }, ignoreInit = FALSE)

  # KPI cards (Tab1)
  output$kpi_cases <- renderText({
    df <- tab1_latest()
    total <- sum(df$total_cases, na.rm = TRUE)
    format(round(total), big.mark = ",")
  })
  output$kpi_deaths <- renderText({
    df <- tab1_latest()
    total <- sum(df$total_deaths, na.rm = TRUE)
    format(round(total), big.mark = ",")
  })
  output$kpi_vax <- renderText({
    df <- tab1_latest()
    v <- mean(df$people_fully_vaccinated_per_hundred, na.rm = TRUE)
    paste0(round(v, 1), "%")
  })

  # TAB 1 plots (same logic, but uses tab1_latest / tab1 choices)
  country_data <- reactive({
    req(input$country)
    covidData %>% filter(location == input$country) %>% mutate(date = as.Date(date))
  })

  output$date_ui <- renderUI({
    data <- country_data()
    dateRangeInput("dates", "Select Date Range:",
                   start = min(data$date, na.rm = TRUE),
                   end = max(data$date, na.rm = TRUE))
  })

  output$vacc_death_plot <- renderPlot({
    req(input$dates)
    data <- country_data() %>%
      filter(date >= input$dates[1], date <= input$dates[2]) %>%
      filter(!is.na(people_vaccinated_per_hundred),
             !is.na(new_deaths_smoothed))

    if (nrow(data) == 0) {
      plot.new()
      text(0.5, 0.5,
           paste("No data available for", input$country, "in this date range"),
           cex = 1.4)
      return()
    }

    ggplot(data, aes(x = people_vaccinated_per_hundred, y = new_deaths_smoothed)) +
      geom_point(color = "blue", alpha = 0.6, size = 3) +
      geom_smooth(method = "lm", color = "red", se = TRUE) +
      labs(
        title = paste("Vaccination vs New COVID-19 Deaths in", input$country),
        x = "People Vaccinated (%)",
        y = "New Deaths (7-day average)"
      ) +
      theme_minimal()
  })

  output$heatmap <- renderPlot({
    req(input$vars)

    df_latest <- tab1_latest()
    data <- df_latest %>% select(all_of(input$vars)) %>% na.omit()

    if (length(input$vars) < 2) {
      plot.new()
      text(0.5, 0.5, "Select at least 2 variables", cex = 1.4)
      return()
    }

    corr_matrix <- cor(data)
    corr_long <- melt(corr_matrix)

    ggplot(corr_long, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "#4575b4", high = "#d73027", mid = "white",
        midpoint = 0, limit = c(-1, 1),
        name = "Correlation"
      ) +
      geom_text(aes(label = round(value, 2)), size = 4) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()
      ) +
      ggtitle("Global COVID-19 Correlation Heatmap")
  })

  output$world_map <- renderPlotly({
    df_latest <- tab1_latest()
    metric <- input$metric

    map_data <- df_latest %>%
      filter(!is.na(iso_code)) %>%
      select(iso_code, location,
             total_cases_per_million,
             total_deaths_per_million,
             people_fully_vaccinated_per_hundred) %>%
      mutate(
        metric_value = case_when(
          metric == "total_cases_per_million" ~ total_cases_per_million,
          metric == "total_deaths_per_million" ~ total_deaths_per_million,
          metric == "people_fully_vaccinated_per_hundred" ~ people_fully_vaccinated_per_hundred,
          TRUE ~ NA_real_
        ),
        metric_log = log10(metric_value + 1)
      )

    map_data$hover <- paste0(
      "<b>", map_data$location, "</b><br>",
      "Cases per million: ", round(map_data$total_cases_per_million, 1), "<br>",
      "Deaths per million: ", round(map_data$total_deaths_per_million, 1), "<br>",
      "Fully vaccinated: ", round(map_data$people_fully_vaccinated_per_hundred, 1), "%"
    )

    plot_ly(
      map_data,
      type = "choropleth",
      locations = ~iso_code,
      z = ~metric_log,
      text = ~hover,
      hoverinfo = "text",
      colorscale = "Reds"
    ) %>%
      layout(
        title = paste("Global COVID-19", input$metric, "(Log Scale)"),
        geo = list(projection = list(type = "natural earth"))
      )
  })

  covid_seasonal <- reactive({
    data <- covidData
    data$date <- as.Date(data$date)

    data %>%
      mutate(
        year = year(date),
        month = month(date),
        month_name = month(date, label = TRUE),
        season_north = case_when(
          month %in% c(12, 1, 2) ~ "Winter",
          month %in% c(3, 4, 5) ~ "Spring",
          month %in% c(6, 7, 8) ~ "Summer",
          month %in% c(9, 10, 11) ~ "Fall"
        ),
        season_south = case_when(
          month %in% c(12, 1, 2) ~ "Summer",
          month %in% c(3, 4, 5) ~ "Fall",
          month %in% c(6, 7, 8) ~ "Winter",
          month %in% c(9, 10, 11) ~ "Spring"
        ),
        hemisphere = case_when(
          continent %in% c("South America", "Africa", "Oceania") ~ "Southern",
          continent %in% c("North America", "Europe", "Asia") ~ "Northern",
          TRUE ~ "Northern"
        ),
        season = ifelse(hemisphere == "Northern", season_north, season_south),
        winter_summer = case_when(
          season %in% c("Winter") ~ "Winter",
          season %in% c("Summer") ~ "Summer",
          TRUE ~ "Other"
        )
      )
  })

  filtered_seasonal <- reactive({
    covid_seasonal() %>%
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        !is.na(continent),
        continent != "",
        continent %in% input$continents
      )
  })

  winter_summer_comp <- reactive({
    filtered_seasonal() %>%
      filter(!is.na(new_cases_smoothed_per_million) &
               winter_summer %in% c("Winter", "Summer")) %>%
      group_by(continent, winter_summer) %>%
      summarize(
        avg_cases_per_million = mean(new_cases_smoothed_per_million, na.rm = TRUE),
        median_cases_per_million = median(new_cases_smoothed_per_million, na.rm = TRUE),
        .groups = "drop"
      )
  })

  output$seasonal_bar <- renderPlot({
    req(nrow(winter_summer_comp()) > 0)

    data <- winter_summer_comp()
    y_var <- if (input$metric_season == "avg") "avg_cases_per_million" else "median_cases_per_million"
    y_label <- if (input$metric_season == "avg") "Average Daily Cases per Million" else "Median Daily Cases per Million"

    ggplot(data, aes(x = continent, y = .data[[y_var]], fill = winter_summer)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      scale_fill_manual(values = c("Winter" = "#4575b4", "Summer" = "#d73027")) +
      labs(
        title = "COVID-19 Cases: Winter vs Summer by Continent",
        x = "Continent",
        y = y_label,
        fill = "Season"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$monthly_north <- renderPlot({
    req(nrow(filtered_seasonal()) > 0)

    monthly_pattern <- filtered_seasonal() %>%
      filter(!is.na(new_cases_smoothed_per_million) & hemisphere == "Northern") %>%
      group_by(month, month_name) %>%
      summarize(avg_cases = mean(new_cases_smoothed_per_million, na.rm = TRUE), .groups = "drop")

    req(nrow(monthly_pattern) > 0)

    ggplot(monthly_pattern, aes(x = month, y = avg_cases)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      geom_point(size = 3, color = "steelblue") +
      annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#4575b4") +
      annotate("rect", xmin = 5.5, xmax = 8.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#d73027") +
      annotate("rect", xmin = 11.5, xmax = 12.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#4575b4") +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      labs(title = "Northern Hemisphere", x = "Month", y = "Avg Cases/Million") +
      theme_minimal()
  })

  output$monthly_south <- renderPlot({
    req(nrow(filtered_seasonal()) > 0)

    monthly_pattern <- filtered_seasonal() %>%
      filter(!is.na(new_cases_smoothed_per_million) & hemisphere == "Southern") %>%
      group_by(month, month_name) %>%
      summarize(avg_cases = mean(new_cases_smoothed_per_million, na.rm = TRUE), .groups = "drop")

    req(nrow(monthly_pattern) > 0)

    ggplot(monthly_pattern, aes(x = month, y = avg_cases)) +
      geom_line(color = "darkorange", linewidth = 1.2) +
      geom_point(size = 3, color = "darkorange") +
      annotate("rect", xmin = 5.5, xmax = 8.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#4575b4") +
      annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#d73027") +
      annotate("rect", xmin = 11.5, xmax = 12.5, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#d73027") +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      labs(title = "Southern Hemisphere", x = "Month", y = "Avg Cases/Million") +
      theme_minimal()
  })

  output$stats_north <- renderPrint({
    monthly_pattern <- filtered_seasonal() %>%
      filter(!is.na(new_cases_smoothed_per_million) & hemisphere == "Northern") %>%
      group_by(month) %>%
      summarize(avg_cases = mean(new_cases_smoothed_per_million, na.rm = TRUE), .groups = "drop")

    if (nrow(monthly_pattern) > 0) {
      winter <- monthly_pattern %>% filter(month %in% c(12, 1, 2))
      summer <- monthly_pattern %>% filter(month %in% c(6, 7, 8))

      cat("Winter (Dec-Feb):", round(mean(winter$avg_cases), 2), "cases/million\n")
      cat("Summer (Jun-Aug):", round(mean(summer$avg_cases), 2), "cases/million\n")
      cat("Difference:", round(mean(winter$avg_cases) - mean(summer$avg_cases), 2), "\n")
    } else {
      cat("No data available")
    }
  })

  output$stats_south <- renderPrint({
    monthly_pattern <- filtered_seasonal() %>%
      filter(!is.na(new_cases_smoothed_per_million) & hemisphere == "Southern") %>%
      group_by(month) %>%
      summarize(avg_cases = mean(new_cases_smoothed_per_million, na.rm = TRUE), .groups = "drop")

    if (nrow(monthly_pattern) > 0) {
      winter <- monthly_pattern %>% filter(month %in% c(6, 7, 8))
      summer <- monthly_pattern %>% filter(month %in% c(12, 1, 2))

      cat("Winter (Jun-Aug):", round(mean(winter$avg_cases), 2), "cases/million\n")
      cat("Summer (Dec-Feb):", round(mean(summer$avg_cases), 2), "cases/million\n")
      cat("Difference:", round(mean(winter$avg_cases) - mean(summer$avg_cases), 2), "\n")
    } else {
      cat("No data available")
    }
  })

  # ------------------------------------------------------
  # TAB 2 preprocessing (local)
  tab2_latest <- reactive({
    covidData %>%
      filter(!is.na(continent), continent != "") %>%
      group_by(location) %>%
      slice_max(date, n = 1, with_ties = FALSE) %>%
      ungroup()
  })

  observeEvent(tab2_latest(), {
    conts <- c("All", sort(unique(tab2_latest()$continent)))
    updateSelectInput(session, "gdp_continent", choices = conts, selected = "All")
    updateSelectInput(session, "deaths_continent", choices = conts, selected = "All")
    updateSelectInput(session, "density_continent", choices = conts, selected = "All")
    updateSelectInput(session, "corr_continent", choices = conts, selected = "All")
    updateSelectInput(session, "income_continent", choices = conts, selected = "All")

    countries <- sort(unique(tab2_latest()$location))
    updateSelectizeInput(session, "corr_countries", choices = countries,
                        selected = c("Jordan", "Palestine", "Saudi Arabia", "Germany", "United States"),
                        server = TRUE)
    updateSelectizeInput(session, "income_countries", choices = countries,
                        selected = c("Jordan", "Iraq", "Saudi Arabia", "Germany", "United States"),
                        server = TRUE)
    updateSelectInput(session, "positivity_country", choices = countries, selected = "United States")
  }, ignoreInit = FALSE)

  output$gdp_test_plot <- renderPlotly({
    df_latest <- tab2_latest()
    df <- df_latest
    if (!is.null(input$gdp_continent) && input$gdp_continent != "All") {
      df <- df %>% filter(continent == input$gdp_continent)
    }

    df <- df %>% filter(!is.na(gdp_per_capita) & !is.na(total_tests_per_thousand))

    df <- df %>% mutate(
      hover_text = paste0(
        "Country: ", location, "<br>",
        "GDP per Capita: $", formatC(gdp_per_capita, format = "f", digits = 0, big.mark = ","), "<br>",
        "Tests per 1000: ", round(total_tests_per_thousand, 2)
      )
    )

    continent_levels <- unique(df$continent)
    dark_colors <- brewer.pal(min(8, length(continent_levels)), "Set1")

    p <- plot_ly(
      df,
      x = ~gdp_per_capita,
      y = ~total_tests_per_thousand,
      type = "scatter",
      mode = "markers",
      color = ~continent,
      colors = dark_colors,
      text = ~hover_text,
      hoverinfo = "text",
      marker = list(size = 10, opacity = 0.9)
    )

    if (isTRUE(input$show_trend_gdp)) {
      fit <- lm(total_tests_per_thousand ~ gdp_per_capita, data = df)
      trend_df <- data.frame(
        gdp_per_capita = seq(min(df$gdp_per_capita), max(df$gdp_per_capita), length.out = 100)
      )
      trend_df$total_tests_per_thousand <- predict(fit, newdata = trend_df)

      p <- p %>% add_lines(
        data = trend_df,
        x = ~gdp_per_capita,
        y = ~total_tests_per_thousand,
        line = list(color = "black", dash = "dash"),
        inherit = FALSE,
        name = "Trend Line"
      )
    }

    p %>% layout(
      title = "GDP vs COVID-19 Testing",
      xaxis = list(title = "GDP per Capita ($)"),
      yaxis = list(title = "Total Tests per Thousand")
    )
  })

  output$deaths_scatter <- renderPlotly({
    df_latest <- tab2_latest()
    df <- df_latest
    if (!is.null(input$deaths_continent) && input$deaths_continent != "All") {
      df <- df %>% filter(continent == input$deaths_continent)
    }

    df <- df %>%
      filter(!is.na(median_age), !is.na(total_deaths_per_million),
             !is.na(people_fully_vaccinated_per_hundred)) %>%
      mutate(
        median_age = round(as.numeric(median_age), 1),
        hover_text = paste0(
          "Country: ", location, "<br>",
          "Median Age: ", median_age, "<br>",
          "Vaccinated %: ", people_fully_vaccinated_per_hundred, "<br>",
          "Total Deaths per Million: ", total_deaths_per_million
        )
      )

    plot_ly(
      df,
      x = ~median_age,
      y = ~total_deaths_per_million,
      text = ~hover_text,
      type = "scatter",
      mode = "markers",
      color = ~people_fully_vaccinated_per_hundred,
      colors = colorRamp(c("lightblue", "darkblue")),
      marker = list(size = 7, opacity = 0.9)
    ) %>%
      layout(
        xaxis = list(title = "Median Age"),
        yaxis = list(title = "Total Deaths per Million")
      )
  })

  observeEvent(list(input$density_continent, tab2_latest()), {
    df_latest <- tab2_latest()
    df <- df_latest
    if (!is.null(input$density_continent) && input$density_continent != "All") df <- df %>% filter(continent == input$density_continent)

    choices <- sort(unique(df$location))
    choices <- choices[!is.na(choices)]

    updateSelectizeInput(
      session, "density_countries",
      choices = choices,
      selected = intersect(input$density_countries, choices),
      server = TRUE
    )
  }, ignoreInit = FALSE)

  output$density_scatter <- renderPlotly({
    df_latest <- tab2_latest()
    df <- df_latest
    if (!is.null(input$density_continent) && input$density_continent != "All") df <- df %>% filter(continent == input$density_continent)

    df <- df %>%
      filter(!is.na(population_density),
             !is.na(total_cases_per_million),
             !is.na(total_deaths_per_million))

    y_col <- input$density_metric

    df <- df %>%
      mutate(
        y_val = .data[[y_col]],
        hover_text = paste0(
          "<b>", location, "</b><br>",
          "Continent: ", continent, "<br>",
          "Population Density: ", format(round(population_density, 1), big.mark=","), " / km²<br>",
          "Cases / Million: ", format(round(total_cases_per_million, 0), big.mark=","), "<br>",
          "Deaths / Million: ", format(round(total_deaths_per_million, 1), big.mark=",")
        ),
        is_selected = location %in% input$density_countries
      )

    base_color <- if (input$density_metric == "total_deaths_per_million") "#111827" else "#b91c1c"

    p <- plot_ly(
      data = df %>% filter(!is_selected),
      x = ~population_density,
      y = ~y_val,
      type = "scatter",
      mode = "markers",
      text = ~hover_text,
      hoverinfo = "text",
      marker = list(size = 8, color = base_color, opacity = 0.55),
      name = "Countries"
    )

    if (length(input$density_countries) > 0) {
      p <- p %>% add_trace(
        data = df %>% filter(is_selected),
        x = ~population_density,
        y = ~y_val,
        type = "scatter",
        mode = "markers+text",
        text = ~location,
        textposition = "top center",
        hoverinfo = "text",
        marker = list(size = 15, color = "darkorange", opacity = 0.95),
        name = "Selected"
      )
    }

    if (isTRUE(input$show_trend_density)) {
      fit <- lm(y_val ~ population_density, data = df)
      trend_df <- data.frame(
        population_density = seq(min(df$population_density), max(df$population_density), length.out = 120)
      )
      trend_df$y <- predict(fit, newdata = trend_df)

      p <- p %>% add_lines(
        data = trend_df,
        x = ~population_density,
        y = ~y,
        inherit = FALSE,
        line = list(color = "black", width = 3, dash = "dash"),
        name = "Trend"
      )
    }

    y_title <- ifelse(input$density_metric == "total_cases_per_million",
                      "Total Cases per Million", "Total Deaths per Million")

    p %>% layout(
      title = paste("Population Density vs", y_title),
      xaxis = list(
        title = "Population Density (people per km²)",
        type = ifelse(isTRUE(input$log_x), "log", "linear")
      ),
      yaxis = list(title = y_title)
    )
  })

  output$corr_heat <- renderPlotly({
    req(input$corr_vars)
    if (length(input$corr_vars) < 2) return(plot_ly() %>% layout(title="Select at least 2 variables"))

    df_latest <- tab2_latest()
    df <- df_latest
    if (!is.null(input$corr_continent) && input$corr_continent != "All") df <- df %>% filter(continent == input$corr_continent)

    df <- df %>% select(all_of(input$corr_vars)) %>% na.omit()
    if (nrow(df) < 10) return(plot_ly() %>% layout(title="Not enough data"))

    corr <- cor(df, use = "pairwise.complete.obs")
    corr_df <- as.data.frame(as.table(corr))
    colnames(corr_df) <- c("Var1","Var2","Corr")

    plot_ly(
      corr_df,
      x = ~Var1, y = ~Var2, z = ~Corr,
      type = "heatmap", zmin = -1, zmax = 1,
      colorscale = list(
        list(0.00, "#EF4444"),
        list(0.50, "#4575b4"),
        list(1.00, "#EEE9E9")
      )
    ) %>% layout(xaxis=list(tickangle=35))
  })

  output$parallel <- renderPlotly({
    req(input$corr_vars, input$corr_countries)

    df_latest <- tab2_latest()
    df <- df_latest
    if (!is.null(input$corr_continent) && input$corr_continent != "All") df <- df %>% filter(continent == input$corr_continent)

    df <- df %>%
      filter(location %in% input$corr_countries) %>%
      select(location, all_of(input$corr_vars)) %>%
      na.omit()

    if (nrow(df) == 0) return(plot_ly() %>% layout(title="No data for selected countries"))

    norm <- df
    for (v in input$corr_vars) {
      rng <- range(norm[[v]], na.rm = TRUE)
      norm[[v]] <- if (diff(rng) == 0) 0.5 else (norm[[v]] - rng[1]) / diff(rng)
    }

    norm <- norm %>% arrange(location)
    countries <- unique(norm$location)
    country_ids <- as.numeric(factor(norm$location, levels = countries))

    dims <- lapply(input$corr_vars, function(v){
      list(label = v, values = norm[[v]], range = c(0,1))
    })

    plot_ly(
      type = "parcoords",
      line = list(color = country_ids, showscale = FALSE),
      dimensions = c(
        list(list(label = "Country", values = country_ids,
                  tickvals = seq_along(countries), ticktext = countries)),
        dims
      )
    )
  })

  make_income_group <- function(x) {
    qs <- quantile(x, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
    qs <- unique(qs)
    if (length(qs) < 4) return(rep(NA_character_, length(x)))
    cut(x, breaks = qs, include.lowest = TRUE,
        labels = c("Low tier", "Middle tier", "High tier"))
  }

  income_data <- reactive({
    df_latest <- tab2_latest()
    df <- df_latest %>% mutate(income_group = make_income_group(gdp_per_capita))
    if (!is.null(input$income_continent) && input$income_continent != "All") df <- df %>% filter(continent == input$income_continent)
    df
  })

  output$income_boxplot <- renderPlotly({
    df <- income_data()
    m <- input$income_metric

    df <- df %>% filter(!is.na(income_group), !is.na(.data[[m]]), is.finite(.data[[m]]))
    if (nrow(df) < 10) return(plot_ly() %>% layout(title="Not enough data"))

    df <- df %>% mutate(val = .data[[m]])
    hi <- df %>% filter(location %in% input$income_countries)

    TIER_COLORS <- c("Low tier" = "#4575b4", "Middle tier" = "#00CD66", "High tier" = "#EF4444")

    p <- plot_ly(
      data = df,
      x = ~income_group,
      y = ~val,
      type = "box",
      color = ~income_group,
      colors = TIER_COLORS,
      boxpoints = FALSE
    )

    if (nrow(hi) > 0) {
      p <- p %>% add_trace(
        data = hi,
        x = ~income_group,
        y = ~val,
        type = "scatter",
        mode = "markers",
        marker = list(size = 14, color = "#f59e0b", symbol = "diamond"),
        name = "Selected"
      )
    }

    p %>% layout(
      xaxis = list(title = "Income Tier"),
      yaxis = list(title = input$income_metric)
    )
  })

  output$income_rank <- renderPlotly({
    df <- income_data()
    m <- input$income_metric

    hi <- df %>%
      filter(location %in% input$income_countries) %>%
      mutate(val = .data[[m]]) %>%
      arrange(desc(val))

    if (nrow(hi) == 0) return(plot_ly())

    plot_ly(
      hi,
      x = ~reorder(location, val),
      y = ~val,
      type = "bar",
      marker = list(color = "#4575b4")
    ) %>% layout(xaxis = list(title = ""), yaxis = list(title = input$income_metric))
  })

  # ------------------------------------------------------
  # TAB 3 preprocessing (FIXED — isolated & safe)
  tab3_clean <- reactive({
    covidData %>%
      filter(!is.na(continent), continent != "", !is.na(date))
  })

  tab3_continent_ts <- reactive({
    tab3_clean() %>%
      group_by(continent, date) %>%
      summarise(
        new_cases = sum(new_cases_smoothed, na.rm = TRUE),
        .groups = "drop"
      )
  })

  tab3_continent_cases <- reactive({
    tab3_clean() %>%
      group_by(continent) %>%
      summarise(
        total_cases = max(total_cases, na.rm = TRUE),
        .groups = "drop"
      )
  })

  # -------- 1. Weekly cases time series --------
  output$cases_timeseries <- renderPlot({
    df <- tab3_continent_ts()
    req(nrow(df) > 0)

    if (input$cases_viz_type == "facet") {

      p <- ggplot(df, aes(x = date, y = new_cases)) +
        geom_area(fill = "#4575b4", alpha = 0.75) +
        facet_wrap(~ continent, ncol = 1, scales = "free_y") +
        scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        labs(
          title = "Weekly COVID-19 Cases by Continent",
          subtitle = "Smoothed new cases",
          x = NULL,
          y = "New Cases (Millions)"
        ) +
        theme_minimal(base_size = 13)

      if (isTRUE(input$show_labels)) {
        latest_vals <- df %>%
          group_by(continent) %>%
          filter(date == max(date)) %>%
          ungroup()

        p <- p +
          geom_text(
            data = latest_vals,
            aes(label = scales::comma(round(new_cases))),
            x = min(df$date),
            hjust = 0,
            size = 3.5,
            fontface = "bold"
          )
      }

    } else {

      p <- ggplot(df, aes(x = date, y = new_cases, fill = continent)) +
        geom_area(alpha = 0.85) +
        scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        labs(
          title = "Weekly COVID-19 Cases (Stacked by Continent)",
          x = NULL,
          y = "New Cases (Millions)",
          fill = "Continent"
        ) +
        theme_minimal(base_size = 13)
    }

    p
  })

  # -------- 2. Total cases by continent --------
  output$total_cases_bar <- renderPlot({
    data <- tab3_continent_cases()
    req(nrow(data) > 0)
    
    ggplot(data, aes(x = continent, y = total_cases, fill = continent)) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
      labs(
        title = "Total COVID-19 Cases by Continent",
        x = "Continent",
        y = "Total Cases (Millions)"
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # -------- 3. Vaccination progress --------
  output$vaccination_boxplot <- renderPlotly({
    vaccination_continent <- covidData %>%
      filter(!is.na(continent), continent != "",
             !is.na(people_fully_vaccinated_per_hundred)) %>%
      group_by(continent, location) %>%
      summarise(
        fully_vaccinated = max(people_fully_vaccinated_per_hundred, na.rm = TRUE),
        .groups = "drop"
      )
    
    p_vacc <- ggplot(
      vaccination_continent,
      aes(x = continent, y = fully_vaccinated, fill = continent)
    ) +
      geom_boxplot(outlier.alpha = 0.5) +
      labs(
        title = "Vaccination Progress by Continent",
        x = "Continent",
        y = "People Fully Vaccinated (%)"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p_vacc)
  })

  # -------- 4. Best vs worst countries --------
  output$best_worst_plot <- renderPlot({
    country_perf <- covidData %>%
      filter(!is.na(continent), continent != "",
             !is.na(total_cases_per_million)) %>%
      group_by(continent, location) %>%
      summarise(
        cases_pm = max(total_cases_per_million, na.rm = TRUE),
        .groups = "drop"
      )
    
    best_worst <- country_perf %>%
      group_by(continent) %>%
      summarise(
        best_country = location[which.min(cases_pm)],
        best_cases = min(cases_pm),
        worst_country = location[which.max(cases_pm)],
        worst_cases = max(cases_pm)
      )
    
    ggplot(best_worst, aes(y = continent)) +
      geom_segment(
        aes(x = best_cases, xend = worst_cases, yend = continent),
        color = "gray60",
        linewidth = 1
      ) +
      geom_point(aes(x = best_cases), color = "#00CD66", size = 4) +
      geom_point(aes(x = worst_cases), color = "#EF4444", size = 4) +
      geom_text_repel(
        aes(x = best_cases, label = best_country),
        color = "#00CD66",
        size = 3.5,
        nudge_x = -0.05 * max(best_worst$worst_cases),
        direction = "x",
        segment.color = "#00CD66"
      ) +
      geom_text_repel(
        aes(x = worst_cases, label = worst_country),
        color = "#EF4444",
        size = 3.5,
        nudge_x = 0.05 * max(best_worst$worst_cases),
        direction = "x",
        segment.color = "#EF4444"
      ) +
      scale_x_continuous(labels = comma) +
      labs(
        title = "Best vs Worst COVID-19 Case Burden per Continent",
        subtitle = "Lowest and highest total cases per million by country",
        x = "Total Cases per Million",
        y = "Continent",
        caption = "Green = Best-performing country | Red = Worst-performing country"
      ) +
      theme_minimal(base_size = 14)
  })

# ------------------------------------------------------
  # TAB 4 preprocessing (local)
  tab4_continents <- reactive({
    sort(unique(covidData$continent[!is.na(covidData$continent) & covidData$continent != ""]))
  })

  observeEvent(tab4_continents(), {
    conts <- tab4_continents()
    updateSelectInput(session, "treemap_continent", choices = conts, selected = "Europe")
    updateSelectInput(session, "response_continent", choices = conts, selected = "Europe")
    updateSelectInput(session, "vax_continent", choices = conts, selected = "Europe")
  }, ignoreInit = FALSE)

  # Section 1: Stringency Treemap
  output$treemap_plot <- renderPlotly({
    req(input$treemap_continent)
    df <- covidData %>%
      filter(continent == input$treemap_continent,
             !is.na(population),
             !is.na(stringency_index),
             !is.na(new_cases)) %>%
      group_by(location, population) %>%
      summarise(
        avg_stringency = mean(stringency_index, na.rm = TRUE),
        total_cases = sum(new_cases, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(cases_per_100k = round((total_cases / population) * 100000, 0)) %>%
      filter(avg_stringency > 0, cases_per_100k > 0) %>%
      arrange(desc(population)) %>%
      head(input$top_n)

    plot_ly(df,
            type = "treemap",
            labels = ~location,
            parents = "",
            values = ~population,
            marker = list(
              colors = ~avg_stringency,
              colorscale = list(c(0, "#1E88E5"), c(0.5, "#EECBAD"), c(1, "#EF4444")),
              colorbar = list(title = "Stringency"),
              line = list(color = "white", width = 3)
            ),
            text = ~paste0("Cases/100k: ", format(cases_per_100k, big.mark = ",")),
            textfont = list(size = 12, color = "white"),
            hovertemplate = paste('<b>%{label}</b><br>',
                                  'Cases/100k: %{customdata[0]}<br>',
                                  'Stringency: %{customdata[1]}<br>',
                                  'Population: %{customdata[2]}<extra></extra>'),
            customdata = ~cbind(format(cases_per_100k, big.mark = ","),
                                round(avg_stringency, 1),
                                format(population, big.mark = ","))) %>%
      layout(title = paste("COVID-19 in", input$treemap_continent))
  })

  # Section 2: Early vs Late Response
  response_data <- reactive({
    req(input$response_continent)
    covidData %>%
      filter(total_cases >= 100,
             continent == input$response_continent,
             !is.na(stringency_index),
             !is.na(population)) %>%
      group_by(location, population) %>%
      arrange(date) %>%
      summarise(
        first_100_cases = min(date[total_cases >= 100], na.rm = TRUE),
        first_stringency_50 = min(date[stringency_index >= 50], na.rm = TRUE),
        total_cases = sum(new_cases, na.rm = TRUE),
        total_deaths = sum(new_deaths, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(!is.infinite(first_stringency_50), !is.infinite(first_100_cases)) %>%
      mutate(
        days_to_respond = as.numeric(difftime(first_stringency_50, first_100_cases, units = "days")),
        response_type = ifelse(days_to_respond <= input$threshold, "Early", "Late"),
        cases_per_100k = (total_cases / population) * 100000,
        deaths_per_100k = (total_deaths / population) * 100000
      ) %>%
      filter(!is.na(days_to_respond), days_to_respond >= 0)
  })

  output$diverging_plot <- renderPlotly({
    df <- response_data()
    if (nrow(df) == 0) return(plotly_empty())

    df <- df %>% arrange(desc(population)) %>% head(input$num_countries)

    if (input$response_metric == "cases") {
      df <- df %>% mutate(value = ifelse(response_type == "Early", cases_per_100k, -cases_per_100k)) %>% arrange(desc(value))
    } else {
      df <- df %>% mutate(value = ifelse(response_type == "Early", deaths_per_100k, -deaths_per_100k)) %>% arrange(desc(value))
    }

    df$location <- factor(df$location, levels = rev(df$location))
    metric_label <- ifelse(input$response_metric == "cases", "Cases", "Deaths")

    plot_ly(df, y = ~location, x = ~value,
            type = 'bar', orientation = 'h',
            color = ~response_type,
            colors = c("Early" = "#1E88E5", "Late" = "#EF4444"),
            text = ~paste0(abs(round(value, 1))),
            textposition = 'outside') %>%
      layout(
        title = paste(metric_label, "per 100k:", input$response_continent),
        xaxis = list(title = paste(metric_label, "per 100k"), zeroline = TRUE, zerolinewidth = 3),
        yaxis = list(title = ""),
        showlegend = TRUE
      )
  })

  # Section 3: Test Positivity
  output$positivity_plot <- renderPlotly({
    req(input$positivity_country, input$positivity_date_range)
    
    df <- covidData %>%
      filter(
        location == input$positivity_country,
        date >= input$positivity_date_range[1],
        date <= input$positivity_date_range[2],
        !is.na(positive_rate),
        !is.na(new_cases_smoothed_per_million)
      ) %>%
      mutate(
        positivity_pct = positive_rate * 100,
        cases_per_100k = new_cases_smoothed_per_million / 10,
        week = floor_date(date, "week")
      ) %>%
      group_by(week) %>%
      summarise(
        avg_positivity = mean(positivity_pct, na.rm = TRUE),
        avg_cases = mean(cases_per_100k, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(df, hovermode = "x unified") %>%
      
      # ---- Positivity ----
    add_lines(
      x = ~week,
      y = ~avg_positivity,
      name = "Test Positivity %",
      line = list(color = "#DC2626", width = 4),
      marker = list(
        color = "#DC2626",   
        size = 6
      ),
      yaxis = "y"
    ) %>%
      
      # ---- Cases ----
    add_lines(
      x = ~week,
      y = ~avg_cases,
      name = "Cases per 100k",
      line = list(color = "#2563EB", width = 4),
      marker = list(
        color = "#2563EB",   
        size = 6
      ),
      yaxis = "y2"
    ) %>%
      
      # ---- Safe threshold ----
    add_lines(
      x = range(df$week), y = c(5, 5),
      name = "Safe (5%)",
      line = list(color = "#10B981", dash = "dash"),
      yaxis = "y",
      inherit = FALSE
    ) %>%
      
      # ---- Danger threshold ----
    add_lines(
      x = range(df$week), y = c(10, 10),
      name = "Danger (10%)",
      line = list(color = "#F59E0B", dash = "dash"),
      yaxis = "y",
      inherit = FALSE
    ) %>%
      
      layout(
        title = paste("Test Positivity vs Cases —", input$positivity_country),
        
        xaxis = list(
          title = "Date",
          showgrid = TRUE,
          gridcolor = "#E5E7EB"
        ),
        
        yaxis = list(
          title = "Test Positivity (%)",
          rangemode = "tozero",
          dtick = 5,                
          showgrid = TRUE,
          gridcolor = "#E5E7EB",
          zeroline = FALSE,
          titlefont = list(color = "#DC2626"),
          tickfont = list(color = "#DC2626")
        ),
        
        yaxis2 = list(
          title = "Cases per 100k",
          overlaying = "y",
          side = "right",
          
          tickmode = "array",                
          tickvals = c(0, 50, 100, 150, 200, 250),
          ticktext = c("0", "50", "100", "150", "200", "250"),
          
          range = c(0, 250),
          
          showgrid = FALSE,
          zeroline = FALSE,
          
          titlefont = list(color = "#2563EB"),
          tickfont  = list(color = "#2563EB")
        ),
        
        legend = list(orientation = "h", x = 0.1, y = -0.25)
      )
  })

  # Section 4: Vaccination Impact
  output$vax_impact_plot <- renderPlot({
    req(input$vax_continent)

    countries_data <- covidData %>%
      filter(continent == input$vax_continent,
             !is.na(people_fully_vaccinated_per_hundred),
             !is.na(new_deaths_smoothed),
             !is.na(population)) %>%
      group_by(location) %>%
      filter(max(people_fully_vaccinated_per_hundred, na.rm = TRUE) >= input$vax_threshold) %>%
      ungroup()

    if (nrow(countries_data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
               theme_void())
    }

    top_countries <- countries_data %>%
      distinct(location, population) %>%
      arrange(desc(population)) %>%
      head(input$vax_num_countries) %>%
      pull(location)

    all_results <- list()

    for (country in top_countries) {
      country_data <- countries_data %>% filter(location == country) %>% arrange(date)
      pop <- first(country_data$population)

      threshold_date <- country_data %>%
        filter(people_fully_vaccinated_per_hundred >= input$vax_threshold) %>%
        pull(date) %>% min()

      if (!is.na(threshold_date) && !is.infinite(threshold_date)) {
        before <- country_data %>%
          filter(date < threshold_date) %>%
          summarise(total_deaths = sum(new_deaths_smoothed, na.rm = TRUE), days = n()) %>%
          mutate(deaths_per_100k_per_day = (total_deaths / pop * 100000) / days)

        after <- country_data %>%
          filter(date >= threshold_date) %>%
          summarise(total_deaths = sum(new_deaths_smoothed, na.rm = TRUE), days = n()) %>%
          mutate(deaths_per_100k_per_day = (total_deaths / pop * 100000) / days)

        if (nrow(before) > 0 && nrow(after) > 0 && before$days > 30 && after$days > 30) {
          all_results[[country]] <- data.frame(
            country = country,
            period = c("Before", "After"),
            deaths_rate = c(before$deaths_per_100k_per_day, after$deaths_per_100k_per_day)
          )
        }
      }
    }

    if (length(all_results) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "Insufficient data", size = 6) +
               theme_void())
    }

    df <- do.call(rbind, all_results)
    df$period <- factor(df$period, levels = c("Before", "After"))

    ggplot(df, aes(x = country, y = deaths_rate, fill = period)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = round(deaths_rate, 3)),
                position = position_dodge(width = 0.8),
                vjust = -0.3, size = 3.5) +
      scale_fill_manual(values = c("Before" = "#E53935", "After" = "#1E88E5")) +
      labs(
        title = paste("Death Rates: Before vs After", input$vax_threshold, "% Vaccination"),
        x = "",
        y = "Daily Deaths per 100k Population"
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top")
  })

  # ------------------------------------------------------
  # TAB 5 preprocessing (local)
  jordan_data <- reactive({
    covidData %>% filter(location == "Jordan") %>% mutate(date = as.Date(date))
  })

  vaccination_start_date <- reactive({
    jordan_data() %>%
      filter(!is.na(new_vaccinations_smoothed),
             new_vaccinations_smoothed > 0) %>%
      summarise(start_date = min(date)) %>%
      pull(start_date)
  })

  output$jordan_waves_plot <- renderPlot({
    plot_data <- jordan_data()
    p <- ggplot(plot_data, aes(x = date))

    if ("Cases" %in% input$jordan_metrics) {
      p <- p + geom_line(
        data = plot_data %>% filter(!is.na(new_cases_smoothed)),
        aes(y = new_cases_smoothed, color = "Cases"),
        linewidth = 1
      )
    }

    if ("Deaths" %in% input$jordan_metrics) {
      p <- p + geom_line(
        data = plot_data %>% filter(!is.na(new_deaths_smoothed)),
        aes(y = new_deaths_smoothed * 50, color = "Deaths"),
        linewidth = 1
      )
    }

    vax_date <- vaccination_start_date()
    if (!is.na(vax_date)) {
      p <- p + geom_vline(xintercept = vax_date, linetype = "dashed",
                          color = "#00CD66", linewidth = 1)
    }

    y_max <- max(plot_data$new_cases_smoothed, na.rm = TRUE)

    p +
      scale_y_continuous(
        name = "New Cases (Smoothed)",
        limits = c(0, y_max),
        sec.axis = sec_axis(~./50, name = "New Deaths (Smoothed)")
      ) +
      scale_x_date(expand = c(0, 0)) +
      scale_color_manual(values = c("Cases" = "#1E88E5", "Deaths" = "red")) +
      labs(
        title = "Jordan: COVID-19 Waves",
        subtitle = paste("Dual-Axis Time Series: Cases vs Deaths (Smoothed)",
                         if (!is.na(vax_date)) paste("\nGreen line: Vaccination start on", vax_date) else ""),
        color = "Metric"
      ) +
      theme_minimal(base_size = 13)
  })

  output$regional_comparison <- renderPlot({
    countries <- c("Jordan", "Syria", "Lebanon", "Palestine", "Saudi Arabia", "Iraq")

    plot_data <- covidData %>%
      filter(location %in% countries, !is.na(new_cases_smoothed_per_million)) %>%
      mutate(location = factor(location, levels = countries))

    ggplot(plot_data, aes(x = date, y = new_cases_smoothed_per_million)) +
      geom_line(color = "#1E88E5", linewidth = 0.9) +
      facet_wrap(~ location, scales = "free_y", ncol = 2) +
      scale_x_date(expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "COVID-19 Case Trends in Jordan and Neighboring Countries",
        subtitle = "Smoothed daily COVID-19 cases per million population",
        x = "Date",
        y = "New Cases per Million"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            strip.text = element_text(face = "bold"),
            panel.grid.minor = element_blank())
  })

  output$jordan_bubble_plot <- renderPlot({
    
   
    y_var <- if (input$jordan_outcome == "cases") {
      "new_cases_smoothed"
    } else {
      "new_deaths_smoothed"
    }
    
 
    plot_data <- jordan_data() %>%
      filter(
        !is.na(stringency_index),
        !is.na(.data[[y_var]]),
        .data[[y_var]] >= 0
      ) %>%
      mutate(
        period = ifelse(
          date < as.Date("2021-01-13"),
          "Before Vaccination",
          "After Vaccination"
        ),
        bubble_size = ifelse(
          period == "Before Vaccination",
          1,
          ifelse(is.na(new_vaccinations_smoothed),
                 1,
                 new_vaccinations_smoothed)
        )
      ) %>%
      filter(!is.na(bubble_size)) %>%
      slice(seq(1, n(), by = 7))  
    
   
    y_limit <- if (input$jordan_outcome == "cases") {
      input$y_max_limit
    } else {
      max(plot_data[[y_var]], na.rm = TRUE) * 1.2
    }
    
    
    plot_title <- if (input$jordan_outcome == "cases") {
      "Policy, Vaccination, and COVID-19 Cases in Jordan"
    } else {
      "Policy, Vaccination, and COVID-19 Deaths in Jordan"
    }
    
    y_label <- if (input$jordan_outcome == "cases") {
      "New COVID-19 Cases (Smoothed)"
    } else {
      "New COVID-19 Deaths (Smoothed)"
    }
    
    
    ggplot(
      plot_data,
      aes(
        x = stringency_index,
        y = .data[[y_var]],
        size = bubble_size,
        color = period
      )
    ) +
      geom_point(alpha = 0.6) +
      
      coord_cartesian(ylim = c(0, y_limit)) +
      
      scale_y_continuous(name = y_label) +
      
      scale_size_continuous(
        name = "Vaccination Level",
        range = c(3, 18)
      ) +
      
      scale_color_manual(
        values = c(
          "Before Vaccination" = "#E53935",
          "After Vaccination"  = "#1E88E5"
        )
      ) +
      
      labs(
        title = plot_title,
        subtitle = "Bubble size = vaccination rate",
        x = "Policy Stringency Index",
        color = "Period"
      ) +
      
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "right"
      )
  })
  

  output$jordan_corrplot <- renderPlot({
    req(input$corr_vars_jordan)
    if (length(input$corr_vars_jordan) < 2) {
      plot.new(); text(0.5, 0.5, "Please select at least 2 variables", cex = 1.5); return()
    }

    corr_data <- jordan_data() %>% select(all_of(input$corr_vars_jordan)) %>% na.omit()
    if (nrow(corr_data) < 10) {
      plot.new(); text(0.5, 0.5, "Insufficient data for correlation", cex = 1.5); return()
    }

    corr_matrix <- cor(corr_data, method = input$corr_method)

    var_labels <- c(
      "new_cases_smoothed_per_million" = "Cases/M",
      "new_deaths_smoothed_per_million" = "Deaths/M",
      "stringency_index" = "Stringency",
      "reproduction_rate" = "R Rate",
      "positive_rate" = "Pos. Rate",
      "new_tests_smoothed_per_thousand" = "Tests/K"
    )

    colnames(corr_matrix) <- var_labels[colnames(corr_matrix)]
    rownames(corr_matrix) <- var_labels[rownames(corr_matrix)]

    corrplot(
      corr_matrix,
      method = "color",
      type = "upper",
      addCoef.col = "black",
      tl.col = "black",
      tl.srt = 45,
      diag = FALSE,
      number.cex = 1.2,
      cl.cex = 1.0,
      title = paste("Correlation Matrix for Jordan (", tools::toTitleCase(input$corr_method), ")", sep = ""),
      mar = c(0, 0, 2, 0)
    )
  })
}

shinyApp(ui = ui, server = server)
