# Load required libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)
library(dataRetrieval)

# Load and preprocess data
data_Q <- readNWISdv(siteNumber = '09024000',
                     parameterCd = '00060',
                     startDate = '1910-10-01',
                     endDate = '2024-09-30') %>% 
  renameNWISColumns()

# Convert flow to mm
data_Q$Q_mm <- data_Q$Flow * 86400 / 27.6 / 5280 / 5280 * 304.8
data_Q$month <- month(data_Q$Date)
data_Q$wyear <- ifelse(data_Q$month > 9, year(data_Q$Date) + 1, year(data_Q$Date))
data_Q$group <- ifelse(data_Q$wyear < 1936, "Pre-diversion", "Diversion")

# UI
ui <- fluidPage(
  # Add custom CSS for background color and styling
  tags$style(HTML("
    body {
      background-color: teal;
    }
    .title-box {
      background-color: white;
      border-radius: 10px;
      padding: 20px;
      margin: 20px auto;
      text-align: center;
      width: 80%;
      box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.3);
    }
    .title-box h2 {
      color: #004d4d;
      font-weight: bold;
      font-family: Arial, sans-serif;
    }
    .content-box {
      background-color: white;
      border-radius: 10px;
      padding: 15px;
      box-shadow: 0px 2px 5px rgba(0, 0, 0, 0.2);
    }
    .tab-content {
      background-color: white !important;
      padding: 10px;
      border-radius: 10px;
    }
    .nav-tabs > li > a {
      background-color: white !important;
      color: #004d4d !important;
      font-weight: bold;
      border: 1px solid #ddd !important;
    }
    .nav-tabs > li.active > a {
      background-color: #f5f5f5 !important;
      color: #004d4d !important;
      border: 1px solid #ddd !important;
      border-bottom: none !important;
    }
    .description {
      background-color: white;
      border-radius: 10px;
      padding: 15px;
      margin-top: 15px;
      margin-bottom: 30px;
      box-shadow: 0px 2px 5px rgba(0, 0, 0, 0.2);
    }
  ")),
  
  # Title box
  div(class = "title-box",
      h2("Fraser River Watershed Analysis")
  ),
  
  fluidRow(
    column(12, 
           div(class = "content-box",
               p("The Fraser River watershed lies in north-central Colorado, surrounded by the Continental Divide on the east and south sides, and by smaller ridges to the west and north. The Fraser River flows from southeast to northwest, ultimately joining the Colorado River. For this study, the streamflow data was taken from USGS stream gauge station (USGS – 09024000) located at Winter Park, Colorado which has a drainage area of 27.6 square miles."),
               p("The watershed's elevation ranges from a minimum of 8,920 feet to a maximum of 13,400 feet. The watershed has a mean slope of 38%, representing moderately steep terrain. Most of the watershed’s area is covered by evergreen forest (53%) followed by shrub/scrub (18%) and herbaceous plants (11%)."),
               p("The watershed consists of 23 different soil types, primarily gravelly and stony soils, which are excessively drained. The watershed contains mostly Leighcan family soils with various slope ranges, which are excessively drained having moderately high to high permeability (Ksat values ranging from 0.6 to 2 in/hr). Denver Water began diverting the water from the Fraser River in 1936 through Moffat Diversion."),
               p("It is estimated that the Moffat Tunnel diverts over 60% of the Fraser River's natural flow on an annual basis (Colorado River Headwaters Chapter of Trout Unlimited, 2014). This study will compare streamflow between two time periods i.e. before and after Moffat diversion.")
           )
    )
  ),
  
  # Add sliders and filters
  sidebarLayout(
    sidebarPanel(
      div(class = "content-box",
          sliderInput("yearRange", "Select Water Year Range:",
                      min = min(data_Q$wyear), max = max(data_Q$wyear),
                      value = c(min(data_Q$wyear), max(data_Q$wyear)),
                      step = 1),
          radioButtons("group", "Streamflow Group:",
                       choices = c("All", "Pre-diversion", "Diversion"), selected = "All"),
          sliderInput("animatedYear", "Year-wise Variation:",
                      min = min(data_Q$wyear),
                      max = max(data_Q$wyear),
                      value = min(data_Q$wyear),
                      step = 1,
                      animate = animationOptions(interval = 800, loop = TRUE)),
          img(src = "Fraser_River_Watershed.jpg", height = "400px", width = "370px", style = "display: block; margin-left: auto; margin-right: auto;")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Yearly Streamflow",
                 div(class = "tab-content",
                     plotlyOutput("yearly_plot"))
        ),
        tabPanel("Year-wise Variation",
                 div(class = "tab-content",
                     plotlyOutput("animated_yearwise_plot"))
        ),
        tabPanel("Mean Monthly Trends",
                 div(class = "tab-content",
                     plotlyOutput("monthly_plot"))
        ),
        tabPanel("Flow Duration Curve",
                 div(class = "tab-content",
                     plotlyOutput("fdc_plot"))
        )
      ),
      div(class = "description",
          p("The mean annual precipitation of Fraser River Watershed is 33.13 inches and mean annual reference evapotranspiration is 37.73 inches. It has an aridity index of 0.88 which indicates water deficit in the area meaning more evapotranspiration than precipitation. Climate data are available only from 1981 to 2023; however, discharge data are available from 1911 to 2024, hence the impact of diversion on streamflow was analyzed before (1911 – 1935) and after diversion periods (1937-2024)."),
          p("The average discharge of the Fraser River before diversion is 553.53 mm (21.79 in) whereas after diversion the average discharge decreased drastically up to 230.52 mm (9.07 in). A drastic decrease in discharge can be observed mainly after the diversion period, indicating the long-term impact of such human interventions on streamflow."),
          p("Statistical analysis further confirms the significant impact of diversion on streamflow. A Welch Two Sample t-test was conducted to compare the mean discharge between the pre-diversion and post-diversion periods. The test results showed a p value <0.001 which indicates significant differences in the mean annual discharge between the prediversion and diversion periods. The variation is further explained by the monthly variation in discharge between the prediversion and diversion periods, which shows more than a 50% decrease in peak streamflow between both periods. During prediversion period, the streamflow reached over 210 mm whereas after diversion it decreased up to 80 mm during peak months highlighting the impact of such human interventions on streamflow."),
          p("Additionally, the flow duration curve provides more information on the altered flow regimes. It shows that during the pre-diversion period, higher flow rates exceeded more frequently compared to the diversion period. Such kind of diversions not only reduce the water availability in the downstream areas but also will result in ecological impacts on the downstream areas.")
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    if (input$group == "Pre-diversion") {
      filter(data_Q, group == "Pre-diversion")
    } else if (input$group == "Diversion") {
      filter(data_Q, group == "Diversion")
    } else {
      data_Q
    } %>% filter(wyear >= input$yearRange[1] & wyear <= input$yearRange[2])
  })
  
  # Yearly streamflow plot
  output$yearly_plot <- renderPlotly({
    yearly <- filtered_data() %>%
      group_by(wyear, group) %>%
      summarize(Q_mm = sum(Q_mm, na.rm = TRUE))
    
    ggplot(yearly, aes(x = wyear, y = Q_mm, color = group)) +
      geom_line(size = 0.3, linetype = "dashed") +  # Dashed line for each group
      geom_point(size = 1) +
      theme_bw() +
      scale_color_manual(
        values = c("Pre-diversion" = "blue", "Diversion" = "red")
      ) +
      labs(
        title = "Yearly Streamflow Trends",
        x = "Water Year",
        y = "Streamflow (mm)",
        color = NULL
      )
  })
  
  # Animated year-wise variation plot
  output$animated_yearwise_plot <- renderPlotly({
    year_data <- filtered_data() %>%
      filter(wyear == input$animatedYear)
    
    if (nrow(year_data) == 0) {
      return(plotly::plot_ly() %>% 
               plotly::layout(title = "No Data Available for Selected Water Year"))
    }
    
    ggplot(year_data, aes(x = Date, y = Flow)) +
      geom_line(size = 1, color = "blue") +
      theme_bw() +
      labs(title = paste("Streamflow Variation in Water Year", input$animatedYear),
           x = "Date", y = "Flow (cfs)")
  })
  
  # Monthly trends plot
  output$monthly_plot <- renderPlotly({
    monthly <- filtered_data() %>%
      group_by(month, group) %>%
      summarize(Q_mm = mean(Q_mm, na.rm = TRUE))
    
    ggplot(monthly, aes(x = month, y = Q_mm, color = group)) +
      geom_line(size = 1) +
      theme_bw() +
      scale_color_manual(values = c("Pre-diversion" = "blue", "Diversion" = "red")) +
      labs(title = "Mean Monthly Streamflow Trends",
           x = "Month", y = "Mean Streamflow (mm)", color = NULL)
  })
  
  # Flow duration curve plot
  output$fdc_plot <- renderPlotly({
    fdc_prediversion <- filtered_data() %>%
      filter(group == "Pre-diversion") %>%
      arrange(Flow) %>%
      mutate(rank = rank(Flow, ties.method = "first"),
             p = rank / n())
    
    fdc_diversion <- filtered_data() %>%
      filter(group == "Diversion") %>%
      arrange(Flow) %>%
      mutate(rank = rank(Flow, ties.method = "first"),
             p = rank / n())
    
    fdc_combined <- bind_rows(
      fdc_prediversion %>% mutate(Group = "Pre-diversion"),
      fdc_diversion %>% mutate(Group = "Diversion")
    )
    
    ggplot(fdc_combined, aes(x = p, y = Flow, color = Group)) +
      geom_line(size = 1) +
      scale_y_log10() +
      theme_bw() +
      scale_color_manual(values = c("Pre-diversion" = "blue", "Diversion" = "red")) +
      labs(title = "Flow Duration Curve",
           x = "Exceedance Probability", y = "Flow (cfs)", color = NULL)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
