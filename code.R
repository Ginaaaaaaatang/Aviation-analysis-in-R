library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(DT)
library(shinyBS)
library(ggpubr)


#read csv file
country.map <-
  read.csv(file = 'country_map.csv', check.names = FALSE)

worlddata <-
  read.csv(
    file = 'AIR.csv',
    check.names = FALSE,
    na.string = c("NA", "", "Unknown")
  )

AADS <-
  read.csv(
    "AviationData.csv",
    colClasses = c("Event.Date" = "Date"),
    check.names = FALSE,
    na.string = c("NA", "", "Unknown")
  )


names(AADS)

# data cleaning
AADS$Injury.Severity[is.na(AADS$Injury.Severity)] <- "Unavailable"
AADS$Total.Fatal.Injuries[is.na(AADS$Total.Fatal.Injuries)] <- 0
AADS$Total.Serious.Injuries[is.na(AADS$Total.Serious.Injuries)] <- 0
AADS$Total.Minor.Injuries[is.na(AADS$Total.Minor.Injuries)] <- 0
AADS$Total.Uninjured[is.na(AADS$Total.Uninjured)] <- 0
#AADS$Number.of.Engines[is.na(AADS$Total.Uninjured)] <- "Unknown"
AADS$Broad.Phase.of.Flight[is.na(AADS$Broad.Phase.of.Flight)] <-
  "UNKNOWN"
AADS$Engine.Type[is.na(AADS$Engine.Type)] <- "Unknown"
#AADS$Investigation.Type[is.na(AADS$Investigation.Type)]<- "Unknown"
#AADS$Aircraft.Damage[is.na(AADS$Aircraft.Damage)] <- "Unknown"
#AADS$ Location [is.na(AADS$Location)]<- "Unknown"
AADS$Country[is.na(AADS$Country)] <- "Unknown"
AADS$Model[is.na(AADS$Model)] <- "Unknown"
#AADS$Make[is.na(AADS$Make)]<- "Unknown"
#AADS$Weather.Condition [is.na(AADS$Weather.Condition)]<- "Unknown"
#AADS$Amateur.Built [is.na(AADS$Amateur.Built)]<- "Unknown"
#AADS$Aircraft.Category [is.na(AADS$Aircraft.Category)]<- "Unknown"

sapply(AADS, function(x)
  sum(is.na(x)))


#data cleaning

AADS$Month <- format(as.Date(AADS$Event.Date), "%m")
AADS$Year <- format(as.Date(AADS$Event.Date), "%Y")
AADS$YearMonth <- format(as.Date(AADS$Event.Date), "%Y-%m")

#AADS$Investigation.Type <- AADS$Investigation.Type %>% as.factor
AADS$Injury.Severity <- AADS$Injury.Severity %>% as.factor
#AADS$Aircraft.Damage <- AADS$Aircraft.Damage %>% as.factor
AADS$Broad.Phase.of.Flight <-
  AADS$Broad.Phase.of.Flight %>% as.factor
#AADS$Weather.Condition <- AADS$Weather.Condition %>% as.factor
AADS$Country <- AADS$Country %>% as.character

AADS <-
  filter(AADS, between(Event.Date, as.Date("1990-12-01"), as.Date("2019-12-31")))

AADS$Year <- as.numeric(AADS$Year)
AADS$Month <- as.numeric(AADS$Month)
AADS$Engine.Type  <- AADS$Engine.Type %>% as.factor

#rename levels
levels(AADS$Engine.Type)

levels(AADS$Engine.Type)[levels(AADS$Engine.Type) == "REC, TJ, REC, TJ"]  <-
  "REC, TJ"
levels(AADS$Engine.Type)[levels(AADS$Engine.Type) == "REC, TJ, TJ"]  <-
  "REC, TJ"
levels(AADS$Engine.Type)[levels(AADS$Engine.Type) == "TJ, REC, REC, TJ"]  <-
  "REC, TJ"

levels(AADS$Injury.Severity)

levels(AADS$Injury.Severity)[1:126] <- "Fatal"

AADS <- AADS %>%
  mutate(Country = tolower(Country)) %>%
  mutate(
    Country = recode(
      Country,
      "united states"    = "united states of america",
      "bahamas" = "the bahamas",
      "congo" = "democratic republic of the congo",
      "korea, republic of" = "south korea",
      "tanzania" = "united republic of tanzania",
      "somalia" = "somaliland",
      "korea, democratic people's republic of" = "north korea"
    )
  )

#world.year

world.year <- worlddata %>%
  summarize_if(is.numeric, sum, na.rm = TRUE)

world.year <-
  select(world.year,-c(1:11))

world.year <- world.year %>%
  gather(Year, world.count, 1:39)

world.year <- world.year %>%
  mutate(world.count = world.count / 10000000) %>%
  mutate_if(is.numeric, ~ round(., 0))

world.year$Year <- as.numeric(world.year$Year)

world.year <- na.omit(world.year, c("world.count", "Year"))

AADS.filtered <- AADS %>%
  group_by(Year) %>%
  summarise(Year.count = n())



world_trend <- full_join(AADS.filtered, world.year, by = c('Year'))

# Q1V1
color1 <- "#69b3a2"
color2 <- "#E866D9"

# distribution of the most country (map)

AADS_map <- AADS

AADS_map <- AADS_map %>%
  filter(AADS$Country         != "") %>% droplevels() %>%
  group_by(Country, Year) %>%
  summarise(count = n())

AADS_map <- AADS_map %>%
  rename(region = Country,
         value = count) %>%
  mutate(region = tolower(region)) %>%
  mutate(
    region = recode(
      region,
      "united states"    = "united states of america",
      "bahamas" = "the bahamas",
      "congo" = "democratic republic of the congo",
      "korea, republic of" = "south korea",
      "tanzania" = "united republic of tanzania",
      "somalia" = "somaliland",
      "korea, democratic people's republic of" = "north korea"
    )
  )

AADS_map <- left_join(AADS_map, country.map, by = "region")

l <- list(color = toRGB("grey"), width = 0.5)

# Define UI for application
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Visualisation Project"),
  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed; overflow: visible;width: 230px;",
      menuItem(
        "Introduction",
        tabName = "Introduction",
        icon = icon('fas fa-child')
      ),
      menuItem(
        "Analysis",
        tabName = "Analysis",
        icon = icon("fas fa-chart-bar")
      ),
      tags$head(tags$style(
        HTML(
          "
      .shiny-output-error-validation {
        color: red;
        font-size : 30px;
      }
    "
        )
      )),
      tipify(
        pickerInput(
          "CountryInput",
          "Select Country",
          choices = unique(AADS$Country),
          options = list(`actions-box` = TRUE),
          selected = unique(AADS$Country),
          multiple = TRUE,
          width = "200px",
          inline = TRUE,
        ),
        title = "Please select the county for the map, donut chart, line chart, and scatter chart. At least one country should be selected"
        ,
        placement = "top"
      ),
      tipify(
        sliderInput(
          "yearInput",
          "Select Year Range:",
          min = 1990,
          max = 2019,
          value = c(1990, 2019),
          step = 1,
          animate = animationOptions(interval = 1800)
        ),
        title = "Please select the time range for the dual y-axis chart, donut chart, line chart, and scatter chart. If you click the button, the time animation will be displayed."
        ,
        placement = "top"
      ),
      tipify(
        sliderInput(
          "Single",
          "Year in Map:",
          min = 1990,
          max = 2019,
          value = c(2000),
          step = 1,
          animate = animationOptions(interval = 500)
        ),
        title = "Please select the map year, if you click the button, the time animation will be displayed."
        ,
        placement = "top"
      ),
      chooseSliderSkin("Sharp"),
      
      tipify(
        awesomeCheckboxGroup(
          inputId = "checkGroup",
          label = "Choose Injury Severity:",
          choices = list(
            "Fatal" =  "Fatal",
            "Incident" = "Incident",
            "Non-Fatal" = "Non-Fatal"
          ),
          selected = c("Fatal", "Incident", "Non-Fatal"),
          status = "info",
          inline = TRUE
        ),
        title = "please check the boxes in different injury severity to interact with the donut chart, line chart and scatter chart.  At least one box should be selected",
        placement = "top"
      ),
      tipify(
        selectInput(
          inputId = "inputbar",
          label = "Select the Flight Phases:",
          multiple = TRUE,
          selected = c(
            "APPROACH",
            "CLIMB",
            "CRUISE",
            "DESCENT",
            "GO-AROUND",
            "LANDING",
            "MANEUVERING",
            "STANDING",
            "TAKEOFF",
            "TAXI"
          ),
          choices = list(
            "APPROACH",
            "CLIMB",
            "CRUISE",
            "DESCENT",
            "GO-AROUND",
            "LANDING",
            "MANEUVERING",
            "STANDING",
            "TAKEOFF",
            "TAXI"
          )
        ),
        title = "please input the flight phases for filtering the line chart. At least one flight phase should be selected"
        ,
        placement = "top"
      ),
      menuItem(
        "Conclusion",
        tabName = "Conclusion",
        icon = icon("fas fa-pen")
      ),
      menuItem(
        "Datasets",
        tabName = "Dataset",
        icon = icon("fas fa-file-csv")
      ),
      menuItem(
        "Contact me",
        tabName = "Contact",
        icon = icon("fal fa-address-card")
      )
    )
  ),
  dashboardBody(
    setBackgroundColor(color = "#F3F6F9", shinydashboard = TRUE),
    
    tabItems(
      tabItem(
        tabName = "Introduction",
        titlePanel(h2(
          strong("Aviation Accident Analysis"),
          style = "color: #625878",
          align = "center"
        )),
        fixedRow(br(),
                 br(),
                 img(
                   src = "1.png",
                   height = 300,
                   width = 1000
                 ), align = "center"),
        br()
        ,
        fixedRow(column(1),
                 column(
                   10,
                   h3(
                     "With the increasing popularity of air travel, the safety of air travel has attracted more and more attention and has been continuously improved.
                          Since many factors may affect the quality of air travel and cause aviation accidents, it is necessary to analyse the risks and factors to reduce the occurrence of aviation accidents.
                          This project aims to conduct some analysis on different factors that lead to air crashes by exploring visualised data.
                          In order to improve survivability in aviation accidents, this project will analyse engine, flight phase factors.
                          The results of the project will introduce the trends, distribution and factors in data visualisation by using two datasets."
                     ,
                     style = "color: #887895; font-family: lato;",
                     align = "center"
                   )
                 ),
                 column(1))
      ),
      
      # Second tab content
      tabItem(
        tabName = "Analysis",
        fixedRow(
          column(2),
          column(
            8,
            align = "center",
            
            h2(
              strong(
                "The distribution and trends of accidents in various countries from 1990 to 2019."
              ),
              style = "color: #625878; font-family: lato;"
            ),
            br(),
            br(),
            h3(
              strong("Initially,", style = "color: #7A6E96"),
              "the flight was very innovative, not a common travel tool.
                                There are few world travellers, and immature flight technologies may have caused an air crash in 1990.",
              "As air travel becomes",
              strong(" generalize", style = "color: #7A6E96"),
              ", as well as the numbers of air travellers. With the popularity of air travel and the decrease in the number of aviation accidents overtimes."
              ,
              style = "color: #887895; font-family: lato;"
            ),
            br()
          ),
          column(2)
        ),
        fixedRow(
          column(1),
          box(
            title = "Dual Y axis in bar and line chart",
            width = 10,
            plotlyOutput("Q1Vis1", width = "auto", height = "auto")
          ),
          column(1)
        ),
        
        fixedRow(column(2),
                 column(
                   8,
                   align = "center",
                   h3(
                     "The United States had the most aviation accidents between 1990 and 2019, and there were",
                     strong("52,251", style = "color: #7A6E96"),
                     "aviation accidents. According to the data set used, the United States has",
                     strong("96%", style = "color: #7A6E96"),
                     "of aviation accident records in all countries/regions.",
                     style = "color: #887895; font-family: lato;"
                   ),
                   br()
                 ),
                 column(2)),
        
        fixedRow(
          column(1),
          box(
            title = "Choropleth map",
            width = 10,
            plotlyOutput('map', width = "auto", height = "auto")
          ),
          column(1)
        ),
        
        fixedRow(
          column(2),
          column(
            8,
            align = "center",
            h2(
              strong(
                "What are the numbers in different kinds of injury severity in the various country?"
              ),
              style = "color: #625878; font-family: lato;"
            ),
            br(),
            h3(
              "The following analysis will focus on the United States and factors that might affect aviation accidents and injury severity.",
              style = "color: #887895; font-family: lato;"
            ),
            h3(
              "There are three types of injury severity in aviation accidents that contain fatal, non-fatal and incident. The non-fatal distribution is the most in aviation accidents at",
              strong(" 74.5%", style = "color: #7A6E96"),
              ". Fatal accidents rank second in the injury severity category, accounting for ",
              strong("20%", style = "color: #7A6E96"),
              "of aviation accidents. The incident distributes",
              strong("4.63%", style = "color: #7A6E96"),
              "to aviation accidents.",
              style = "color: #887895; font-family: lato;"
            ),
            br(),
            h3(
              "As mentioned before, the number of aviation accidents has dramatically decreased in numbers with improved aircraft technology. The incident and non-fatal accidents case has decreased by over",
              strong("90%", style = "color: #7A6E96"),
              "in 30 years. In contrast, the number of fatal accidents cases only decreased by approximately ",
              strong("50%", style = "color: #7A6E96"),
              ". However, fatal accidents cause the most fatal and severe injuries.",
              style = "color: #887895; font-family: lato;"
            ),
            br()
          ),
          column(2)
        ),
        
        fixedRow(
          box(title = "Donut graph", width = 6,
              plotlyOutput("Q2V1")),
          box(title = "Line plot", width = 6,
              plotlyOutput("Q2V2"))
        ),
        fixedRow(
          column(2),
          column(
            8,
            align = "center",
            h2(
              strong(
                "What factors contribute the most to the fatality in the various countries?"
              ),
              style = "color: #625878; font-family: lato;"
            ),
            br(),
            h3(
              "Aircraft systems are divided into the most frequently mentioned failures or failure groups: engine or fuel system, flight control or structure,
                         and landing gear or hydraulic system (Reveley et al., 2011). The scatter plot shows that different engine types cause different degrees of damage.",
              strong("Turbofans", style = "color: #7A6E96"),
              " cause the most fatal accidents as well as a large number of deaths. ",
              strong("Turboprop engines", style = "color: #7A6E96"),
              "have also caused a considerably large number of fatal aviation accidents.",
              style = "color: #887895; font-family: lato;"
            ),
            br()
          ),
          column(2)
        ),
        
        fixedRow(
          column(2),
          box(
            title = "Scatter plot",
            width = 8,
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Fatal Injuries",
                plotOutput("Q3V1", click = "plot_click", brush = "plot_brush")
              ),
              tabPanel(
                "Serious Injuries",
                plotOutput("Q3V2", click = "plot_click", brush = "plot_brush")
              ),
              tabPanel(
                "Minor Injuries",
                plotOutput("Q3V3", click = "plot_click", brush = "plot_brush")
              ),
              tabPanel(
                "Uninjuries Incident",
                plotOutput("Q3V4", click = "plot_click", brush = "plot_brush")
              )
            )
          ),
          column(2)
        ),
        fixedRow(
          column(1),
          tipify(
            box(width = 10,
                verbatimTextOutput("info")),
            title = "You can click the point in the plot, this will displays the one point that is closest to the click.",
            placement = "left"
          ),
          column(1)
        ),
        
        fixedRow(
          column(1),
          tipify(
            box(width = 10,
                verbatimTextOutput("brushed")),
            title = "You can select a range of points and this will returns the rows of data that are under the brush.",
            placement = "left"
          ),
          column(1)
        ),
        
        
        
        fixedRow(column(2),
                 column(
                   8,
                   align = "center",
                   br(),
                   h3(
                     "The fatality rate in aircraft accidents varies according to the stage of flight. Wong et al. (2006) mention take-off and approach phases of flight account for ",
                     strong("90%", style = "color: #7A6E96"),
                     " of aviation accidents before 1998.  Overall, the line chart shows that the number of deaths has ",
                     strong("fallen sharply over time", style = "color: #7A6E96"),
                     ". It can be seen that the flight phase closer to the ground has ",
                     strong("a higher survival rate.", style = "color: #7A6E96")
                     ,
                     style = "color: #887895; font-family: lato;"
                   ),
                   br()
                 ),
                 column(2)),
        
        fixedRow(
          column(1),
          box(title = "Line plot", width = 10,
              plotlyOutput('Q3V5')),
          column(1)
        ),
        fixedRow(
          column(1),
          column(
            11,
            br(),
            br(),
            h4("Data Sources:", style = "color: #887895; font-family: lato;"),
            h5("NTSB. 2021. Aviation Accident Database & Synopses. ", style = "color: #887895; font-family: lato;"),
            tags$a(
              href = "https://www.ntsb.gov/_layouts/ntsb.aviation/index.aspx",
              "https://www.ntsb.gov/_layouts/ntsb.aviation/index.aspx"
            ),
            br(),
            br(),
            h5("The world bank. 2021. Air transport, passengers carried.", style = "color: #887895; font-family: lato;"),
            tags$a(
              href = "https://data.worldbank.org/indicator/IS.AIR.PSGR?end=2019&start=1970&view=chart",
              "https://data.worldbank.org/indicator/IS.AIR.PSGR?end=2019&start=1970&view=chart"
            ),
            br(),
            br(),
            h4("Reference:", style = "color: #887895; font-family: lato;"),
            h5(
              " Reveley, M. S., Briggs, J. L., Evans, J. K., Jones, S. M., Kurtoglu, T., Leone, K. M., & Sandifer, C. E.(2011).",
              style = "color: #887895; font-family: lato;"
            ),
            h5(
              "Causal factors and adverse events of aviation accidents and incidents related to integrated vehicle health management.",
              style = "color: #887895; font-family: lato;"
            ),
            tags$a(
              href = " https://ntrs.nasa.gov/api/citations/20110009984/downloads/20110009984.pdf",
              "https://ntrs.nasa.gov/api/citations/20110009984/downloads/20110009984.pdf"
            ),
            br(),
            br(),
            br(),
            h5(
              " Wong, D. K., Pitfield, D. E., Caves, R. E., & Appleyard, A. J. (2006). Quantifying and characterising",
              style = "color: #887895; font-family: lato;"
            ),
            h5(
              "Quantifying and characterising aviation accident risk factors. Journal of Air Transport Management, 12(6), 352-357.",
              style = "color: #887895; font-family: lato;"
            ),
            tags$a(href = "https://doi.org/10.1016/j.jairtraman.2006.09.002", "https://doi.org/10.1016/j.jairtraman.2006.09.002")
          )
        )
      ),
      #conclusion page
      tabItem(tabName = "Conclusion",
              fixedRow(
                column(2),
                column(
                  8,
                  align = "center",
                  h1(strong("Conclusion"), style = "color: #625878; font-family: lato;"),
                  br(),
                  br(),
                  h3(
                    "This project first analyses the distribution of aviation accidents in various countries and seeks the highest distribution. Second, the trend of aviation accidents and the popularity of air travel will be provided, which will give insights into aircraft safety and technological development. Third, the analysis of fatal accident rates and survivability from different factors will be linked to the trend of air travel."
                    ,
                    style = "color: #887895; font-family: lato;"
                  ),
                  br(),
                  h3(
                    "Throughout the analysis process, the results showed that the United States accounted for the highest proportion of aviation accidents, and aircraft technology is constantly improving.
The project shows the engine’s factor analysis found out the types of engines that can affect the degree of injury severity. It seems that turbofan causes the most fatal accidents case as well as a large number of deaths.
Turboprop engine has also caused a considerably large number of fatal aviation accidents. The second factor is the flight phase. The flight phase, which is closer to the ground, has a higher survival rate.
In addition, we found that the survival rate has a clear growth trend, and it is related to advanced aircraft technology and equipment. Through data exploration, we can see a specific connection between factors and the incidence of fatal accidents.
                 Aviation analysis must be conducted annually to improve the quality of air travel continuously."
                    ,
                    style = "color: #887895; font-family: lato;"
                  )
                ),
                column(2)
                
              )),
      
      #dataset
      tabItem(
        tabName = "Dataset",
        fixedRow(h1(strong("Dataset"), style = "color: #625878; font-family: lato; "), align = "center"),
        br(),
        br(),
        DT::dataTableOutput("AADS"),
        br(),
        br(),
        DT::dataTableOutput("worlddata")
      ),
      
      tabItem(tabName = "Contact",
              fixedRow(
                box(
                  width = 7,
                  h1(
                    tags$div(
                      HTML('<i class="fas fa-phone" style = "color:#6BD4D0;"></i>'),
                      strong("Contact Details:")
                      ,
                      style = "color: #2E1245; font-family: lato; "
                    ),
                    align = "center"
                  ),
                  br(),
                  h3("Name: Yu Hing Tang", style = "color: #3C1959; font-family: lato;", align = "center"),
                  h3(
                    "Email: ytan0177@student.monash.edu",
                    style = "color: #3C1959; font-family: lato;",
                    align = "center"
                  ),
                  h3("Monash University", style = "color: #3C1959; font-family: lato;", align = "center")
                ),
                column(
                  5,
                  align = "center",
                  br(),
                  h1(strong("Contact"), style = "color: #625878; font-family: lato; "),
                  br(),
                  br(),
                  h3(
                    "For all enquiries, please contact me via the following contact.",
                    style = "color: #887895; font-family: lato;"
                  )
                )
              ))
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map = renderPlotly({
    validate(need(input$CountryInput >= 1, "Please select at least one country."))
    
    map1 <- AADS_map %>%
      filter(Year <= input$Single,
             region %in% input$CountryInput) %>%
      plot_geo() %>%
      add_trace(
        z = ~ value,
        color = ~ value,
        colors = "PRGn",
        text = ~ region,
        locations = ~ iso_a3,
        marker = list(line = l),
        showlegend = FALSE,
        zmax  = 300,
        zmin = 0
      ) %>%
      layout(
        geo = list(
          showframe = T,
          showcoastlines = F,
          projection = list(type = 'Mercator')
        ),
        title = "The distribution of accidents in countries from 1990 to 2019"
      ) %>%
      colorbar(title = "Accident Scale")
    
    
  })
  
  output$Q1Vis1 = renderPlotly({
    plot_ly(world_trend) %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2]) %>%
      add_trace(
        x = ~ Year,
        y = ~ world.count,
        type = 'bar',
        name = 'Air trallver',
        marker = list(color = '#B0DAC9'),
        hoverinfo = "text",
        text = ~ paste("Air trallver amount:", world.count,
                       "<br>",
                       "Year:", Year)
      ) %>%
      add_trace(
        x = ~ Year,
        y = ~ Year.count,
        type = 'scatter',
        mode = 'lines',
        name = 'World accident amount',
        yaxis = 'y2',
        line = list(color = '#E866D9', width = 4),
        hoverinfo = "text",
        text = ~ paste("World accident amount:", Year.count,
                       "<br>",
                       "Year:", Year)
      )  %>%
      layout(
        title = 'Comparison of world Air Travellers and World Air Accidents Between 1990 and 2019',
        xaxis = list(title = ""),
        yaxis = list(
          side = 'left',
          title = 'World Air Travellers',
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis2 = list(
          side = 'right',
          overlaying = "y",
          title = 'World accident amount',
          showgrid = FALSE,
          zeroline = FALSE,
          automargin = T
        ),
        legend = list(
          orientation = "h",
          # show entries horizontally
          xanchor = "center",
          # use center of legend as anchor
          x = 0.4,
          y = -0.2
        )
      )
    
  })
  
  #Q2
  
  output$Q2V1 <- renderPlotly({
    validate(
      need(input$checkGroup >= 1, "Please check at least one box"),
      need(input$CountryInput >= 1, "Please select at least one country")
    )
    
    AADS %>%
      filter(
        Injury.Severity         != "",
        Injury.Severity != "Unavailable",
        Year >= input$yearInput[1],
        Year <= input$yearInput[2],
        Injury.Severity %in% input$checkGroup,
        Country %in% input$CountryInput
      ) %>% droplevels() %>%
      group_by(Injury.Severity, Year, Country) %>%
      summarise(count = n()) %>%
      plot_ly(
        labels = ~ Injury.Severity,
        values = ~ count,
        textinfo = 'label+percent' ,
        marker = list(line = list(color = '#FFFFFF', width = 1)),
        showlegend = TRUE
      ) %>%
      add_pie(hole = 0.6) %>%
      layout(
        title = "The Injury Severity Distribution in Various Country",
        showlegend = T,
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        colorway = c('#f3cec9', '#e7a4b6', '#cd7eaf')
      )
    
  })
  
  output$Q2V2 <- renderPlotly({
    validate(
      need(input$checkGroup >= 1, "Please check at least one box"),
      need(input$CountryInput >= 1, "Please select at least one country")
    )
    
    AADS %>% select(Year, Injury.Severity, Country) %>%
      filter(
        Injury.Severity         != "",
        Injury.Severity != "Unavailable",
        Year >= input$yearInput[1],
        Year <= input$yearInput[2],
        Injury.Severity %in% input$checkGroup,
        Country %in% input$CountryInput
      ) %>% droplevels() %>%
      group_by(Year, Injury.Severity, Country) %>%
      summarise(Amount = n()) %>%
      ggplot(aes(
        x = Year,
        y = Amount,
        color = Injury.Severity,
        group = Country
      )) +
      geom_line(size = 1.3, alpha = 0.7) +
      labs(x = "Years",
           y = "Case Numbers",
           color = "Type") +
      scale_color_manual(values = c("#EAB5DC", "#5DADE2", "#B48AF3")) +
      ggtitle("Trends in different severity of injuries case each year") +
      theme(
        legend.position = "right",
        plot.title = element_text (h = 0.5 , size = 13),
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 13),
        strip.background = element_rect(
          colour = "black",
          fill = "white",
          size = 1.5,
          linetype = "solid"
        ),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
      ) +
      font("xlab", size = 12) +
      font("ylab", size = 12) +
      font("xy.text", size = 10)
    
  })
  
  #  Question 3
  
  
  output$Q3V1 = renderPlot({
    validate(
      need(input$checkGroup >= 1, "Please check at least one box"),
      need(input$CountryInput >= 1, "Please select at least one country")
    )
    
    AADS %>%
      filter(
        Injury.Severity != "Unavailable",
        Engine.Type != "Unknown",
        Engine.Type != "Hybrid Rocket",
        Engine.Type != "REC, ELEC",
        Engine.Type != "REC, TJ",
        Injury.Severity %in% input$checkGroup,
        Year >= input$yearInput[1],
        Year <= input$yearInput[2],
        Country %in% input$CountryInput
      ) %>%
      ggplot(
        aes(
          x = Engine.Type,
          y = Total.Fatal.Injuries,
          color = Injury.Severity,
          size = Total.Fatal.Injuries
        )
      ) +
      geom_jitter(alpha = 0.7) +
      theme_linedraw() +
      theme(legend.position = "right") +
      ggtitle("The Total Number of Fatal Injuries Among the Top 7 Engine Types from 1990 to 2019") +
      scale_color_manual(values = c("#5D3FD3", "#87CEEB", "#FFC0CB")) +
      labs(subtitle =
             "Relationship break down by Different Injury Severity",
           x = "Engine Type",
           y = "Total Fatal Injuries Amount") +
      theme(
        plot.title = element_text (h = 0.5, size = 16),
        plot.subtitle = element_text (h = 0.5, size = 13),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
      ) +
      font("xlab", size = 14) +
      font("ylab", size = 14) +
      font("xy.text", size = 12)
  })
  
  output$Q3V2 = renderPlot({
    validate(
      need(input$checkGroup >= 1, "Please check at least one box"),
      need(input$CountryInput >= 1, "Please select at least one country")
    )
    
    AADS %>%
      filter(
        Injury.Severity != "Unavailable",
        Engine.Type != "Unknown",
        Engine.Type != "Hybrid Rocket",
        Engine.Type != "REC, ELEC",
        Engine.Type != "REC, TJ",
        Injury.Severity %in% input$checkGroup,
        Year >= input$yearInput[1],
        Year <= input$yearInput[2],
        Country %in% input$CountryInput
      ) %>%
      ggplot(
        aes(
          x = Engine.Type,
          y = Total.Serious.Injuries,
          color = Injury.Severity,
          size = Total.Serious.Injuries
        )
      ) +
      geom_jitter(alpha = 0.7) +
      theme_linedraw() +
      theme(legend.position = "right") +
      ggtitle("The Total Number of Serious Injuries Among the Top 7 Engine Types from 1990 to 2019") +
      scale_color_manual(values = c("#5D3FD3", "#87CEEB", "#FFC0CB")) +
      labs(subtitle =
             "Relationship break down by Different Injury Severity",
           x = "Engine Type",
           y = "Total Serious Injuries Amount") +
      theme(
        plot.title = element_text (h = 0.5, size = 16),
        plot.subtitle = element_text (h = 0.5, size = 13),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
      ) +
      font("xlab", size = 14) +
      font("ylab", size = 14) +
      font("xy.text", size = 12)
  })
  
  output$Q3V3 = renderPlot({
    validate(
      need(input$checkGroup >= 1, "Please check at least one box"),
      need(input$CountryInput >= 1, "Please select at least one country")
    )
    
    AADS %>%
      filter(
        Injury.Severity != "Unavailable",
        Engine.Type != "Unknown",
        Engine.Type != "Hybrid Rocket",
        Engine.Type != "REC, ELEC",
        Engine.Type != "REC, TJ",
        Injury.Severity %in% input$checkGroup,
        Year >= input$yearInput[1],
        Year <= input$yearInput[2],
        Country %in% input$CountryInput
      ) %>%
      ggplot(
        aes(
          x = Engine.Type,
          y = Total.Minor.Injuries,
          color = Injury.Severity,
          size = Total.Minor.Injuries
        )
      ) +
      geom_jitter(alpha = 0.7) +
      theme_linedraw() +
      theme(legend.position = "right") +
      scale_color_manual(values = c("#5D3FD3", "#87CEEB", "#FFC0CB")) +
      ggtitle("The Total Number of Minor Injuries Among the Top 7 Engine Types from 1990 to 2019") +
      labs(subtitle =
             "Relationship break down by Different Injury Severity",
           x = "Engine Type",
           y = "Total Minor Injuries Amount") +
      theme(
        plot.title = element_text (h = 0.5, size = 16),
        plot.subtitle = element_text (h = 0.5, size = 13),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
      ) +
      font("xlab", size = 14) +
      font("ylab", size = 14) +
      font("xy.text", size = 12)
  })
  
  output$Q3V4 = renderPlot({
    validate(
      need(input$checkGroup >= 1, "Please check at least one box"),
      need(input$CountryInput >= 1, "Please select at least one country")
    )
    
    AADS %>%
      filter(
        Injury.Severity != "Unavailable",
        Engine.Type != "Unknown",
        Engine.Type != "Hybrid Rocket",
        Engine.Type != "REC, ELEC",
        Engine.Type != "REC, TJ",
        Injury.Severity %in% input$checkGroup,
        Year >= input$yearInput[1],
        Year <= input$yearInput[2],
        Country %in% input$CountryInput
      ) %>%
      ggplot(
        aes(
          x = Engine.Type,
          y = Total.Uninjured,
          color = Injury.Severity,
          size = Total.Uninjured
        )
      ) +
      geom_jitter(alpha = 0.7) +
      theme_linedraw() +
      theme(legend.position = "right") +
      ggtitle("The Total Number of Uninjured Injuries Among the Top 7 Engine Types from 1990 to 2019") +
      scale_color_manual(values = c("#5D3FD3", "#87CEEB", "#FFC0CB")) +
      labs(subtitle =
             "Relationship break down by Different Injury Severity",
           x = "Engine Type",
           y = "Total Uninjured Injuries Amount") +
      theme(
        plot.title = element_text (h = 0.5, size = 16),
        plot.subtitle = element_text (h = 0.5, size = 13),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
      ) +
      font("xlab", size = 14) +
      font("ylab", size = 14) +
      font("xy.text", size = 12)
  })
  
  output$Q3V5 = renderPlotly({
    validate(
      need(input$inputbar >= 1, "Please input at least one flight phase"),
      need(input$CountryInput >= 1, "Please select at least one country")
    )
    
    Q3V5 <- AADS %>%
      filter(
        Broad.Phase.of.Flight != "UNKNOWN",
        Broad.Phase.of.Flight != "OTHER",
        Year >= input$yearInput[1] &
          Year <= input$yearInput[2],
        Broad.Phase.of.Flight %in% input$inputbar,
        Country %in% input$CountryInput
      ) %>%
      group_by(Year, Broad.Phase.of.Flight) %>%
      summarise_each(funs(sum), Total.Fatal.Injuries) %>% arrange(desc(Total.Fatal.Injuries)) %>%
      ggplot(aes(x = Year, y = Total.Fatal.Injuries, colour = Broad.Phase.of.Flight)) +
      geom_line(size = 0.8, alpha = 0.8) +
      labs(x = "Years", y = "Fatality Count") +
      coord_cartesian(ylim = c(0, 375)) +
      theme(legend.position = "right") +
      theme_linedraw() +
      labs(title = "The Flight Phases of U.S. Aircraft Accidents Between 1990 and 2019",
           subtitle = "The Flight Phase of the Most Fatality Accident.") +
      theme(
        plot.title = element_text (h = 0.5, size = 13),
        plot.subtitle = element_text (h = 0.5, size = 12),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        axis.text = element_text(
          hjust = 1,
          size = 10,
          colour = "black"
        ),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5))
      ) +
      font("xlab", size = 12) +
      font("ylab", size = 12) +
      font("xy.text", size = 10)
    
    
  })
  
  output$info <- renderPrint({
    AADS %>%
      select(
        Engine.Type,
        Total.Fatal.Injuries,
        Total.Serious.Injuries,
        Total.Minor.Injuries,
        Total.Uninjured,
        Injury.Severity
      ) %>%
      nearPoints(
        input$plot_click,
        threshold = 35,
        maxpoints = 1,
        addDist = TRUE
      )
    
  })
  
  output$brushed <- renderPrint({
    AADS %>%
      select(
        Engine.Type,
        Total.Fatal.Injuries,
        Total.Serious.Injuries,
        Total.Minor.Injuries,
        Total.Uninjured,
        Injury.Severity
      ) %>%
      brushedPoints(input$plot_brush, allRows = FALSE)
  })
  
  #dataset output
  output$AADS <- DT::renderDataTable({
    DT::datatable(AADS)
  })
  
  output$worlddata <- DT::renderDataTable({
    DT::datatable(world_trend)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
