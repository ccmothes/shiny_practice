library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)

# Read in dataset
plants <- read_csv("data/plant_data.csv") %>% 
  # calc min/max vars
  separate(`Soil pH`, into = c("soil_ph_min", "soil_ph_max"), sep = "-") %>% 
  separate(Size, into = c("min_size", "max_size"), sep = "-") %>% 
  separate(`Hardiness Zone`, into = c("hardiness_zone_min", "hardiness_zone_max"), sep = "-") %>% 
  rename(sun_requirements = `Sun Requirements`) %>% 
  # drop an NA
  drop_na(soil_ph_min:sun_requirements) %>% 
  # make them all numeric
  mutate(across(soil_ph_min:max_size, ~as.numeric(str_extract(.x, "\\d+"))))


# Create UI
ui <- page_fluid(
  theme = bslib::bs_theme(
    bg = "#F0F8EA",
    # Light garden green
    fg = "#2C5F2D",
    # Dark green text
    primary = "#6A994E",
    # Fresh green accent
    secondary = "#A7C957",
    # Lighter green highlight
    success = "#BC4749",
    # Earthy red
    bootswatch = "darkly",
    base_font = font_google("Space Mono"),
    code_font = font_google("Space Mono")
  ),
  page_sidebar(
    title = "Gardener's Plant Finder",
    
    sidebar = sidebar(
      title = "Filter Plants",
      
      # Hardiness zone filter
      sliderInput(
        "zone",
        "Hardiness Zone:",
        min = min(plants$hardiness_zone_min),
        max = max(plants$hardiness_zone_max),
        value = c(
          min(plants$hardiness_zone_min),
          max(plants$hardiness_zone_max)
        )
      ),
      
      # Soil pH filter
      sliderInput(
        "ph",
        "Soil pH:",
        min = min(plants$soil_ph_min),
        max = max(plants$soil_ph_max),
        value = c(min(plants$soil_ph_min), max(plants$soil_ph_max)),
        step = 0.1
      ),
      
      # Sun requirements
      checkboxGroupInput(
        "sun",
        "Sun Requirements:",
        choices = unique(plants$sun_requirements),
        selected = unique(plants$sun_requirements)
      ),
      
      # Plant size filters
      sliderInput(
        "size",
        "Plant Size (inches):",
        min = min(plants$min_size),
        max = max(plants$max_size),
        value = c(min(plants$min_size), max(plants$max_size))
      ),
      
      # Reset filters button
      actionButton("reset", "Reset Filters", class = "btn-secondary")
    ),
    
    navset_tab(
      nav_panel(
        title = "Plants Table",
        icon = shiny::icon("table"),
        DTOutput("plants_table")
      ),
      nav_panel(
        title = "Plant Visualizations",
        icon = shiny::icon("chart-bar"),
        
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          card(
            full_screen = TRUE,
            card_header("Plant Sizes"),
            plotOutput("size_plot", height = "500px")
          ),
          
          card(
            full_screen = TRUE,
            card_header("Sun Requirements"),
            plotOutput("sun_plot", height = "500px")
          ),
          
          card(
            full_screen = TRUE,
            card_header("Hardiness Zones"),
            plotOutput("zone_plot", height = "500px")
          ),
          
          card(
            full_screen = TRUE,
            card_header("Soil pH Ranges"),
            plotOutput("ph_plot", height = "500px")
          )
        )
      ),
      nav_panel(
        title = "Plant Finder Guide",
        icon = shiny::icon("info-circle"),
        
        card(
          card_header("How to Use This App"),
          p(
            "This Gardener's Plant Finder app helps you select plants based on your garden's conditions."
          ),
          
          h4("Understanding the Filters:"),
          tags$ul(
            tags$li(
              strong("Hardiness Zone:"),
              " The USDA hardiness zone range where the plant can thrive."
            ),
            tags$li(
              strong("Soil pH:"),
              " The acidity/alkalinity range the plant prefers."
            ),
            tags$li(
              strong("Sun Requirements:"),
              " The amount of sunlight the plant needs."
            ),
            tags$li(
              strong("Plant Size:"),
              " The potential height range of the plant in inches."
            )
          ),
          
          h4("Using the Visualizations:"),
          p(
            "Use the visualization tab to compare different characteristics across plants."
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive filtered data
  filtered_plants <- reactive({
    plants_filtered <- plants
    
    # Apply filters
    plants_filtered <- plants_filtered %>%
      filter(
        # Hardiness zone filter - show plants that can grow in the selected zone range
        hardiness_zone_min <= input$zone[2] &
          hardiness_zone_max >= input$zone[1],
        
        # Soil pH filter - show plants that can tolerate the selected pH range
        soil_ph_min <= input$ph[2] & soil_ph_max >= input$ph[1],
        
        # Size filter
        min_size <= input$size[2] & max_size >= input$size[1],
        
        # Sun requirements filter
        sun_requirements %in% input$sun
      )
    
    return(plants_filtered)
  })
  
  # Reset button
  observeEvent(input$reset, {
    updateSliderInput(session, "zone", value = c(
      min(plants$hardiness_zone_min),
      max(plants$hardiness_zone_max)
    ))
    updateSliderInput(session, "ph", value = c(min(plants$soil_ph_min), max(plants$soil_ph_max)))
    updateSliderInput(session, "size", value = c(min(plants$min_size), max(plants$max_size)))
    updateSelectInput(session, "sun", selected = unique(plants$sun_requirements))
  })
  
  # Render table
  output$plants_table <- renderDT({
    datatable(
      filtered_plants(),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  # Plant Size Plot
  output$size_plot <- renderPlot({
    req(nrow(filtered_plants()) > 0)
    
    ggplot(filtered_plants(),
           aes(
             x = reorder(`Common Name`, max_size),
             ymin = min_size,
             ymax = max_size
           )) +
      geom_linerange(color = "gray20", size = 1) +  # Min-max range lines
      geom_point(aes(y = min_size),
                 color = "#A7C957",
                 size = 3) +  # Min size points
      geom_point(aes(y = max_size),
                 color = "forestgreen",
                 size = 3) +  # Max size points
      labs(title = "Min/Max Size of Plant Species", x = "Plant Species", y = "Size (cm)") +
      coord_flip() +  # Flip x and y axes
      theme_minimal()
    
  })
  
  # Sun Requirements Plot
  output$sun_plot <- renderPlot({
    req(nrow(filtered_plants()) > 0)
    
    sun_counts <- filtered_plants() %>%
      count(sun_requirements)
    
    ggplot(sun_counts, aes(x = "", y = n, fill = sun_requirements)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Distribution of Sun Requirements", fill = "Sun Requirement") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })
  
  # Hardiness Zone Plot
  output$zone_plot <- renderPlot({
    req(nrow(filtered_plants()) > 0)
    
    ggplot(filtered_plants(), aes(x = reorder(`Common Name`, hardiness_zone_max))) +
      geom_segment(
        aes(xend = `Common Name`, y = hardiness_zone_min, yend = hardiness_zone_max),
        size = 1.5,
        color = "gray20"
      ) +
      geom_point(aes(y = hardiness_zone_min),
                 color = "#A7C957",
                 size = 3) +
      geom_point(aes(y = hardiness_zone_max),
                 color = "darkred",
                 size = 3) +
      coord_flip() +
      labs(x = "", y = "Hardiness Zone", title = "Hardiness Zone Ranges") +
      theme_minimal()
  })
  
  # pH plot
  output$ph_plot <- renderPlot({
    req(nrow(filtered_plants()) > 0)
    
    ggplot(filtered_plants(),
           aes(
             x = reorder(`Common Name`, soil_ph_max),
             ymin = soil_ph_min,
             ymax = soil_ph_max
           )) +
      geom_linerange(color = "gray20", size = 1) +  # Min-max range lines
      geom_point(aes(y = soil_ph_min),
                 color = "#A7C957",
                 size = 3) +  # Min size points
      geom_point(aes(y = soil_ph_max),
                 color = "forestgreen",
                 size = 3) +  # Max size points
      labs(title = "Min/Max Soil pH", x = "Plant Species", y = "pH") +
      coord_flip() +  # Flip x and y axes
      theme_minimal()
    
  })
  
  
}


shinyApp(ui, server)