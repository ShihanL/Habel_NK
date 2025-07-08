# Pregnancy Immunology Dashboard - Shiny Application
# Interactive analysis of flow cytometry data
# Kedzierska Lab - University of Melbourne

# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(readr)

# Data processing functions
load_flow_cytometry_data <- function(data_path = "data/250707_data.csv") {
  tryCatch({
    df <- read_csv(data_path, show_col_types = FALSE)
    return(df)
  }, error = function(e) {
    cat("Error: Data file not found at", data_path, "\n")
    return(NULL)
  })
}

get_gene_columns <- function(df) {
  metadata_cols <- c("Donor", "Group", "Cell")
  gene_cols <- setdiff(names(df), metadata_cols)
  return(sort(gene_cols))
}


get_cell_types <- function(df) {
  return(sort(unique(df$Cell)))
}

filter_data_by_cell_types <- function(df, selected_cell_types) {
  if (length(selected_cell_types) == 0) {
    return(df[0, ])  # Return empty dataframe
  }
  return(df[df$Cell %in% selected_cell_types, ])
}

# Load data
df <- load_flow_cytometry_data()

if (is.null(df)) {
  # Create error UI if data loading fails
  ui <- fluidPage(
    titlePanel("Error Loading Data"),
    mainPanel(
      div(class = "alert alert-danger",
          h4("Data Loading Error"),
          p("Could not load data file. Please check if data/250707_data.csv exists.")
      )
    )
  )
  
  server <- function(input, output) {}
  
} else {
  # Get data options
  gene_options <- get_gene_columns(df)
  cell_types <- get_cell_types(df)
  default_gene <- gene_options[1]
  
  # Define UI
  ui <- dashboardPage(
    
    # Header
    dashboardHeader(title = "Pregnancy Immunology Dashboard"),
    
    # Sidebar
    dashboardSidebar(
      width = 250,
      
      # Gene selection
      div(style = "padding: 15px;",
          h4("Filters"),
          
          # Gene dropdown
          div(
            h5("Select Gene/Marker:"),
            selectInput("gene_dropdown", 
                       label = NULL,
                       choices = setNames(gene_options, gene_options),
                       selected = default_gene,
                       width = "100%"),
            p(paste("Choose from", length(gene_options), "available genes/markers"), 
              style = "font-size: 12px; color: #666; margin-top: 5px;")
          ),
          
          hr(),
          
          # Cell type selection
          div(
            h5("Select Cell Types:"),
            checkboxGroupInput("cell_type_checklist",
                              label = NULL,
                              choices = setNames(cell_types, cell_types),
                              selected = cell_types,
                              width = "100%"),
            p(paste("Filter by", length(cell_types), "available cell types"), 
              style = "font-size: 12px; color: #666; margin-top: 5px;")
          ),
          
          hr(),
          
          # Scatter plot gene selections
          div(
            h5("Scatter Plot Genes:"),
            div(
              h6("X-axis Gene:"),
              selectInput("scatter_x_gene", 
                         label = NULL,
                         choices = setNames(gene_options, gene_options),
                         selected = gene_options[1],
                         width = "100%"),
              style = "margin-bottom: 10px;"
            ),
            div(
              h6("Y-axis Gene:"),
              selectInput("scatter_y_gene", 
                         label = NULL,
                         choices = setNames(gene_options, gene_options),
                         selected = gene_options[2],
                         width = "100%")
            ),
            p("Select different genes for X and Y axes", 
              style = "font-size: 12px; color: #666; margin-top: 5px;")
          )
      )
    ),
    
    # Main body
    dashboardBody(
      # Custom CSS
      tags$head(
        tags$style(HTML("
          .main-header .navbar {
            background-color: #667eea !important;
          }
          .main-header .logo {
            background-color: #667eea !important;
          }
          .skin-blue .main-sidebar {
            background-color: #f8f9fa;
          }
          .content-wrapper {
            background-color: #ffffff;
          }
          .box.box-primary {
            border-top-color: #667eea;
          }
          .box-header.with-border {
            border-bottom: 1px solid #667eea;
          }
          /* Make all text black */
          body, .sidebar-menu, .sidebar-menu li a, .content-wrapper, 
          .box-title, .form-control, .checkbox, label, p, h1, h2, h3, h4, h5, h6 {
            color: #000000 !important;
          }
          .sidebar-menu li a:hover {
            color: #000000 !important;
          }
          /* Full width layout */
          .content-wrapper, .right-side {
            margin-left: 250px !important;
          }
          .main-sidebar {
            width: 250px !important;
          }
          /* Ensure plots use full available width */
          .box {
            margin-bottom: 20px;
          }
          /* Responsive adjustments */
          @media (max-width: 768px) {
            .content-wrapper, .right-side {
              margin-left: 0 !important;
            }
            .main-sidebar {
              width: 100% !important;
            }
          }
        "))
      ),
      
      # Main content
      fluidRow(
        # Boxplot - left side
        box(
          title = "Gene Expression by Groups", 
          status = "primary", 
          solidHeader = TRUE,
          width = 6,
          
          plotlyOutput("gene_boxplot", height = "600px")
        ),
        
        # Scatter plot - right side
        box(
          title = "Gene Expression Correlation", 
          status = "primary", 
          solidHeader = TRUE,
          width = 6,
          
          plotlyOutput("gene_scatterplot", height = "600px")
        )
      ),
      
      # Footer
      fluidRow(
        column(12,
               hr(),
               div(style = "text-align: center; color: #666; padding: 20px;",
                   p("Kedzierska Lab - Pregnancy Immunology Study"),
                   p(paste("Dataset:", nrow(df), "samples ×", length(gene_options), "markers"))
               )
        )
      )
    )
  )
  
  # Define server logic
  server <- function(input, output, session) {
    
    # Reactive data filtering
    filtered_data <- reactive({
      req(input$cell_type_checklist)
      filter_data_by_cell_types(df, input$cell_type_checklist)
    })
    
    # Reactive boxplot
    output$gene_boxplot <- renderPlotly({
      req(input$gene_dropdown)
      
      data_to_plot <- filtered_data()
      selected_gene <- input$gene_dropdown
      
      if (nrow(data_to_plot) == 0) {
        # Empty plot if no data
        p <- plot_ly() %>%
          add_annotations(
            text = "No data available for selected cell types",
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 16, color = "black")
          ) %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
        
        return(p)
      }
      
      # Get unique cell types in the filtered data
      cell_types_in_data <- unique(data_to_plot$Cell)
      n_cell_types <- length(cell_types_in_data)
      
      if (n_cell_types == 1) {
        # Single cell type - create regular boxplot
        p <- plot_ly(data_to_plot, 
                     x = ~Group, 
                     y = ~get(selected_gene),
                     type = "box",
                     name = cell_types_in_data[1],
                     marker = list(color = "#1f77b4")) %>%
          layout(
            title = list(
              text = paste("Expression of", selected_gene, "in", cell_types_in_data[1]),
              x = 0.5,
              font = list(size = 16, color = "black")
            ),
            xaxis = list(
              title = "Donor Group",
              titlefont = list(size = 14, color = "black"),
              tickfont = list(color = "black")
            ),
            yaxis = list(
              title = "Expression Level (%)",
              titlefont = list(size = 14, color = "black"),
              tickfont = list(color = "black")
            ),
            showlegend = FALSE,
            height = 600,
            margin = list(t = 80, b = 80, l = 80, r = 40),
            plot_bgcolor = "white",
            paper_bgcolor = "white"
          )
      } else {
        # Multiple cell types - create subplots
        plot_list <- list()
        colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
        
        for (i in 1:n_cell_types) {
          cell_type <- cell_types_in_data[i]
          cell_data <- data_to_plot[data_to_plot$Cell == cell_type, ]
          
          plot_list[[i]] <- plot_ly(cell_data,
                                   x = ~Group,
                                   y = ~get(selected_gene),
                                   type = "box",
                                   name = cell_type,
                                   marker = list(color = colors[i %% length(colors) + 1])) %>%
            layout(
              title = list(text = cell_type, font = list(size = 14, color = "black")),
              xaxis = list(
                title = if(i == n_cell_types) "Donor Group" else "",
                titlefont = list(size = 12, color = "black"),
                tickfont = list(color = "black")
              ),
              yaxis = list(
                title = "Expression Level (%)",
                titlefont = list(size = 12, color = "black"),
                tickfont = list(color = "black")
              ),
              showlegend = FALSE,
              plot_bgcolor = "white",
              paper_bgcolor = "white"
            )
        }
        
        # Combine plots into subplots
        plot_height <- max(600, 100 * n_cell_types)
        p <- subplot(plot_list, nrows = n_cell_types, shareY = TRUE, titleY = TRUE) %>%
          layout(
            title = list(
              text = paste("Expression of", selected_gene, "by Cell Type"),
              x = 0.5,
              font = list(size = 14, color = "black")
            ),
            height = plot_height,
            margin = list(t = 60, b = 60, l = 60, r = 20),
            plot_bgcolor = "white",
            paper_bgcolor = "white"
          )
      }
      
      return(p)
    })
    
    # Reactive scatter plot
    output$gene_scatterplot <- renderPlotly({
      req(input$scatter_x_gene, input$scatter_y_gene)
      
      data_to_plot <- filtered_data()
      x_gene <- input$scatter_x_gene
      y_gene <- input$scatter_y_gene
      
      if (nrow(data_to_plot) == 0) {
        # Empty plot if no data
        p <- plot_ly() %>%
          add_annotations(
            text = "No data available for selected cell types",
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 16, color = "black")
          ) %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
        
        return(p)
      }
      
      # Create scatter plot
      p <- plot_ly(data_to_plot,
                   x = ~get(x_gene),
                   y = ~get(y_gene),
                   color = ~Cell,
                   symbol = ~Group,
                   type = "scatter",
                   mode = "markers",
                   colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"),
                   symbols = c("circle", "square", "diamond", "cross"),
                   marker = list(size = 8, opacity = 0.7),
                   text = ~paste("Donor:", Donor, 
                                "<br>Cell:", Cell, 
                                "<br>Group:", Group,
                                "<br>", x_gene, ":", get(x_gene), "%",
                                "<br>", y_gene, ":", get(y_gene), "%"),
                   hovertemplate = "%{text}<extra></extra>") %>%
        layout(
          title = list(
            text = paste(y_gene, "vs", x_gene),
            x = 0.5,
            font = list(size = 16, color = "black")
          ),
          xaxis = list(
            title = paste(x_gene, "Expression (%)"),
            titlefont = list(size = 14, color = "black"),
            tickfont = list(color = "black"),
            gridcolor = "#E5E5E5"
          ),
          yaxis = list(
            title = paste(y_gene, "Expression (%)"),
            titlefont = list(size = 14, color = "black"),
            tickfont = list(color = "black"),
            gridcolor = "#E5E5E5"
          ),
          legend = list(
            title = list(text = "Cell Type"),
            font = list(color = "black")
          ),
          height = 600,
          margin = list(t = 80, b = 80, l = 80, r = 80),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
      
      return(p)
    })
  }
}

# Run the application
shinyApp(ui = ui, server = server)