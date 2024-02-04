library(shiny)
library(ggplot2)

calculate_dotsize <- function(num_samples) {
  # Set a base size and a scaling factor
  base_size <- 0.3
  scaling_factor <- 0.5 / 400  # Assuming 100 is the maximum number of samples
  
  # Calculate dotsize
  dotsize <- base_size - (num_samples * scaling_factor)
  
  # Ensure dotsize does not go below a minimum threshold (e.g., 0.1)
  return(max(dotsize, 0.1))
}


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Alien Statistics Simulations"),
  
  # Add custom CSS to allow text wrapping for verbatimTextOutput
  tags$head(
    tags$style(HTML("
      #ciText {
        white-space: normal !important;
      }
    ")),
    tags$style(HTML("
      #ciTextNormal {
        white-space: normal !important;
      }
    "))
  ),
  
  # Tabs for different distributions
  tabsetPanel(
    tabPanel("Normal Distribution: Alien Heights",
             sidebarLayout(
               sidebarPanel(
                 tags$p("In this simulation, we are generating confidence intervals for the height of aliens. The data is assumed to follow a Normal distribution."),
                 selectInput("confidence_level_normal", 
                             "Confidence Level (%):", 
                             choices = c("90%", "95%", "99%"), 
                             selected = "95%"),
                 numericInput("num_samples_normal", 
                              "Number of samples:", 
                              value = 100, 
                              min = 1, 
                              max = 200),
                 actionButton("goButtonNormal", "Go"),
                 checkboxInput("colorOutNormal", "Color CIs not containing the true mean", FALSE),
                 checkboxInput("greyOutNormal", "Grey out CIs containing the true mean", FALSE),
                 verbatimTextOutput("ciTextNormal")
               ),
               mainPanel(
                 plotOutput("ciPlotNormal", height = "535px"),
                 plotOutput("meanDistributionPlotNormal", height = "200px")
               )
             )
    ),
  
  # Sidebar with controls, CI count, and Animate button
  tabPanel("Poisson Distribution: Alien Eyes", 
      sidebarLayout(
      sidebarPanel(
      tags$p("In this simulation, we are generating confidence intervals for the number of eyes in a population of aliens. The data is assumed to follow a Poisson distribution."),
      selectInput("confidence_level", 
                  "Confidence Level (%):", 
                  choices = c("90%", "95%", "99%"), 
                  selected = "95%"),
      numericInput("num_samples", 
                   "Number of samples:", 
                   value = 100, 
                   min = 1, 
                   max = 200),
      actionButton("goButton", "Go"),
      checkboxInput("colorOut", "Color CIs not containing the true mean", FALSE),
      checkboxInput("greyOut", "Grey out CIs containing the true mean", FALSE),
      verbatimTextOutput("ciText")
    ),
    
    # Main panel for displaying the plots
    mainPanel(
      plotOutput("ciPlot", height = "535px"),
      plotOutput("meanDistributionPlot", height = "200px")
    )
  )
)))

# Define server logic 
server <- function(input, output) {

  true_mean <- 8  # For Poisson distribution
  true_mean_height <- 160  # For Normal distribution
  
  ci_data <- reactive({
    # Generate random samples
    num_samples <- input$num_samples
    sample_size <- 30
    samples <- matrix(rpois(num_samples * sample_size, true_mean), ncol = sample_size)
    
    # Calculate sample means and standard deviations
    sample_means <- rowMeans(samples)
    sample_sds <- apply(samples, 1, sd)
    
    # Calculate confidence intervals
    # Adjust the z-score calculation based on the specified confidence level
    alpha <- (100 - as.numeric(sub("%", "", input$confidence_level))) / 100
    z_score <- qnorm(1 - alpha/2)
    ci_bounds <- matrix(ncol = 2, nrow = num_samples)
    ci_bounds[, 1] <- sample_means - z_score * (sample_sds / sqrt(sample_size))
    ci_bounds[, 2] <- sample_means + z_score * (sample_sds / sqrt(sample_size))
    
    # Create a data frame for plotting
    data.frame(
      sample = factor(1:num_samples),
      lower = ci_bounds[, 1],
      upper = ci_bounds[, 2],
      mean = rowMeans(ci_bounds),
      contains_true_mean = (ci_bounds[, 1] <= true_mean & ci_bounds[, 2] >= true_mean)
    )
  })
  
  observeEvent(input$goButton, {
    
    output$ciPlot <- renderPlot({
      # Access the reactive ci_data
      data <- ci_data()
      
      # Define colors based on the checkbox inputs
      colors <- if (input$greyOut && !input$colorOut) {
        c("TRUE" = "grey", "FALSE" = "blue")
      } else if (!input$greyOut && input$colorOut) {
        c("TRUE" = "blue", "FALSE" = "red")
      } else if (input$greyOut && input$colorOut) {
        c("TRUE" = "grey", "FALSE" = "red")
      } else {
        c("TRUE" = "blue", "FALSE" = "blue")
      }
      
      true_mean <- 8  # Corrected true mean for the Poisson distribution
      
      ggplot(data, aes(y = sample)) +
        geom_errorbarh(aes(xmin = lower, xmax = upper, color = contains_true_mean), height = 0.2) +
        geom_point(aes(x = mean, color = contains_true_mean), size = 2) +
        geom_vline(xintercept = true_mean, color = "red", linetype = "dashed") +
        scale_color_manual(values = colors) +
        scale_x_continuous(limits = c(4, 12)) +
        labs(title = "Confidence Intervals for Alien Eye Counts",
             x = "Number of Eyes") +
        theme_minimal() +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(color = "black")
        )
    })
    
    output$meanDistributionPlot <- renderPlot({
      # Access the reactive ci_data
      data <- ci_data()
      
      true_mean <- 8  # Corrected true mean for the Poisson distribution
      
      # Set the x-axis limits based on the range of the confidence intervals
      x_limits <- range(c(data$lower, data$upper))
      
      # Define colors based on the checkbox inputs
      colors <- if (input$greyOut && !input$colorOut) {
        c("TRUE" = "grey", "FALSE" = "blue")
      } else if (!input$greyOut && input$colorOut) {
        c("TRUE" = "blue", "FALSE" = "red")
      } else if (input$greyOut && input$colorOut) {
        c("TRUE" = "grey", "FALSE" = "red")
      } else {
        c("TRUE" = "blue", "FALSE" = "blue")
      }
      
      # Calculate dynamic dotsize
      dot_size <- calculate_dotsize(input$num_samples)
      
      ggplot(data, aes(x = mean, fill = contains_true_mean)) +
        geom_dotplot(dotsize = calculate_dotsize(input$num_samples), aes(fill = contains_true_mean)) + 
        geom_vline(xintercept = true_mean, color = "red", linetype = "dashed") +
        scale_fill_manual(values = colors) +
        labs(title = "Sample Mean Distribution for Alien Eye Counts",
             x = "Sample Mean Number of Eyes") +
        theme_minimal() +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(color = "black")
        ) +
        scale_x_continuous(limits = c(4, 12))  # Set the x-axis limits to match the ciPlot
    })
    
    # Display the number of confidence intervals not containing the true mean
    output$ciText <- renderText({
      data <- ci_data()
      not_containing <- sum(!data$contains_true_mean)
      percent <- (not_containing / input$num_samples) * 100
      paste0("The number of confidence intervals that do not contain the true mean is: ", not_containing, ", (", round(percent, 2), "%)")
    })
  })
  
  ci_data_normal <- reactive({
    # Generate random samples
    true_sd_height <- 20     # Assume a standard deviation of 20 cm
    num_samples <- input$num_samples_normal
    sample_size <- 30
    samples <- matrix(rnorm(num_samples * sample_size, true_mean_height, true_sd_height), ncol = sample_size)
    
    # Calculate sample means and standard deviations
    sample_means <- rowMeans(samples)
    sample_sds <- apply(samples, 1, sd)
    
    # Calculate confidence intervals
    alpha <- (100 - as.numeric(sub("%", "", input$confidence_level_normal))) / 100
    z_score <- qnorm(1 - alpha/2)
    ci_bounds <- matrix(ncol = 2, nrow = num_samples)
    ci_bounds[, 1] <- sample_means - z_score * (sample_sds / sqrt(sample_size))
    ci_bounds[, 2] <- sample_means + z_score * (sample_sds / sqrt(sample_size))
    
    # Create a data frame for plotting
    data.frame(
      sample = factor(1:num_samples),
      lower = ci_bounds[, 1],
      upper = ci_bounds[, 2],
      mean = rowMeans(ci_bounds),
      contains_true_mean = (ci_bounds[, 1] <= true_mean_height & ci_bounds[, 2] >= true_mean_height)
    )
  })
  
  observeEvent(input$goButtonNormal, {
    
    output$ciPlotNormal <- renderPlot({
      # Access the reactive ci_data_normal
      data <- ci_data_normal()
      
      # Define colors based on the checkbox inputs
      colors <- if (input$greyOutNormal && !input$colorOutNormal) {
        c("TRUE" = "grey", "FALSE" = "blue")
      } else if (!input$greyOutNormal && input$colorOutNormal) {
        c("TRUE" = "blue", "FALSE" = "red")
      } else if (input$greyOutNormal && input$colorOutNormal) {
        c("TRUE" = "grey", "FALSE" = "red")
      } else {
        c("TRUE" = "blue", "FALSE" = "blue")
      }
      
      ggplot(data, aes(y = sample)) +
        geom_errorbarh(aes(xmin = lower, xmax = upper, color = contains_true_mean), height = 0.2) +
        geom_point(aes(x = mean, color = contains_true_mean), size = 2) +
        geom_vline(xintercept = true_mean_height, color = "red", linetype = "dashed") +
        scale_color_manual(values = colors) +
        labs(title = "Confidence Intervals for Alien Heights",
             x = "Height (cm)") +
        theme_minimal() +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(color = "black")
        )
    })
    
    output$meanDistributionPlotNormal <- renderPlot({
      # Access the reactive ci_data_normal
      data <- ci_data_normal()
      
      # Define colors based on the checkbox inputs
      colors <- if (input$greyOutNormal && !input$colorOutNormal) {
        c("TRUE" = "grey", "FALSE" = "blue")
      } else if (!input$greyOutNormal && input$colorOutNormal) {
        c("TRUE" = "blue", "FALSE" = "red")
      } else if (input$greyOutNormal && input$colorOutNormal) {
        c("TRUE" = "grey", "FALSE" = "red")
      } else {
        c("TRUE" = "blue", "FALSE" = "blue")
      }
      
      ggplot(data, aes(x = mean, fill = contains_true_mean)) +
        geom_dotplot(dotsize = calculate_dotsize(input$num_samples_normal), aes(fill = contains_true_mean)) + 
        geom_vline(xintercept = true_mean_height, color = "red", linetype = "dashed") +
        scale_fill_manual(values = colors) +
        labs(title = "Sample Mean Distribution for Alien Heights",
             x = "Sample Mean Height (cm)") +
        theme_minimal() +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(color = "black")
        )
    })
    
    # Display the number of confidence intervals not containing the true mean
    output$ciTextNormal <- renderText({
      data <- ci_data_normal()
      not_containing <- sum(!data$contains_true_mean)
      percent <- (not_containing / input$num_samples_normal) * 100
      paste0("The number of confidence intervals that do not contain the true mean height is: ", not_containing, ", (", round(percent, 2), "%)")
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
