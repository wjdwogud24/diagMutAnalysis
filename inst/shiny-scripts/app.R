# This example is adapted from
# Grolemund, G. (2015). Learn Shiny - Video Tutorials. URL:https://shiny.rstudio.com/tutorial/

library(shiny)
library(shinyalert)

# Define UI
ui <- fluidPage(

  # Change title
  titlePanel("diagMutAnalysis Shiny implementation"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      tags$p("This is a simple Shiny App that is part of the TestingPackage in R.
             Its purpose is to illustrate the functionality of a simple
             Shiny App."),
      # br() element to introduce extra vertical spacing ----
      br(),

      tags$b("Description: TestingPackage is an R package to demonstrate components
             of a simple R package. This Shiny App is part of the TestingPackage. It
             permits to calculate Bayesian information criterion (BIC), Integrated
             Complete Likelihood (ICL) criterion, and Akaike Information Criterion (AIC)
             values, given log-likelihood, number of clusters, dimension of dataset,
             number of observations, and the probability. Provided the original
             RNAseq dataset of counts, the dataset could be visualized. For more
             details, see ?InfCriteriaCalculation."),

      # br() element to introduce extra vertical spacing ----
      br(),
      br(),

      # input
      tags$p("Instructions: Below, enter or select values required to perform the analysis. Default
             values are shown. Then press 'Run'. Navigate through
             the different tabs to the right to explore the results."),

      # br() element to introduce extra vertical spacing ----
      br(),

      # input
      shinyalert::useShinyalert(force = TRUE),  # Set up shinyalert
      uiOutput("tab"),
      actionButton(inputId = "data",
                   label = "Dataset 1 Details"),
      fileInput(inputId = "file1",
                label = "Select an icgc dataset to visualize. File should be in .csv format",
                accept = c(".csv")),
      textInput(inputId = "population",
                label = "Enter population size to view for dataset.
                This should be a positive numeric value:", "1000"),
      selectInput(inputId = "chromosome",
                  label = "Want to view data for specific chromosome",
                  choices = list("All chromosomes" = 'A',
                                 "1" = '1',"2" = '2',"3" = '3',"4" = '4',
                                 "5" = '5', "6" = '6', "7" = '7',
                                 "8" = '8',"9" = '9',"10" = '10',
                                 "11" = '11', "12" = '12', "13" = '13',
                                 "14" = '14', "15" = '15',
                                 "16" = '16', "17" = '17'
                                 ,"18" = '18', "19" = '19', "20" = '20',
                                 "21" = '21', "22" = '22', "X" = 'X'),
                  selected = 1),
      #selectInput(inputId = "plot_type",
       #           lable = "Choose mutation or substitution type plot",
        #          choices = list("Mutation Type Plot" = "mut_plot",
         #                        "Substitution Type Plot = sub_plot")),

      # br() element to introduce extra vertical spacing ----
      br(),

      # actionButton
      actionButton(inputId = "button",
                   label = "Run"),

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, percentage
      tabsetPanel(type = "tabs",
                  tabPanel("Plot of Mutation Type",
                           h3("Mutation Plot"),
                           br(),
                           plotOutput("mut_plot"),
                           plotOutput("sub_plot")),
                  tabPanel("Mutation percentage",
                           h3("Mutation percentage table"),
                           br(),
                           tableOutput("percentage_table"))
      )

    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {

  # Save input csv as a reactive
  matrixInput <- eventReactive(eventExpr = input$button, {
    if (! is.null(input$file1))
      as.data.frame(read.csv(input$file1$datapath, sep = "\t"))
  })

  # Calculate Plot
  start_plot <- eventReactive(eventExpr = input$button, {

    diagMutAnalysis::mutationTypePlot(matrixInput(),
                                      chromosome = input$chromosome,
                                      population = as.numeric(input$population))
  })

  # Create percentage table
  start_table <- eventReactive(eventExpr = input$button, {

    diagMutAnalysis::mutationPercentage(matrixInput())
  })


  # Plotting mutation type plot
  output$mut_plot <- renderPlot({
    if (! is.null(start_plot))
      start_plot()$mutation_types
  })

  # Plotting substitution type plot
  output$sub_plot <- renderPlot({
    if (! is.null(start_plot))
      start_plot()$substitution_types
  })

  # Plotting RNAseq dataset
  output$percentage_table <- renderTable({
    if (! is.null(start_table))
      start_table()
  })


  # URLs for downloading data
  url1 <- a("Example Dataset", href="https://github.com/wjdwogud24/diagMutAnalysis/blob/a6b07d2cb2940e300ce6e08acc8d20fc692f8694/data/icgc_data.rda")
  output$tab <- renderUI({
    tagList("Download:", url1)
  })

  observeEvent(input$data, {
    # Show a modal when the button is pressed
    shinyalert(title = "Example Dataset",
               text = "Data of AML United States cancer patients updated from the ICGC DATA PORTAL.
               File name is simple_somatic_mutation.open.AML-US.tsv.gz extracted from portal.",
               type = "info")
  })

}

# Create Shiny app ----
shiny::shinyApp(ui, server)


# [END]
