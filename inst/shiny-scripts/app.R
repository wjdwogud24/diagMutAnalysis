# This example is adapted from
# Grolemund, G. (2015). Learn Shiny - Video Tutorials. URL:https://shiny.rstudio.com/tutorial/
#
# Jung, J. (2022) diagMutAnalysis: R package that analyzes somatic mutation data for BCB410H: Applied Bioinformatics. Unpublished. URL https://github.com/wjdwogud24/diagMutAnalysis.

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

      tags$p("This is a simple Shiny App that is part of the diagMutAnalysis in R."),
      # br() element to introduce extra vertical spacing ----
      br(),

      tags$b("Description: diagMutAnalysis is an R package that takes data
      formats curated same as the ICGC DCC database open somatic .tsv files.
      The shiny App is part of the diagMutAnalysis package. The package provides
      a function on analyzing somatic mutation sample donors by producing values
      such as the proportion and frequency of all gene mutations that has had an
      effect. Another function creates plots not existing in the ICGC database
      as a different perspective to the data. Provides a pie chart of mutation
      types in sample, and a bar plot of substitution mutation types in sample.
      See ?mutationTypePlot and ?mutationPercentage for more details.
      To see exact format of data check browseVignettes(\"diagMutAnalysis\")
             "),

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
                label = "Select an icgc dataset to visualize. File should be in .tsv format",
                accept = c(".csv")),
      textInput(inputId = "population",
                label = "Enter population size to view for dataset.
                Must enter a value.
                This should be a positive numeric value:","Whole data set"),
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
                  selected = "A"),

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
                           h4("Data is provided to only 2 decimal places"),
                           h4("For a more detailed analysis use the function"),
                           h4("?mutationPercentage"),
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

    diagMutAnalysis::mutationTypePlot(mutationData = matrixInput(),
                                      chromosome = input$chromosome,
                                      population = as.numeric(input$population))
  })

  # Create percentage table
  start_table <- eventReactive(eventExpr = input$button, {

    diagMutAnalysis::mutationPercentage(mutationData = matrixInput())
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

  # Plotting percentage table
  output$percentage_table <- renderTable({
    if (! is.null(start_table))
      start_table()
  })


  # URLs for downloading data
  url1 <- a("Example Dataset", href="https://github.com/wjdwogud24/diagMutAnalysis/blob/a79cc79d4d181cf2d5456bb43bb78286ca647ca2/inst/extdata/simple_somatic_mutation.open.AML-US.tsv")
  output$tab <- renderUI({
    tagList("Download:", url1)
  })

  observeEvent(input$data, {
    # Show a modal when the button is pressed
    shinyalert(title = "Example Dataset",
               text = "Data of AML United States cancer patients updated from the ICGC DATA PORTAL.
               File name is simple_somatic_mutation.open.AML-US.tsv.gz extracted from portal.
               Citation: Zhang J, Bajari R, Andric D, et al. The International Cancer Genome Consortium Data Portal.
               Nat Biotechnol. 2019;37(4):367‐369. doi:10.1038/s41587-019-0055-9",
               type = "info")
  })

}

# Create Shiny app ----
shiny::shinyApp(ui, server)


# [END]
