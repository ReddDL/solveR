# DE LEON, Richard Emmanuel D. 
# CMSC 150 - AB5L
# Final project: Polynomial Regression, Quadratic Spline Interpolation, Diet Solver (Simplex)

# Libraries used
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)

# Get files for calculators
source("src/polyRegres.R")
source("src/QSI.R")
source("src/simplex.R")

# Food choices for the diet solver
foodChoices = c("Frozen Broccoli","Carrots, Raw", "Celery, Raw", "Frozen Corn", "Lettuce, Iceberg, Raw", "Peppers, Sweet, Raw", "Potatoes, Baked", "Tofu", "Roasted Chicken", "Spaghetti W/ Sauce", "Tomato, Red, Ripe, Raw", "Apple, Raw, W/ Skin", "Banana", "Grapes", "Kiwifruit, Raw, Fresh", "Oranges", "Bagels", "Wheat Bread", "White Bread", "Oatmeal Cookies",  "Apple Pie", "Chocolate Chip Cookies", "Butter, Regular", "Cheddar Cheese", "3.3% Fat, Whole Milk", "2% Lowfat Milk", "Skim Milk", "Poached Eggs", "Scrambled Eggs", "Bologna, Turkey", "Frankfurter, Beef", "Ham, Sliced, Extralean", "Kielbasa, Prk", "Cap'N Crunch", "Cheerios", "Corn Flks, Kellogg'S", "Raisin Brn, Kellg'S", "Rice Krispies", "Special K", "Oatmeal", "Malt-OMeal, Choc", "Pizza W/ Pepperoni", "Taco", "Hamburger W/ Toppings", "Hotdog, Plain", "Couscous", "White Rice", "Macaroni, Ckd", "Peanut Butter", "Pork", "Sardines in Oil", "White Tuna in Water", "Popcorn, Air-Popped", "Potato Chips, Bbqflvr", "Pretzels", "Tortilla Chip", "Chicknoodl Soup", "Splt Pea&Hamsoup", "Vegetbeef Soup", "Neweng Clamchwd", "Tomato Soup", "New E Clamchwd, W/ Mlk", "Crm Mshrm Soup, W/ Mlk","Beanbacn Soup, W/ Watr")

# ================================================================================ UI COMPONENTS
# Header
header = dashboardHeader(title=" solveR")

# Sidebar
sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("house")),
    menuItem("Polynomial Regression", tabName = "Regres", icon = icon("chart-line")),
    menuItem("Quadratic Spline Interpolation", tabName = "QSI", icon = icon("table")),
    menuItem("Diet Solver", tabName = "dietSolver", icon = icon("bowl-food"))
  )
)

# Body of each tab
body = dashboardBody(
  useShinyjs(),
  tags$head(includeCSS("www/styles.css")),
# ================================================================================ HOME TAB
  tabItems(
    tabItem(tabName = "home",
      fluidRow(
            box(
              width = 12,
              solidHeader = TRUE,
              h2("Welcome!"),
              p("I am Richard Emmanuel de Leon, a BS Computer Science student at the University of the Philippines - Los Baños. This is my final project for CMSC 150 - Numerical and Symbolic computation. ")
            ),
      ),
      fluidRow(
            box(
              width = 6,
              solidHeader = TRUE,
              h2("What is this?"),
              p("This is a web application built using R and Shiny that contains generic solvers for Polynomial Regression and Quadratic Spline Interpolation. It also has a diet solver that gives the minimum cost to spend to meet nutrtional requirements given your diet.")
            ),
            box(
              width = 6,
              solidHeader = TRUE,
              h2("Resources"),
              tags$ol(
                tags$li(a(href="https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html", "Shiny Documentation")),
                tags$li(a(href="http://nmbooks.eng.usf.edu/ebooks/05inp_spline/inp_05_spline_300_quadratic_example.html", "Quadratic Spline Interpolation")),
                tags$li("Lab Handouts"),
                tags$li("StackOverflow"),
              )
            ),
      )
    ), #home tab item
# ================================================================================================= REGRESSION TAB
    tabItem(tabName = "Regres",
            fluidRow(
              valueBoxOutput("estimateResultRegres", width = 12),
            ),
            fluidRow(
              hidden(
                div(
                  id = "regresDataPoints",
                  box(
                    title = "Data Points",
                    height = 180,
                    width = 12,
                    solidHeader = TRUE,
                    color = "white",
                    DTOutput("regresCSVToTable"),
                  ) # box
                ) # div
              ) # hidden
            ), #fluidrow 1
            fluidRow(
              hidden(
                div(
                  id = "regresFunctionBox",
                  box(
                    title = "Function",
                    width = 12,
                    solidHeader = TRUE,
                    textOutput("orderResultRegres")
                  )
                ) # div
              ) # hidden
            ),
            fluidRow(
              box(
                title = "Input", 
                height = "400px",
                solidHeader = TRUE,
                div(
                  class = "regresInputBox",
                  fileInput("regresCSV", "Enter CSV file here:", accept = ".csv"),
                  numericInput("regresOrder", "Order", value = ""),
                  numericInput("regresEstimate", "Estimate", value = ""),
                  actionButton("solveRegres", label = "Solve", class = "primaryBtn"),
                ) # div
              ), # box input
              hidden(
                div(
                  id = "plotBox",
                  box(
                    title = "Plot",
                    height = "400px",
                    solidHeader = TRUE,
                    div(class = "regresPlotBox", plotOutput("regresPlot"))
                  ) # plot box
                )
              )  # hidden
            ), # fluid row 1
            
            fluidRow(
              hidden(
                div(
                  id = "regresEliminationBox",
                  box(
                    title = "Elimination", 
                    width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    sliderInput("regresEliminationSlider", "Select Iteration:", min = 1, max = 1, value = 1, step = 1),
                    div(
                    class = "scrollable",
                    column(12, align = "center", tableOutput("regresElimination"))
                    )
                  ), #elimination box
                ) # div 
              ) # hidden
            ) # fluid row 1
    ), # regres tab item
# =============================================================================== QSI TAB
    tabItem(tabName = "QSI",
            fluidRow(
              valueBoxOutput("qsiEstimateResult", width = 12)
            ),
            fluidRow(
              hidden(
                div(
                  id = "qsiDataPoints",
                  box(
                    title = "Data Points",
                    height = 180,
                    width = 12,
                    solidHeader = TRUE,
                    color = "white",
                    DTOutput("qsiCSVToTable"),
                  ) # box
                ) # div
              ) # hidden
            ), #fluidrow 1
            fluidRow(
              box(
                title = "Input", 
                height = 300,
                width = 6,
                solidHeader = TRUE,
                fileInput("qsiCSV", "Enter CSV file here: ", accept = ".csv"),
                numericInput("qsiEstimate", "Estimate: ", value = ""),
                actionButton("solveQSI", label = "Solve", class = "primaryBtn"),
                # actionButton("resetQSI", label = "Reset"),
                # background = 'light-blue'
              ), # input
              hidden(
                div(
                  id = "qsiResultsBox",
                  box(
                    title = "Results", 
                    height = 300,
                    width = 6,
                    solidHeader = TRUE,
                    div(class = "scrollable",
                    tableOutput("intervalsAndFunctions")),
                  ) # results
                ) # div
              ) # hidden
            ), # fluid row 1
            
            fluidRow(
              hidden(
                div(
                  id = "qsiEliminationBox",
                  box(
                    title = "Elimination", 
                    width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    sliderInput("qsiEliminationSlider", "Select Iteration:", min = 1, max = 1, value = 1, step = 1),
                    column(12, align = "center", tableOutput("qsiElimination"))
                  ), #elimination
                ) # div
              ) # hidden
            ) # fluid row 1
    ), # qsi tab item
# ============================================================== DIET SOLVER TAB
    tabItem(tabName = "dietSolver",
            fluidRow(
              box(
                class = "dietSolverTopBox",
                width = 12,
                height = "70px",
                solidHeader = TRUE,
                h2("Select your food choices: ")
              )
            ),
            fluidRow(
              box( width = 12, solidHeader = TRUE,
                
              fluidRow(
                div(
                  class = "foodChoicesCols",
                  checkboxGroupInput("inputDiet", label = NULL, choices = setNames(foodChoices, foodChoices))
                ), # div
              ), # first fluid row
              fluidRow(
                box(
                  width = 12, solidHeader = TRUE,
                  div(
                    class = "dietSolverBtn",
                    actionButton("resetDS", label = "Reset"),
                    actionButton("selectAllDS", label = "Select All"),
                    actionButton("solveDS", label = "Solve", class = "primaryBtn"),
                  )
                )
              ), #2nd fluid row
              ) # main box
            
            ), # fluidrow
            fluidRow(
              valueBoxOutput("optimalCostDS", width = 12)
            ), # optimal cost
            fluidRow(
              hidden(
                div(
                  id = "breakdownBox",
                  box(
                    title = "Cost breakdown",
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput("breakdownDS"),
                  ),
                ) # div
              ) # hidden
            ),
            fluidRow(
              hidden(
                div(
                  id = "finalTableuBox",
                  box( 
                    title = "Final Tableu",
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    class = "finalTableu",
                    tableOutput("finalTableuDS"),
                    tableOutput("finalBasicSolDS"),
                  )
                ) # div
              ) # hidden
            ),
            fluidRow(
              hidden(
                div(
                  id = "iterationSliderDSBox",
                  box(
                    width = 12,
                    sliderInput("matIterSliderDS", "Select Iteration:", min = 1, max = 1, value = 1, step = 1),
                  ),
                ) # div
              ) # hidden
            ),
            fluidRow(
              hidden(
                div(
                  id = "iterationDSBox",
                  box( 
                    title = "Simplex iterations",
                    width = 12,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    class = "simplexIterations",
                    tableOutput("iterationDS"),
                    tableOutput("basicSolDS")
                )
                ) # div
              ) # hidden
            )
          )    
  )
)

# Put components in UI 
ui = dashboardPage(header,sidebar,body)

# =============================================================================== SERVER
server = function(input,output,session){

# ===================================================================================== FOR REGRESSION
  # Renders the default value of the estimate value box
  output$estimateResultRegres = renderValueBox({
    valueBox("Please enter CSV, order, and estimate", subtitle = "Error", icon = icon("triangle-exclamation"), color = "red")
  })
  
  # Prompts to enter the CSV and order
  output$orderResultRegres = renderText({
    paste("Please enter CSV and order")
  })
  
  # This reads the CSV file (returns a dataframe)
  dataToRegres = reactive({
    regresCSV = input$regresCSV
    req(regresCSV)
    read.csv(regresCSV$datapath, header=FALSE)
  }) # dataToRegres
  
  # Shows the data points if the user has uploaded a CSV file
  observeEvent(input$regresCSV, {
    shinyjs::show("regresDataPoints")
  })
  
  # Renders the CSV file as a table horizontally
  output$regresCSVToTable = renderDataTable({
    datatable(
      t(dataToRegres()[order(dataToRegres()[,1]), ]),
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        searching = FALSE,
        dom = 't'
      ),
      rownames = c("x","y")
    )
  })
  
  # Runs if the solve button is clicked
  observeEvent(input$solveRegres, {
    # Checks invalid cases: 
    # 1.) No CSV is entered
    # 2.) No Estimate is entered
    # 3.) No order is entered
    if (is.null(input$regresCSV) || is.na(input$regresEstimate) || is.na(input$regresOrder)) {
      # Show error in value box
      output$estimateResultRegres = renderValueBox({
        valueBox("Please check your inputs", subtitle = "Error", icon = icon("triangle-exclamation"), color = "red")
      })
    # Checks if order entered is invalid: 
    # 1.) Order is less than 1
    # 2.) Order is greater than n-1
    } else if (input$regresOrder < 1 || input$regresOrder > (length(dataToRegres()[, 1]) - 1)) {
      # Hide the boxes
      shinyjs::hide("plotBox")
      shinyjs::hide("regresFunctionBox")
      shinyjs::hide("regresEliminationBox")
      # Show error value box
      output$estimateResultRegres = renderValueBox({
        valueBox("Please enter valid order", subtitle = "Error", icon = icon("triangle-exclamation"), color = "red")
      })
    # If all inputs are valid
    } else {
      
      # Get the order of the polynomial
      orderRegres = reactive ({
        as.numeric(input$regresOrder)
      }) # reactive order
      # Get the estimation
      estimateRegres = reactive({
        as.numeric(input$regresEstimate)
      }) # reactive estimate

      # Gets the left side of the CSV file
      xValuesRegres = reactive({
        req(dataToRegres())
        dataToRegres()[,1]
      }) # xValues

      # Gets the right side of the CSV file
      yValuesRegres = reactive({
        req(dataToRegres())
        dataToRegres()[,2]
      }) #yValues

      # Puts the left and right side into a list
      dataListRegres = reactive({
        list(xValuesRegres(), yValuesRegres())
      }) # dataList
      
      # Runs the PolynomialRegression function from polyRegres.R
      regresResult = PolynomialRegression(orderRegres(), dataListRegres(), estimateRegres(), FALSE)
      # shows the boxes
      shinyjs::show("plotBox")
      shinyjs::show("regresFunctionBox")
      shinyjs::show("regresEliminationBox")
      
      # Gets the iterations of the elimination
      regresElimination = regresResult$mat_iter
      
      # Updates the slider depending on the number of iterations
      updateSliderInput(session, "regresEliminationSlider", max = length(regresResult$mat_iter))
      
      # Outputs the polynomial string
      output$estimateResultRegres = renderValueBox({
        valueBox(paste(regresResult$estimate), "Estimate", icon = icon("check"), color = "green")
      }) # polynomialString
      
      # Outputs the polynomial function
      output$orderResultRegres = renderText({
        paste(regresResult$Polynomial_string)
      }) # output$orderResult
      
      # Outputs the elimination iteration depending on the slider input
      output$regresElimination = renderTable({
        as.data.frame(regresElimination[[input$regresEliminationSlider]])
      }) # regresElimination
      
      # Outputs the plot
      output$regresPlot = renderPlot({
        # Gets a linear regression model
        regresModel = lm(yValuesRegres() ~ poly(xValuesRegres(), orderRegres(), raw = TRUE))
        # Gets the predicted values using the model 
        predictedValues = predict(regresModel, newdata = dataListRegres())
        # Plot the points
        plot(xValuesRegres(), yValuesRegres(), pch = 16, col = "#6BF9B5", main = "Polynomial Regression", xlab = "X", ylab = "Y")
        # Create a line using the predicted values
        lines(xValuesRegres(), predictedValues, col = "#13192D", lwd = 2)
      }) # render plot
    } # outer else
  }) # button click 

  
  # ============================================================================== FOR QUADRATIC SPLINE INTERPOLATION
  # Outputs the default value box
  output$qsiEstimateResult = renderValueBox({
    valueBox("Please enter CSV and estimate", subtitle = "Error", icon = icon("triangle-exclamation"), color = "red")
  })
  
  # Gets the data from the CSV
  dataToQSI = reactive({
    qsiCSV = input$qsiCSV
    req(qsiCSV)
    read.csv(qsiCSV$datapath, header=FALSE)
  })
  
  # Shows the data points upon successful upload
  observeEvent(input$qsiCSV, {
    shinyjs::show("qsiDataPoints")
  })
  
  # Outputs the CSV file as table horizontally
  output$qsiCSVToTable <- renderDataTable({
    datatable(
      t(dataToQSI()[order(dataToQSI()[,1]), ]),
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        searching = FALSE,
        dom = 't'
      ),
      rownames = c("x","y")
    )
  })
  
  # Runs if the solve button is clicked
  observeEvent(input$solveQSI, {
    # Checks invalid inputs
    # 1.) No CSV is entered
    # 2.) No estimate is entered
    if (is.null(input$qsiCSV) || is.null(input$qsiEstimate)) {
      # Handle the case when "SOLVE" is clicked without a CSV file
      output$qsiEstimateResult = renderValueBox({
        valueBox("Please enter CSV or estimate", subtitle = "Error", icon = icon("triangle-exclamation"), color = "red")
      })
    
    # If inputs are valid
    } else {
      # Gets the estiamte
      qsiEstimate = as.numeric(input$qsiEstimate)
      
      # Runs quadraticSplineInterpolation from QSI.R
      functionResult = quadraticSplineInterpolation(dataToQSI(), qsiEstimate, FALSE)
      
      # If the function returned a null result (error), hide the boxes and show error
      if(is.null(functionResult)){
        shinyjs::hide("qsiResultsBox")
        shinyjs::hide("qsiEliminationBox")
        output$qsiEstimateResult = renderValueBox({
          valueBox(paste("Enter valid estimate"), "Error", icon = icon("triangle-exclamation"), color = "red")
        })
      # If no error occured
      } else {
        # Show the intervals and the eliminations
        shinyjs::show("qsiResultsBox")
        shinyjs::show("qsiEliminationBox")
        
        # Outputs the estimate
        output$qsiEstimateResult = renderValueBox({
          valueBox(paste(functionResult$estimate), "Estimate", icon = icon("check"), color = "green")
        })
        
        # Updates the max of the slider
        updateSliderInput(session, "qsiEliminationSlider", max = length(functionResult$mat_iter))
        
        # Gets the iterations of the elimination
        eliminationMatrices = functionResult$mat_iter
        
        # Outputs the intervals and the functions
        output$intervalsAndFunctions = renderTable({
          qsiFunctionResult = quadraticSplineInterpolation(dataToQSI(),qsiEstimate,FALSE)
          colnames(qsiFunctionResult$intervals) = c("intervals", "functions")
          qsiFunctionResult$intervals
        })
        
        # Outputs the elimination iterations
        output$qsiElimination = renderTable({
          as.data.frame(eliminationMatrices[[input$qsiEliminationSlider]])
        })
      } # inner else
    } # outer else
  })
  
  # ========================================= FOR THE DIET SOLVER (SIMPLEX)
  # The default error message of the value box
  output$optimalCostDS = renderValueBox({
    valueBox(paste("No input"), subtitle = "Please select your food", icon = icon("triangle-exclamation"),color = "red")
  })
  
  # Checks all the food choices if the select all button is clicked
  observeEvent(input$selectAllDS, {
    allChoices = foodChoices
    updateCheckboxGroupInput(session, "inputDiet", selected=allChoices)
  })
  
  # Unchecks all the food boxes if the reset button is clicked, outputs error, hides the boxes
  observeEvent(input$resetDS, {
    updateCheckboxGroupInput(session, "inputDiet", selected=character(0))
    output$optimalCostDS = renderValueBox({
      valueBox(paste("No input"), subtitle = "Error", icon = icon("triangle-exclamation"),color = "red")
    })
    shinyjs::hide("breakdownBox")
    shinyjs::hide("finalTableuBox")
    shinyjs::hide("iterationSliderDSBox")
    shinyjs::hide("iterationDSBox")
  })
  
  # Runs if the solve button is clicked
  observeEvent(input$solveDS, {
    # Generates the selected food
    selectedFoodIds = input$inputDiet
    
    # Checks if there are no selected foods, show error, hide boxes
    if (is.null(selectedFoodIds)){
      output$optimalCostDS = renderValueBox({
        valueBox(paste("No input"), subtitle = "Please select your food", icon = icon("triangle-exclamation"),color = "red")
      })
      shinyjs::hide("breakdownBox")
      shinyjs::hide("finalTableuBox")
      shinyjs::hide("iterationSliderDSBox")
      shinyjs::hide("iterationDSBox")
    
    # If inputs are valid
    } else {
      # Run the simplexMethod from simplex.R
      simplexResult = simplexMethod(selectedFoodIds)
      
      # If the function returned null, show error message, hide boxes
      if (is.null(simplexResult)){
        output$optimalCostDS = renderValueBox({
          valueBox(paste("INFEASIBLE"), subtitle = "Cannot meet nutritional requirements", icon = icon("triangle-exclamation"), color = "red")
        })
        shinyjs::hide("breakdownBox")
        shinyjs::hide("finalTableuBox")
        shinyjs::hide("iterationSliderDSBox")
        shinyjs::hide("iterationDSBox")
      # If no error was encountered
      } else {
        # Output the optimal cost
        output$optimalCostDS = renderValueBox({
          valueBox(paste("$",simplexResult$FinalSol$OptimalCost), subtitle = "Optimal Cost", icon = icon("dollar-sign"),color = "green")
        })
        
        # Show the boxes
        shinyjs::show("breakdownBox")
        shinyjs::show("finalTableuBox")
        shinyjs::show("iterationSliderDSBox")
        shinyjs::show("iterationDSBox")
        
        # Output the breakdown of the optimal diet
        output$breakdownDS = renderTable({
          simplexResult$FinalSol$Breakdown
        })
        
        # Output the final tableau
        output$finalTableuDS = renderTable({
          simplexResult$FinalSol$FinalTableu
        })
        
        # Output the final basic solution
        output$finalBasicSolDS = renderTable({
          simplexResult$FinalSol$FinalBasicSol
        })
        
        # Update max of the slider for iterations
        updateSliderInput(session, "matIterSliderDS", max = length(simplexResult$Iterations))
        
        # Output the iteration of the simplex method depending on the slider
        output$iterationDS = renderTable({
          as.data.frame(simplexResult$Iterations[[input$matIterSliderDS]]$tableu)
        })
        
        # Output the basic solution per iteration, does not output for the last iteration
        output$basicSolDS = renderTable({
          if(input$matIterSliderDS != length(simplexResult$Iterations)){
            as.data.frame(simplexResult$Iterations[[input$matIterSliderDS]]$BasicSolution)
          }
        }) # output basic sol
      } # inner else (not null) 
    } # outer else (inputs are valid)
  }) # solve button click 
} # server

# Put UI and SERVER into the shiny app 
shinyApp(ui = ui, server = server)
