# ui.R
library(shiny)
shinyUI(fluidPage(
  
  titlePanel("", windowTitle = "Exploring Priors"),
  
  h1(strong("Exploring Prior Densities"), align = "center"),
  
  br(), br(),
  
  column(4, wellPanel(
    
    h2("Distribution Specifications", align = "center"),
    
    br(),
    
    # Select Distribution
    selectInput("dist", "Choose a distribution:", 
                c("", "Normal", "Beta", "Gamma", "Exponential")),
    
    # Normal Distribution
    conditionalPanel(
        condition = "input.dist == 'Normal'", 
        numericInput("mean", HTML("Mean ( &mu; ):"),     value = 0),
        numericInput("sd",   HTML("Standard Deviation ( &sigma; ):"), value = 1, min = 0),
        numericInput("xmin_n", "Minimum x Value:", value = -4),
        numericInput("xmax_n", "Maximum x Value:", value = 4),
        numericInput("by_n",   "Increment x by:", value = 0.05),
        
        checkboxInput("multinormal", label = "Add More Curves", value = FALSE),
        conditionalPanel(
          condition = "input.multinormal == false",
          actionButton("plotNormal", "Update Plot")
        ),
        conditionalPanel(
          condition = "input.multinormal == true",
          
          selectInput("nnormal", "Number of Additional Curves", choices = c("", "1", "2", "3", "4")),
          
          conditionalPanel(
            condition = "input.nnormal == '1'",
            h4("Curve 2 Parameters (red)", align = "center"),
            numericInput("mean2_1", HTML("Mean ( &mu; ):"), value = 0),
            numericInput("sd2_1",   HTML("Standard Deviation ( &sigma; ):"), value = 1),
            actionButton("addNormal1", "Update Plot")
          ),
          
          conditionalPanel(
            condition = "input.nnormal == '2'",
            h4("Curve 2 Parameters (red)", align = "center"),
            numericInput("mean2_2", HTML("Mean ( &mu; ):"), value = 0),
            numericInput("sd2_2",   HTML("Standard Deviation ( &sigma; ):"), value = 1),
            h4("Curve 3 Parameters (blue)", align = "center"),
            numericInput("mean3_2", HTML("Mean ( &mu; ):"), value = 0),
            numericInput("sd3_2",   HTML("Standard Deviation ( &sigma; ):"), value = 1),
            actionButton("addNormal2", "Update Plot")
          ),
          
          conditionalPanel(
            condition = "input.nnormal == '3'",
            h4("Curve 2 Parameters (red)", align = "center"),
            numericInput("mean2_3", HTML("Mean ( &mu; ):"), value = 0),
            numericInput("sd2_3",   HTML("Standard Deviation ( &sigma; ):"), value = 1),
            h4("Curve 3 Parameters (blue)", align = "center"),
            numericInput("mean3_3", HTML("Mean ( &mu; ):"), value = 0),
            numericInput("sd3_3",   HTML("Standard Deviation ( &sigma; ):"), value = 1),
            h4("Curve 4 Parameters (green)", align = "center"),
            numericInput("mean4_3", HTML("Mean ( &mu; ):"), value = 0),
            numericInput("sd4_3",   HTML("Standard Deviation ( &sigma; ):"), value = 1),
            actionButton("addNormal3", "Update Plot")
          ),
          
          conditionalPanel(
            condition = "input.nnormal == '4'",
            h4("Curve 2 Parameters (red)", align = "center"),
            numericInput("mean2_4", HTML("Mean ( &mu; ):"), value = 0),
            numericInput("sd2_4",   HTML("Standard Deviation ( &sigma; ):"), value = 1),
            h4("Curve 3 Parameters (blue)", align = "center"),
            numericInput("mean3_4", HTML("Mean ( &mu; ):"), value = 0),
            numericInput("sd3_4",   HTML("Standard Deviation ( &sigma; ):"), value = 1),
            h4("Curve 4 Parameters (green)", align = "center"),
            numericInput("mean4_4", HTML("Mean ( &mu; ):"), value = 0),
            numericInput("sd4_4",   HTML("Standard Deviation ( &sigma; ):"), value = 1),
            h4("Curve 5 Parameters (magenta)", align = "center"),
            numericInput("mean5_4", HTML("Mean ( &mu; ):"), value = 0),
            numericInput("sd5_4",   HTML("Standard Deviation ( &sigma; ):"), value = 1),
            actionButton("addNormal4", "Update Plot")
          )
        )
    ),
    
    # Beta Distribution
    conditionalPanel(
      condition = "input.dist == 'Beta'", 
      numericInput("alpha", HTML("Shape ( &alpha; ):"), value = 2),
      numericInput("beta",  HTML("Scale ( &beta; ):"), value = 5), 
      numericInput("by_b",   "Increment x by:", value = 0.005),
      
      checkboxInput("multibeta", label = "Add More Curves", value = FALSE),
      conditionalPanel(
        condition = "input.multibeta == false",
        actionButton("plotBeta", "Update Plot")
      ),
      conditionalPanel(
        condition = "input.multibeta == true",
        
        selectInput("nbeta", "Number of Additional Curves", choices = c("", "1", "2", "3", "4")),
        
        conditionalPanel(
          condition = "input.nbeta == '1'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("alpha2_1", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("beta2_1",  HTML("Scale ( &beta; ):"), value = 5), 
          actionButton("addBeta1", "Update Plot")
        ),
        
        conditionalPanel(
          condition = "input.nbeta == '2'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("alpha2_2", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("beta2_2",  HTML("Scale ( &beta; ):"), value = 5), 
          h4("Curve 3 Parameters (blue)", align = "center"),
          numericInput("alpha3_2", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("beta3_2",  HTML("Scale ( &beta; ):"), value = 5), 
          actionButton("addBeta2", "Update Plot")
        ),
        
        conditionalPanel(
          condition = "input.nbeta == '3'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("alpha2_3", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("beta2_3",  HTML("Scale ( &beta; ):"), value = 5), 
          h4("Curve 3 Parameters (blue)", align = "center"),
          numericInput("alpha3_3", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("beta3_3",  HTML("Scale ( &beta; ):"), value = 5), 
          h4("Curve 4 Parameters (green)", align = "center"),
          numericInput("alpha4_3", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("beta4_3",  HTML("Scale ( &beta; ):"), value = 5), 
          actionButton("addBeta3", "Update Plot")
        ),
        
        conditionalPanel(
          condition = "input.nbeta == '4'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("alpha2_4", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("beta2_4",  HTML("Scale ( &beta; ):"), value = 5), 
          h4("Curve 3 Parameters (blue)", align = "center"),
          numericInput("alpha3_4", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("beta3_4",  HTML("Scale ( &beta; ):"), value = 5), 
          h4("Curve 4 Parameters (green)", align = "center"),
          numericInput("alpha4_4", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("beta4_4",  HTML("Scale ( &beta; ):"), value = 5), 
          h4("Curve 5 Parameters (magenta)", align = "center"),
          numericInput("alpha5_4", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("beta5_4",  HTML("Scale ( &beta; ):"), value = 5), 
          actionButton("addBeta4", "Update Plot")
        )
      )
    ),
    
    # Gamma Distribution
    conditionalPanel(
      condition = "input.dist == 'Gamma'", 
      numericInput("k", HTML("Shape ( &alpha; ):"), value = 2),
      numericInput("b",  HTML("Scale ( &beta; ):"), value = 2),
      numericInput("xmax_g",  "Maximum x Value:", value = 10),
      numericInput("by_g",   "Increment x by:", value = 0.05),
      
      checkboxInput("multigamma", label = "Add More Curves", value = FALSE),
      conditionalPanel(
        condition = "input.multigamma == false",
        actionButton("plotGamma", "Update Plot")
      ),
      conditionalPanel(
        condition = "input.multigamma == true",
        
        selectInput("ngamma", "Number of Additional Curves", choices = c("", "1", "2", "3", "4")),
        
        conditionalPanel(
          condition = "input.ngamma == '1'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("k2_1", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("b2_1",  HTML("Scale ( &beta; ):"), value = 2), 
          actionButton("addGamma1", "Update Plot")
        ),
        
        conditionalPanel(
          condition = "input.ngamma == '2'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("k2_2", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("b2_2",  HTML("Scale ( &beta; ):"), value = 2), 
          h4("Curve 3 Parameters (blue)", align = "center"),
          numericInput("k3_2", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("b3_2",  HTML("Scale ( &beta; ):"), value = 2), 
          actionButton("addGamma2", "Update Plot")
        ),
        
        conditionalPanel(
          condition = "input.ngamma == '3'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("k2_3", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("b2_3",  HTML("Scale ( &beta; ):"), value = 2), 
          h4("Curve 3 Parameters (blue)", align = "center"),
          numericInput("k3_3", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("b3_3",  HTML("Scale ( &beta; ):"), value = 2), 
          h4("Curve 4 Parameters (green)", align = "center"),
          numericInput("k4_3", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("b4_3",  HTML("Scale ( &beta; ):"), value = 2), 
          actionButton("addGamma3", "Update Plot")
        ),
        
        conditionalPanel(
          condition = "input.ngamma == '4'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("k2_4", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("b2_4",  HTML("Scale ( &beta; ):"), value = 2), 
          h4("Curve 3 Parameters (blue)", align = "center"),
          numericInput("k3_4", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("b3_4",  HTML("Scale ( &beta; ):"), value = 2), 
          h4("Curve 4 Parameters (green)", align = "center"),
          numericInput("k4_4", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("b4_4",  HTML("Scale ( &beta; ):"), value = 2), 
          h4("Curve 5 Parameters (magenta)", align = "center"),
          numericInput("k5_4", HTML("Shape ( &alpha; ):"), value = 2),
          numericInput("b5_4",  HTML("Scale ( &beta; ):"), value = 2), 
          actionButton("addGamma4", "Update Plot")
        )
      )
    ),


    
    # Exponential Distribution
    conditionalPanel(
      condition = "input.dist == 'Exponential'", 
      numericInput("rate", HTML("Scale ( &beta; ):"), value = 1),
      numericInput("xmax_e",  "Maximum x Value:", value = 5),
      numericInput("by_e",   "Increment x by:", value = 0.05),
      
      checkboxInput("multiexponential", label = "Add More Curves", value = FALSE),
      conditionalPanel(
        condition = "input.multiexponential == false",
        actionButton("plotExponential", "Update Plot")
      ),
      conditionalPanel(
        condition = "input.multiexponential == true",
        
        selectInput("nexponential", "Number of Additional Curves", 
                    choices = c("", "1", "2", "3", "4")),
        
        conditionalPanel(
          condition = "input.nexponential == '1'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("rate2_1", HTML("Scale ( &beta; ):"), value = 1),
          actionButton("addExponential1", "Update Plot")
        ),
        
        conditionalPanel(
          condition = "input.nexponential == '2'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("rate2_2",  HTML("Scale ( &beta; ):"), value = 1), 
          h4("Curve 3 Parameters (blue)", align = "center"),
          numericInput("rate3_2",  HTML("Scale ( &beta; ):"), value = 1), 
          actionButton("addExponential2", "Update Plot")
        ),
        
        conditionalPanel(
          condition = "input.nexponential == '3'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("rate2_3",  HTML("Scale ( &beta; ):"), value = 1), 
          h4("Curve 3 Parameters (blue)", align = "center"),
          numericInput("rate3_3",  HTML("Scale ( &beta; ):"), value = 1), 
          h4("Curve 4 Parameters (green)", align = "center"),
          numericInput("rate4_3",  HTML("Scale ( &beta; ):"), value = 1), 
          actionButton("addExponential3", "Update Plot")
        ),
        
        conditionalPanel(
          condition = "input.nexponential == '4'",
          h4("Curve 2 Parameters (red)", align = "center"),
          numericInput("rate2_4",  HTML("Scale ( &beta; ):"), value = 1), 
          h4("Curve 3 Parameters (blue)", align = "center"),
          numericInput("rate3_4",  HTML("Scale ( &beta; ):"), value = 1), 
          h4("Curve 4 Parameters (green)", align = "center"),
          numericInput("rate4_4",  HTML("Scale ( &beta; ):"), value = 1), 
          h4("Curve 5 Parameters (magenta)", align = "center"),
          numericInput("rate5_4",  HTML("Scale ( &beta; ):"), value = 1), 
          actionButton("addExponential4", "Update Plot")
        )
      )
    )
    
    
    # Inputs -- Chi-Square
    
    # Inputs -- Pareto
  )),
  
  column(8, align = "center",
         
         
         conditionalPanel("input.dist == 'Normal'",
                          h2("Plot of Normal Density", align = "center"),
                          plotOutput("normalPlot", inline = TRUE)),
         
         
         conditionalPanel("input.dist == 'Beta'",
                          h2("Plot of Beta Density", align = "center"),
                          plotOutput("betaPlot", inline = TRUE)),
         
         conditionalPanel("input.dist == 'Gamma'",
                          h2("Plot of Gamma Density", align = "center"),
                          plotOutput("gammaPlot", inline = TRUE)),
         
         conditionalPanel("input.dist == 'Exponential'",
                          h2("Plot of Exponential Density", align = "center"),
                          plotOutput("exponentialPlot", inline = TRUE))
  )
         
))
