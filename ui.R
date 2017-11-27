#################################
#### Google Analytics - ui.R ####
#################################

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
        tags$head(
                tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Cookie');
                 @import url('//fonts.googleapis.com/css?family=Lato:400,400i,700'); 
                      
                      h1 {
                        font-family: Lato;
                        font-weight: 500;
                        line-height: 1.1;
                        color: steelblue;
                      }
                        h3 {
                                font-family: 'Cookie', cursive;
                                 color: #48ca3b;
                        }
                        h4 {
                                font-family: Lato;
                                color: darkorange;
                        }
                
                    "))
        ),
  h1(HTML("Taylor Polynomial Approximations, <br> centered at 0, 
         of the cosine function")),
  
  # Simple setup, controls on left, output on right 
  sidebarLayout(
    sidebarPanel(
            # submitButton(text="Produce output"),
       radioButtons(inputId='graph',
                    label='Choose the degree of the Taylor Polynomial', 
                    choices=list('Constant function'='0',
                                 'Quadratic polynomial'='2',
                                 '4th degree polynomial'='4',
                                 '6th degree polynomial'='6',
                                 '8th degree polynomial'='8',
                                 '10th degree polynomial'='10',
                                 '12th degree polynomial'='12')),
       hr(),
       conditionalPanel(
              condition = "input.theTabs == 'summary'", 
               sliderInput(inputId = "sliders",
                           label = "Find the approximate value of cosine at:",
                           min = -6, max = 6, value = 0, step = 0.05)
              
       ),
       
       conditionalPanel(
               condition = "input.theTabs == 'table'" 
               # sliderInput(inputId = "slider",
               #             label = "Find the approximate value of cosine at:",
               #             min = -6, max = 6, value = 0, step = 0.05)
               
       ),
       conditionalPanel(
               condition = "input.theTabs == 'plots'", 
               sliderInput(inputId = "sliderp",
                           label = "Find the approximate value of cosine at:",
                           min = -6, max = 6, value = 0, step = 0.05,
                           animate = animationOptions(interval = 100, loop=TRUE)),
               radioButtons(inputId = "plotPoly",
                            label = "Plot the polynomial approximation?",
                            choices = list("Yes" = TRUE, "No" = FALSE), selected=FALSE)
               # actionButton("plotPoly", "Update")
               
       ),
      # 
       # sliderInput(inputId = "slider",
       #             label = "Find the approximate value of cosine at:",
       #             min = -6, max = 6, value = 0, step = 0.05,
       #             animate = animationOptions(interval = 100, loop=TRUE)),

       hr(),
  
       textInput(inputId = "comment",
                 label = "Comment?",
                 value = "Shiny rocks!")),
 
    # Show a plot of the generated distribution
 mainPanel(
            tabsetPanel(id = "theTabs",
                    tabPanel('Summary',htmlOutput('textDisplay'),
                             value = "summary"),
                    tabPanel('Absolute Errors', DT::dataTableOutput('absoluteError'),
                             value = "table"),
                    tabPanel('Plots', plotOutput("plotDisplay"),
                             value = "plots")
            ),
            
            htmlOutput('someText')
          
          )

   )
))
