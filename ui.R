require(shiny)
require(markdown)

shinyUI(pageWithSidebar(
  
  headerPanel("One parameter conjugate Bayesian analysis"),
  
  sidebarPanel(
    #style="min-width:235px;max-width:275px",
    selectInput('Model:', 
                list('Binomial' = 'binomial')),
    helpText('Prior'),
    numericInput('alpha', 'alpha (>0)', 1, 0),
    numericInput('beta',  'beta (>0)',  1, 0),
    hr(),
    numericInput('n', 'Total attempts:', 1, 0),
    numericInput('y', 'Total successes',  1, 0)
  ),
    
  mainPanel(
    tabsetPanel(
      tabPanel('Plot', plotOutput('plot')),
      tabPanel('Help', includeMarkdown('help.md'))
    )
  )
))
