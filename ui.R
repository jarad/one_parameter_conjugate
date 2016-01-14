require(shiny)
require(markdown)

shinyUI(navbarPage("Model:",
  tabPanel("Help", includeMarkdown("help.md")),
  
  
  ####################################################
  # Binomial
  ####################################################
  navbarMenu("Binomial",
    tabPanel("unknown p",
      sidebarLayout(
        sidebarPanel(
          helpText('Prior: Be(a,b)'),
          numericInput('bin_p_a', 'a', 1, 0),
          numericInput('bin_p_b', 'b',  1, 0),
          hr(),
          helpText('Data'),
          numericInput('bin_p_n', 'Total attempts:', 10, 0),
          numericInput('bin_p_y', 'Total successes:',  3, 0),
          hr(),
          helpText('Plot x limits'),
          numericInput('bin_p_min', 'min', 0),
          numericInput('bin_p_max', 'max', 1)
        ),
        mainPanel(
          plotOutput("bin_p_plot")
        )
      )
    ),
    tabPanel("unknown n", includeMarkdown("under_construction.md"))
  ),
  
  ####################################################
  # Poisson
  ####################################################
  navbarMenu("Poisson",
    tabPanel("unknown mean",
      sidebarLayout(
        sidebarPanel(
          helpText('Prior: Ga(a,b)'),
          numericInput('pois_m_a', 'a', 1, 0),
          numericInput('pois_m_b', 'b', 1, 0),
          hr(),
          helpText('Data'),
          numericInput('pois_m_n', 'Number of observations:', 1, 0),
          numericInput('pois_m_ybar', 'Sample average:',  2, 0),
          hr(),
          helpText('Plot x limits'),
          numericInput('pois_m_min', 'min', 0),
          numericInput('pois_m_max', 'max', 3)
        ),
        mainPanel(plotOutput('pois_m_plot'))
      )
    )
  ),
  
  ####################################################
  # Normal
  ####################################################
  navbarMenu("Normal",
    tabPanel("unknown mean",
      sidebarLayout(
        sidebarPanel(
          numericInput('norm_m_sigma', 'Data standard deviation', 1, 0),
          helpText('Prior: N(m,s^2)'),
          numericInput('norm_m_m', 'm', 0, 0),
          numericInput('norm_m_s', 's', 1, 0),
          hr(),
          helpText('Data'),
          numericInput('norm_m_n', 'Number of observations:', 1, 0),
          numericInput('norm_m_ybar', 'Sample average:',  1, 0),
          hr(),
          helpText('Plot x limits'),
          numericInput('norm_m_min', 'min', -3),
          numericInput('norm_m_max', 'max',  3)
        ),
        mainPanel(plotOutput('norm_m_plot'))
      )
    ),
    tabPanel("unknown variance",
      sidebarLayout(
        sidebarPanel(
          helpText('Prior: IG(a,b)'),
          numericInput('norm_v_a', 'a', 1, 0),
          numericInput('norm_v_b', 'b', 1, 0),
          hr(),
          helpText('Data'),
          numericInput('norm_v_n', 'Number of observations:', 1, 0),
          numericInput('norm_v_s', 'Average sum of squared deviations from the mean:',  1, 0),
          hr(),
          helpText('Plot x limits'),
          numericInput('norm_v_min', 'min', 0),
          numericInput('norm_v_max', 'max', 3)
        ),
        mainPanel(plotOutput('norm_v_plot'))
      )
    )
  ),
  
  ####################################################
  # Exponential
  ####################################################
  navbarMenu("Exponential",
             tabPanel("unknown mean",
                      sidebarLayout(
                        sidebarPanel(
                          helpText('Prior: IG(a,b)'),
                          numericInput('exp_m_a', 'a', 1, 0),
                          numericInput('exp_m_b', 'b', 1, 0),
                          hr(),
                          helpText('Data'),
                          numericInput('exp_m_n', 'Number of observations:', 2, 0),
                          numericInput('exp_m_ybar', 'Sample average:',  1, 0),
                          hr(),
                          helpText('Plot x limits'),
                          numericInput('exp_m_min', 'min', 0),
                          numericInput('exp_m_max', 'max', 3)
                        ),
                        mainPanel(plotOutput('exp_m_plot'))
                      )
             ),
             tabPanel("unknown rate",
                      sidebarLayout(
                        sidebarPanel(
                          helpText('Prior: Ga(a,b)'),
                          numericInput('exp_r_a', 'a', 1, 0),
                          numericInput('exp_r_b', 'b', 1, 0),
                          hr(),
                          helpText('Data'),
                          numericInput('exp_r_n', 'Number of observations:', 2, 0),
                          numericInput('exp_r_ybar', 'Sample average:',  1, 0),
                          hr(),
                          helpText('Plot x limits'),
                          numericInput('exp_r_min', 'min', 0),
                          numericInput('exp_r_max', 'max', 3)
                        ),
                        mainPanel(plotOutput('exp_r_plot'))
                      )
             )
  )
  
))
