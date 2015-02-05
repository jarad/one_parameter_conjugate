library(shiny)
library(ggplot2)

source("dinvgamma.R")

shinyServer(function(input,output,session) {
  
  ######################################################################
  # Binomial (unknown p)
  ######################################################################
  
  bin_p_xx = reactive({ seq(input$bin_p_min, input$bin_p_max, length=1001) })
  
  bin_p_prior = reactive({
    x = bin_p_xx()
    data.frame("Distribution" = "prior",
               x = x,
               y = dbeta(x, input$bin_p_a, input$bin_p_b))
  })
  
  bin_p_like = reactive({
    x = bin_p_xx()
    data.frame("Distribution" = "likelihood",
               x = x,
               y = dbeta(x, input$bin_p_y, input$bin_p_n-input$bin_p_y))
  })  
  
  bin_p_post = reactive({
    x = bin_p_xx()
    data.frame("Distribution" = "posterior",
               x = x,
               y = dbeta(x, 
                         input$bin_p_a+input$bin_p_y, 
                         input$bin_p_b+input$bin_p_n-input$bin_p_y))
  })
  
  bin_p_data = reactive({
    rbind(bin_p_prior(),
          bin_p_like(),
          bin_p_post())
  })
  
  output$bin_p_plot = renderPlot({
    ggplot(bin_p_data(), aes(x=x,y=y,color=Distribution)) + geom_line()
  })
  
  
  ######################################################################
  # Normal (unknown mean)
  ######################################################################
  
  norm_m_xx = reactive({ seq(input$norm_m_min, input$norm_m_max, length=1001) })
  
  norm_m_prior = reactive({
    x = norm_m_xx()
    data.frame("Distribution" = "prior",
               x = x,
               y = dnorm(x, input$norm_m_m, input$norm_m_s))
  })
  
  norm_m_like = reactive({
    x = norm_m_xx()
    data.frame("Distribution" = "likelihood",
               x = x,
               y = dnorm(x, input$norm_m_ybar, input$norm_m_sigma/sqrt(input$norm_m_n)))
  })  
  
  norm_m_post = reactive({
    x = norm_m_xx()
    vr = 1/input$norm_m_s^2 + input$norm_m_n/input$norm_m_sigma^2
    mn = vr*(input$norm_m_m/input$norm_m_s^2 + input$norm_m_n*input$norm_m_ybar/input$norm_m_sigma^2)
    data.frame("Distribution" = "posterior",
               x = x,
               y = dnorm(x, mn, sqrt(vr)))
  })
  
  norm_m_data = reactive({
    rbind(norm_m_prior(),
          norm_m_like(),
          norm_m_post())
  })
  
  output$norm_m_plot = renderPlot({
    ggplot(norm_m_data(), aes(x=x,y=y,color=Distribution)) + geom_line()
  })
  
  
  ######################################################################
  # Normal (unknown variance)
  ######################################################################
  
  norm_v_xx = reactive({ seq(input$norm_v_min, input$norm_v_max, length=1001) })
  
  norm_v_prior = reactive({
    x = norm_v_xx()
    data.frame("Distribution" = "prior",
               x = x,
               y = dinvgamma(x, input$norm_v_a, input$norm_v_b))
  })
  
  norm_v_like = reactive({
    x = norm_v_xx()
    data.frame("Distribution" = "likelihood",
               x = x,
               y = dinvgamma(x, 
                             input$norm_v_n/2, 
                             input$norm_v_n*input$norm_v_s^2/2))
  })  
  
  norm_v_post = reactive({
    x = norm_v_xx()
    data.frame("Distribution" = "posterior",
               x = x,
               y = dinvgamma(x, 
                             input$norm_v_a+input$norm_v_n/2, 
                             input$norm_v_b+input$norm_v_n*input$norm_v_s/2))
  })
  
  norm_v_data = reactive({
    rbind(norm_v_prior(),
          norm_v_like(),
          norm_v_post())
  })
  
  output$norm_v_plot = renderPlot({
    ggplot(norm_v_data(), aes(x=x,y=y,color=Distribution)) + geom_line()
  })
  
  
  ######################################################################
  # Poisson (unknown mean)
  ######################################################################
  
  pois_m_xx = reactive({ seq(input$pois_m_min, input$pois_m_max, length=1001) })
  
  pois_m_prior = reactive({
    x = pois_m_xx()
    data.frame("Distribution" = "prior",
               x = x,
               y = dgamma(x, input$pois_m_a, input$pois_m_b))
  })
  
  pois_m_like = reactive({
    x = pois_m_xx()
    data.frame("Distribution" = "likelihood",
               x = x,
               y = dgamma(x, 
                          input$pois_m_n*input$pois_m_ybar, 
                          input$pois_m_n))
  })  
  
  pois_m_post = reactive({
    x = pois_m_xx()
    data.frame("Distribution" = "posterior",
               x = x,
               y = dinvgamma(x, 
                             input$pois_m_a+input$pois_m_n*input$pois_m_ybar, 
                             input$pois_m_b+input$pois_m_n))
  })
  
  pois_m_data = reactive({
    rbind(pois_m_prior(),
          pois_m_like(),
          pois_m_post())
  })
  
  output$pois_m_plot = renderPlot({
    ggplot(pois_m_data(), aes(x=x,y=y,color=Distribution)) + geom_line()
  })
  
})
