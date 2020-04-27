library(shiny)
library(shinydashboard)
library(tidyverse)

shinyApp(
  
  ui = fixedPage(fixed = TRUE,
                 sidebarLayout(fluid = FALSE, 
                               sidebarPanel(width = 4,
                                            tags$head(
                                              tags$style(type="text/css", "select { max-width: 140px; }"),
                                              tags$style(type="text/css", ".span4 { max-width: 190px; }"),
                                              tags$style(type="text/css", ".well { max-width: 180px; }")
                                            ),
                                            p("Estimando m ótimo"),
                                            verbatimTextOutput("info"),
                                            sliderInput(inputId = "corr",
                                                        label = "Confiabilidade
                                                        Necessária",
                                                        value = 0.9,
                                                        min = 0,
                                                        max = 1,
                                                        step = 0.05)
                               ),
                               
                               mainPanel(width = 8,
                                         tableOutput("m1")
                               )
                 )
  ),
  ##======================================================================
  server = function(input, output) {
    
    da <- read.table("da.txt", header = TRUE)
    da.new <- plyr::ddply(da, "Local", transform, m = mean(Concentra))
    # SQResiduos
    QMR <- sum((da.new$Concentra - da.new$m)^2) * (20 - 5)^(-1)
    
    # SQA
    ydd <- sum(da.new$Concentra)/20
    QMA <- sum(4 * (da.new$m - ydd)^2)/((5 - 1)*4)
    
    # Estimativa do coeficiente de correlação intraclasse
    rho.est <- (QMA - QMR)/(QMA + (4 -1) * QMR)
    
    # V(rho)
    v.rho <-  (2*(1 - rho.est)^2 * (1 + 3*rho.est)^2)/(3*(20-4))
    
    rho.est.m <- (QMA - QMR)/(QMA + (4/4 -1) * QMR)
    
    da2 <- da %>% filter(Local != 2)
    
    # Sem o local 2
    da.new2 <- plyr::ddply(da2, "Local", transform, m = mean(Concentra))
    
    # SQResiduos
    QMR2 <- sum((da.new2$Concentra - da.new2$m)^2) * (16 - 4)^(-1)
    
    # SQA
    ydd2 <- sum(da.new2$Concentra)/16
    QMA2 <- sum(4 * (da.new2$m - ydd2)^2)/((4 - 1)*4)
    
    # Estimativa do coeficiente de correlação intraclasse
    rho.est2 <- (QMA2 - QMR2)/(QMA2 + (4 -1) * QMR2)
        
    

    output$m1 = renderTable({
      m <-  (input$corr * (1 - rho.est))/(rho.est * (1 - input$corr))
      m2 <-  (input$corr * (1 - rho.est2))/(rho.est2 * (1 - input$corr))
      df <- data.frame("m" = m, 
                       "m-2" = m2)
      
      df
      
    })
    
    
    
  },
  options = list(height = 300)
)