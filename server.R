# server.R
library(shiny)
shinyServer(function(input, output) {
  
  
  # Plotting -- Normal Distribution
  norm <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plotNormal, {
    norm$x <- seq(input$xmin_n, input$xmax_n, by = input$by_n)
    norm$y <- dnorm(norm$x, input$mean, input$sd)
  })
  addNorm1 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, distNames = NULL)
  observeEvent(input$addNormal1, {
    addNorm1$x <- seq(input$xmin_n, input$xmax_n, by = input$by_n)
    addNorm1$y <- dnorm(addNorm1$x, input$mean, input$sd)
    addNorm1$y2 <- dnorm(addNorm1$x, input$mean2_1, input$sd2_1)
    addNorm1$ylim <- max(addNorm1$y, addNorm1$y2)
    addNorm1$distNames <- c(paste("Normal(",input$mean,   ", ",input$sd,   ")", sep=""), 
                            paste("Normal(",input$mean2_1,", ",input$sd2_1,")", sep=""))
  })
  addNorm2 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, distNames = NULL)
  observeEvent(input$addNormal2, {
    addNorm2$x <- seq(input$xmin_n, input$xmax_n, by = input$by_n)
    addNorm2$y <- dnorm(addNorm2$x, input$mean, input$sd)
    addNorm2$y2 <- dnorm(addNorm2$x, input$mean2_2, input$sd2_2)
    addNorm2$y3 <- dnorm(addNorm2$x, input$mean3_2, input$sd3_2)
    addNorm2$ylim <- max(addNorm2$y, addNorm2$y2, addNorm2$y3)
    addNorm2$distNames <- c(paste("Normal(",input$mean,   ", ",input$sd,   ")", sep=""), 
                            paste("Normal(",input$mean2_2,", ",input$sd2_2,")", sep=""),
                            paste("Normal(",input$mean3_2,", ",input$sd3_2,")", sep=""))
  })
  addNorm3 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, 
                             y4 = NULL, distNames = NULL)
  observeEvent(input$addNormal3, {
    addNorm3$x <- seq(input$xmin_n, input$xmax_n, by = input$by_n)
    addNorm3$y <- dnorm(addNorm3$x, input$mean, input$sd)
    addNorm3$y2 <- dnorm(addNorm3$x, input$mean2_3, input$sd2_3)
    addNorm3$y3 <- dnorm(addNorm3$x, input$mean3_3, input$sd3_3)
    addNorm3$y4 <- dnorm(addNorm3$x, input$mean4_3, input$sd4_3)
    addNorm3$ylim <- max(addNorm3$y, addNorm3$y2, addNorm3$y3, addNorm3$y4)
    addNorm3$distNames <- c(paste("Normal(",input$mean,   ", ",input$sd,   ")", sep=""), 
                            paste("Normal(",input$mean2_3,", ",input$sd2_3,")", sep=""),
                            paste("Normal(",input$mean3_3,", ",input$sd3_3,")", sep=""),
                            paste("Normal(",input$mean4_3,", ",input$sd4_3,")", sep=""))
  })
  addNorm4 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, y4 = NULL, y5 = NULL, ylim = NULL, distNames = NULL)
  observeEvent(input$addNormal4, {
    addNorm4$x <- seq(input$xmin_n, input$xmax_n, by = input$by_n)
    addNorm4$y <- dnorm(addNorm4$x, input$mean, input$sd)
    addNorm4$y2 <- dnorm(addNorm4$x, input$mean2_4, input$sd2_4)
    addNorm4$y3 <- dnorm(addNorm4$x, input$mean3_4, input$sd3_4)
    addNorm4$y4 <- dnorm(addNorm4$x, input$mean4_4, input$sd4_4)
    addNorm4$y5 <- dnorm(addNorm4$x, input$mean5_4, input$sd5_4)
    addNorm4$ylim <- max(addNorm4$y, addNorm4$y2, addNorm4$y3, addNorm4$y4, addNorm4$y5)
    addNorm4$distNames <- c(paste("Normal(",input$mean,   ", ",input$sd,   ")", sep=""), 
                            paste("Normal(",input$mean2_4,", ",input$sd2_4,")", sep=""),
                            paste("Normal(",input$mean3_4,", ",input$sd3_4,")", sep=""),
                            paste("Normal(",input$mean4_4,", ",input$sd4_4,")", sep=""),
                            paste("Normal(",input$mean5_4,", ",input$sd5_4,")", sep=""))
  })
  output$normalPlot <- renderPlot(
    {
      
      # Plotting for Single Curves
      if (input$multinormal == FALSE) {
        if (input$plotNormal == 0) {
          # Default Plot
          x <- seq(-4, 4, by = 0.05)
          plot(x, dnorm(x, 0, 1), type = "l", xlab = "x", ylab = "Density")
        }else {
          plot(norm$x, norm$y, type = "l", xlab = "x", ylab = "Density")
        }
      }
      
      # Plotting for multiple curves
      if (input$multinormal == TRUE) {
        
        if (input$addNormal1 == 0 & input$nnormal == "1") {
          if (input$plotNormal == 0) {
            x <- seq(-4, 4, by = 0.05)
            plot(x, dnorm(x, 0, 1), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(norm$x, norm$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nnormal == "1") {
          plot(addNorm1$x, addNorm1$y, type = "l", ylim = c(0, addNorm1$ylim), 
               xlab = "x", ylab = "Density")
          lines(addNorm1$x, addNorm1$y2, type = "l", col = "red")
          legend("topright", legend = addNorm1$distNames, 
                 col = c("black", "red"), lty = 1)
        }
        
        if (input$addNormal2 == 0 & input$nnormal == "2") {
          if (input$plotNormal == 0) {
            x <- seq(-4, 4, by = 0.05)
            plot(x, dnorm(x, 0, 1), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(norm$x, norm$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nnormal == "2") {
          plot(addNorm2$x, addNorm2$y, type = "l", ylim = c(0, addNorm2$ylim), 
               xlab = "x", ylab = "Density")
          lines(addNorm2$x, addNorm2$y2, type = "l", col = "red")
          lines(addNorm2$x, addNorm2$y3, type = "l", col = "blue")
          legend("topright", legend = addNorm2$distNames, 
                 col = c("black", "red", "blue"), lty = 1)
        }
        
        if (input$addNormal3 == 0 & input$nnormal == "3") {
          if (input$plotNormal == 0) {
            x <- seq(-4, 4, by = 0.05)
            plot(x, dnorm(x, 0, 1), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(norm$x, norm$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nnormal == "3") {
          plot(addNorm3$x, addNorm3$y, type = "l", ylim = c(0, addNorm3$ylim), 
               xlab = "x", ylab = "Density")
          lines(addNorm3$x, addNorm3$y2, type = "l", col = "red")
          lines(addNorm3$x, addNorm3$y3, type = "l", col = "blue")
          lines(addNorm3$x, addNorm3$y4, type = "l", col = "green3")
          legend("topright", legend = addNorm3$distNames, 
                 col = c("black", "red", "blue", "green3"), lty = 1)
        }
        
        if (input$addNormal4 == 0 & input$nnormal == "4") {
          if (input$plotNormal == 0) {
            x <- seq(-4, 4, by = 0.05)
            plot(x, dnorm(x, 0, 1), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(norm$x, norm$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nnormal == "4") {
          plot(addNorm4$x, addNorm4$y, type = "l", ylim = c(0, addNorm4$ylim), 
               xlab = "x", ylab = "Density")
          lines(addNorm4$x, addNorm4$y2, type = "l", col = "red")
          lines(addNorm4$x, addNorm4$y3, type = "l", col = "blue")
          lines(addNorm4$x, addNorm4$y4, type = "l", col = "green3")
          lines(addNorm4$x, addNorm4$y5, type = "l", col = "magenta")
          legend("topright", legend = addNorm4$distNames, 
                 col = c("black", "red", "blue", "green3", "magenta"), lty = 1)
        }
      }
    },
  width = 600, height = 600
  )
  
  
  # Plotting -- Beta Distribution
  bet <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plotBeta, {
    bet$x <- seq(0, 1, by = input$by_b)
    bet$y <- dbeta(bet$x, input$alpha, input$beta)
  })
  addBet1 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, ylim = NULL, distNames = NULL)
  observeEvent(input$addBeta1, {
    addBet1$x <- seq(0, 1, by = input$by_b)
    addBet1$y <- dbeta(addBet1$x, input$alpha, input$beta)
    addBet1$y2 <- dbeta(addBet1$x, input$alpha2_1, input$beta2_1)
    addBet1$ylim <- max(addBet1$y, addBet1$y2)
    addBet1$distNames <- c(paste("Beta(",input$alpha,   ", ",input$beta,   ")", sep=""), 
                           paste("Beta(",input$alpha2_1,", ",input$beta2_1,")", sep=""))
  })
  addBet2 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, ylim = NULL, distNames = NULL)
  observeEvent(input$addBeta2, {
    addBet2$x <- seq(0, 1, by = input$by_b)
    addBet2$y <- dbeta(addBet2$x, input$alpha, input$beta)
    addBet2$y2 <- dbeta(addBet2$x, input$alpha2_2, input$beta2_2)
    addBet2$y3 <- dbeta(addBet2$x, input$alpha3_2, input$beta3_2)
    addBet2$ylim <- max(addBet2$y, addBet2$y2, addBet2$y3)
    addBet2$distNames <- c(paste("Beta(",input$alpha,   ", ",input$beta,   ")", sep=""), 
                           paste("Beta(",input$alpha2_2,", ",input$beta2_2,")", sep=""),
                           paste("Beta(",input$alpha3_2,", ",input$beta3_2,")", sep=""))
  })
  addBet3 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, y4 = NULL, ylim = NULL, distNames = NULL)
  observeEvent(input$addBeta3, {
    addBet3$x <- seq(0, 1, by = input$by_b)
    addBet3$y <- dbeta(addBet3$x, input$alpha, input$beta)
    addBet3$y2 <- dbeta(addBet3$x, input$alpha2_3, input$beta2_3)
    addBet3$y3 <- dbeta(addBet3$x, input$alpha3_3, input$beta3_3)
    addBet3$y4 <- dbeta(addBet3$x, input$alpha4_3, input$beta4_3)
    addBet3$ylim <- max(addBet3$y, addBet3$y2, addBet3$y3, addBet3$y4)
    addBet3$distNames <- c(paste("Beta(",input$alpha,   ", ",input$beta,   ")", sep=""), 
                           paste("Beta(",input$alpha2_3,", ",input$beta2_3,")", sep=""),
                           paste("Beta(",input$alpha3_3,", ",input$beta3_3,")", sep=""),
                           paste("Beta(",input$alpha4_3,", ",input$beta4_3,")", sep=""))
  })
  addBet4 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, y4 = NULL, y5 = NULL, ylim = NULL, distNames = NULL)
  observeEvent(input$addBeta4, {
    addBet4$x <- seq(0, 1, by = input$by_b)
    addBet4$y <- dbeta(addBet4$x, input$alpha, input$beta)
    addBet4$y2 <- dbeta(addBet4$x, input$alpha2_4, input$beta2_4)
    addBet4$y3 <- dbeta(addBet4$x, input$alpha3_4, input$beta3_4)
    addBet4$y4 <- dbeta(addBet4$x, input$alpha4_4, input$beta4_4)
    addBet4$y5 <- dbeta(addBet4$x, input$alpha5_4, input$beta5_4)
    addBet4$ylim <- max(addBet4$y, addBet4$y2, addBet4$y3, addBet4$y4, addBet4$y5)
    addBet4$distNames <- c(paste("Beta(",input$alpha,   ", ",input$beta,   ")", sep=""), 
                           paste("Beta(",input$alpha2_4,", ",input$beta2_4,")", sep=""),
                           paste("Beta(",input$alpha3_4,", ",input$beta3_4,")", sep=""),
                           paste("Beta(",input$alpha4_4,", ",input$beta4_4,")", sep=""),
                           paste("Beta(",input$alpha5_4,", ",input$beta5_4,")", sep=""))
  })
  output$betaPlot <- renderPlot(
    {
      
      # Plotting for Single Curves
      if (input$multibeta == FALSE) {
        if (input$plotBeta == 0) {
          # Default Plot
          x <- seq(0, 1, by = 0.005)
          plot(x, dbeta(x, 2, 5), type = "l", xlab = "x", ylab = "Density")
        }else {
          plot(bet$x, bet$y, type = "l", xlab = "x", ylab = "Density")
        }
      }
      
      # Plotting for multiple curves
      if (input$multibeta == TRUE) {
        if (input$addBeta1 == 0 & input$nbeta == "1") {
          if (input$plotBeta == 0) {
            x <- seq(0, 1, by = 0.005)
            plot(x, dbeta(x, 2, 5), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(bet$x, bet$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nbeta == "1") {
          plot(addBet1$x, addBet1$y, type = "l", ylim = c(0, addBet1$ylim), 
               xlab = "x", ylab = "Density")
          lines(addBet1$x, addBet1$y2, type = "l", col = "red")
          legend("topright", legend = addBet1$distNames, 
                 col = c("black", "red"), lty = 1)
        }
        
        if (input$addBeta2 == 0 & input$nbeta == "2") {
          if (input$plotBeta == 0) {
            x <- seq(0, 1, by = 0.005)
            plot(x, dbeta(x, 2, 5), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(bet$x, bet$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nbeta == "2") {
          plot(addBet2$x, addBet2$y, type = "l", ylim = c(0, addBet2$ylim), 
               xlab = "x", ylab = "Density")
          lines(addBet2$x, addBet2$y2, type = "l", col = "red")
          lines(addBet2$x, addBet2$y3, type = "l", col = "blue")
          legend("topright", legend = addBet2$distNames, 
                 col = c("black", "red", "blue"), lty = 1)
        }
        
        if (input$addBeta3 == 0 & input$nbeta == "3") {
          if (input$plotBeta == 0) {
            x <- seq(0, 1, by = 0.005)
            plot(x, dbeta(x, 2, 5), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(bet$x, bet$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nbeta == "3") {
          plot(addBet3$x, addBet3$y, type = "l", ylim = c(0, addBet3$ylim), 
               xlab = "x", ylab = "Density")
          lines(addBet3$x, addBet3$y2, type = "l", col = "red")
          lines(addBet3$x, addBet3$y3, type = "l", col = "blue")
          lines(addBet3$x, addBet3$y4, type = "l", col = "green3")
          legend("topright", legend = addBet3$distNames, 
                 col = c("black", "red", "blue", "green3"), lty = 1)
        }
        
        if (input$addBeta4 == 0 & input$nbeta == "4") {
          if (input$plotBeta == 0) {
            x <- seq(0, 1, by = 0.005)
            plot(x, dbeta(x, 2, 5), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(bet$x, bet$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nbeta == "4") {
          plot(addBet4$x, addBet4$y, type = "l", ylim = c(0, addBet4$ylim), 
               xlab = "x", ylab = "Density")
          lines(addBet4$x, addBet4$y2, type = "l", col = "red")
          lines(addBet4$x, addBet4$y3, type = "l", col = "blue")
          lines(addBet4$x, addBet4$y4, type = "l", col = "green3")
          lines(addBet4$x, addBet4$y5, type = "l", col = "magenta")
          legend("topright", legend = addBet4$distNames, 
                 col = c("black", "red", "blue", "green3", "magenta"), lty = 1)
        }
      }
    },
    width = 600, height = 600
  )
  

  # Plotting -- Gamma Distribution
  gam <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plotGamma, {
    gam$x <- seq(0, input$xmax_g, by = input$by_g)
    gam$y <- dgamma(gam$x, input$k, input$b)
  })
  addGam1 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, ylim = NULL, distNames = NULL)
  observeEvent(input$addGamma1, {
    addGam1$x <- seq(0, input$xmax_g, by = input$by_g)
    addGam1$y <- dgamma(addGam1$x, input$k, input$b)
    addGam1$y2 <- dgamma(addGam1$x, input$k2_1, input$b2_1)
    addGam1$ylim <- max(addGam1$y, addGam1$y2)
    addGam1$distNames <- c(paste("Gamma(",input$k,   ", ",input$b,   ")", sep=""), 
                           paste("Gamma(",input$k2_1,", ",input$b2_1,")", sep=""))
  })
  addGam2 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, ylim = NULL, distNames = NULL)
  observeEvent(input$addGamma2, {
    addGam2$x <- seq(0, input$xmax_g, by = input$by_g)
    addGam2$y <- dgamma(addGam2$x, input$k, input$b)
    addGam2$y2 <- dgamma(addGam2$x, input$k2_2, input$b2_2)
    addGam2$y3 <- dgamma(addGam2$x, input$k3_2, input$b3_2)
    addGam2$ylim <- max(addGam2$y, addGam2$y2, addGam2$y3)
    addGam2$distNames <- c(paste("Gamma(",input$k,   ", ",input$b,   ")", sep=""), 
                           paste("Gamma(",input$k2_2,", ",input$b2_2,")", sep=""),
                           paste("Gamma(",input$k3_2,", ",input$b3_2,")", sep=""))
  })
  addGam3 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, y4 = NULL, ylim = NULL, distNames = NULL)
  observeEvent(input$addGamma3, {
    addGam3$x <- seq(0, input$xmax_g, by = input$by_g)
    addGam3$y <- dgamma(addGam3$x, input$k, input$b)
    addGam3$y2 <- dgamma(addGam3$x, input$k2_3, input$b2_3)
    addGam3$y3 <- dgamma(addGam3$x, input$k3_3, input$b3_3)
    addGam3$y4 <- dgamma(addGam3$x, input$k4_3, input$b4_3)
    addGam3$ylim <- max(addGam3$y, addGam3$y2, addGam3$y3, addGam3$y4)
    addGam3$distNames <- c(paste("Gamma(",input$k,   ", ",input$b,   ")", sep=""), 
                           paste("Gamma(",input$k2_3,", ",input$b2_3,")", sep=""),
                           paste("Gamma(",input$k3_3,", ",input$b3_3,")", sep=""),
                           paste("Gamma(",input$k4_3,", ",input$b4_3,")", sep=""))
  })
  addGam4 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, y4 = NULL, y5 = NULL, ylim = NULL, distNames = NULL)
  observeEvent(input$addGamma4, {
    addGam4$x <- seq(0, input$xmax_g, by = input$by_g)
    addGam4$y <- dgamma(addGam4$x, input$k, input$b)
    addGam4$y2 <- dgamma(addGam4$x, input$k2_4, input$b2_4)
    addGam4$y3 <- dgamma(addGam4$x, input$k3_4, input$b3_4)
    addGam4$y4 <- dgamma(addGam4$x, input$k4_4, input$b4_4)
    addGam4$y5 <- dgamma(addGam4$x, input$k5_4, input$b5_4)
    addGam4$ylim <- max(addGam4$y, addGam4$y2, addGam4$y3, addGam4$y4, addGam4$y5)
    addGam4$distNames <- c(paste("Gamma(",input$k,   ", ",input$b,   ")", sep=""), 
                           paste("Gamma(",input$k2_4,", ",input$b2_4,")", sep=""),
                           paste("Gamma(",input$k3_4,", ",input$b3_4,")", sep=""),
                           paste("Gamma(",input$k4_4,", ",input$b4_4,")", sep=""),
                           paste("Gamma(",input$k5_4,", ",input$b5_4,")", sep=""))
  })
  output$gammaPlot <- renderPlot(
    {
      
      # Plotting for Single Curves
      if (input$multigamma == FALSE) {
        if (input$plotGamma == 0) {
          # Default Plot
          x <- seq(0, 10, by = 0.05)
          plot(x, dgamma(x, 2, 2), type = "l", xlab = "x", ylab = "Density")
        }else {
          plot(gam$x, gam$y, type = "l", xlab = "x", ylab = "Density")
        }
      }
      
      # Plotting for multiple curves
      if (input$multigamma == TRUE) {
        if (input$addGamma1 == 0 & input$ngamma == "1") {
          if (input$plotGamma == 0) {
            x <- seq(0, 10, by = 0.05)
            plot(x, dgamma(x, 2, 2), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(gam$x, gam$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$ngamma == "1") {
          plot(addGam1$x, addGam1$y, type = "l", ylim = c(0, addGam1$ylim), 
               xlab = "x", ylab = "Density")
          lines(addGam1$x, addGam1$y2, type = "l", col = "red")
          legend("topright", legend = addGam1$distNames, 
                 col = c("black", "red"), lty = 1)
        }
        
        if (input$addGamma2 == 0 & input$ngamma == "2") {
          if (input$plotGamma == 0) {
            x <- seq(0, 10, by = 0.05)
            plot(x, dgamma(x, 2, 2), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(gam$x, gam$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$ngamma == "2") {
          plot(addGam2$x, addGam2$y, type = "l", ylim = c(0, addGam2$ylim), 
               xlab = "x", ylab = "Density")
          lines(addGam2$x, addGam2$y2, type = "l", col = "red")
          lines(addGam2$x, addGam2$y3, type = "l", col = "blue")
          legend("topright", legend = addGam2$distNames, 
                 col = c("black", "red", "blue"), lty = 1)
        }
        
        if (input$addGamma3 == 0 & input$ngamma == "3") {
          if (input$plotGamma == 0) {
            x <- seq(0, 10, by = 0.05)
            plot(x, dgamma(x, 2, 2), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(gam$x, gam$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$ngamma == "3") {
          plot(addGam3$x, addGam3$y, type = "l", ylim = c(0, addGam3$ylim), 
               xlab = "x", ylab = "Density")
          lines(addGam3$x, addGam3$y2, type = "l", col = "red")
          lines(addGam3$x, addGam3$y3, type = "l", col = "blue")
          lines(addGam3$x, addGam3$y4, type = "l", col = "green3")
          legend("topright", legend = addGam3$distNames, 
                 col = c("black", "red", "blue", "green3"), lty = 1)
        }
        
        if (input$addGamma4 == 0 & input$ngamma == "4") {
          if (input$plotGamma == 0) {
            x <- seq(0, 10, by = 0.05)
            plot(x, dgamma(x, 2, 2), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(gam$x, gam$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$ngamma == "4") {
          plot(addGam4$x, addGam4$y, type = "l", ylim = c(0, addGam4$ylim), 
               xlab = "x", ylab = "Density")
          lines(addGam4$x, addGam4$y2, type = "l", col = "red")
          lines(addGam4$x, addGam4$y3, type = "l", col = "blue")
          lines(addGam4$x, addGam4$y4, type = "l", col = "green3")
          lines(addGam4$x, addGam4$y5, type = "l", col = "magenta")
          legend("topright", legend = addGam4$distNames, 
                 col = c("black", "red", "blue", "green3", "magenta"), lty = 1)
        }
      }
    },
    width = 600, height = 600
  )
  

  # Plotting -- Exponential Distribution
  expo <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plotExponential, {
    expo$x <- seq(0, input$xmax_e, by = input$by_e)
    expo$y <- dexp(expo$x, rate = input$rate)
  })
  addExp1 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, 
                            ylim = NULL, distNames = NULL)
  observeEvent(input$addExponential1, {
    addExp1$x <- seq(0, input$xmax_e, by = input$by_e)
    addExp1$y <- dexp(addExp1$x, rate = input$rate)
    addExp1$y2 <- dexp(addExp1$x, rate = input$rate2_1)
    addExp1$ylim <- max(addExp1$y, addExp1$y2)
    addExp1$distNames <- c(paste("Exponential(",input$rate,   ")", sep=""), 
                           paste("Exponential(",input$rate2_1,")", sep=""))
  })
  addExp2 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, 
                            ylim = NULL, distNames = NULL)
  observeEvent(input$addExponential2, {
    addExp2$x <- seq(0, input$xmax_e, by = input$by_e)
    addExp2$y <- dexp(addExp2$x, rate = input$rate)
    addExp2$y2 <- dexp(addExp2$x, rate = input$rate2_2)
    addExp2$y3 <- dexp(addExp2$x, rate = input$rate3_2)
    addExp2$ylim <- max(addExp2$y, addExp2$y2, addExp2$y3)
    addExp2$distNames <- c(paste("Exponential(", input$rate,   ")", sep=""), 
                           paste("Exponential(", input$rate2_2,")", sep=""),
                           paste("Exponential(", input$rate3_2,")", sep=""))
  })
  addExp3 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, 
                            y4 = NULL, ylim = NULL, distNames = NULL)
  observeEvent(input$addExponential3, {
    addExp3$x <- seq(0, input$xmax_e, by = input$by_e)
    addExp3$y <- dexp(addExp3$x, rate = input$rate)
    addExp3$y2 <- dexp(addExp3$x, rate = input$rate2_3)
    addExp3$y3 <- dexp(addExp3$x, rate = input$rate3_3)
    addExp3$y4 <- dexp(addExp3$x, rate = input$rate4_3)
    addExp3$ylim <- max(addExp3$y, addExp3$y2, addExp3$y3, addExp3$y4)
    addExp3$distNames <- c(paste("Exponential(",input$rate,   ")", sep=""), 
                           paste("Exponential(",input$rate2_3,")", sep=""),
                           paste("Exponential(",input$rate3_3,")", sep=""),
                           paste("Exponential(",input$rate4_3,")", sep=""))
  })
  addExp4 <- reactiveValues(x = NULL, y = NULL, y2 = NULL, y3 = NULL, 
                            y4 = NULL, y5 = NULL, ylim = NULL, 
                            distNames = NULL)
  observeEvent(input$addExponential4, {
    addExp4$x <- seq(0, input$xmax_e, by = input$by_e)
    addExp4$y <- dexp(addExp4$x, rate = input$rate)
    addExp4$y2 <- dexp(addExp4$x, rate = input$rate2_4)
    addExp4$y3 <- dexp(addExp4$x, rate = input$rate3_4)
    addExp4$y4 <- dexp(addExp4$x, rate = input$rate4_4)
    addExp4$y5 <- dexp(addExp4$x, rate = input$rate5_4)
    addExp4$ylim <- max(addExp4$y, addExp4$y2, addExp4$y3, addExp4$y4, addExp4$y5)
    addExp4$distNames <- c(paste("Exponential(",input$rate,   ")", sep=""), 
                           paste("Exponential(",input$rate2_4,")", sep=""),
                           paste("Exponential(",input$rate3_4,")", sep=""),
                           paste("Exponential(",input$rate4_4,")", sep=""),
                           paste("Exponential(",input$rate5_4,")", sep=""))
  })
  output$exponentialPlot <- renderPlot(
    {
      
      # Plotting for Single Curves
      if (input$multiexponential == FALSE) {
        if (input$plotExponential == 0) {
          # Default Plot
          x <- seq(0, 5, by = 0.05)
          plot(x, dexp(x, rate = 1), type = "l", xlab = "x", ylab = "Density")
        }else {
          plot(expo$x, expo$y, type = "l", xlab = "x", ylab = "Density")
        }
      }
      
      # Plotting for multiple curves
      if (input$multiexponential == TRUE) {
        if (input$addExponential1 == 0 & input$nexponential == "1") {
          if (input$plotExponential == 0) {
            x <- seq(0, 5, by = 0.05)
            plot(x, dexp(x, rate = 1), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(expo$x, expo$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nexponential == "1") {
          plot(addExp1$x, addExp1$y, type = "l", ylim = c(0, addExp1$ylim), 
               xlab = "x", ylab = "Density")
          lines(addExp1$x, addExp1$y2, type = "l", col = "red")
          legend("topright", legend = addExp1$distNames, 
                 col = c("black", "red"), lty = 1)
        }
        
        if (input$addExponential2 == 0 & input$nexponential == "2") {
          if (input$plotExponential == 0) {
            x <- seq(0, 5, by = 0.05)
            plot(x, dexp(x, rate = 1), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(expo$x, expo$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nexponential == "2") {
          plot(addExp2$x, addExp2$y, type = "l", ylim = c(0, addExp2$ylim), 
               xlab = "x", ylab = "Density")
          lines(addExp2$x, addExp2$y2, type = "l", col = "red")
          lines(addExp2$x, addExp2$y3, type = "l", col = "blue")
          legend("topright", legend = addExp2$distNames, 
                 col = c("black", "red", "blue"), lty = 1)
        }
        
        if (input$addExponential3 == 0 & input$nexponential == "3") {
          if (input$plotExponential == 0) {
            x <- seq(0, 5, by = 0.05)
            plot(x, dexp(x, rate = 1), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(expo$x, expo$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nexponential == "3") {
          plot(addExp3$x, addExp3$y, type = "l", ylim = c(0, addExp3$ylim), 
               xlab = "x", ylab = "Density")
          lines(addExp3$x, addExp3$y2, type = "l", col = "red")
          lines(addExp3$x, addExp3$y3, type = "l", col = "blue")
          lines(addExp3$x, addExp3$y4, type = "l", col = "green3")
          legend("topright", legend = addExp3$distNames, 
                 col = c("black", "red", "blue", "green3"), lty = 1)
        }
        
        if (input$addExponential4 == 0 & input$nexponential == "4") {
          if (input$plotExponential == 0) {
            x <- seq(0, 5, by = 0.05)
            plot(x, dexp(x, rate = 1), type = "l", xlab = "x", ylab = "Density")
          }else {
            plot(expo$x, expo$y, type = "l", xlab = "x", ylab = "Density")
          }
        }else if (input$nexponential == "4") {
          plot(addExp4$x, addExp4$y, type = "l", ylim = c(0, addExp4$ylim), 
               xlab = "x", ylab = "Density")
          lines(addExp4$x, addExp4$y2, type = "l", col = "red")
          lines(addExp4$x, addExp4$y3, type = "l", col = "blue")
          lines(addExp4$x, addExp4$y4, type = "l", col = "green3")
          lines(addExp4$x, addExp4$y5, type = "l", col = "magenta")
          legend("topright", legend = addExp4$distNames, 
                 col = c("black", "red", "blue", "green3", "magenta"), lty = 1)
        }
      }
    },
    width = 600, height = 600
  )
  
  
  
  

    
})
