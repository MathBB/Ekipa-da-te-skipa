library("Rcpp")
library("shiny")
library("reshape2")
library("plyr")
library("ggplot2")
library("PerformanceAnalytics")
library("grid")

#################################################################################################################################   
source("poprava.r")
source("priklop_na_bazo.r")
source("razvrscanjeFF_C.r")
source("razvrscanjeQ.r")
source("ff.factors.r")
source("car.factors.r")
#################################################################################################################################   
#################################################################################################################################   


shinyServer(function(input, output) {
  
#################################################################################################################################   
  var <- reactive({
    switch(input$select2,
           "Vnesi st. posameznih delnic" = c(0, 500, 100, 1),
           "Vnesi utezi" = c(0, 1, 0.5, 0.05)
    )
  })
  
  output$izbira0 <- renderUI({
    switch(input$select2,
           "Vnesi st. posameznih delnic" = helpText("S spreminjanjem koli?ine delnic spreminjate ute?i !"),
           "Vnesi utezi" = helpText("Pozor! Utezi se morajo sesteti v 1! ")
    )
  })
  
  output$izbira1 <- renderUI({
    
    lapply(1:length(input$select), function(i) {
      sliderInput(inputId = paste0("slidr1", i), label = input$select[i], min = var()[1], max = var()[2], value = var()[3], step = var()[4] )
    })
  })
  #################################################################################################################################  
  output$izbira3 <- renderUI({
    
    lapply(1:length(input$select3), function(i) {
      sliderInput(inputId = paste0("slidr2", i), label = input$select3[i], min = var()[1], max = var()[2], value = var()[3], step = var()[4] )
    })
  })
  #################################################################################################################################
  racunanje_utezi1 <- reactive ({
    switch(input$select2,
           "Vnesi utezi"= {
             utez <-racunanje()$utezi1
             utez
           },
           
           "Vnesi st. posameznih delnic" = {
             vrednost_nalozb_1 <- tail(sweep(racunanje()$cene1, MARGIN = 2, racunanje()$nakup1,"*"),1)
             zadnji_dan <- as.numeric(tail(apply.daily(vrednost_nalozb_1, sum),1))
             utez <- vrednost_nalozb_1/coredata(zadnji_dan)
             utez
           }  
    )
  })
  #################################################################################################################################
  racunanje_utezi2 <- reactive ({
    switch(input$select2,
           "Vnesi utezi"= {
             utez <-racunanje()$utezi2
             utez
           },
           
           "Vnesi st. posameznih delnic" = {
             vrednost_nalozb_2 <- tail(sweep(racunanje()$cene2, MARGIN = 2, racunanje()$nakup2,"*"),1)
             zadnji_dan <- as.numeric(tail(apply.daily(vrednost_nalozb_2, sum),1))
             utez <- vrednost_nalozb_2/coredata(zadnji_dan)
             utez
           }  
    )
  })
  #################################################################################################################################
  histogram_podatki <- reactive({
    
    switch(input$select2,
           "Vnesi utezi"= {
             
             ggdata <- data.frame(racunanje()$portfelj1, racunanje()$portfelj2)
             
             ggdata
           },
           
           "Vnesi st. posameznih delnic" = {
             
             portfelj_1 <- na.omit(Return.calculate(racunanje()$skupna_vrednost_nalozbe1, method = "discrete"))
             portfelj_2 <- na.omit(Return.calculate(racunanje()$skupna_vrednost_nalozbe2, method = "discrete"))
             ggdata <- data.frame(portfelj_1, portfelj_2)
             
             ggdata 
           }  
    )
  })
  #################################################################################################################################
  
  #################################################################################################################################
  var3 <- reactive({
    switch(input$select2,
           "Vnesi utezi"= {
             
             portfelj_1 <- Return.portfolio(na.omit(racunanje()$donosi1), weights = racunanje()$utezi1)
             portfelj_2 <- Return.portfolio(na.omit(racunanje()$donosi2), weights = racunanje()$utezi2)
             chart.CumReturns(cbind(portfelj_1,portfelj_2))
             
           },
           
           "Vnesi st. posameznih delnic" = {
             
             chart.TimeSeries(cbind(racunanje()$skupna_vrednost_nalozbe1, racunanje()$skupna_vrednost_nalozbe2), main = "Priverjava portfeljev")
             legend("topleft", c("Portfelj_1","Portfelj_2"), pch = 1, title = "Legenda", col = c("black","red"))
             #plot(skupna_vrednost_nalozb)
           }       
    )
  })
  
  
  output$portfolio_1 <- renderPlot({   
    var3()  
  })
  #########################################################################################################
  funkcije <- list(
    "1" = function(x) geom_vline(xintercept= VaR(x, p = 0.99), linetype="dashed", colour= "red"),
    "2" = function(x) geom_vline(xintercept= ES(x, p = 0.999) , linetype="dashed", colour="purple"),
    "3" = function(x) geom_vline(xintercept= c(mean(x) - sd(x), mean(x) + sd(x)) , linetype="longdash", colour="green"),
    "4" = function (x) geom_vline(xintercept= mean(x) , linetype="solid", colour="blue")
  )
  #########################################################################################################
  funkcije1 <- list(
    "1" = function(x) geom_vline(xintercept= VaR(x, p = 0.99) , linetype="dashed", colour= "darkred"),
    "2" = function(x) geom_vline(xintercept= ES(x, p = 0.999) , linetype="dashed", colour="blueviolet"),
    "3" = function(x) geom_vline(xintercept= c(mean(x) - sd(x), mean(x) + sd(x)) , linetype="longdash", colour="darkgreen"),
    "4" = function (x) geom_vline(xintercept= mean(x) , linetype="solid", colour="black")
  )
  #########################################################################################################
  output$histogram <- renderPlot({
    bins <- seq(min(min(histogram_podatki()[,1]),min(histogram_podatki()[,2])), max(max(histogram_podatki()[,1]),max(histogram_podatki()[,2])), length.out = input$bins + 1)
    ggdata <- melt(histogram_podatki())
    
    # Set the data frame, & add ecdf() data.
    ggdata <- ddply(ggdata, .(variable), transform, ecd=ecdf(value)(value))
    
    hist <- ggplot(ggdata, aes(x=value, fill=variable)) + geom_histogram(alpha=0.4, position="identity", aes(y=..density..), breaks = bins) + 
            geom_density(alpha=0.2)+ labs(title="Histograma dnevnih donosov portfeljev") + labs(x="Dnevni donosi", y="Frequency") +  scale_x_continuous(breaks= seq(-0.3, 0.3, by = 0.02))  
    
    izbrani1 = as.numeric(input$checkGroup1)
    izbrani2 = as.numeric(input$checkGroup2)    

  for(i in izbrani1){ 
    hist <- hist + funkcije[[i]](histogram_podatki()[,1])
  }
  for(j in izbrani2){ 
    hist <- hist + funkcije1[[j]](histogram_podatki()[,2])
  }

  hist  
  })
  
  #############################################################################################
  racunanje <- reactive({
    
    
    data.portfolio1<-na.omit(ClosePrices[,input$select])
    data.portfolio2<-na.omit(ClosePrices[,input$select3])
    donosi_1 <- Return.calculate(data.portfolio1, method = "discrete")
    donosi_2 <- Return.calculate(data.portfolio2, method = "discrete")
    utezi_1 <- sapply(1:length(input$select), function(i) {
      as.numeric(input[[paste0("slidr1", i)]])})
    utezi_2 <- sapply(1:length(input$select3), function(i) {
      as.numeric(input[[paste0("slidr2", i)]])})
    nakup_1 <- sapply(1:length(input$select), function(i) {
      as.numeric(input[[paste0("slidr1", i)]])})
    nakup_2 <- sapply(1:length(input$select3), function(i) {
      as.numeric(input[[paste0("slidr2", i)]])})
    portfelj_1 <- Return.portfolio(na.omit(donosi_1), weights = utezi_1)
    portfelj_2 <- Return.portfolio(na.omit(donosi_2), weights = utezi_2)
    volatilnost1 <- apply(data.frame(coredata(na.omit(donosi_1))), 2, sd)
    volatilnost2 <- apply(data.frame(coredata(na.omit(donosi_2))), 2, sd)
    vrednost_nalozb_1 <- sweep(data.portfolio1, MARGIN = 2, nakup_1,"*")
    vrednost_nalozb_2 <- sweep(data.portfolio2, MARGIN = 2, nakup_2,"*")
    skupna_vrednost_nalozbe1 <- apply.daily(vrednost_nalozb_1,sum)
    skupna_vrednost_nalozbe2 <- apply.daily(vrednost_nalozb_2,sum)
    
    list(cene1 = data.portfolio1,
         cene2 = data.portfolio2,
         donosi1 = donosi_1,
         donosi2 = donosi_2,
         utezi1 = utezi_1,
         utezi2 = utezi_2,
         nakup1 = nakup_1,
         nakup2 = nakup_2,
         portfelj1 = portfelj_1,
         portfelj2 = portfelj_2,
         volatilnost1 = volatilnost1,
         volatilnost2 = volatilnost2,
         vrednost_nalozb_1 = vrednost_nalozb_1,
         vrednost_nalozb_2 = vrednost_nalozb_2,
         skupna_vrednost_nalozbe1 = skupna_vrednost_nalozbe1,
         skupna_vrednost_nalozbe2 = skupna_vrednost_nalozbe2)
    
    
  })
  #############################################################################################
  tabela1 <- reactive({
    
    # Naredi data frame
    data.frame(
      Delnice_Portfelj1 = colnames(ClosePrices[,input$select]),
      Povprecni_letni_donos = as.character(round(Return.annualized(racunanje()$donosi1), digits = 4)),
      Utezi = as.character(round(racunanje_utezi1(), digits = 4)),
      Volatilnost = as.character(round(racunanje()$volatilnost1, digits =4)),
      stringsAsFactors=FALSE)
  })
  output$tabela_statistik1 <- renderDataTable({
    
    tabela1()
  })
  ########################################################################################################
  tabela2 <- reactive({
    
    # Naredi data frame
    data.frame(
      Delnice_Portfelj2 = colnames(ClosePrices[,input$select3]),
      Povprecni_letni_donos = as.character(round(Return.annualized(racunanje()$donosi2), digits = 4)),
      Utezi = as.character(round(racunanje_utezi2(), digits = 4)),
      Volatilnost = as.character(round(racunanje()$volatilnost2, digits =4)),
      stringsAsFactors=FALSE)
  })
  output$tabela_statistik2 <- renderDataTable({
    
    tabela2()
  })
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  #########################################################################################################
  #Regresija za faktorske modele
  #########################################################################################################

  priprava <- reactive({
#     portfelja <- cbind(racunanje()$portfelj1,racunanje()$portfelj2)
#     df.portfelja <- data.frame(coredata(portfelja))
#     colnames(df.portfelja) = c("Portfelj1", "Portfelj2")
#     data_za_regresije <- cbind(df.portfelja, faktorjiCar)
#     data_za_regresije[-1,]
       data <- data.frame(
                     Portfelj1 = racunanje()$portfelj1,
                     Portfelj2 = racunanje()$portfelj2,
                     MKT = faktorjiCar$MKT,
                     SMB = faktorjiCar$SMB,
                     HML = faktorjiCar$HML,
                     WML = faktorjiCar$WML,
                     risk_free_r = faktorjiCar$risk_free_r)
       #names(data) <- c("Portfelj1","Portfelj2", "MKT", "SMB", "HML", "WML") 
       names(data)[names(data)=="portfolio.returns"] <- "Portfelj1"
       names(data)[names(data)=="portfolio.returns.1"] <- "Portfelj2"
       data[-1,]
  })

  
  
#   output$independent <- renderUI({
#     checkboxGroupInput("independent", "Independent Variables:", colnames(priprava())[!colnames(priprava()) %in% input$dependent1], colnames(priprava())[!colnames(priprava()) %in% input$dependent1])
#   })

  runRegression <- reactive({
    lm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))), data = priprava())
  })
  
  output$regTab <- renderTable({
    if(!is.null(input$independent)){
      summary(runRegression())$coefficients
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }
  })
  

  output$priprava_podatkov <- renderDataTable({
    priprava()
  })
  #########################################################################################################
  # Create a reactive text
  text <- reactive({
    paste(input$variable1, 'VS.', input$variable2)
  })
  
  text1 <- reactive({
    paste(input$variable3, 'VS.', input$variable4)
  })
  
  # Return as text the selected variables
  output$caption <- renderText({
    text()
  })
  
  output$caption1 <- renderText({
    text1()
  })
  
  # Generate a plot of the requested variables
  output$plot1 <- renderPlot({
    p <- ggplot(priprava(), aes_string(x=input$variable1, y=input$variable2)) + geom_point(shape = 1) + geom_smooth(method=lm, formula = as.formula(input$fit1))
    print(p)
  })
  
  output$plot2 <- renderPlot({
    p <- ggplot(priprava(), aes_string(x=input$variable3, y=input$variable4)) + geom_point(shape = 1) + geom_smooth(method=lm, formula = as.formula(input$fit2))
    print(p)
  })
  
  output$independent <- renderUI({
    checkboxGroupInput("independent", "Independent Variables:",names(dat)[!names(dat) %in% input$dependent],names(dat)[!names(dat) %in% input$dependent])
  })
  
  
  runRegression <- reactive({
    lm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))),data= priprava())
  })
  
  mojaRegresija <- reactive({

    lm(as.formula(input$regresija), data = priprava())
    #I(x+z)
    #I(z^2)
  })
  
  output$regTab <- renderTable({
    if(!is.null(input$independent)){
      summary(runRegression())$coefficients
    } else {
      print(data.frame(Warning="Prosim izberite parametre modela."))
    }
  })
  
  output$test <- renderTable({
    if (inherits( try( summary(mojaRegresija())$coefficients, silent=TRUE),  "try-error")){
      print(data.frame(Warning="Prosim napi?ite svojo regresijsko formulo."))
    }
    else{
      summary(mojaRegresija())$coefficients
    }
  })
  
  output$value <- renderPrint({ input$regresija })

  #########################################################################################################
  
  #########################################################################################################
  portfolioSdDecomposition <- function(w.vec, cov.assets) {
    ## Inputs:
    ## w.vec         n x 1 vector of portfolio weights
    ## cov.assets     n x n asset covariance matrix
    ## Output:
    ## A list with the following components:
    ## sd.p          scalar, portfolio sd
    ## mcsd.p        1 x n vector, marginal contributions to portfolio sd
    ## csd.p         1 x n vector, contributions to portfolio sd
    ## pcsd.p        1 x n vector, percent contribution to portfolio sd
    
    if (any(diag(chol(cov.assets)) == 0))
      warning("Asset covariance matrix is not positive definite")
    ## compute portfolio level variance
    var.p = as.numeric(t(w.vec) %*% cov.assets %*% w.vec)
    sd.p = sqrt(var.p)
    ## compute marginal, component and percentage contributions to risk
    mcsd.p = (cov.assets %*% w.vec)/sd.p
    csd.p = w.vec*mcsd.p
    pcsd.p = csd.p/sd.p
    colnames(mcsd.p) = "MCSD"
    colnames(csd.p) = "CSD"
    colnames(pcsd.p) = "PCSD"
    ## return results
    ans = list(sd.p=sd.p,
               mcsd.p=t(mcsd.p),
               csd.p=t(csd.p),
               pcsd.p=t(pcsd.p))
    return(ans)
  }
  #########################################################################################################
  
#   graf_contribution <- reactive({
#     
#     izbrani_faktorji <- faktorjiCar[,input$independent]
#     data1 <- cbind(racunanje()$donosi_1, izbrani_faktorji)
#     data1 = data1[-1,]
#     data2 <- cbind(racunanje()$donosi_2, izbrani_faktorji)
#     data2 = data2[-1,]
#     
#     # create data frame for regression analysis
#   
#     # with xts objects, you extract data using coredata() and you extract
#     # dates with index()
#     data.df1 = as.data.frame(coredata(data1))
#     data.df2 = as.data.frame(coredata(data2))
#     rownames(data.df1) = as.character(index(data1))
#     rownames(data.df2) = as.character(index(data2))
#     
#     
#     # subtract "US 3m TR" (risk free rate) from all returns. note: apply() changes
#     # data.df to class "matrix" to coerce result back to data.frame
#     if ( is.element("risk_free_r", colnames(head(faktorjiCar))) ){
#     
#       data.df1 = apply(data.df1, 2,
#                        function(x) {x - data.df1[,"risk_free_r"]})
#       data.df1 = as.data.frame(data.df1)
#       
#       data.df2 = apply(data.df2, 2,
#                        function(x) {x - data.df2[,"risk_free_r"]})
#       data.df2 = as.data.frame(data.df2)
#       
#       # remove US 3m TR from data.frame
#       data.df1 = data.df1[, -ncol(data.df1)]
#       data.df2 = data.df2[, -ncol(data.df2)]
#   
#       st.faktorjev = length(input$independent)- 1
#     }
#     
#     else{
#       
#       st.faktorjev = length(input$independent)
#     }
#     
#     data.names1 = colnames(data.df1)[1:(ncol(data.df1) - (st.faktorjev))]
#     data.names2 = colnames(data.df2)[1:(ncol(data.df2) - (st.faktorjev))]
#     
#     # eliminate spaces in factor names which cause problems later if not removed
#     factor.names1 = colnames(data.df1)[((ncol(data.df1) - st.faktorjev)+1): ncol(data.df1)]
#     factor.names2 = colnames(data.df2)[((ncol(data.df2) - st.faktorjev)+1): ncol(data.df2)]
#     
#     
#     # initialize list object to hold regression objects
#     reg.list1 = list()
#     # initialize matrices and vectors to hold estimated betas,
#     # residual variances, and R-square values from
#     # fitted factor models
#     Betas1 = matrix(0, length(data.names1), length(factor.names1))
#     colnames(Betas1) = factor.names1
#     rownames(Betas1) = data.names1
#     Alphas1 = ResidVars1 = R2values1 = rep(0, length(data.names1))
#     names(Alphas1) = names(ResidVars1) = names(R2values1) = data.names1
#     
#     # loop over all assets and estimate time series regression
#     for (i in data.names1) {
#       reg.df = data.df1[, c(i, factor.names1)]
#       fm.formula = as.formula(paste(i,"~", ".", sep=" ")) #npr ("HAM1 ~ .") tale pika pomen po vseh stolpcih
#       fm.fit = lm(fm.formula, data=reg.df)
#       fm.summary = summary(fm.fit)
#       reg.list1[[i]] = fm.fit
#       Alphas1[i] = coef(fm.fit)[1]
#       Betas1[i, ] = coef(fm.fit)[-1]
#       ResidVars1[i] = fm.summary$sigma^2
#       R2values1[i] =  fm.summary$r.squared
#     }
#     
#     
#     #############################################################
#     reg.list2 = list()
#     # initialize matrices and vectors to hold estimated betas,
#     # residual variances, and R-square values from
#     # fitted factor models
#     Betas2 = matrix(0, length(data.names2), length(factor.names2))
#     colnames(Betas2) = factor.names2
#     rownames(Betas2) = data.names2
#     Alphas2 = ResidVars2 = R2values2 = rep(0, length(data.names2))
#     names(Alphas2) = names(ResidVars2) = names(R2values2) = data.names2
#     
#     # loop over all assets and estimate time series regression
#     for (i in data.names2) {
#       reg.df = na.omit(data.df2[, c(i, factor.names2)])
#       fm.formula = as.formula(paste(i,"~", ".", sep=" ")) #npr ("HAM1 ~ .") tale pika pomen po vseh stolpcih
#       fm.fit = lm(fm.formula, data=reg.df)
#       fm.summary = summary(fm.fit)
#       reg.list2[[i]] = fm.fit
#       Alphas2[i] = coef(fm.fit)[1]
#       Betas2[i, ] = coef(fm.fit)[-1]
#       ResidVars2[i] = fm.summary$sigma^2
#       R2values2[i] =  fm.summary$r.squared
#     }
#     ##################################################################
#     # compute factor model covariance matrix
#     # risk factor sample covariance matrix
#     cov.factors1 = var(data.df1[, factor.names1])
#     # FM covariance matrix
#     cov.fm1 = Betas1%*%cov.factors1%*%t(Betas1) + diag(ResidVars1) # formula glej rinfinancefactormodel Covariance Structure
#     # FM correlation matrix
#     cor.fm1 = cov2cor(cov.fm1)
#     # plot correlations using plotcorr() from ellipse package
#     rownames(cor.fm1) = colnames(cor.fm1)
#     
#     cov.factors2 = var(data.df2[, factor.names2])
#     # FM covariance matrix
#     cov.fm2 = Betas2%*%cov.factors2%*%t(Betas2) + diag(ResidVars2) # formula glej rinfinancefactormodel Covariance Structure
#     # FM correlation matrix
#     cor.fm2 = cov2cor(cov.fm2)
#     # plot correlations using plotcorr() from ellipse package
#     rownames(cor.fm2) = colnames(cor.fm2)
#   
# 
#     w.vec1 = racunanje_utezi1()
#     w.vec2 = racunanje_utezi2()
# 
#     # portfolio factor model
#     alpha.p1 = as.numeric(crossprod(Alphas1,w.vec1))
#     beta.p1 = t(Betas1)%*%w.vec1
#     var.p.systematic1 = t(beta.p1)%*%cov.factors1%*%beta.p1
#     var.p.resid1 = t(w.vec1)%*%diag(ResidVars1)%*%w.vec1
#     var.fm.p1 = var.p.systematic1 + var.p.resid1
#     var.fm.p1 = as.numeric(var.fm.p1)
#     r.square.p1 = as.numeric(var.p.systematic1/var.fm.p1)
#     fm.p1 = c(alpha.p1, beta.p1, sqrt(var.fm.p1), r.square.p1)
#     names(fm.p1) = c("intercept", factor.names1, "sd", "r-squared")
# 
#     alpha.p2 = as.numeric(crossprod(Alphas2,w.vec2))
#     beta.p2 = t(Betas2)%*%w.vec2
#     var.p.systematic2 = t(beta.p2)%*%cov.factors2%*%beta.p2
#     var.p.resid2 = t(w.vec2)%*%diag(ResidVars2)%*%w.vec2
#     var.fm.p2 = var.p.systematic2 + var.p.resid2
#     var.fm.p2 = as.numeric(var.fm.p2)
#     r.square.p2 = as.numeric(var.p.systematic2/var.fm.p2)
#     fm.p2 = c(alpha.p2, beta.p2, sqrt(var.fm.p2), r.square.p2)
#     names(fm.p2) = c("intercept", factor.names2, "sd", "r-squared")
#     
#     factor.sd.decomp.list1 = list()
#     for (i in data.names1) {
#       factor.sd.decomp.list1[[i]] = factorModelFactorSdDecomposition(Betas1[i,],
#                                                                     cov.factors1, ResidVars1[i])
#     }
#     # add portfolio factor SD decomposition to list
#     factor.sd.decomp.list1[["PORT"]] = factorModelFactorSdDecomposition(beta.p1,
#                                                                        cov.factors1, var.p.resid1)
#     names(factor.sd.decomp.list1)
#     
#     # stacked bar charts of percent contributions to SD
#     getCSD = function(x) {
#       x$cr.fm
#     }
#     cr.sd1 = sapply(factor.sd.decomp.list1, getCSD)
#     rownames(cr.sd1) = c(factor.names1, "residual")
# 
#     barplot(cr.sd1, main="Factor Contributions to SD",
#             legend.text=T, args.legend=list(x="topleft"),
#             col=c("orange","red","green","white","blue"))
# })
# ###
#     output$contribution <- renderPlot({
#       graf_contribution()
#     })

###
  #########################################################################################################
})