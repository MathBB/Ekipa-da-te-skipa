library("shiny")

shinyUI(navbarPage("Projekt za osnove podatkovnih baz", fluid = TRUE,
                   tabPanel("Portfelj",
                            wellPanel(                         
                              ################################################################################################################################# 
                              fluidRow(
                                column(6,
                                       helpText("Izberite delnice za sestavo portfelja 1"),
                                       
                                       selectInput("select", label = ("Izberite delnice"), 
                                                   choices = colnames(ClosePrices), multiple = TRUE, selected = colnames(ClosePrices)[1])),
                                column(6,
                                       helpText("Izberite delnice za sestavo portfelja 2"),
                                       
                                       selectInput("select3", label = ("Izberite delnice"), 
                                                   choices = colnames(ClosePrices), multiple = TRUE, selected = colnames(ClosePrices)[2]))),
                              fluidRow(
                                column(4, offset = 4,
                                       selectInput("select2", 
                                                   label = ("Izberite nacin sestave portfelja"), 
                                                   choices = c("Vnesi st. posameznih delnic","Vnesi utezi")),
                                       uiOutput("izbira0"))),
                              fluidRow(
                                column(6,
                                       uiOutput("izbira1")),
                                column(6,
                                       uiOutput("izbira3"))),
                              #####################################################################################################################################
                              fluidRow(
                                column(6,
                                       h3("Analiza portfelja1"),
                                       checkboxGroupInput("checkGroup1", label = h4("Osnovne statistike"), 
                                                          choices = list("VaR" = 1, "ES" = 2, "Odklon" = 3, "Mat. upanje" = 4),
                                                          selected = 4)      
                                ),
                                column(6,
                                       h3("Analiza portfelja2"),
                                       checkboxGroupInput("checkGroup2", label = h4("Osnovne statistike"), 
                                                          choices = list("VaR" = 1, "ES" = 2, "Odklon" = 3, "Mat. upanje" = 4),
                                                          selected = 4)
                                )
                              ),
                              fluidRow(
                                column(4, offset = 4,
                                       sliderInput("bins", label = h4("Histogram dnevnih donosnosti"), min = 1, max = 300, value = 30)))
                            ),
                            plotOutput("histogram", height = "550px"),
                            plotOutput("portfolio_1", height = "550px"),
                            dataTableOutput("tabela_statistik1"),
                            dataTableOutput("tabela_statistik2")
                   ),
                   tabPanel("Faktorski model",

                            sidebarPanel(
                              selectInput("dependent", "Odvisna spremenljivka:", c("Portfelj1", "Portfelj2", "MKT", "SMB", "HML", "WML", "risk-free_r")),
                              checkboxGroupInput("independent", "Neodvisna spremenljivka:", c("MKT", "SMB", "HML", "WML", "risk_free_r"), selected = "risk_free_r"),
                              br(),
                              br(),
                            
                              h3("Navodilo za uporabo"),
                              
                              helpText("v spodnje okence vpisite svojo regresijsko formulo. Med spremenljivkami dodajajte + .
                                       Za polinomske funkcije uporabite funkcijo I() Npr. I(x^2). Druge funkcije ki se pridejo prav exp(x), log(x).
                                       Vkolikor delate regresijo brez regresijske konstante na koncu dodajte -1   "),
                                
                                textInput("regresija", label = h3("Napi?ite poljubno regresijsko formulo"), value = "Portfelj1 ~ "),
                                br(),
                                br(),
                                textInput("fit1", label = h5("Napisite funkcijo ki aproksimira podatke"), value = "y ~ x"),
                                
                                selectInput("variable1", "Pojasnjevalna spremenljivka:", c("Portfelj1", "Portfelj2", "MKT", "SMB", "HML", "WML", "risk-free_r")),
                                
                                selectInput("variable2", "Odvisna spremenljivka:", c("Portfelj1", "Portfelj2", "MKT", "SMB", "HML", "WML", "risk-free_r")),
                                br(),
                                br(),
                                
                                checkboxInput("dodaj_nov_graf", label = "zelim dodati nov scatter plot", value = FALSE),
                                conditionalPanel(
                                  condition = "input.dodaj_nov_graf == true",
                                  
                                  textInput("fit2", label = h5("Napisite funkcijo ki aproksimira podatke"), value = "y ~ x"),
                                  selectInput("variable3", "Pojasnjevalna spremenljivka:", c("Portfelj1", "Portfelj2", "MKT", "SMB", "HML", "WML", "risk-free_r")),
                                  selectInput("variable4", "Odvisna spremenljivka:", c("Portfelj1", "Portfelj2", "MKT", "SMB", "HML", "WML", "risk-free_r"))
                                )
                                
                              ),
                              
                              mainPanel(h3(textOutput("caption")),
                                        plotOutput("plot1"),
                                        conditionalPanel("input.dodaj_nov_graf == true",
                                                         h3(textOutput("caption1")),
                                                         plotOutput("plot2")
                                        ),
                                        helpText(h3("Rezultat linerane regresije")),
                                        tableOutput("regTab"),
                                        helpText(h3("Rezultat uporabnikove regresije")),
                                        textOutput("value"),
                                        tableOutput("test"),
                                        plotOutput("contribution")
                                        
                                        #dataTableOutput("priprava_podatkov")
                                        )
                   )

))



#############################################################
plotOutput("portfolio_1")
