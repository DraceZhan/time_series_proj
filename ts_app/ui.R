
shinyUI(dashboardPage(
  
  dashboardHeader(title = 'Time Series Project of Restaurant Visit Data in Japan'),
  
  dashboardSidebar(
    sidebarUserPanel("Content"),
    sidebarMenu(
      menuItem("Intro", tabName = "markdown"),
      menuItem("Time Series Visualization", tabName = "dgraph"),
      menuItem("Diagnostics", tabName = 'diag'),
      menuItem("Periodogram", tabName = "freq"),
      menuItem("Model Evaluations",tabName = 'eval_'),
      menuItem("ARIMA Model", tabName = 'arima'),
      menuItem("Data", tabName = "data", icon = icon("database"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "markdown", h1(img(src="pic.jpg", height = 400)),
              h1(class = 'text-muted','Time Series Research Project'),
              p(paste("This is a project for STA9701 Time Series Forecasting by Drace Zhan"))),
      tabItem(tabName = "dgraph", fluidRow(
              p(class = "text-muted",paste("Visualizing the Data"
              )),h1(dygraphOutput("dygraph1")),h1(dygraphOutput("dygraph2")),
              h1(plotOutput("rolling_average"))
              )),
      tabItem(tabName = "diag", fluidRow(box(width = 12, title = "Controls",
                                             selectInput(inputId = "dataset",
                                                         label = "Original/Differenced",
                                                         choices = list("Original",
                                                                        "Differenced at lag = 7")),
                                             selectInput(inputId = "val_",
                                                         label = "Transformed/Boxcox",
                                                         choices = list("Visitors" = 2,
                                                                        "Box Cox Transformed Visitors" = 3)),
                                                         p(class = "text-muted",paste("ACF/PACF"
                                                         )),h1(plotOutput('acf')), h1(plotOutput('pacf')), 
                      p(class = "text-muted",paste("Augmented Dickey-Fuller Test to Evaluate Stationary"
                                                         )),h1(verbatimTextOutput("adf_")),
                      p(class = "text-muted",paste("KPSS Test to Evaluate Unit Root, Failure to Reject Augments Dickey-Fuller Alternate"
                      )),h1(verbatimTextOutput("kpss_")),
      h1(verbatimTextOutput("summary"))
      ))),
      tabItem(tabName = "freq", 
              p(class = "text-muted",paste("Periodogram for Evaluation of Freq/Seasonality"
              )),h1(plotOutput('period_')), infoBox(title = 'Seasonality Observed', tableOutput('time_freq'), width = 6)
      ),
      tabItem(tabName = "eval_", 
            fluidRow(p(class = "text-muted",paste("Evaluations for AR(p) Model - calculated using Yule-Walker")),
                     box(width = 12, sliderInput("p_", "Select p (1,8, 15 were tested prior as best values",
                                                 min = 0, max=40, value=1),
                         checkboxInput(inputId = "bool_",
                                     label = "AIC for controlling model complexity", value=TRUE),
                         p(class = "text-muted",paste("Unit Root Test for model, all Inverse Roots must fall within Unit Circle"
                         )),
                         h1(plotOutput('unit_root')),
                         p(class = "text-muted",paste("Box-Ljung-Pierce Test for independence of residuals, null = independent"
                         )),
                         h1(verbatimTextOutput('box_lj')),p(class = "text-muted",paste("Variance explained by model"
                         )),
                         h1(verbatimTextOutput('var_expl'),
                         p(class = "text-muted",paste("Distribution of Errors to check for Normality of Errors"
                         )),
                         h1(plotOutput('resids_plot')),
                         
                         p(class = "text-muted",paste("AIC plot with Order.Max set to False to evaluate AIC"
                         )),
                         h1(plotOutput('aic_plot'))
                         )))),
        tabItem(tabName = 'arima', fluidRow(p(class = "text-muted",paste(
          "ARIMA Model experimented with Seasonal Weekly Trend")),infoBox(title = 'RSME', tableOutput('RSME')),
          box(h1(plotOutput('arima_plots'))), 
          box(h1(dygraphOutput('dygraph4'))),
          box(h1(plotOutput('resids_Arima_plt'))))                      
                                     
            ),
        tabItem(tabName = "data","Data Base", fluidRow(box(width=12, DT::dataTableOutput("table")))))
    
    )
    
  ))
  