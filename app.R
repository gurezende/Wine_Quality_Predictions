
# Criando um Dashboard com Shiny
library(shiny)
library(shinythemes)
library(plotly)
library(corrplot)
library(caret)
library(randomForest)

# Carregando o dataset
df <- read.csv('winequality2.csv')

# Carregar modelo preditivo
model_wines <- readRDS("model_rf_wines.rds")


# User Interface
ui <- fluidPage(theme = shinytheme("sandstone"),
                # This is the panel with all the tabs on top of the pagex
                navbarPage(
                  theme = 'sandstone',
                  'Wine Quality Dataset Project',
                  
                  
                  # Tab About
                  tabPanel("About",
                           mainPanel(fluidRow(
                             h3('The Dataset'),
                             helpText('Project developed using a modified version of the Wine Quality Dataset, from UCI.
                                You can find the original version of the data at: https://archive.ics.uci.edu/ml/datasets/Wine+Quality.'),
                             h3('Author'),
                             helpText('My name is Gustavo R Santos. I am a Data Analyst | Data Scientist and work with R, Python, MS Excel and SQL to extract insights from data, 
                                      helping companies to make better decisions.'),
                             h3('The Project'),
                             helpText('In this project, the idea was to simulate a business problem where a client (winery) had some data about their
                                      products and they wanted to see how the variables measured could possibly affect the wine quality, therefore,
                                      resulting in higher or lower grades for their wines.'),
                             helpText('The work was divided in 3 main parts:'),
                             helpText('1. Exploring the data and creating some data visualizations, understanding the correlations between variables.
                                      The result is on the first three tabs Histograms, Boxplots and Report.'),
                             helpText('2. Divide the dataset in two clusters. I have gathered both datasets (red + white) to simulate a request from the
                                      client where they needed to know which wine was what kind. The result was 98% accurate and that can be seen on the
                                      Clustering tab.'),
                             helpText('3. The final part was to create a Prediction Interactive Tool for the White Wines from the dataset that allows the client to input some values and have a prediction
                                      whether that wine is going to be qualified as High Quality (grades from 7-10) or Low Quality (grades from 3-6).
                                      You can interact with the tool in the tab Predictions. The accuracy is 95%.')
                           )) # )fluidRow ) mainPanel-ABOUT
                  ), #tabPanel-ABOUT
                  
                  
                  # Here is the tab HISTOGRAMS
                  tabPanel("Histograms",
                           sidebarPanel(
                             selectInput('col', h5('Select Feature for Histogram'),
                                         choices = names(df[-1]),
                                         selected = names(df)[[13]])
                           ), # sidebarPanel-histograms
                           mainPanel(
                            plotOutput(outputId = "histogram")
                           ) # mainPanel-histograms
                  ), # HistogramsTab-tabPanel
                  
                  
                  
                  # Here is the tab BOXPLOTS
                  tabPanel("Boxplots",
                           sidebarPanel(
                    selectInput('col1', h5('Select Feature'),
                                choices = names(df[-1]),
                                selected = names(df)[[13]]),

                  ), # sidebarPanel
                  mainPanel(
                    plotOutput(outputId = "boxplot") # Boxplots-tabPanel
                  ) # mainPanel boxplots
                  ), # tabPanel-boxplots
                  
                  
                  
                  # Here is the tab REPORT
                  tabPanel("Report",
                           fluidRow(column(10, helpText("REPORT | On this tab you will see the correlation matrix for the variables and
                          a couple of graphics with insights about how they relate to the quality of the wines from the dataset."), 
                           )), # ) column ) fluidRow
                           
                           mainPanel(
                             h5('CORRELATION MATRIX | _________________________________________________________________________'),
                             helpText("Analyzing the correlations in the Wines Dataset, notice that the higher correlations with quality
                              are Volatile Acidity, Density and Alcohol."),
                             plotOutput(outputId = "correlation"),
                             
                             h5('GRAPHIC 1 | _________________________________________________________________________'),
                             helpText("The ideal pH level for white wines is in the range from 3,1 to 3,4. For red wines,
                              the majority of the wine producers seek the levels between 3,3 and 3,6. 
                              This dataset is coherent with the reference values, as the higher grades are concentrated in that same range."), helpText('.'),
                             plotlyOutput(outputId = "pHplot"),
                             
                             h5('GRAPHIC 2 | _________________________________________________________________________'),
                             helpText("In this graphic, we conclude that the best quality grades go to the wines between 11% and 13% of alcohol."),
                             helpText('.'),
                             plotlyOutput(outputId = "alcoholplot"),
                             
                             h5('GRAPHIC 3 | _________________________________________________________________________'),
                             helpText("The Volatile Acidity is a component from the wines that, when highly concentrated, it develops the vinegar smell.
                              When in excess, it may mean lack of quality during the wine making process.
                              Here, it makes a lot of sense, because we can see that the variable is negatively correlated to quality,
                              dropping when the quality goes up."), helpText('.'),
                             plotlyOutput(outputId = "volatplot")
                    ) # mainPanel-report
                  ), # tabPanel-Report
                  
                  
                  
                  # Here is the tab CLUSTERING
                  tabPanel("Clustering", h3('Clusterization'),
                  
                  fluidRow(column(10, helpText("This dataset contains both white and red wines. However, the client does not know which 
                  wine is associated with what type and they don't have time to analyze five thousand rows to classify each product.
                  Ergo, using the KMeans algorithm, we can help the winery to separate this dataset in two clusters."),
                  h4("How was it performed?"),
                  helpText("On researchs related to the wine making process, we have learned that the **maceration** process consists on keeping the grapes
                  in a refrigerated environment, since it is incubated in fermentation tanks or inside the presses, where it can stay for a few hours,
                  in the case of white wines, up to weeks, for some red wines.
                  Therefore, we know that, to be a white wine, the grape cannot spend more than a few hours in maceration.
                  That leads us to use the column Maceration as a key for the clustering process.")),
                    helpText('________________________________________________________________________________________')),
                  
                  mainPanel(
                    h3('Hopkins Statistics'),
                    h3('0.17'),
                    h5('Explanation: When this number is lower than 0.5, the dataset is clusterizable.'),
                    plotOutput('clustering'),
                    h3('Model Accuracy:'),
                    h2('98%')
                  ) #mainPanel-Clustering
                  ), # Clustering-tabPanel
                  
                  # Here is the tab PREDICTIONS
                  tabPanel("Predictions", h4("Predictions Interactive Tool"),
                      fluidRow(column(10, helpText('Interact with the sliders to get some predictions from a Random Forest model
                      trained and validated with * 95% * of accuracy. Choose any values on the sliders below and see 
                      what is the prediction of the model about the quality of the White Wine.'),
                      helpText("Values: 
                      Grades from 3 to 6 (Low Quality)  |  Grades from 7 to 10 (High Quality)."), helpText('.'))),
                      sidebarPanel(
                        sliderInput("slideralc", 'Alcohol',
                                    min = min(df$alcohol), max = max(df$alcohol), value = 12.0),
                        sliderInput("slidervol", "Volatile Acidity",
                                    min = min(df$volatile.acidity), max = max(df$volatile.acidity), value = 0.20),
                        sliderInput("sliderchlor", 'Chlorides',
                                    min = min(df$chlorides), max = max(df$chlorides), value = 0.055),
                        sliderInput("sliderdens", "Density",
                                    min = min(df$density), max = max(df$density), value = 0.99),
                        sliderInput("sliderpH", 'pH',
                                    min = min(df$pH), max = max(df$pH), value = 3.3)
                           ), #sidebar-predictions
                        mainPanel(
                          h4('Your Choices'),
                          tableOutput('showchoices'),
                          h1(tableOutput('prediction'))
                        ) #mainPanel-Predictions
                           ) # Predictions-tabPanel
                ) # navbarPage
      ) # fluidPage-Main

# Server
server <- function (input, output) {
  
  # Piece of Code for the Histograms
  output$histogram <- renderPlot({
    trace <- switch(input$col,
                   'fixed.acidity' = df$fixed.acidity,
                   'volatile.acidity' = df$volatile.acidity,
                   'citric.acid' = df$citric.acid,
                   'residual.sugar' = df$residual.sugar,
                   'chlorides' = df$chlorides,
                   'free.sulfur.dioxide' = df$free.sulfur.dioxide,
                   'total.sulfur.dioxide' = df$total.sulfur.dioxide,
                   'density' = df$density,
                   'pH' = df$pH,
                   'sulphates' = df$sulphates,
                   'alcohol' = df$alcohol,
                   'quality' = df$quality,
                   'Maceration' = df$Maceration)
                   
    hist(trace, col = 'darkred', main = paste('Histogram of', input$col), xlab = input$col)
  }) # output$hist
  
  
  # Piece of Code for the Boxplots
  output$boxplot <- renderPlot({
    feature1 <- switch(input$col1,
                    'fixed.acidity' = df$fixed.acidity,
                    'volatile.acidity' = df$volatile.acidity,
                    'citric.acid' = df$citric.acid,
                    'residual.sugar' = df$residual.sugar,
                    'chlorides' = df$chlorides,
                    'free.sulfur.dioxide' = df$free.sulfur.dioxide,
                    'total.sulfur.dioxide' = df$total.sulfur.dioxide,
                    'density' = df$density,
                    'pH' = df$pH,
                    'sulphates' = df$sulphates,
                    'alcohol' = df$alcohol,
                    'quality' = df$quality,
                    'Maceration' = df$Maceration)
    
    boxplot(feature1~quality,data=df, main=paste("Boxplot Quality vs",input$col1), xlab="Quality", ylab="Quality", col= 'darkred')
  }) # output$boxplot  

  
  # Piece of Code for the Plotly plot for Correlations Plot
  output$correlation <- renderPlot({
    correlation <- cor(df[,-1])
    corrplot(correlation, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
  })
  
  
  # Piece of Code for the Plotly plot for pH vs Quality
  output$pHplot <- renderPlotly({
    df_best <- df[df$quality > 5, c(10,13)]
    # Colocar os pHs em bins para facilitar a visualização
    df_best$pH_group <- cut(df_best$pH, seq(2.7,4.1 , 0.1))
    
    # Fazer agrupamento dos dados por bins e qualidade
    df_best_notas <- as.data.frame(with(df_best, table(df_best$pH_group, df_best$quality)))
    colnames(df_best_notas) = c('pH','nota','freq')
    # Separar o dataset por notas
    df_best_nota6 <- df_best_notas[df_best_notas$nota==6,]
    df_best_nota7 <- df_best_notas[df_best_notas$nota==7,]
    df_best_nota8 <- df_best_notas[df_best_notas$nota==8,]
    df_best_nota9 <- df_best_notas[df_best_notas$nota==9,]
    
    #Plotando o resultado
    fig <- plot_ly(data = df_best_nota6, x = ~pH, y = ~freq, type = 'bar', name='Quality 6', alpha = 0.5)
    fig <- fig %>% add_trace(data = df_best_nota7, y = ~freq, name = 'Quality 7', alpha = 0.6)
    fig <- fig %>% add_trace(data = df_best_nota8, y = ~freq, name = 'Quality 8', alpha = 0.7)
    fig <- fig %>% add_trace(data = df_best_nota9, y = ~freq, name = 'Quality 9', alpha = 0.9)
    fig <- fig %>% layout(title='Quality Grades by pH' ,barmode = 'overlay')
    
  }) #output#pHplot
  
  
  # Piece of Code for the Plotly plot for Alcohol vs Quality
  output$alcoholplot <- renderPlotly({
    #Primeiramente vamos criar um subset com os vinhos nota 6-9 e teor de alcool
    df_alcool <- df[df$quality > 5, c(12,13)]
    # Fazer agrupamento dos dados por bins
    df_alcool$groups <- cut(df_alcool$alcohol, seq(8.0, 14.2, 1))
    
    df_alcool_notas <- as.data.frame(with(df_alcool, table(df_alcool$groups, df_alcool$quality)))
    colnames(df_alcool_notas) = c('alcohol','nota','freq')
    
    # Separar o dataset por notas
    df_alcool_nota6 <- df_alcool_notas[df_alcool_notas$nota==6,]
    df_alcool_nota7 <- df_alcool_notas[df_alcool_notas$nota==7,]
    df_alcool_nota8 <- df_alcool_notas[df_alcool_notas$nota==8,]
    df_alcool_nota9 <- df_alcool_notas[df_alcool_notas$nota==9,]
    
    #Plotando o resultado
    fig2 <- plot_ly(data = df_alcool_nota6, x = ~alcohol, y = ~freq, type = 'bar', name='Quality 6', alpha = 0.5)
    fig2 <- fig2 %>% add_trace(data = df_alcool_nota7, y = ~freq, name = 'Quality 7', alpha = 0.7)
    fig2 <- fig2 %>% add_trace(data = df_alcool_nota8, y = ~freq, name = 'Quality 8', alpha = 0.7)
    fig2 <- fig2 %>% add_trace(data = df_alcool_nota9, y = ~freq, name = 'Quality 9', alpha = 0.9)
    fig2 <- fig2 %>% layout(title='Quality Grades by Alcohol %',barmode = 'overlay')
    
  }) #output#alcoholplot
  
  
  # Piece of Code for the Plotly plot for Volatile Acidity vs Quality
  output$volatplot <- renderPlotly({
    #Primeiramente vamos criar um subset com os vinhos nota 6-9 e teor de alcool
    df_volatile <- df[df$quality > 5, c(3,13)]
    # Fazer agrupamento dos dados por bins

    df_volatile$groups <- cut(df_volatile$volatile.acidity, seq(0.05, 1.58, 0.25))
 
    df_volatile_notas <- as.data.frame(with(df_volatile, table(df_volatile$groups, df_volatile$quality)))
    colnames(df_volatile_notas) = c('volatile','nota','freq')
    
    # Separar o dataset por notas
    df_volatile_nota6 <- df_volatile_notas[df_volatile_notas$nota==6,]
    df_volatile_nota7 <- df_volatile_notas[df_volatile_notas$nota==7,]
    df_volatile_nota8 <- df_volatile_notas[df_volatile_notas$nota==8,]
    df_volatile_nota9 <- df_volatile_notas[df_volatile_notas$nota==9,]
    
    #Plotando o resultado
    fig3 <- plot_ly(data = df_volatile_nota6, x = ~volatile, y = ~freq, type = 'bar', name='Quality 6', alpha = 0.5)
    fig3 <- fig3 %>% add_trace(data = df_volatile_nota7, y = ~freq, name = 'Quality 7', alpha = 0.7)
    fig3 <- fig3 %>% add_trace(data = df_volatile_nota8, y = ~freq, name = 'Quality 8', alpha = 0.7)
    fig3 <- fig3 %>% add_trace(data = df_volatile_nota9, y = ~freq, name = 'Quality 9', alpha = 0.9)
    fig3 <- fig3 %>% layout(title='Quality by Volatile Acidity',barmode = 'overlay')
    
  }) #output#volatplot

  
  # Piece of code for the Clustering
  output$clustering <- renderPlot({
  
  # Incluímos o pH, pois sabemos que são diferentes para vinhos brancos e tintos.
  df_tocluster2 <- df[,c(10,12,14)]

  # KMeans
  modelo2 <- kmeans(df_tocluster2, 2)
  df$cluster <- modelo2$cluster
  
  # Reduzindo dimensionalidade
  df_pca <- prcomp(df[c(2:14)], center = TRUE, scale = TRUE)
  # Plotando nosso modelo
  plot(df_pca$x[,1], df_pca$x[,2], col = df$cluster)
  
  }) # output$clustering
  
  
  # Piece of code for the User Choices Table
  output$showchoices <- renderTable({
    # Create DF
    choices <- data.frame(volatile.acidity=input$slidervol,
                          chlorides=input$sliderchlor,
                          density=input$sliderdens, 
                          pH=input$sliderpH,
                          alcohol=input$slideralc)
    
    # Show Table
    choices
    
  }) #output showcoices
  
  
  # Piece of code for the Prediction
  output$prediction <- renderTable({
    # Create DF
    datapred <- data.frame(fixed.acidity=0, volatile.acidity=input$slidervol, citric.acid=0,
                           residual.sugar=0, chlorides=input$sliderchlor, free.sulfur.dioxide=0,
                           total.sulfur.dioxide= 0, density=input$sliderdens, pH=input$sliderpH,
                           sulphates=0, alcohol=input$slideralc)
    
    # Predict
    prediction <- predict(model_wines,datapred)
    data.frame('Wine_Quality_Prediction' = prediction)
    
  }) #output prediction
  
  
} # function


# Run Application
shinyApp(ui = ui, server = server)

