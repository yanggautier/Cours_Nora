library(shiny)

ui <- fluidPage(
  h1('Pima Indian Diabetes', align='center'),
  hr(),
  sidebarLayout(
    sidebarPanel(
      h4('Colonnes'),
      p('Glucose : taux de glucose dans le sang'),
      p('pression artérielle'),
      p('niveau d\'insuline'),
      p('skin thickness (triceps)'),
      p('nombre de grossesses'),
      p('Diabetes pedigree function => risque héréditaire'),
      p('target : résultat test du diabètes'),
      br(),
      p('Nombre d\'observations'),
      textOutput('nb_obs'),
      br(),
      p('Nombre de variables'),
      textOutput('nb_var'),
      hr(),
      sliderInput(inputId = 'nb_obs_to_show', label= 'Number of observations à montrer:', min = 1, max=30, value=10),
    ),
    mainPanel(
      h2('Description du jeu de données'),
      p('Il s\'agit d\'un dataset regroupant des données sur des femmes Pimas
majeures aux USA (> 21 ans)'),
      tableOutput('pima'),
      hr()
    )
  ),
  fluidRow(
    column(12, 
      h3('Problématique'),
      p('La communauté Pima est celle la plus touchée par le diabète est l\'obésité dans le monde. Selon les données sur les patientes, on veut déterminer si elles sont atteintes du diabète.') 
    )
  ),
  fluidRow(
    column(12,
       h3('Analyse exploratoire'),
       tabsetPanel(
         tabPanel('Summary', verbatimTextOutput('summary')),
         tabPanel('Neg/Pos Nb of Diabetes', plotOutput(outputId='diabetes_nb')),
         tabPanel('Neg/Pos Nb of Diabetes by age', plotOutput(outputId='diabetes_nb_age')),
         tabPanel('Blood Pressure and diabetes', plotOutput(outputId='blood_pressure')),
         tabPanel('Mass vs Diabetes', plotOutput(outputId='mass_diabetes'))
       ),
    )
  ),
  fluidRow(
    column(12, 
      h3('Matrice de corrélations')
    )
  ),
  fluidRow(
    column(4, 
      p('A moderate correlation (0.54) is observed between Pregnancies and Age. 
        The intensity of the colour shows that correlation between variables is weaker or stronger. 
        No strong correlation observed between variables. 
        So, no need to drop any of them for analysis')
    ),
    column(8, 
      plotOutput(outputId='corrplot')
    )
  )
)

server <- function(input, output){
  
  output$pima <- renderTable(
    head(PimaIndiansDiabetes, input$nb_obs_to_show)
  )
  
  output$nb_obs <- renderText({
    nrow(PimaIndiansDiabetes)
  })
  
  output$nb_var <- renderText({
    ncol(PimaIndiansDiabetes)
  })
  
  output$summary <- renderPrint({
    summary(PimaIndiansDiabetes)
  })
  
  output$diabetes_nb <- renderPlot({
    ggplot(PimaIndiansDiabetes,aes(diabetes,fill = diabetes)) + geom_bar()
  })
  
  output$diabetes_nb_age <- renderPlot({
    # Age and diabetes
    ggplot(PimaIndiansDiabetes, aes(age,fill = diabetes)) + geom_bar(aes(group=diabetes))
  })
  
  output$blood_pressure <- renderPlot({
    # BloodPressure and diabetes
    ggplot(PimaIndiansDiabetes, aes(pressure)) + geom_bar(aes(group=diabetes)) + facet_wrap(~diabetes)
  })
  
  output$mass_diabetes <- renderPlot({
    # Mass and diabetes
    ggplot(PimaIndiansDiabetes, aes(mass,fill = diabetes))+ geom_bar(aes(group=diabetes))
  })
  
  output$corrplot <- renderPlot({ 
    db_cor <- cor(PimaIndiansDiabetes[1:8])
    corrplot(db_cor, method="number", type = "lower")
  })
  
  # output$eval <- renderPrint({
    
 # })
}

shinyApp(ui=ui, server=server)