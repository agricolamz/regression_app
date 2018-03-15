library(shiny)
library(ggplot2)
library(latex2exp)

ui <- fluidPage(
   
   # Application title
   titlePanel("Regression line"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("intercept",
                     "Intercept:",
                     min = -2,
                     max = 2,
                     value = 0.7038,
                     step = 0.001),
         sliderInput("slope",
                     "Slope:",
                     min = -2,
                     max = 2,
                     value = 0.5452,
                     step = 0.001)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   df <- read.csv("https://goo.gl/DhpmLe")
   RSS_lm <- sum((df$child - df$mother*0.5452 - 0.7038)^2)
   output$distPlot <- renderPlot({
     RSS <- sum((df$child - df$mother*input$slope - input$intercept)^2)
     ggplot(data = df, aes(mother, child))+
       geom_point()+
       theme_bw()+
       labs(x = "number of NP per utterance (mother speech)",
            y = "number of NP per utterance (mother speech)",
            title = paste("Residual sum of squares:", RSS), 
            subtitle = paste("Residual sum of squares from linear regression:", RSS_lm, "with intercept = 0.7038 and slope = 0.5452"),
            caption = "data from [Huttenlocher, Vasilyeva, Cymerman, Levine 2002]")+
       geom_abline(slope = input$slope, intercept = input$intercept)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

