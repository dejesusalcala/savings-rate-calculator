
# Packages needed

library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)


# Main function to compute data table

ComputeTable = function(income = 60000, age = 25, target = 500000, rate = 7) {
  
  rate = rate/100
  savings_rate = c(5,10,15,20,25,30,35,40,45,50)
  annual_contribution = income*(savings_rate/100)
  number_of_years = c()
  
  for (i in 1:length(savings_rate)){
    t = (log((target*rate/annual_contribution[i]) + 1) ) / log(1 + rate)
    number_of_years = c(number_of_years,t)
  }
  
  number_of_years = round(number_of_years,2)
  
  total_contribution = annual_contribution*number_of_years
  
  total_growth = target - total_contribution
  
  percent_contribution = (total_contribution/target)*100
  
  percent_growth = (total_growth/target)*100
  
  age_at_target = age + number_of_years
  
  SavingsRateTable = data.frame(savings_rate,
                                annual_contribution,
                                total_contribution,
                                total_growth,
                                percent_contribution,
                                percent_growth,
                                number_of_years,
                                age_at_target)
  
  return(SavingsRateTable)
}

#.Colors to use

gray = "#cdcdcd"
dark_gray = "#4C4C4C"

blue = "#0492DF"
orange = "#ffa600"
pink = "#ff6cba"


ui <- fluidPage(
  
    theme = shinytheme("cyborg"),
    titlePanel("Savings Rate Calculator"),
    tags$head(
      tags$style(HTML("hr {border-top:1.8px solid #4c4c4c;}"))
    ),
    
    hr(),
    
    h3("Inputs"),

    fluidRow(
        column(width = 2,
               numericInput(inputId = "annual_income", 
                            label = "Annual Income",
                            value = 60000,
                            min = 0)
               ),
          column(width = 2,
                 numericInput(inputId = "current_age", 
                              label = "Current age",
                              value = 25,
                              min = 0),
          ),
          column(width = 2,
                 numericInput(inputId = "target_amount", 
                              label = "Target amount",
                              value = 500000,
                              min = 0),
          ),
          column(width = 5,
                 sliderInput(inputId = "annual_rate",
                             label = "Adjust the annual rate (%)",
                             min = 3,
                             max = 15,
                             value = 7,
                             step = 1)
          )
        ),
    hr(),
    h3("Graphs"),
    plotOutput(outputId = "plot1"),
    hr(),
    plotOutput(outputId = "plot2"),
    hr(),
    h3("Summary Table"),
    tableOutput(outputId = 'table1')
        
    
)

server <- function(input, output) {
  
  SavingsRateData = reactive({ComputeTable(income = input$annual_income,
                                           age = input$current_age,
                                           target = input$target_amount,
                                           rate = input$annual_rate)})
  
  pivotedSavingsRateData = reactive({pivot_longer(data = select(SavingsRateData(),c(1,5:6)),
                                                  cols = c("percent_contribution","percent_growth"),
                                                  names_to = "type",
                                                  values_to = "percent") 
  })

    output$plot1 <- renderPlot({
        Plot = ggplot(data = SavingsRateData(),aes(x = savings_rate, y = number_of_years)) +
          geom_col(fill = blue) +
          geom_text(aes(label = number_of_years), 
                    vjust = -.8,
                    color = blue,
                    size = 5) +
          labs(title = "Savings Rates Calculator",
               subtitle = paste0("Number of years to reach target of $",
                                 format(round(input$target_amount, digits = 2), big.mark = ",", scientific = FALSE),
                                 " given an annual income of $",
                                 format(round(input$annual_income, digits = 2), big.mark = ",", scientific = FALSE),
                                 " and an anual rate of ",
                                 format(round(input$annual_rate, digits = 2), big.mark = ",", scientific = FALSE),
                                 "%"),
               x = "Savings rate (%)",
               y = "Years to reach target amount") + 
          scale_x_continuous(labels = as.character(SavingsRateData()$savings_rate), 
                             breaks = SavingsRateData()$savings_rate) + 
          scale_y_continuous(limits = c(0,max(SavingsRateData()$number_of_years) + 10)) +
          theme(plot.title = element_text(size = 20,
                                          color = gray),
                plot.subtitle = element_text(size = 16,
                                             color = gray),
                plot.margin = unit(c(.5,.5,.5,.5), "cm"),
                axis.ticks = element_blank(),
                axis.line = element_line(color = gray),
                panel.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
                panel.grid.major.y = element_line(color = dark_gray,
                                                  size = .2,
                                                  linetype = "solid"),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                axis.text = element_text(size = 16, color = gray),
                axis.title.y = element_text(size = 16, color = gray),
                axis.title.x = element_text(size = 16, color = gray))

        Plot
    })
    
    output$plot2 <- renderPlot({
      
      Plot = ggplot(data = pivotedSavingsRateData()) + 
        geom_col(aes(x = savings_rate, y = percent, fill = type), position = "dodge") +
        labs(x = "Savings rate (%)",
             y = "Proportion (%)",
             title = "Proportion of Savings",
             subtitle = paste0("The proportion of contribution vs the growth of several savings rates given a target amount of $",
                               format(round(input$target_amount, digits = 2), big.mark = ",", scientific = FALSE),
                               ", an annual income of $",
                               format(round(input$annual_income, digits = 2), big.mark = ",", scientific = FALSE),
                               ", and an anual rate of ",
                               format(round(input$annual_rate, digits = 2), big.mark = ",", scientific = FALSE),
                               "%")) +
        scale_x_continuous(labels = as.character(SavingsRateData()$savings_rate), 
                           breaks = SavingsRateData()$savings_rate) + 
        scale_fill_manual(values = c("#ff6c71","#6cfffa"),
                          labels = c("Percentage of contribution", "Percantage of growth")) +
        theme(plot.title = element_text(size = 20,
                                        color = gray),
              plot.subtitle = element_text(size = 16,
                                           color = gray),
              legend.position = "bottom",
              plot.margin = unit(c(.5,.5,.5,.5), "cm"),
              axis.ticks = element_blank(),
              axis.line = element_line(color = gray),
              panel.background = element_rect(fill = "black"),
              plot.background = element_rect(fill = "black"),
              panel.grid.major.y = element_line(color = dark_gray,
                                                size = .2,
                                                linetype = "solid"),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.text = element_text(size = 16, color = gray),
              axis.title.y = element_text(size = 16, color = gray),
              axis.title.x = element_text(size = 16, color = gray),
              legend.title = element_blank(),
              legend.background = element_rect(fill = "black"),
              legend.text = element_text(color = gray, size = 16)
              )
      
      Plot
    })
    
    
    output$table1 <- renderTable({
      
      SavingsData = summarise(SavingsRateData(),
                              savings_rate = paste0(savings_rate,"%"),
                              annual_contribution = paste0("$", 
                                                           format(annual_contribution,
                                                                  big.mark = ",", scientific = FALSE)),
                              total_contribution = paste0("$", 
                                                          format(total_contribution,
                                                                 big.mark = ",", scientific = FALSE)),
                              total_growth = paste0("$", 
                                                    format(total_growth,
                                                           big.mark = ",", scientific = FALSE)),
                              percent_contribution = paste0(format(round(percent_contribution, digits = 1), 
                                                                   big.mark = ",", scientific = FALSE),
                                                            "%"),
                              percent_growth = paste0(format(round(percent_growth, digits = 1), 
                                                             big.mark = ",", scientific = FALSE),
                                                      "%"),
                              number_of_years = round(number_of_years, digits = 1),
                              age_at_target = round(age_at_target, digits = 1)
                              )
      
      HeaderName = c("Savings rate",
                     "Annual contribution", 
                     "Total contribution",
                     "Total growth",
                     "Percentage of contribution",
                     "Percentage of growth",
                     "Years to reach target",
                     "Age at target")
      
      colnames(SavingsData) = HeaderName
      
      SavingsData
    })
}

shinyApp(ui = ui, server = server)
