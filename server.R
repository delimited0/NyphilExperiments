library(ggplot2)
library(plyr)
library(shinysky)
load("comp_cond.RData")

shinyServer(
  function(input, output) {
    plotInput <- reactive({
      conductor <- comp_cond[comp_cond$conductorName == input$cond_name,]
      conductor_table <- table(conductor$composerName)
      conductor_df <- as.data.frame(sort(conductor_table, decreasing=TRUE), 
                                  stringsAsFactors = FALSE)
      
      validate(
        need(input$num_comps > 0, "Please choose a number greater than 0"),
        need(nrow(conductor_df) > 0, "Please choose a conductor")
      )
      
      ggplot(data=conductor_df[1:min(input$num_comps, nrow(conductor_table)),], 
             aes(x=reorder(Var1, -Freq), y=Freq)) +
        geom_bar(stat="identity") + 
        xlab("Composer") + ylab("Count") +
        theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1, size=14),
              axis.text.y = element_text(size = 14),
              axis.title = element_text(size = 14)) 
    })
    
    output$plot <- renderPlot({
      print(plotInput())
    })
  }
)