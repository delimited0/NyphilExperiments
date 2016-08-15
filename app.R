library(ggplot2)
library(plyr)
library(shinysky)
load("comp_cond.RData")
autocomplete_list <- unique(comp_cond$conductorName)

server <- function(input, output) {
  plotInput <- reactive({
    conductor <- comp_cond[comp_cond$conductorName == input$cond_name,]
    conductor_table <- table(conductor$composerName)
    conductor_table <- sort(conductor_table, decreasing=TRUE)
    conductor_df <- as.data.frame(conductor_table, stringsAsFactors = FALSE)
    if (nrow(conductor_df) == 1) {
      conductor_df$Var1 <- rownames(conductor_df)
      conductor_df$Freq <- conductor_df$conductor_table
    }
    
    validate(
      need(input$num_comps > 0, "Please choose a number greater than 0"),
      need(nrow(conductor_df) > 0, "Please choose a conductor")
    )
    
    ggplot(data=conductor_df[1:min(input$num_comps, nrow(conductor_df)),], 
           aes(x=reorder(Var1, -Freq), y=Freq)) +
      geom_bar(stat="identity") + 
      xlab("Composer") + ylab("Count") + ggtitle(input$cond_name) + 
      theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1, size=14),
            axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 18, face="bold")) 
  })
  
  output$plot <- renderPlot({
    print(plotInput())
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$dataset, '.pdf', sep='') },
    content = function(file) {
      pdf(file, height=5, useDingbats=FALSE)
      print(plotInput())
      dev.off() 
    }
  )
}

ui <- fluidPage(
  titlePanel('NY Phil conductors and their most-performed composers'),
  fluidRow(
    column(1, 
           textInput.typeahead(id = "cond_name", placeholder = "Conductor name",
                               local = data.frame(name=c(autocomplete_list)),
                               valueKey = "name",
                               tokens = c(1:length(autocomplete_list)),
                               template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>"))
    )
  ),
  plotOutput('plot'),
  fluidRow(
    column(width = 3,
           numericInput(inputId = "num_comps", label = h4("Number of composers"), 
                        value = "10", min = 1, width = '100%'),
           downloadButton('downloadPlot', 'Download Plot')
    )
  )
)

shinyApp(ui = ui, server = server)
