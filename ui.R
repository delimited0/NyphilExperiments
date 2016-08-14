autocomplete_list <- unique(comp_cond$conductorName)

shinyUI(fluidPage(
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
      textInput("num_comps", label = h4("Number of composers"), value = "10")
      # downloadButton('downloadPlot', 'Download Plot')
    )
  )
))