something <- reactive({
  hot.to.df(input$hotable1) # this will convert your input into a data.frame
})