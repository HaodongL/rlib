output$network_manip <- renderVisNetwork({
  nodes <- data.frame(id = 1:3)
  edges <- data.frame(from = c(1,2), to = c(1,3))
  
  visNetwork(nodes, edges) %>%
    visOptions(manipulation = list(enabled = TRUE, 
                                   editEdgeCols = c("label"), 
                                   editNodeCols = c("group"), 
                                   addNodeCols = c("id", "label")))
})

output$view_manip <- renderPrint({
  input$network_manip_graphChange
})

output$code_network_manip <- renderText({
  '
  visNetwork(nodes, edges)
    visOptions(manipulation = list(enabled = TRUE, 
                                   editEdgeCols = c("label"), 
                                   editNodeCols = c("group"), 
                                   addNodeCols = c("id", "label")))
 '
})
