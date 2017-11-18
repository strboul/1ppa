library(shiny)
library(rsconnect)
library(shinythemes)
library(tidyverse)
library(dygraphs)

Pricing_Data <- read_csv("pricing_data.csv")

ui <- fluidPage(theme = shinytheme("united"),
                
                tags$head(
                  tags$style(HTML("
                                  .g-1 {
                                  background-color: #ededed;
                                  }
                                  .g-2 {
                                  background-color: #e0e0e0;
                                  }
                                  
                                  .b-red {
                                  border-left: 6px solid red;
                                  }
                                  .b-orange {
                                  border-left: 6px solid orange;
                                  }
                                  .b-purple {
                                  border-left: 6px solid purple;
                                  }
                                  .b-green {
                                  border-left: 6px solid green;
                                  }
                                  .b-blue {
                                  border-left: 6px solid blue;
                                  }
                                  .b-black {
                                  border-left: 6px solid black;
                                  }
                                  "))
                  ),
                
                h1("Product Pricing Analysis", style="color:#E95420;"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("kgsInputSlider", "Batch (min 480, max 2000)", min = 480,                       max = 2000, value = 540, post = "kg"),
                    br(),
                    radioButtons("palletTypeInput", "Type of the pallet",
                                 choices = c("EURO", "STANDARD"),
                                 selected = "EURO")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Output",
                               fluidRow(
                                 column(width = 5,
                              br(),
                              img(src='pallet.png', align = "left"),
                              h3("Batch Output"),
                              tags$div(class="g-1",htmlOutput("selected_kgs")),
                              tags$div(class="g-2",htmlOutput("selected_pallet")),
                              tags$div(class="g-1",htmlOutput("decimal_box")),
                              tags$div(class="g-2",htmlOutput("net_box")),
                              tags$div(class="g-1",htmlOutput("number_pallet")),
                              tags$div(class="g-2",htmlOutput("total_pallet_weight")),
                              tags$div(class="g-1",htmlOutput("net_product_weight")),
                              tags$div(class="g-2",htmlOutput("total_weight")),
                              br()
                                 ),
                                 column(width = 5,
                                        br(),
                                        img(src='cost.png', align = "left"),
                                        h3("Finance Output"),
                                        
                                        withTags({
                                          div(class="g-1",
                                              htmlOutput("input.1"))
                                        }),
                                        
                                        withTags({
                                          div(class="g-2",
                                              div(class="b-orange",
                                                  htmlOutput("unit_cost"))
                                          ) } ),
                                        
                                        withTags({
                                          div(class="g-1",
                                              div(class="b-purple",
                                                  htmlOutput("unit_selling"))
                                          ) } ),
                                        
                                        tags$div(class="g-2", htmlOutput("transport_cost")),
                                        tags$div(class="g-1", htmlOutput("transport_selling")),
                                        
                                        withTags({
                                          div(class="g-2",
                                              div(class="b-blue",
                                                  htmlOutput("sub_total_selling"))
                                          ) } ),
                                        
                                        withTags({
                                          div(class="g-1",
                                              div(class="b-red",
                                                  htmlOutput("total_selling"))
                                          ) } ),
                                        
                                        withTags({
                                          div(class="g-2",
                                              div(class="b-black",
                                                  htmlOutput("cost_per_kg"))
                                          ) } ),
                                        
                                        withTags({
                                          div(class="g-1",
                                              div(class="b-green",
                                                  htmlOutput("selling_per_kg"))
                                          ) } ),
                                        
                                        br()
                                 )
                               )
                      ),
                      tabPanel("Plot total",
                               dygraphOutput("plot1")
                      ),
                      tabPanel("Plot per kg",
                               dygraphOutput("plot2")
                      ),
                      tabPanel("All Table",
                               tableOutput("alltable")
                      )
                    )
                  )
                )
                  )


server <- function(input, output) {
  
  filtered <- reactive({
    Pricing_Data %>%
      filter(
        Input == input$kgsInputSlider,
        PalletType == input$palletTypeInput
      )
  })
  
  ####BATCH####
  output$selected_kgs <- renderUI({
    HTML(paste("Selected kg:","<b>",input$kgsInputSlider,"</b>"))
  })
  output$selected_pallet <- renderUI({ 
    HTML(paste("Selected pallet type:","<b>",input$palletTypeInput,"</b>"))
  })
  output$decimal_box <- renderUI({
    HTML(paste("Decimal Box Number:","<b>",round(filtered()$DecimalBoxNumber,4),"</b>"))
  })
  output$net_box <- renderUI({
    HTML(paste("Net Box Number:","<b>",filtered()$NetBoxNumber,"</b>"))
  })
  output$number_pallet <- renderUI({
    HTML(paste("Number Pallet(s):","<b>",round(filtered()$NumberPallet,4),"</b>"))
  })
  output$total_pallet_weight <- renderUI({
    HTML(paste("Total Pallet Weight:","<b>",round(filtered()$TotalPalletWeight,4),"</b>"))
  })
  output$net_product_weight <- renderUI({
    HTML(paste("Net Product Weight:","<b>",filtered()$NetProductWeight,"</b>"))
  })
  output$total_weight <- renderUI({
    HTML(paste("Total Weight:","<b>",round(filtered()$TotalWeight,4),"</b>"))
  })
  
  ####FINANCE####
  output$input.1 <- renderUI({
    HTML(paste("Net Product Weight:","<b>",filtered()$Input.1,"</b>"))
  })
  output$unit_cost <- renderUI({
    HTML(paste("Unit Cost:","<b>",filtered()$UnitCost,"$","</b>"))
  })
  output$unit_selling <- renderUI({
    HTML(paste("Unit Selling:","<b>",filtered()$UnitSelling,"$","</b>"))
  })
  output$transport_cost <- renderUI({
    HTML(paste("Transport Cost:","<b>",filtered()$TransportCost,"$","</b>"))
  })
  output$transport_selling <- renderUI({
    HTML(paste("Transport Selling:","<b>",filtered()$TransportSelling,"$","</b>"))
  })
  output$sub_total_selling <- renderUI({
    HTML(paste("Sub Total Selling:","<b>",filtered()$SubTotalSelling,"$","</b>"))
  })
  output$total_selling <- renderUI({
    HTML(paste("Total Selling:","<b>",round(filtered()$TotalSelling,4),"$","</b>"))
  })
  output$cost_per_kg <- renderUI({
    HTML(paste("Cost per kg:","<b>",round(filtered()$CostPerKg,4),"$","</b>"))
  })
  output$selling_per_kg <- renderUI({
    HTML(paste("Selling per kg:","<b>",round(filtered()$SellingPerKg,4),"$","</b>"))
  })
  #output$profit_per_kg <- renderUI({
  #HTML(paste("Profit per kg:","<b>",round(filtered()$ProfitPerKg,4),"$","</b>"))
  #})
  
  output$plot1 <- renderDygraph({
    Pricing_Data %>%
      filter(PalletType == "EURO") %>%
      select(Input, UnitCost, UnitSelling, SubTotalSelling, TotalSelling) %>%
      dygraph() %>%
      dySeries("UnitCost", label = "Unit Cost", color = "#FF851B") %>%
      dySeries("UnitSelling", label = "Unit Selling", color = "#85144b") %>%
      dySeries("SubTotalSelling", label = "Sub Total Selling", color = "#0074D9") %>%
      dySeries("TotalSelling", label = "Total Selling", color = "#FF4136") %>%
      dyOptions(stackedGraph = TRUE) %>%
      dyRangeSelector(height= 25)
  })
  
  output$plot2 <- renderDygraph({
    Pricing_Data %>%
      filter(PalletType == "EURO") %>%
      select(Input, CostPerKg, SellingPerKg) %>%
      dygraph() %>%
      dySeries("CostPerKg", label = "Cost Per Kg", color = "#001f3f") %>%
      dySeries("SellingPerKg", label = "Selling Per Kg", color = "#3D9970") %>%
      dyOptions(stackedGraph = TRUE) %>%
      dyRangeSelector(height= 25)
  })
  
  output$alltable <- renderTable({
    filtered <- Pricing_Data %>%
      filter(Input == input$kgsInputSlider,
             PalletType == input$palletTypeInput
      )
    filtered},
    striped = TRUE,
    hover = TRUE,
    spacing = 'xs',
    digits = 3
  )
  
}

shinyApp(ui = ui, server = server)
