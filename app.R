library(shiny)
library(rsconnect)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(dygraphs)

Pricing_Data <- read_csv("pricing_data.csv")

ui <- dashboardPage(skin="red",
                    
        dashboardHeader(title = "PPA", titleWidth = 180),
        dashboardSidebar(width = 180,
                       sidebarMenu(
                         menuItem("Operations", tabName = "dashboard", icon = icon("cog")),
                         menuItem("Charts", tabName = "charts", icon = icon("area-chart")),
                         menuItem("Rationale", icon = icon("sticky-note-o"), 
                                  href = "http://data.metinyazici.org/2017/11/Product-pricing-analysis-shiny-app.html"),
                         menuItem("Source code", icon = icon("code"), 
                                  href = "https://github.com/strboul/1ppa")
      )
        ),
        dashboardBody(
        
        ###CSS area###              
        #Changing size of `infoBox`
        tags$head(tags$style(HTML('.info-box {min-height: 45px; min-width: 25px;} .info-box-icon {height: 45px; line-height: 45px; transform: scale(0.9);} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
        
        #Slider color
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {border-color: red;background: #dd4b39}")),
        
        #Radio button color
        tags$style("input[type='radio']:checked+span{
                   font-weight: bold;
                   color: #ff851b;
                   }
                   input[type='radio']+span{ 
                   color: gray; 
                   }"),
    
        tabItems(
          tabItem(tabName = "dashboard",
                  h2("Product Pricing Analysis", style="color:#dd4b39;"),
                  column(width = 12,
                   tabBox(width=12,
                          title = "Outputs",
                          id = "tabset1", height = "268px",
                          tabPanel("Batch",
                                   infoBoxOutput("selected_kgs"),
                                   infoBoxOutput("selected_pallet"),
                                   infoBoxOutput("decimal_box"),
                                   infoBoxOutput("net_box"),
                                   infoBoxOutput("number_pallet"),
                                   infoBoxOutput("total_pallet_weight"),
                                   infoBoxOutput("net_product_weight"),
                                   infoBoxOutput("total_weight")
                          ),
                          tabPanel("Finance",
                                   infoBoxOutput("input.1"),
                                   infoBoxOutput("unit_cost"),
                                   infoBoxOutput("unit_selling"),
                                   infoBoxOutput("transport_cost"),
                                   infoBoxOutput("transport_selling"),
                                   infoBoxOutput("sub_total_selling"),
                                   infoBoxOutput("total_selling"),
                                   infoBoxOutput("cost_per_kg"),
                                   infoBoxOutput("selling_per_kg")
                          )
                   )),
              fluidRow(
                column(width = 12,
                       box(width=12,
                           title = "Controls",
                           sliderInput("slider1", "Select kg:", min = 480, max = 2000,
                                       value = 540, step = 1, post = "kg"),
                           radioButtons("palletTypeInput", "Type of the pallet",
                                        choices = c("EURO", "STANDARD"),
                                        selected = "EURO")
                       ))
              )
      ),
                        
        ####Second tab item: "Charts"
        tabItem(tabName = "charts",
                tabBox(width=12,
                       title = "Charts",
                       id = "tabset1", height = "268px",
                       tabPanel("Cost & Selling",
                                dygraphOutput("plot1")
                       ),
                       tabPanel("Per Kg",
                                dygraphOutput("plot2")
                       )
            ))
        )
    )
)


server <- function(input, output) { 
  
  filtered <- reactive({
    Pricing_Data %>%
      filter(
        Input == input$slider1,
        PalletType == input$palletTypeInput
      )
  })
  
  ####BATCH####
  output$selected_kgs <- renderInfoBox({
    infoBox(
      "Selected kg", paste(input$slider1), icon = icon("sort"),
      color = "red"
    )
  })
  
  output$selected_pallet <- renderInfoBox({
    infoBox(
      "Selected pallet type", paste(input$palletTypeInput), icon = icon("circle-o"),
      color = "orange"
    )
  })
  
  output$decimal_box <- renderInfoBox({
    infoBox(
      "Decimal Box Number", paste(round(filtered()$DecimalBoxNumber,3)), icon = icon("square"),
      color = "purple"
    )
  })
  
  output$net_box <- renderInfoBox({
    infoBox(
      "Net Box Number", paste(filtered()$NetBoxNumber), icon = icon("archive"),
      color = "purple"
    )
  })
  
  output$number_pallet <- renderInfoBox({
    infoBox(
      "Number Pallet(s)", paste(round(filtered()$NumberPallet,3)), icon = icon("bars"),
      color = "orange"
    )
  })
  
  output$total_pallet_weight <- renderInfoBox({
    infoBox(
      "Total Pallet Weight", paste(round(filtered()$TotalPalletWeight,3)), icon = icon("bars"),
      color = "orange"
    )
  })
  
  output$net_product_weight <- renderInfoBox({
    infoBox(
      "Net Product Weight", paste(round(filtered()$NetProductWeight,3)), icon = icon("arrow-circle-o-down"),
      color = "purple"
    )
  })
  
  output$total_weight <- renderInfoBox({
    infoBox(
      "Total Weight", paste(round(filtered()$TotalWeight,3)), icon = icon("shopping-bag"),
      color = "orange"
    )
  })
  
  ####FINANCE####
  output$input.1 <- renderInfoBox({
    infoBox(
      "Net Product Weight", paste(filtered()$Input.1), icon = icon("arrow-circle-o-down"),
      color = "purple"
    )
  })
  
  output$unit_cost <- renderInfoBox({
    infoBox(
      "Unit Cost", paste(format(round(filtered()$UnitCost,3),decimal.mark = ",",big.mark = "."),"$"), icon = icon("minus"),
      color = "green"
    )
  })
  
  output$unit_selling <- renderInfoBox({
    infoBox(
      "Unit Selling", paste(format(round(filtered()$UnitSelling,3),decimal.mark = ",",big.mark = "."),"$"), icon = icon("money"),
      color = "green"
    )
  })
  
  output$transport_cost <- renderInfoBox({
    infoBox(
      "Transport Cost", paste(format(round(filtered()$TransportCost,3),decimal.mark = ",",big.mark = "."),"$"), icon = icon("minus"),
      color = "green"
    )
  })
  
  output$transport_selling <- renderInfoBox({
    infoBox(
      "Transport Selling", paste(format(round(filtered()$TransportSelling,3),decimal.mark = ",",big.mark = "."),"$"), icon = icon("ship"),
      color = "green"
    )
  })
  
  output$sub_total_selling <- renderInfoBox({
    infoBox(
      "Sub Total Selling", paste(format(round(filtered()$SubTotalSelling,3),decimal.mark = ",",big.mark = "."),"$"), icon = icon("money"),
      color = "green"
    )
  })
  
  output$total_selling <- renderInfoBox({
    infoBox(
      "Total Selling", paste(format(round(filtered()$TotalSelling,3),decimal.mark = ",",big.mark = "."),"$"), icon = icon("money"),
      color = "green"
    )
  })
  
  output$cost_per_kg <- renderInfoBox({
    infoBox(
      "Cost per kg", paste(format(round(filtered()$CostPerKg,3),decimal.mark = ",",big.mark = "."),"$"), icon = icon("dollar"),
      color = "green"
    )
  })
  
  output$selling_per_kg <- renderInfoBox({
    infoBox(
      "Selling per kg", paste(format(round(filtered()$SellingPerKg,3),decimal.mark = ",",big.mark = "."),"$"), icon = icon("dollar"),
      color = "green"
    )
  })
  
  ####CHARTS####
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
  
}

shinyApp(ui=ui, server=server)