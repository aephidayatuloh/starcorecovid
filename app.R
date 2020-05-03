library(shiny)
library(bs4Dash)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate) 
library(plotly)
library(stringr)
library(leaflet)
library(DT)
library(curl)
library(shinyalert)

source("global.R")
date_range <- c(min(dailynasional$Dates), if_else(day(max(dailynasional$Dates)) < 15, ymd(paste(year(max(dailynasional$Dates)), month(max(dailynasional$Dates)), 15, sep = "-")), max(dailynasional$Dates) %m+% months(1) %>% rollback() + 1))

ui <- bs4DashPage(navbar = bs4DashNavbar(skin = "dark", status = "white", 
                                         leftUi = column(12, 
                                                         fluidRow(
                                                           img(src= "img/StarCoreLow.png", width = "60px", style = "margin: auto -25px;"), 
                                                           h5(HTML("Indonesia Covid-19 Center"), style = "margin: auto 28px;color: #009b4b;font-weight: bold;")
                                                           )
                                                         )#,
                                         # rightUi = p(today_stats$pembaruan, style = "margin-top: auto;margin-bottom: auto;margin-right: 10px;color: #009b4b;font-weight: bold;")
                                         ), 
                  sidebar = bs4DashSidebar(inputId = "sidebar", 
                                           disable = TRUE
                                           # bs4SidebarMenu(
                                           #   bs4SidebarMenuItem(text = "Nasional")
                                           # )
                                           ), 
                  body = bs4DashBody(
                    tags$style('body{background-color:#000000;}'),
                    tags$head(
                      tags$link(rel = "shortcut icon", href = "img/StarCoreLow.png")
                    ),
                    useShinyalert(),
                    br(),
                    fluidRow(
                      column(width = 3,
                             p(today_stats$pembaruan, style = "margin-top: auto;margin-bottom: auto;margin-right: 10px;color: #009b4b;font-weight: bold;"),
                             plotlyOutput("asean", height = 200),
                             DT::DTOutput(outputId = "provinsi")
                             ),
                      column(6,
                             fluidRow(
                               # valueBoxOutput("positif", width = 4),
                               # valueBoxOutput("sembuh", width = 4),
                               # valueBoxOutput("meninggal", width = 4)
                               column(6,
                                      fluidRow(
                                        valueBoxOutput("positif", width = 6),
                                        valueBoxOutput("dirawat", width = 6)
                                      )
                               ),
                               column(6,
                                      fluidRow(
                                        valueBoxOutput("sembuh", width = 6),
                                        valueBoxOutput("meninggal", width = 6)
                                      )
                               )
                             ),
                             fluidRow(
                               column(6,
                                      # bs4Card(width = 12, closable = FALSE, collapsible = FALSE, maximizable = TRUE,
                                              leafletOutput("provmap", height = 360)
                                      # )
                               ),
                               column(6,
                                      # bs4Card(width = 12, closable = FALSE, collapsible = FALSE, maximizable = TRUE,
                                              plotlyOutput("plotharian", height = 360)
                                      # )
                               )
                             )
                      ),
                      column(3,
                             # bs4Card(width = 12, height = 430, closable = FALSE, maximizable = TRUE, collapsible = FALSE, 
                                     plotlyOutput("trenkumulatif", height = 250),
                                     plotlyOutput("trenratio", height = 250)
                             # )
                      )
                    )
                  ),
                  controlbar = NULL, 
                  footer = bs4DashFooter(copyrights = HTML("&copy; Starcore Analytics"), right_text = "Covid-19 Monitoring Dashboard"), 
                  sidebar_collapsed = TRUE
)

server <- function(input, output, session){
  options(scipen = 99)
  # shinyalert(title = "Welcome", text = "")
  
  output$asean <- renderPlotly({
    asean %>% 
      arrange(TotalCases) %>% 
      mutate(Country = factor(Country, levels = Country)) %>% 
      plot_ly(x = ~TotalCases, y = ~Country, 
              text = ~paste("Total Cases:", TotalCases, "\nCFR:", paste0(TotalDeaths," (", round(TotalDeaths/TotalCases*100, 2)), "%)"),
              hoverinfo = "text",
              type = "bar", orientation = "h", marker = list(color = ~I(Color))
              ) %>% 
      layout(showlegend = FALSE, 
             xaxis = list(title = "Indonesia Among Asean\nby Total Cases", range = date_range, side = "top"), 
             yaxis = list(title = ""), 
             # title = l"Indonesia Among Asean\nby Total Cases",
             margin = list(t = 50, b = 5),
             plot_bgcolor='transparent',
             paper_bgcolor='transparent') %>% 
      # config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
      config(displayModeBar = FALSE)
      
  })
  
  output$provinsi <- DT::renderDT(
    tbl_provinsi %>% 
      select(Province, TotalCases) %>%
      mutate(TotalCases = formatC(TotalCases, big.mark = ",", decimal.mark = ".")),
    extensions = 'Scroller', server = TRUE, selection = "single",
    options = list(
      pageLength = 36,
      autoWidth = FALSE,
      columnDefs = list(list(className = 'dt-left', targets = 0),
                        list(className = 'dt-right', targets = 1)
                        ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}"),
      # ajax = 'large.txt',
      deferRender = TRUE,
      dom = 't',
      scrollY = 200,
      scrollCollapse = TRUE
    ), rownames = FALSE
  )
  
  prov_selected <- reactive({
    if(is.null(input$provinsi_rows_selected)){
      tbl_provinsi
    } else {
      tbl_provinsi[input$provinsi_rows_selected, ]
    }
  })
  
  observe({
    prov_selected <- prov_selected()
    if(nrow(prov_selected) == 1){
        output$positif <- renderValueBox({
          valueBox(value = NULL, footer = "Total Cases", #"Total Cases", 
                   subtitle = h6(sprintf("%s (+%s)", formatC(prov_selected$TotalCases, big.mark = ",", decimal.mark = "."), prov_selected$DailyCases), style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "clipboard-check", status = "warning"
          )
        })
        output$dirawat <- renderValueBox({
          valueBox(value = NULL, footer = "Being Treated", #"Dirawat",
                   subtitle = h6(sprintf("%s (%s%%)", formatC(prov_selected$Treated, big.mark = ",", decimal.mark = "."), prov_selected$PctTreated), style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "hospital", status = "primary"
          )
        })
        output$sembuh <- renderValueBox({
          valueBox(value = NULL, footer = "Recovered", #"Recovered", 
                   subtitle = h6(sprintf("%s (%s%%)", formatC(prov_selected$Recovered, big.mark = ",", decimal.mark = "."), prov_selected$PctRecovered), style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "heartbeat", status = "success"
          )
        })
        output$meninggal <- renderValueBox({
          valueBox(value = NULL, footer = "Deaths", #"Deaths", 
                   subtitle = h6(sprintf("%s (%s%%)", formatC(prov_selected$Deaths, big.mark = ",", decimal.mark = "."), prov_selected$PctDeaths), style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "medrt", status = "danger"
          )
        })
        
        output$provmap <- renderLeaflet({
          prov_selected %>% 
          leaflet() %>% 
            addTiles() %>%
            addPopups(lng = ~longitude, lat = ~latitude, 
                      popup = ~HTML(paste0("<span style='color:#56c5db;'>", Province, "</span><br/><br/>Total Cases: ", formatC(TotalCases, big.mark = ",", decimal.mark = "."), "<br/>CFR: ", PctDeaths, "%"))
                      ) %>% 
            setView(lng = prov_selected$longitude, lat = prov_selected$latitude, zoom = 5)
        })
        
        output$plotharian <- renderPlotly({
          dailyprovinsi %>% 
            filter(Province == prov_selected$Province) %>% 
            plot_ly(x = ~Dates, y = ~DailyCases, name = "New", color = I(col_palet$positif)) %>% 
            add_lines() %>% 
            # add_lines(x = ~Dates, y = ~DailyTreated, name = "Being Treated", color = I(col_palet$dirawat)) %>% 
            add_lines(x = ~Dates, y = ~DailyRecovered, name = "Recovered", color = I(col_palet$sembuh)) %>% 
            add_lines(x = ~Dates, y = ~DailyDeaths, name = "Death", color = I(col_palet$meninggal)) %>% 
            layout(showlegend = FALSE, 
                   xaxis = list(title = "Date", range = date_range), 
                   yaxis = list(title = "Daily Cases"),
                   plot_bgcolor='transparent',
                   paper_bgcolor='transparent') %>% 
            config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
          
        })
        
        output$trenkumulatif <- renderPlotly({
          dailyprovinsi %>% 
            filter(Province == prov_selected$Province) %>% 
            plot_ly(x = ~Dates, y = ~TotalCases, name = "Confirmed", color = I(col_palet$positif)) %>% 
            add_lines() %>% 
            add_lines(x = ~Dates, y = ~Treated, name = "Being Treated", color = I(col_palet$dirawat)) %>% 
            add_lines(x = ~Dates, y = ~Recovered, name = "Recovered", color = I(col_palet$sembuh)) %>% 
            add_lines(x = ~Dates, y = ~Deaths, name = "Death", color = I(col_palet$meninggal)) %>% 
            layout(showlegend = FALSE, 
                   xaxis = list(title = "", range = date_range), 
                   yaxis = list(title = "Total Cases Cummulative"),
                   plot_bgcolor='transparent',
                   paper_bgcolor='transparent') %>% 
            config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
          
        })
        
        output$trenratio <- renderPlotly({
          dailyprovinsi %>% 
            filter(Province == prov_selected$Province) %>% 
            plot_ly(x = ~Dates) %>% 
            add_lines(x = ~Dates, y = ~round(PctTreated, 2), name = "Being Treated", color = I(col_palet$dirawat)) %>% 
            add_lines(x = ~Dates, y = ~round(PctRecovered, 2), name = "Recovered", color = I(col_palet$sembuh)) %>% 
            add_lines(x = ~Dates, y = ~round(PctDeaths, 2), name = "Death", color = I(col_palet$meninggal)) %>% 
            layout(showlegend = FALSE, 
                   xaxis = list(title = "Date", range = date_range), 
                   yaxis = list(title = "Ratio to Total Cases (%)"),
                   plot_bgcolor='transparent',
                   paper_bgcolor='transparent') %>% 
            config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
        })
    } else {
      output$positif <- renderValueBox({
        valueBox(value = NULL, footer = "Total Cases", #"Total Cases", 
                 subtitle = h6(sprintf("%s (+%s)", formatC(today_stats$konfirmasi, big.mark = ",", decimal.mark = "."), today_stats$kasusbaru), style = "font-weight: bold;margin: 0;text-align: center;"),
                 icon = "clipboard-check", status = "warning"
        )
      })
      output$dirawat <- renderValueBox({
        valueBox(value = NULL, footer = "Being Treated", #"Dirawat",
                 subtitle = h6(sprintf("%s (%s%%)", formatC(today_stats$perawatan, big.mark = ",", decimal.mark = "."), today_stats$pctperawatan), style = "font-weight: bold;margin: 0;text-align: center;"),
                 icon = "hospital", status = "primary"
        )
      })
      output$sembuh <- renderValueBox({
        valueBox(value = NULL, footer = "Recovered", #"Recovered", 
                 subtitle = h6(sprintf("%s (%s%%)", formatC(today_stats$sembuh, big.mark = ",", decimal.mark = "."), today_stats$pctsembuh), style = "font-weight: bold;margin: 0;text-align: center;"),
                 icon = "heartbeat", status = "success"
        )
      })
      output$meninggal <- renderValueBox({
        valueBox(value = NULL, footer = "Deaths", #"Deaths", 
                 subtitle = h6(sprintf("%s (%s%%)", formatC(today_stats$meninggal, big.mark = ",", decimal.mark = "."), today_stats$pctmeninggal), style = "font-weight: bold;margin: 0;text-align: center;"),
                 icon = "medrt", status = "danger"
        )
      })
      
      output$provmap <- renderLeaflet({
        prov_selected %>% 
          leaflet() %>% 
          addTiles() %>%
          addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                     radius = ~as.numeric(gsub("[.,]", "", prov_selected$TotalCases))*50, color = "red"
          )
      })
      
      output$plotharian <- renderPlotly({
        dailynasional %>% 
          plot_ly(x = ~Dates, y = ~DailyCases, name = "New", color = I(col_palet$positif)) %>% 
          add_lines() %>% 
          # add_lines(x = ~Dates, y = ~DailyTreated, name = "Being Treated", color = I(col_palet$dirawat)) %>% 
          add_lines(x = ~Dates, y = ~DailyRecovered, name = "Recovered", color = I(col_palet$sembuh)) %>% 
          add_lines(x = ~Dates, y = ~DailyDeaths, name = "Death", color = I(col_palet$meninggal)) %>% 
          layout(showlegend = FALSE, 
                 xaxis = list(title = "Date", range = date_range), 
                 yaxis = list(title = "Daily Cases"),
                 plot_bgcolor='transparent',
                 paper_bgcolor='transparent') %>% 
          config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
        
      })
      
      output$trenkumulatif <- renderPlotly({
        dailynasional %>% 
          plot_ly(x = ~Dates, y = ~TotalCases, name = "Confirmed", color = I(col_palet$positif)) %>% 
          add_lines() %>% 
          add_lines(x = ~Dates, y = ~Treated, name = "Being Treated", color = I(col_palet$dirawat)) %>% 
          add_lines(x = ~Dates, y = ~Recovered, name = "Recovered", color = I(col_palet$sembuh)) %>% 
          add_lines(x = ~Dates, y = ~Deaths, name = "Death", color = I(col_palet$meninggal)) %>% 
          layout(showlegend = FALSE, 
                 xaxis = list(title = "", range = date_range), 
                 yaxis = list(title = "Total Cases Cummulative"),
                 plot_bgcolor='transparent',
                 paper_bgcolor='transparent') %>% 
          config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
        
      })
      
      output$trenratio <- renderPlotly({
        dailynasional %>% 
          plot_ly(x = ~Dates) %>% 
          add_lines(x = ~Dates, y = ~round(PctTreated, 2), name = "Being Treated", color = I(col_palet$dirawat)) %>% 
          add_lines(x = ~Dates, y = ~round(PctRecovered, 2), name = "Recovered", color = I(col_palet$sembuh)) %>% 
          add_lines(x = ~Dates, y = ~round(PctDeaths, 2), name = "Death", color = I(col_palet$meninggal)) %>% 
          layout(showlegend = FALSE, 
                 xaxis = list(title = "Date", range = date_range), 
                 yaxis = list(title = "Ratio to Total Cases (%)"),
                 plot_bgcolor='transparent',
                 paper_bgcolor='transparent') %>% 
          config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
      })
    }
  })
}
shinyApp(ui = ui, server = server)
