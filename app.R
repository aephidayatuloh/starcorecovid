library(shiny)
library(bs4Dash)
library(rvest)
library(jsonlite)
# library(readr)
library(dplyr)
# library(tidyr)
library(lubridate) 
library(plotly)
# library(stringr)
library(leaflet)
library(DT)
library(curl)
library(waiter)
# library(xts)
library(shinyalert)

ui <- bs4DashPage(navbar = bs4DashNavbar(skin = "dark", status = "white", 
                                         leftUi = column(12, 
                                                         fluidRow(
                                                           a(href = "https://www.starcore.co/", img(src = "img/StarCoreLow.png", alt = "Starcore Analytics", width = "60px", style = "margin: auto -25px;")), 
                                                           h5(HTML("Indonesia Covid-19 Monitoring"), style = "margin: auto 28px;color: #009b4b;font-weight: bold;")
                                                           )
                                                         )
                                         ), 
                  sidebar = bs4DashSidebar(inputId = "sidebar", 
                                           disable = TRUE
                                           ), 
                  body = bs4DashBody(
                    tags$style('body{background-color:#000000;}'),
                    tags$head(
                      tags$link(rel = "shortcut icon", href = "img/StarCoreLow.png")
                    ),
                    use_waiter(),
                    waiter_show_on_load(),
                    useShinyalert(),
                    br(),
                    fluidRow(
                      column(width = 3,
                             p("Select province:", style = "margin-bottom: 0;margin-top: 0;font-weight: bold;"),
                             DT::DTOutput(outputId = "provinsi", height = "100%"),
                             # tabsetPanel(id = "listprovtbl", side = "left", vertical = FALSE,
                             #             tabPanel(tabName = "List", active = TRUE,
                             #                      DT::DTOutput(outputId = "provinsi", height = "100%")
                             #             ),
                             #             tabPanel(tabName = "Covid Growth", active = FALSE,
                             #                      img(src = "media/PerkembanganCovidProvinsiHarian.gif", width = "100%")
                             #             )
                             # ),
                             br()
                             ),
                      column(6,
                             fluidRow(
                               # div(style = "display: flex;height: 50%;", 
                               #     valueBoxOutput("positif", width = 4),
                               #     valueBoxOutput("dirawat", width = 4),
                               #     valueBoxOutput("sembuh", width = 4),
                               #     valueBoxOutput("meninggal", width = 4)
                               #     )
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
                               column(12,
                                      tabsetPanel(id = "plotmap", side = "left", vertical = FALSE,
                                                  tabPanel(tabName = "Daily Trend", active = TRUE,
                                                           plotlyOutput("plotharian", height = 310, width = "100%")
                                                           ),
                                                  tabPanel(tabName = "Maps", active = FALSE,
                                                           leafletOutput("distmap", height = 310, width = "100%")
                                                           )
                                                  )
                                      )
                               )
                             ),
                      column(3,
                             plotlyOutput("trenkumulatif", height = 240, width = "100%"),
                             plotlyOutput("trenratio", height = 240, width = "100%")
                             )
                      )
                    ), 
                  title = "Starcore - Indonesia Covid-19 Monitoring", controlbar_overlay = TRUE, 
                  controlbar = bs4DashControlbar(skin = "light", title = "Epidemic Simulation", width = 400), 
                  footer = bs4DashFooter(copyrights = HTML("&copy; <a href='https://www.starcore.co'>Starcore Analytics</a>"), 
                                         right_text = uiOutput("latest")), 
                  sidebar_collapsed = TRUE
)

server <- function(input, output, session){
  waiter_show( # show the waiter
    # spin_fading_circles() # use a spinner
    spin_dual_circle()
  )
  
  options(scipen = 99)
  
  # source("global_db.R")
  # source("global_api.R")
  source("global_api_dt.R")
  
  # pemabruan <- today_stats$pembaruan
  output$latest <- renderUI({
    HTML(paste(today_stats$pembaruan, "Source: <a href='https://bnpb-inacovid19.hub.arcgis.com/search?collection=Dataset' target='_blank'>https://bnpb-inacovid19.hub.arcgis.com/</a>", sep = " - "))
  })
  
  date_range <- c(min(dailynasional$Dates), if_else(day(max(dailynasional$Dates)) < 15, ymd(paste(year(max(dailynasional$Dates)), month(max(dailynasional$Dates)), 15, sep = "-")), max(dailynasional$Dates) %m+% months(1) %>% rollback() + 1))

  output$provinsi <- DT::renderDT(
    tbl_provinsi %>% 
      select(Province, TotalCases) %>%
    mutate(TotalCases = formatC(TotalCases, big.mark = ",", decimal.mark = ".", format = "d")),
    extensions = 'Scroller', server = FALSE, selection = "single",
    options = list(
      pageLength = 35,
      lengthMenu = c(10, 15),
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
      scrollY = 420,
      scrollCollapse = FALSE
    ), rownames = FALSE
  )
  
  output$positif <- renderValueBox({
    valueBox(value = NULL, footer = "Total Cases", #"Total Cases", 
             subtitle = h6(sprintf("%s (+%s)", 
                                   formatC(today_stats$TotalCases, big.mark = ",", decimal.mark = "."), 
                                   formatC(today_stats$DailyCases, big.mark = ",", decimal.mark = ".")), 
                           style = "font-weight: bold;margin: 0;text-align: center;"),
             icon = "clipboard-check", status = "warning"
    )
  })
  output$dirawat <- renderValueBox({
    valueBox(value = NULL, footer = "Being Treated", #"Dirawat",
             subtitle = h6(sprintf("%s (%s%%)", 
                                   formatC(today_stats$Treated, big.mark = ",", decimal.mark = "."), 
                                   formatC(today_stats$PctTreated, big.mark = ",", decimal.mark = ".")),
                           style = "font-weight: bold;margin: 0;text-align: center;"),
             icon = "hospital", status = "primary"
    )
  })
  output$sembuh <- renderValueBox({
    valueBox(value = NULL, footer = "Recovered", #"Recovered", 
             subtitle = h6(sprintf("%s (%s%%)", 
                                   formatC(today_stats$Recovered, big.mark = ",", decimal.mark = "."), 
                                   formatC(today_stats$PctRecovered, big.mark = ",", decimal.mark = ".")), 
                           style = "font-weight: bold;margin: 0;text-align: center;"),
             icon = "heartbeat", status = "success"
    )
  })
  output$meninggal <- renderValueBox({
    valueBox(value = NULL, footer = "Deaths", #"Deaths", 
             subtitle = h6(sprintf("%s (%s%%)", 
                                   formatC(today_stats$Deaths, big.mark = ",", decimal.mark = "."), 
                                   formatC(today_stats$PctDeaths, big.mark = ",", decimal.mark = ".")), 
                           style = "font-weight: bold;margin: 0;text-align: center;"),
             icon = "medrt", status = "danger"
    )
  })
  
  output$distmap <- renderLeaflet({
    tbl_provinsi[tbl_provinsi$Province != "Indonesia", ] %>%
      leaflet() %>%
      addTiles() %>%
      addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                 radius = ~as.numeric(gsub("[.,]", "", tbl_provinsi[tbl_provinsi$Province != "Indonesia", ]$TotalCases))*30, color = "red"
      )
  })
  
  output$plotharian <- renderPlotly({
    dates <- dailynasional %>% .[["Dates"]]
    data.ts <- dailynasional %>% .[["DailyCases"]]
    # xx <- xts(x = data.ts, order.by = dates)
    # xx <- as.ts(xx)
    xx <- ts(data = data.ts, start = c(2020, as.numeric(format(dates[1], "%j"))), frequency = 365)
    x.info <- attr(xx, "tsp")
    tt <- seq(from = x.info[1], to = x.info[2], by = 1/x.info[3])
    
    # ks <- ksmooth(x = tt, y = xx, kernel = "normal", bandwidth = 10, x.points = tt)
    ks <- ksmooth(x = dates, y = xx, kernel = "normal", bandwidth = 10, x.points = dates)
    
    dailynasional %>% 
      plot_ly(x = ~Dates, y = ~DailyCases, type = "scatter", mode = "lines", name = "New", color = I(col_palet$positif)) %>% 
      add_lines(x = ~Dates, y = ~DailyRecovered, name = "Recovered", color = I(col_palet$sembuh)) %>% 
      add_lines(x = ~Dates, y = ~DailyDeaths, name = "Death", color = I(col_palet$meninggal)) %>% 
      add_lines(x = ~Dates, y = round(ks$y, 2), 
                line = list(dash = "solid", width = 1.5, color = rgb(0.8, 0.8, 0.8, 0.8)), 
                name = "Trend") %>% 
      layout(#showlegend = FALSE, 
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5, y = 1.2,
                      font = list(size = 10)),
        margin = list(t = 0, b = 0, l = 0, r = 5),
        xaxis = list(title = "Date", 
                     range = date_range,
                     font = list(size = 5)), 
        yaxis = list(title = "Daily Cases",
                     font = list(size = 5)),
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
             margin = list(t = 0, b = 0, l = 0, r = 5),
             xaxis = list(title = "", 
                          range = date_range,
                          font = list(size = 5)), 
             yaxis = list(title = "Total Cases Cummulative",
                          font = list(size = 5)),
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
             margin = list(t = 0, b = 0, l = 0, r = 5),
             xaxis = list(title = "Date", 
                          range = date_range,
                          font = list(size = 5)), 
             yaxis = list(title = "Ratio to Total Cases (%)",
                          font = list(size = 5)),
             plot_bgcolor='transparent',
             paper_bgcolor='transparent') %>% 
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
  })
  
  
  prov_selected <- reactive({
    if(is.null(input$provinsi_rows_selected)){
      tbl_provinsi[tbl_provinsi$Province != "Indonesia", ]
    } else {
      tbl_provinsi[input$provinsi_rows_selected, ]
    }
  })
  
  observe({
    prov_selected <- prov_selected()
    if(nrow(prov_selected) == 1){
      if(prov_selected$Province == "Indonesia"){
        output$positif <- renderValueBox({
          valueBox(value = NULL, footer = "Total Cases", #"Total Cases", 
                   subtitle = h6(sprintf("%s (+%s)", 
                                         formatC(today_stats$TotalCases, big.mark = ",", decimal.mark = ".", format = "d"), 
                                         formatC(today_stats$DailyCases, big.mark = ",", decimal.mark = ".")), 
                                 style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "clipboard-check", status = "warning"
          )
        })
        output$dirawat <- renderValueBox({
          valueBox(value = NULL, footer = "Being Treated", #"Dirawat",
                   subtitle = h6(sprintf("%s (%s%%)", 
                                         formatC(today_stats$Treated, big.mark = ",", decimal.mark = ".", format = "d"), 
                                         formatC(today_stats$PctTreated, big.mark = ",", decimal.mark = ".")),
                                 style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "hospital", status = "primary"
          )
        })
        output$sembuh <- renderValueBox({
          valueBox(value = NULL, footer = "Recovered", #"Recovered", 
                   subtitle = h6(sprintf("%s (%s%%)", 
                                         formatC(today_stats$Recovered, big.mark = ",", decimal.mark = ".", format = "d"), 
                                         formatC(today_stats$PctRecovered, big.mark = ",", decimal.mark = ".")), 
                                 style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "heartbeat", status = "success"
          )
        })
        output$meninggal <- renderValueBox({
          valueBox(value = NULL, footer = "Deaths", #"Deaths", 
                   subtitle = h6(sprintf("%s (%s%%)", 
                                         formatC(today_stats$Deaths, big.mark = ",", decimal.mark = ".", format = "d"), 
                                         formatC(today_stats$PctDeaths, big.mark = ",", decimal.mark = ".")), 
                                 style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "medrt", status = "danger"
          )
        })
        
        output$distmap <- renderLeaflet({
          tbl_provinsi[tbl_provinsi$Province != "Indonesia", ] %>%
            leaflet() %>%
            addTiles() %>%
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       radius = ~as.numeric(gsub("[.,]", "", tbl_provinsi[tbl_provinsi$Province != "Indonesia", ]$TotalCases))*30, color = "red"
            )
        })
        
        output$plotharian <- renderPlotly({
          dates <- dailynasional %>% .[["Dates"]]
          data.ts <- dailynasional %>% .[["DailyCases"]]
          # xx <- xts(x = data.ts, order.by = dates)
          # xx <- as.ts(xx)
          xx <- ts(data = data.ts, start = c(2020, as.numeric(format(dates[1], "%j"))), frequency = 365)
          x.info <- attr(xx, "tsp")
          tt <- seq(from = x.info[1], to = x.info[2], by = 1/x.info[3])
          
          ks <- ksmooth(x = dates, y = xx, kernel = "normal", bandwidth = 10, x.points = dates)
          
          dailynasional %>% 
            plot_ly(x = ~Dates, y = ~DailyCases, type = "scatter", mode = "lines", name = "New", color = I(col_palet$positif)) %>% 
            add_lines(x = ~Dates, y = ~DailyRecovered, name = "Recovered", color = I(col_palet$sembuh)) %>% 
            add_lines(x = ~Dates, y = ~DailyDeaths, name = "Death", color = I(col_palet$meninggal)) %>% 
            add_lines(x = ~Dates, y = round(ks$y, 2), 
                      line = list(dash = "solid", width = 1.5, color = rgb(0.8, 0.8, 0.8, 0.8)), 
                      name = "Trend") %>% 
            layout(#showlegend = FALSE, 
              legend = list(orientation = "h",   # show entries horizontally
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5, y = 1.2,
                            font = list(size = 10)),
              margin = list(t = 0, b = 0, l = 0, r = 5),
              xaxis = list(title = "Date", 
                           range = date_range,
                           font = list(size = 5)), 
              yaxis = list(title = "Daily Cases",
                           font = list(size = 5)),
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
                   margin = list(t = 0, b = 0, l = 0, r = 5),
                   xaxis = list(title = "", 
                                range = date_range,
                                font = list(size = 5)), 
                   yaxis = list(title = "Total Cases Cummulative",
                                font = list(size = 5)),
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
                   margin = list(t = 0, b = 0, l = 0, r = 5),
                   xaxis = list(title = "Date", 
                                range = date_range,
                                font = list(size = 5)), 
                   yaxis = list(title = "Ratio to Total Cases (%)",
                                font = list(size = 5)),
                   plot_bgcolor='transparent',
                   paper_bgcolor='transparent') %>% 
            config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
        })
      } else {
        output$positif <- renderValueBox({
          valueBox(value = NULL, footer = "Total Cases", #"Total Cases", 
                   subtitle = h6(sprintf("%s (+%s)", 
                                         formatC(prov_selected$TotalCases, big.mark = ",", decimal.mark = ".", format = "d"), 
                                         formatC(prov_selected$DailyCases, big.mark = ",", decimal.mark = ".", format = "d")), 
                                 style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "clipboard-check", status = "warning"
          )
        })
        output$dirawat <- renderValueBox({
          valueBox(value = NULL, footer = "Being Treated", #"Dirawat",
                   subtitle = h6(sprintf("%s (%s%%)",
                                         formatC(prov_selected$Treated, big.mark = ",", decimal.mark = ".", format = "d"),
                                         formatC(prov_selected$PctTreated, big.mark = ",", decimal.mark = ".")),
                                 style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "hospital", status = "primary"
          )
        })

        output$sembuh <- renderValueBox({
          valueBox(value = NULL, footer = "Recovered", #"Recovered", 
                   subtitle = h6(sprintf("%s (%s%%)", 
                                         formatC(prov_selected$Recovered, big.mark = ",", decimal.mark = ".", format = "d"), 
                                         formatC(prov_selected$PctRecovered, big.mark = ",", decimal.mark = ".")), 
                                 style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "heartbeat", status = "success"
          )
        })
        output$meninggal <- renderValueBox({
          valueBox(value = NULL, footer = "Deaths", #"Deaths", 
                   subtitle = h6(sprintf("%s (%s%%)", 
                                         formatC(prov_selected$Deaths, big.mark = ",", decimal.mark = ".", format = "d"), 
                                         formatC(prov_selected$PctDeaths, big.mark = ",", decimal.mark = ".")), style = "font-weight: bold;margin: 0;text-align: center;"),
                   icon = "medrt", status = "danger"
          )
        })
        
        output$distmap <- renderLeaflet({
          prov_selected %>%
          leaflet() %>%
            addTiles() %>%
            addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                       radius = ~as.numeric(gsub("[.,]", "", prov_selected$TotalCases))*20, color = "red"
                       ) %>%
            addPopups(lng = ~longitude, lat = ~latitude,
                      popup = ~HTML(paste0("<span style='color:#56c5db;'>", Province, "</span><br/><br/>Total Cases: ", formatC(TotalCases, big.mark = ",", decimal.mark = ".", format = "d"), "<br/>CFR: ", PctDeaths, "%"))
                      ) %>%
            setView(lng = prov_selected$longitude, lat = prov_selected$latitude, zoom = 5)
        })
        
        output$plotharian <- renderPlotly({
          dailyprov <- dailyprovinsi %>% 
            filter(Province == prov_selected$Province)
          dates <- dailyprov %>% .[["Dates"]]
          data.ts <- dailyprov %>% .[["DailyCases"]]
          # xx <- xts(x = data.ts, order.by = dates)
          xx <- ts(data = data.ts, start = c(2020, as.numeric(format(dates[1], "%j"))), frequency = 365)
          x.info <- attr(xx, "tsp")
          tt <- seq(from = x.info[1], to = x.info[2], by = 1/x.info[3])
          
          ks <- ksmooth(x = dates, y = xx, kernel = "normal", bandwidth = 10, x.points = dates)
          
          dailyprov %>% 
            plot_ly(x = ~Dates, y = ~DailyCases, type = "scatter", mode = "lines", name = "New", color = I(col_palet$positif)) %>% 
            add_lines(x = ~Dates, y = ~DailyRecovered, name = "Recovered", color = I(col_palet$sembuh)) %>%
            add_lines(x = ~Dates, y = ~DailyDeaths, name = "Death", color = I(col_palet$meninggal)) %>%
            add_lines(x = ~Dates, y = round(ks$y, 2),
                      line = list(dash = "solid", width = 1.5, color = rgb(0.8, 0.8, 0.8, 0.8)),
                      name = "Trend") %>%
            layout(#showlegend = FALSE, 
                   legend = list(orientation = "h",   # show entries horizontally
                                 xanchor = "center",  # use center of legend as anchor
                                 x = 0.5, y = 1.2),
                   margin = list(t = 0, b = 0, l = 0, r = 5),
                   xaxis = list(title = "Date", 
                                range = date_range,
                                font = list(size = 10)), 
                   yaxis = list(title = "Daily Cases",
                                font = list(size = 10)),
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
                   margin = list(t = 0, b = 0, l = 0, r = 5),
                   xaxis = list(title = "", 
                                range = date_range,
                                font = list(size = 5)), 
                   yaxis = list(title = "Total Cases Cummulative",
                                font = list(size = 5)),
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
                   margin = list(t = 0, b = 0, l = 0, r = 5),
                   xaxis = list(title = "Date", 
                                range = date_range,
                                font = list(size = 5)), 
                   yaxis = list(title = "Ratio to Total Cases (%)",
                                font = list(size = 5)),
                   plot_bgcolor='transparent',
                   paper_bgcolor='transparent') %>% 
            config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
        })
      }
    } else {
      return(NULL)
    }
  })
  waiter_hide() # hide the waiter
  
  shinyalert(title = "<span style='color: #009b4b;font-weight: bold;'>Indonesia Covid-19 Monitoring</span>", text = today_stats$pembaruan, animation = "slide-from-top", imageUrl = "img/StarCoreLow.png", imageWidth = 170, imageHeight = 165, html = TRUE)
  
}
shinyApp(ui = ui, server = server)
