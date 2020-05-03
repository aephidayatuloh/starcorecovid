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

# source("global.R")
date_range <- c(min(nasional$Tanggal), if_else(day(max(nasional$Tanggal)) < 15, ymd(paste(year(max(nasional$Tanggal)), month(max(nasional$Tanggal)), 15, sep = "-")), max(nasional$Tanggal) %m+% months(1) %>% rollback() + 1))

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
                             plotlyOutput("asean", height = 240),
                             DT::DTOutput(outputId = "provinsi")
                             ),
                      column(6,
                             fluidRow(
                               column(6,
                                      fluidRow(
                                        # bs4Card(title = p("Total Positive", style = "text-align: center;margin: 0 !important;"), 
                                        #         status = "warning", solidHeader = FALSE, collapsible = FALSE, maximizable = FALSE, closable = FALSE,
                                        #         width = 6,
                                        #         h6(sprintf("%s (+%s)", today_stats$konfirmasi, today_stats$kasusbaru), style = "font-weight: bold;margin: 0;text-align: center;")
                                        # ),
                                        valueBoxOutput("positif", width = 6),
                                        # bs4Card(title = p("Dirawat", style = "text-align: center;margin: 0 !important;"), 
                                        #         status = "info", solidHeader = FALSE, collapsible = FALSE, maximizable = FALSE, closable = FALSE,
                                        #         width = 6,
                                        #         h6(sprintf("%s (%s)", today_stats$perawatan, today_stats$pctperawatan), style = "font-weight: bold;margin: 0;text-align: center;")
                                        # )
                                        valueBoxOutput("dirawat", width = 6)
                                      )
                               ),
                               column(6,
                                      fluidRow(
                                        # bs4Card(title = p("Recovered", style = "text-align: center;margin: 0 !important;"), 
                                        #         width = 6, 
                                        #         status = "success", solidHeader = FALSE, collapsible = FALSE, maximizable = FALSE, closable = FALSE,
                                        #         h6(sprintf("%s (%s)", today_stats$sembuh, today_stats$pctsembuh), style = "font-weight: bold;margin: 0;text-align: center;")
                                        # ),
                                        valueBoxOutput("sembuh", width = 6),
                                        # bs4Card(title = p("Deaths", style = "text-align: center;margin: 0 !important;"), 
                                        #         width = 6, 
                                        #         status = "danger", solidHeader = FALSE, collapsible = FALSE, maximizable = FALSE, closable = FALSE,
                                        #         h6(sprintf("%s (%s)", today_stats$meninggal, today_stats$pctmeninggal), style = "font-weight: bold;margin: 0;text-align: center;")
                                        # )
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
                                     # echarts4rOutput("plotharian", height = "50%"),
                                     # br(),
                                     # echarts4rOutput("trennasional", height = "50%")
                                     plotlyOutput("trennasional", height = "50%"),
                                     plotlyOutput("trenratio", height = "50%")
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
  shinyalert(title = "Welcome", text = "")
  
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
             xaxis = list(title = "Indonesia Among Asean\nTotal Cases", range = date_range, side = "top"), 
             yaxis = list(title = ""), 
             # title = l"Indonesia Among Asean\nby Total Cases",
             margin = list(t = 50),
             plot_bgcolor='transparent',
             paper_bgcolor='transparent') %>% 
      # config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
      config(displayModeBar = FALSE)
      
  })
  output$provinsi <- DT::renderDT(
    provinsi %>% select(Provinsi, ConfirmedCases) %>% 
      rename(Region = Provinsi, Confirmed = ConfirmedCases),
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
  

  output$positif <- renderValueBox({
    valueBox(value = NULL, footer = "Total Cases", #"Total Cases", 
            subtitle = h6(sprintf("%s (+%s)", today_stats$konfirmasi, today_stats$kasusbaru), style = "font-weight: bold;margin: 0;text-align: center;"),
            icon = "clipboard-check", status = "warning"
            )
  })
  output$dirawat <- renderValueBox({
    valueBox(value = NULL, footer = "Being Treated", #"Dirawat", 
             h6(sprintf("%s (%s)", today_stats$perawatan, today_stats$pctperawatan), style = "font-weight: bold;margin: 0;text-align: center;"),
             icon = "hospital", status = "primary"
    )
  })
  output$sembuh <- renderValueBox({
    valueBox(value = NULL, footer = "Recovered", #"Recovered", 
             subtitle = h6(sprintf("%s (%s)", today_stats$sembuh, today_stats$pctsembuh), style = "font-weight: bold;margin: 0;text-align: center;"),
             icon = "heartbeat", status = "success"
    )
  })
  output$meninggal <- renderValueBox({
    valueBox(value = NULL, footer = "Deaths", #"Deaths", 
             subtitle = h6(sprintf("%s (%s)", today_stats$meninggal, today_stats$pctmeninggal), style = "font-weight: bold;margin: 0;text-align: center;"),
             icon = "medrt", status = "danger"
    )
  })

  output$trennasional <- renderPlotly({
    nasional %>% 
      plot_ly(x = ~Tanggal, y = ~Jumlah_Kasus_Kumulatif, 
              name = "Confirmed", color = I(col_palet$positif)) %>% 
      add_lines() %>% 
      add_lines(x = ~Tanggal, y = ~Jumlah_pasien_dalam_perawatan, name = "Being Treated", color = I(col_palet$dirawat)) %>% 
      add_lines(x = ~Tanggal, y = ~Jumlah_Pasien_Sembuh, name = "Recovered", color = I(col_palet$sembuh)) %>% 
      add_lines(x = ~Tanggal, y = ~Jumlah_Pasien_Meninggal, name = "Death", color = I(col_palet$meninggal)) %>% 
      layout(showlegend = FALSE, 
             xaxis = list(title = "", range = date_range), 
             yaxis = list(title = "Total Cases Cummulative"),
             plot_bgcolor='transparent',
             paper_bgcolor='transparent') %>% 
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
    
  })
  
  
  output$trenratio <- renderPlotly({
    nasional %>% 
      plot_ly(x = ~Tanggal) %>% 
      add_lines(x = ~Tanggal, y = ~round(Persentase_Pasien_dalam_Perawatan, 2), name = "Being Treated", color = I(col_palet$dirawat)) %>% 
      add_lines(x = ~Tanggal, y = ~round(Persentase_Pasien_Sembuh, 2), name = "Recovered", color = I(col_palet$sembuh)) %>% 
      add_lines(x = ~Tanggal, y = ~round(Persentase_Pasien_Meninggal, 2), name = "Death", color = I(col_palet$meninggal)) %>% 
      layout(showlegend = FALSE, 
             xaxis = list(title = "Date", range = date_range), 
             yaxis = list(title = "Ratio to Total Cases (%)"),
             plot_bgcolor='transparent',
             paper_bgcolor='transparent') %>% 
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
  })

  output$plotharian <- renderPlotly({
    
    nasional %>% 
      plot_ly(x = ~Tanggal, y = ~Jumlah_Kasus_Baru_per_Hari, name = "New", color = I(col_palet$positif)) %>% 
      add_lines() %>% 
      add_lines(x = ~Tanggal, y = ~Jumlah_Kasus_Dirawat_per_Hari, name = "Being Treated", color = I(col_palet$dirawat)) %>% 
      add_lines(x = ~Tanggal, y = ~Jumlah_Kasus_Sembuh_per_Hari, name = "Recovered", color = I(col_palet$sembuh)) %>% 
      add_lines(x = ~Tanggal, y = ~Jumlah_Kasus_Meninggal_per_Hari, name = "Death", color = I(col_palet$meninggal)) %>% 
      layout(showlegend = FALSE, 
             xaxis = list(title = "Date", range = date_range), 
             yaxis = list(title = "Daily Cases"),
             plot_bgcolor='transparent',
             paper_bgcolor='transparent') %>% 
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "sendDataToCloud", "editInChartStudio", "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d"))
    
  })
  
  # Tab Provinsi
  prov_selected <- reactive({
    if(is.null(input$provinsi_rows_selected)){
      provinsi[provinsi$Provinsi != "Indonesia", ]
    } else {
      provinsi[input$provinsi_rows_selected, ]
    }
  })
  
  output$provmap <- renderLeaflet({
    if(nrow(prov_selected()) == 1){
      if(prov_selected()$Provinsi != "Indonesia"){
        prov_selected() %>% 
          leaflet() %>% 
          addTiles() %>%
          addPopups(lng = ~x, lat = ~y, #weight = 1,
                    # radius = ~as.numeric(gsub("[.,]", "", prov_selected()$ConfirmedCases)), color = "red",
                    popup = ~HTML(paste("<span style='color:#56c5db;'>", Provinsi, "</span><br/><br/>Confirmed:", ConfirmedCases, "<br/>CFR:", FatalityRate))
          ) %>% 
          setView(lng = prov_selected()$x, lat = prov_selected()$y, zoom = 5)
      } else {
        provinsi[provinsi$Provinsi != "Indonesia", ] %>% 
          leaflet() %>% 
          addTiles() %>%
          addCircles(lng = ~x, lat = ~y, weight = 1,
                     radius = ~as.numeric(gsub("[.,]", "", provinsi$ConfirmedCases[provinsi$Provinsi != "Indonesia"]))*50, color = "red",
                     popup = ~HTML(paste(Provinsi, "<br/><br/>Confirmed:", ConfirmedCases, "<br/>CFR:", FatalityRate))
          )
      }
    } else {
      prov_selected() %>% 
        leaflet() %>% 
        addTiles() %>%
        addCircles(lng = ~x, lat = ~y, weight = 1,
                   radius = ~as.numeric(gsub("[.,]", "", prov_selected()$ConfirmedCases))*50, color = "red",
                   popup = ~paste(Provinsi, ConfirmedCases, "Cases")
        )
    }
  })
  
}
shinyApp(ui = ui, server = server)
