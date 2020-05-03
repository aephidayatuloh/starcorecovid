# global.R

library(RPostgreSQL)

pgcon <- dbConnect(PostgreSQL(),
                   user = "iuziipkn",
                   dbname = "iuziipkn",
                   password = "iHvas9HczAQSlA-8YkqmrKlnZalS9Zc3",
                   host = "rajje.db.elephantsql.com")
pgcon
dbListTables(pgcon)

asean <- dbReadTable(pgcon, "asean")
dailynasional <- dbReadTable(pgcon, "dailynasional")
dailyprovinsi <- dbReadTable(pgcon, "dailyprovinsi")
geom_provinsi <- dbReadTable(pgcon, "geom_provinsi")

# pembaruan <- format(nasional$Pembaruan_Terakhir[1] - 7*60*60, "Pembaruan Data Terakhir %d %B %Y %H:%M")
today_stats <- tibble(pembaruan = format(dailynasional$Pembaruan_Terakhir[1] - 7*60*60, "Latest Update %d %B %Y %H:%M"),
                      konfirmasi = formatC(last(dailynasional$TotalCases), big.mark = ".", decimal.mark = ","),
                      kasusbaru = formatC(last(dailynasional$DailyCases), big.mark = ".", decimal.mark = ","),
                      perawatan = formatC(last(dailynasional$Treated), big.mark = ".", decimal.mark = ","),
                      pctperawatan = paste0(formatC(round(last(dailynasional$PctTreated), 2), big.mark = ".", decimal.mark = ","), "%"),
                      sembuh = formatC(last(dailynasional$Recovered), big.mark = ".", decimal.mark = ","),
                      pctsembuh = paste0(formatC(round(last(dailynasional$PctRecovered), 2), big.mark = ".", decimal.mark = ","), "%"),
                      meninggal = formatC(last(dailynasional$Deaths), big.mark = ".", decimal.mark = ","),
                      pctmeninggal = paste0(formatC(round(last(dailynasional$PctDeaths), 2), big.mark = ".", decimal.mark = ","), "%")
)


tbl_provinsi <- dailyprovinsi %>% 
  arrange(desc(Dates)) %>% 
  distinct(Province, .keep_all = TRUE) %>% 
  arrange(desc(TotalCases)) %>% 
  left_join(geom_provinsi, by = "Province")

dbDisconnect(pgcon)

col_palet <- list(positif = "#ffc107", dirawat = "#007bff", sembuh = "#28a745", meninggal = "#dc3545")
