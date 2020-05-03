library(rvest)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

asean <- read_html("https://www.worldometers.info/coronavirus/") %>% 
  html_table() %>% 
  .[[1]] %>% 
  filter(`Country,Other` %in% c("Indonesia", "Singapore", "Malaysia", "Thailand", "Brunei", "Vietnam", "Philippines")) %>% 
  rename(Country = `Country,Other`) %>% 
  select(Country, TotalCases, TotalDeaths, TotalRecovered) %>% 
  mutate(TotalCases = as.numeric(gsub(",", "", TotalCases)),
         TotalDeaths = as.numeric(gsub(",", "", TotalDeaths)),
         TotalRecovered = as.numeric(gsub(",", "", TotalRecovered)),
         FatalityRate = round(TotalDeaths/TotalCases, 4)) %>% 
  arrange(TotalCases) %>% 
  left_join(tibble(Country = c("Indonesia", "Singapore", "Malaysia", "Thailand", "Brunei", "Vietnam", "Philippines"), 
                   Color = c("#f0d948", "#4b86bd", "#4b86bd", "#4b86bd", "#4b86bd", "#4b86bd", "#4b86bd")), 
            by = "Country")

dailynasional <- fromJSON("https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/Statistik_Perkembangan_COVID19_Indonesia/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")$features$attributes %>%
  mutate(Tanggal = as_date(as_datetime(Tanggal/1000, tz = "Asia/Jakarta")),
         Pembaruan_Terakhir = as_datetime(Pembaruan_Terakhir/1000, tz = "Asia/Jakarta")) %>%
  filter(!is.na(Jumlah_Kasus_Kumulatif)) %>% 
  rename(Dates = Tanggal,
         TotalCases = Jumlah_Kasus_Kumulatif,
         Recovered = Jumlah_Pasien_Sembuh,
         Deaths = Jumlah_Pasien_Meninggal,
         Treated = Jumlah_pasien_dalam_perawatan,
         DailyCases = Jumlah_Kasus_Baru_per_Hari,
         DailyRecovered = Jumlah_Kasus_Sembuh_per_Hari,
         DailyDeaths = Jumlah_Kasus_Meninggal_per_Hari,
         DailyTreated = Jumlah_Kasus_Dirawat_per_Hari,
         PctRecovered = Persentase_Pasien_Sembuh,
         PctDeaths = Persentase_Pasien_Meninggal,
         PctTreated = Persentase_Pasien_dalam_Perawatan,
         ExaminedSpecimen = Jumlah_Spesimen_Diperiksa,
         TotalExaminedSpecimen = Jumlah_Kasus_Diperiksa_Spesimen,
         Negative = Jumlah_Negatif,
         DailyNewSpecimen = Kasus_Diperiksa_Spesimen_Baru_Harian
         )

provinsi <- fromJSON("https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/COVID19_Indonesia_per_Provinsi/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")$features
geom_provinsi <- bind_cols(Province = provinsi$attributes$Provinsi, provinsi$geometry) %>%
  filter(Province != "Indonesia") %>%
  rename(longitude = x, latitude = y)

dailyprovinsi <- readr::read_csv("https://opendata.arcgis.com/datasets/685be21cd0034247b5ceeac996d947fe_0.csv")
# fromJSON("https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/Statistik_Harian_per_Provinsi_COVID19_Indonesia_Rev/FeatureServer/0/query?outFields=*&where=1%3D1&outFields=*&outSR=4326&f=json")
dailyprovinsi <- dailyprovinsi %>%
  mutate(Tanggal = as_date(as_datetime(Tanggal/1000, tz = "Asia/Jakarta")),
         Treated = Kasus_Terkonfirmasi_Akumulatif - (Kasus_Sembuh_Akumulatif + Kasus_Meninggal_Akumulatif),
         PctTreated = round(Treated/Kasus_Terkonfirmasi_Akumulatif*100, 2),
         PctRecovered = round(Kasus_Sembuh_Akumulatif/Kasus_Terkonfirmasi_Akumulatif*100, 2),
         PctDeaths = round(Kasus_Meninggal_Akumulatif/Kasus_Terkonfirmasi_Akumulatif*100, 2)
         ) %>%
  arrange(Tanggal) %>% 
  filter(Provinsi != "Indonesia") %>%
  rename(Province = Provinsi,
         Dates = Tanggal,
         TotalCases = Kasus_Terkonfirmasi_Akumulatif,
         DailyCases = Penambahan_Harian_Kasus_Terkonf,
         Recovered = Kasus_Sembuh_Akumulatif,
         DailyRecovered = Penambahan_Harian_Kasus_Sembuh,
         Deaths = Kasus_Meninggal_Akumulatif,
         DailyDeaths = Penambahan_Harian_Kasus_Meningg,
         Active = Kasus_Aktif_Akumulatif,
         )
col_palet <- list(positif = "#ffc107", dirawat = "#007bff", sembuh = "#28a745", meninggal = "#dc3545")
