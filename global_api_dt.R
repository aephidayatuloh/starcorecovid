library(rvest)
library(jsonlite)
library(data.table)
library(dplyr)
library(lubridate)
# library(stringr)

dailynasional <- as.data.table(fromJSON("https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/Statistik_Perkembangan_COVID19_Indonesia/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")$features$attributes) 
dailynasional[, c("Hari_ke", "FID") := NULL]
dailynasional[, ':=' (Tanggal = as_date(as_datetime(Tanggal/1000, tz = "Asia/Jakarta")),
         Pembaruan_Terakhir = as_datetime(Pembaruan_Terakhir/1000, tz = "Asia/Jakarta"))]

dailynasional <- na.omit(dailynasional, cols = "Jumlah_Kasus_Kumulatif")
dailynasional <- dailynasional[order(Tanggal)]

dailynasional[, Persentase_Pasien_Sembuh := ifelse(Tanggal >= as.Date("2020-07-01"), Persentase_Pasien_Sembuh/1000, Persentase_Pasien_Sembuh)]
dailynasional[, Persentase_Pasien_Meninggal := ifelse(Tanggal >= as.Date("2020-07-01"), Persentase_Pasien_Meninggal/1000, Persentase_Pasien_Meninggal)]
dailynasional[, Persentase_Pasien_dalam_Perawatan := ifelse(Tanggal >= as.Date("2020-07-01"), Persentase_Pasien_dalam_Perawatan/1000, Persentase_Pasien_dalam_Perawatan)] 

dailynasional[is.na(Persentase_Pasien_Sembuh), Persentase_Pasien_Sembuh := Jumlah_Pasien_Sembuh/Jumlah_Kasus_Kumulatif*100]
dailynasional[is.na(Persentase_Pasien_Meninggal), Persentase_Pasien_Meninggal := Jumlah_Pasien_Meninggal/Jumlah_Kasus_Kumulatif*100]
dailynasional[is.na(Persentase_Pasien_dalam_Perawatan), Persentase_Pasien_dalam_Perawatan := Jumlah_pasien_dalam_perawatan/Jumlah_Kasus_Kumulatif*100]

setnames(dailynasional, 
         old = c("Tanggal", 
                  "Jumlah_Kasus_Kumulatif", 
                  "Jumlah_Pasien_Sembuh", 
                  "Jumlah_Pasien_Meninggal",
                  "Jumlah_pasien_dalam_perawatan",
                  "Jumlah_Kasus_Baru_per_Hari",
                  "Jumlah_Kasus_Sembuh_per_Hari",
                  "Jumlah_Kasus_Meninggal_per_Hari",
                  "Jumlah_Kasus_Dirawat_per_Hari",
                  "Persentase_Pasien_Sembuh",
                  "Persentase_Pasien_Meninggal",
                  "Persentase_Pasien_dalam_Perawatan"), 
         new = c("Dates", 
                 "TotalCases", 
                 "Recovered", 
                 "Deaths",
                 "Treated",
                 "DailyCases",
                 "DailyRecovered",
                 "DailyDeaths",
                 "DailyTreated",
                 "PctRecovered",
                 "PctDeaths",
                 "PctTreated"))

today_stats <- data.table(pembaruan = format(dailynasional$Pembaruan_Terakhir[1], "Latest Update %d %B %Y %H:%M"),
                      TotalCases = last(dailynasional$TotalCases), 
                      DailyCases = last(dailynasional$DailyCases), 
                      Treated = last(dailynasional$Treated), 
                      DailyTreated = last(dailynasional$DailyTreated), 
                      PctTreated = round(last(dailynasional$PctTreated), 2), 
                      Recovered = last(dailynasional$Recovered), 
                      DailyRecovered = last(dailynasional$DailyRecovered), 
                      PctRecovered = round(last(dailynasional$PctRecovered), 2),
                      Deaths = last(dailynasional$Deaths), 
                      DailyDeaths = last(dailynasional$DailyDeaths), 
                      PctDeaths = round(last(dailynasional$PctDeaths), 2)
                      )


dailyprovinsi <- fread("https://opendata.arcgis.com/datasets/685be21cd0034247b5ceeac996d947fe_0.csv")
dailyprovinsi[, c("Object_ID", "ObjectId", "FID", "RI_Harian", "CFR_Harian") := NULL]

dailyprovinsi[, ':=' (Tanggal = as_date(as_datetime(Tanggal, tz = "Asia/Jakarta")),
                      Treated = Kasus_Terkonfirmasi_Akumulatif - (Kasus_Sembuh_Akumulatif + Kasus_Meninggal_Akumulatif))]
dailyprovinsi[, ':=' (PctTreated = round(Treated/Kasus_Terkonfirmasi_Akumulatif*100, 2),
                      PctRecovered = round(Kasus_Sembuh_Akumulatif/Kasus_Terkonfirmasi_Akumulatif*100, 2),
                      PctDeaths = round(Kasus_Meninggal_Akumulatif/Kasus_Terkonfirmasi_Akumulatif*100, 2)
                      )]
dailyprovinsi <- dailyprovinsi[order(Tanggal)] 
dailyprovinsi <- dailyprovinsi[Provinsi != "Indonesia"] 
setnames(dailyprovinsi, 
         new = c("Province", 
                 "Dates", 
                 "TotalCases", 
                 "DailyCases", 
                 "Recovered", 
                 "DailyRecovered", 
                 "Deaths",
                 "DailyDeaths",
                 "Active"), 
         old = c("Provinsi", 
                 "Tanggal", 
                 "Kasus_Terkonfirmasi_Akumulatif", 
                 "Penambahan_Harian_Kasus_Terkonf", 
                 "Kasus_Sembuh_Akumulatif", 
                 "Penambahan_Harian_Kasus_Sembuh",
                 "Kasus_Meninggal_Akumulatif",
                 "Penambahan_Harian_Kasus_Meningg",
                 "Kasus_Aktif_Akumulatif")
         )

# geom_provinsi <- fromJSON("https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/COVID19_Indonesia_per_Provinsi/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")$features
geom_provinsi <- fread("data/geom_provinsi.csv") 

tbl_provinsi <- dailyprovinsi[order(-Dates)]
tbl_provinsi <- tbl_provinsi[!duplicated(tbl_provinsi$Province)]
tbl_provinsi <- merge(tbl_provinsi, geom_provinsi, by = "Province", all.x = TRUE)
tbl_provinsi <- tbl_provinsi[order(-TotalCases)] 

tbl_provinsi <- rbind(data.table(Province = "Indonesia", 
                       TotalCases = today_stats$TotalCases), 
                      tbl_provinsi, fill = TRUE)

col_palet <- list(positif = "#ffc107", dirawat = "#007bff", sembuh = "#28a745", meninggal = "#dc3545")

