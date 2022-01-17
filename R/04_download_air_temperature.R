# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Download air temperature in 2019 at the Tuktoyaktuk airport.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=26987&Year=2019&Month=1&Day=1&time=&timeframe=2&submit=Download+Data"

download_daily_airtemp <- function(date, outdir = here("data/raw/air_temperature")) {
  if (!fs::dir_exists(outdir)) {
    fs::dir_create(outdir)
  }

  year <- lubridate::year(date)
  month <- lubridate::month(date)
  day <- lubridate::day(date)

  url <- glue("https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=26987&Year={year}&Month={month}&Day={day}&time=&timeframe=2&submit=Download+Data")

  filename <- fs::path(outdir, glue("{year}_{str_pad(month, width = 2, side = 'left', pad = 0)}_{str_pad(day, width = 2, side = 'left', pad = 0)}.csv"))

  curl::curl_download(url, filename)
}

dates <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = "1 day")

download_daily_airtemp <- Vectorize(download_daily_airtemp)

download_daily_airtemp(dates[1])
