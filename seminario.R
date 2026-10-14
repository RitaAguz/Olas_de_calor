install.packages("scales")
install.packages("vctrs")
install.packages("lifecycle")
install.packages("climaemet")


aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJzYW50aWFnb2hlcnJlcm9ydWlwZXJlekBnbWFpbC5jb20iLCJqdGkiOiJmMTk1MGIxNS02ZTRhLTRlNDItOTFmOC00N2Y2NWFlNTFiM2IiLCJleHAiOjE3OTkyMTgzNDUsImlzcyI6IkFFTUVUIiwiaWF0IjoxNzkwNTc4MzQ1LCJ1c2VySWQiOiJmMTk1MGIxNS02ZTRhLTRlNDItOTFmOC00N2Y2NWFlNTFiM2IiLCJyb2xlIjoiIn0.LtEUbPj5lkXjO1rb2dew51L0hIMRshfRe1K-g4_8HL4", install = TRUE)


library("dplyr")
library(climaemet)


clima_diario <- climaemet::aemet_daily_clim(
  station = "all", 
  start = "2000-01-01",
  end = "2001-01-01"
)
