###############################
### CONNECT TO IFWIS SERVER ###
###############################

## Connect to IFWIS_WildlifeReporting database (Idaho Fish and Wildlife 
##    Information System) without dsn

con <- dbConnect(
  odbc::odbc(),
  driver = "SQL Server",
  database = "IFWIS_WildlifeReporting",
  uid = "ShinyUserInternal",
  pwd = "hurt seven sat pupil",
  server = "164.165.105.241",
  port = "1433"
)




