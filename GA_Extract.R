# Carga/Instalacion de Librerias ---------------------------------------------------------
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load("googleAnalyticsR",
               "lubridate",
               "ggplot2",
               "tidyverse",
               "devtools")
# install.packages("devtools")
devtools::install_github("tidyverse/googlesheets4")
3
library(googlesheets4)

ga_auth()
1
#ID de la vista de GA.
ga.cuentas <- ga_account_list()
#Modificar el regex del nombre la cuenta para obtener el ID dequerido
cuentas <- ga.cuentas %>%
  select(accountName, webPropertyName, viewName, viewId) %>% 
  filter(grepl("bestbuy",accountName,ignore.case = TRUE))
cuentas
#Colocar el numero de fila de la vista requerida
view.id <- cuentas$viewId[7]

# Configuraciones Globales ---------------------------------------------------------
#Fechas
start.date <- "2019-01-01"
end.date <- "2019-12-31"
#TRUE para dejar el sampleo de la data y FALSE para desactivarlo.
sampling <- FALSE

# Extracción de la Data de la API de Google Analytics -------------------------
## create filters on dimensions
df <- dim_filter("campaign","REGEX","^fb.*vendor",not = FALSE,caseSensitive = FALSE)
df2 <- dim_filter("campaign","REGEX","citibanamex|cobranded",not = TRUE,caseSensitive = FALSE)

## construct filter objects
fc <- filter_clause_ga4(list(df, df2), operator = "AND")

#Primera llamada a la API
data = google_analytics(
  viewId = view.id,
  date_range = c(start.date,
                 end.date),
  metrics = c("sessions","transactions","transactionRevenue","adCost"),
  dimensions = c("date","campaign"),
  dim_filter = fc,
  max = -1,
  anti_sample = sampling,
)
head(data)

# Escribir la data en un google sheets
(ss <-  sheets_create(
  "sheets-create-demo-6",
  sheets = list(iris = head(iris), mtcars = head(mtcars))
))

