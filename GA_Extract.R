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
viewId <- cuentas$viewId[7]

# Configuraciones Globales ---------------------------------------------------------
#Fechas
startDate <- "2019-01-01"
endDate <- "2019-12-31"
#TRUE para dejar el sampleo de la data y FALSE para desactivarlo.
sampling <- FALSE

# Extracción de la Data de la API de Google Analytics -------------------------
## Crear los filtros en las dimensiones
df1 <- dim_filter("campaign","REGEX","^(fb|aw|bn|bg|ma)",not = FALSE,caseSensitive = FALSE)

## Construir la clusala de los filtros
fc <- filter_clause_ga4(list(df1), operator = "AND")

# Extraer la data
data = google_analytics(
  viewId = viewId,
  date_range = c(startDate,
                 endDate),
  metrics = c("sessions","transactions","transactionRevenue","adCost"),
  dimensions = c("date","adwordsCustomerID","campaign"),
  dim_filter = fc,
  max = -1,
  anti_sample = sampling,
)
head(data)

performanceAW <- data %>% 
  filter(grepl("6546925640|3671871019|7904158796|3671871019", adwordsCustomerID, ignore.case = TRUE)) %>% 
  filter(!grepl("vendor", campaign, ignore.case = TRUE))

performanceFB <- data %>% 
  filter(grepl("^fb", campaign, ignore.case = TRUE)) %>%
  filter(!grepl("brn|vendor", campaign, ignore.case = TRUE))

performanceMA <- data %>% 
  filter(grepl("^(bn|bg|ma)", campaign, ignore.case = TRUE))

brandingAW <- data %>% 
  filter(grepl("3420793172|3558320754", adwordsCustomerID, ignore.case = TRUE)) %>%
  filter(grepl("brn", campaign, ignore.case = TRUE)) %>% 
  filter(!grepl("citibanamex|cobranded", campaign, ignore.case = TRUE))

brandingFB <- data %>%
  filter(grepl("^fb", campaign, ignore.case = TRUE)) %>%
  filter(!grepl("vendor|citibanamex|cobranded", campaign, ignore.case = TRUE))
  
vendorsFB <- data %>%
  filter(grepl("(^fb).*(vendor)", campaign, ignore.case = TRUE)) %>%
  filter(!grepl("citibanamex|cobranded", campaign, ignore.case = TRUE))


# Escribir la data en un google sheets
(ss <-  sheets_create(
  "inversion2019",
  sheets = list(performanceAW = performanceAW,
                performanceFB = performanceFB,
                performanceMA = performanceMA,
                brandingAW = brandingAW,
                brandingFB = brandingFB,
                vendorsFB = vendorsFB)
))