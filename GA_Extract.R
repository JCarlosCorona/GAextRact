# Carga/Instalacion de Librerias ----
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load("googleAnalyticsR",
               "lubridate",
               "ggplot2",
               "tidyverse",
               "devtools",
               "fbRads",
               "googlesheets4",
               "rlist",
               "RJSONIO")

# Configuraciones Globales ----
#Fechas
startDate <- "2019-01-01"
endDate <- "2019-12-31"
#TRUE para dejar el sampleo de la data y FALSE para desactivarlo.
antisampling <- T

## Autentificar
ga_auth()
1

## ID de la vista de GA.
gaCuentas <- ga_account_list()

## Modificar el regex del nombre la cuenta para obtener el ID dequerido
cuentas <- gaCuentas %>%
  select(accountName, webPropertyName, viewName, viewId) %>% 
  filter(grepl("bestbuy",accountName,ignore.case = TRUE))
cuentas

## Colocar el numero de fila de la vista requerida
viewId <- cuentas$viewId[7]

# Extracción de la Data de Google Ads desde Google Analytics----
## Crear los filtros en las dimensiones
df1AW <- dim_filter("adwordsCustomerID","REGEX","(7904158796|3671871019|6546925640|4755132|3420793172|3558320754|5031692073|3558320754|6941729749|3552304223|7073092319|4081716793|6815298146|8463576785|7822914609|1847787813|7168817092|5700747781|9146585150|7415056701|3773692678|7995493623|7539159080|1193168225|3371937354|7871932150|7962726021|8169387117|3982756797|6310033058|7481130880|3633218348|8157339835|7807622629|4829544596|5145355603|2818077456|8389228439|2829335068|8810168256|3926617178|2176264564|5690826198)",not = FALSE,caseSensitive = FALSE)

## Construir la clusala de los filtros
fcAW <- filter_clause_ga4(list(df1AW), operator = "AND")

# Extraer la data
awData = google_analytics(
  viewId = viewId,
  date_range = c(startDate,
                 endDate),
  metrics = c("adCost","adClicks","sessions","transactions","transactionRevenue"),
  dimensions = c("date","campaign"),
  dim_filter = fcAW,
  max = -1,
  anti_sample = antisampling,
)
head(awData)
awData <- mutate(awData,Medio = "Google Ads")

# Extracción de la Data de la API de Facebook Ads con su data de Google Analytics----
# Colocar el token para la app de FBads
tkn <- "EAAEY7njrS3IBADEHKurmJ8R4HucBkRAQyawaA3LEGuvOO1aa92YiVa6S1h8i6h15T3EypHs0i8vsudEFdJNk2DIYTZB39jvSFQuZCaB8BnE8McBjryiZAqedoRbwa0NOTp7WkXIfK4HVhBgIDSwpaDjRSx3udhTReMda6AELKZAPoLed10wQfGrZCPMbx0Abg6e2rObZBpRwZDZD"

# Colocar los ID de las cuentas de FB
fbAccounts <- c("2210499132335057","278997015949788","502688596903032","1701073816614460")

#Extraer la data de la API de FB Ads
for (i in 1:length(fbAccounts)) {
  
  fbad_init(fbAccounts[i], tkn, version = "6.0")
  
  assign(paste("fb",fbAccounts[i] , sep = "") , value = fb <- fb_insights(level = 'campaign',
                                                                          date_preset = 'last_3d', 
                                                                          job_type = "async",
                                                                          time_increment = "1",
                                                                          fields = toJSON(c('campaign_name','spend', 'clicks'))))
  
}
# Convertir en data frame y limpiar
fb1 <- do.call(rbind.data.frame, fb2210499132335057)
fb1$account_id <- NULL
fb2 <- do.call(rbind.data.frame, fb278997015949788)
fb2$account_id <- NULL
fb3 <- do.call(rbind.data.frame, fb502688596903032)
fb3$account_id <- NULL
fb4 <- do.call(rbind.data.frame, fb1701073816614460)
fb4$account_id <- NULL
fb <- do.call("rbind", list(fb1,fb2,fb3,fb4))

fb <- fb %>% 
  select(date = date_start, campaign = campaign_name, adCost = spend, adClicks = clicks)
fb$date <- as.Date(fb$date)
fb$adCost <- as.numeric(fb$adCost)
fb$adClicks <- as.numeric(fb$adClicks)

# Extraer la data de GA para FB
## Crear los filtros en las dimensiones
df1FB <- dim_filter("campaign","REGEX","^(fb|sm)",not = FALSE,caseSensitive = FALSE)

## Construir la clusala de los filtros
fcFB <- filter_clause_ga4(list(df1FB), operator = "AND")

# Extraer la data
gaDataFB = google_analytics(
  viewId = viewId,
  date_range = c(startDate,
                 endDate),
  metrics = c("sessions","transactions","transactionRevenue"),
  dimensions = c("date","campaign"),
  dim_filter = fcFB,
  max = -1,
  anti_sample = antisampling,
)
head(gaDataFB)

# Hacer el merge de FB con GA
fbData <- merge(fb, gaDataFB, by = c("date", "campaign"))
fbData <- mutate(fbData, Medio = "Facebook Ads")

# Extracción de la Data de la API de Microsoft Ads -----
#Extraer la data de Microsoft Ads



# Extraer la data de GA para MA
## Crear los filtros en las dimensiones
df1MA <- dim_filter("campaign","REGEX","^(ma|bn|bg)",not = FALSE,caseSensitive = FALSE)

## Construir la clusala de los filtros
fcMA <- filter_clause_ga4(list(df1MA), operator = "AND")

# Extraer la data
gaDataMA = google_analytics(
  viewId = viewId,
  date_range = c(startDate,
                 endDate),
  metrics = c("sessions","transactions","transactionRevenue"),
  dimensions = c("date","campaign"),
  dim_filter = fcMA,
  max = -1,
  anti_sample = antisampling,
)
head(gaDataMA)

# Hacer el merge de MA con GA
maData <- merge(ma, gaDataMA, by = c("date", "campaign"))
maData <- mutate(maData, Medio = "Microsoft Ads")

# Juntar todos los medios ----
allData <- do.call("rbind", list(awData, fbData))

# Clasificar las campañas ----
fullReport <- allData %>% mutate(UNE = case_when(
  grepl("_(vendor)_?", campaign, ignore.case = TRUE) ~ "VENDOR",  # VENDOR
  grepl("_(BRANDING|casa|azul)_?", campaign, ignore.case = TRUE) ~ "AWARENES", # AWARENES
  grepl("_(Plaza|cancun|cuernavaca|leon|merida|morelia|puebla|queretaro|san.*luisp|veracruz|acoxpa|chihuahua|andares|buenavista|citadel|ciudadela|contry|cuautitlan|ecatepec|galerias|interlomas|linda.*vista|monterrey|mundo.*e|pedregal|polanco|santa_fe|tlaquepaque|universidad|léon)_?", campaign, ignore.case = TRUE) ~ "AWARENES", # AWARENES
  grepl(".*", campaign, ignore.case = TRUE) ~ "PERFORMANCE")) # PERFORMANCE

#Agrupar Performance por mes
fullReport %>% 
  filter(grepl("PERFORMANCE",UNE)) %>% 
  select(date,adCost,transactionRevenue) %>% 
  mutate(Month = month(date)) %>% 
  group_by(Month) %>% 
  summarise(sum(adCost),sum(transactionRevenue)) %>% 
  View()
  
# Escribir la data en un google sheets ----
(ss <-  sheets_create(
  "inversion2019",
  sheets = list(performanceAW = performanceAW,
                performanceFB = performanceFB,
                performanceMA = performanceMA,
                brandingAW = brandingAW,
                brandingFB = brandingFB,
                vendorsFB = vendorsFB)
))

