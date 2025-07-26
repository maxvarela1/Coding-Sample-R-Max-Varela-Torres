#Code Sample R Max Varela Torres 
library(tidyverse)
library(readxl)
library(httr)

#Plot permanent residence applications resolved by country
url<- "https://serviciomigraciones.cl/wp-content/uploads/estudios/Datos-abiertos/RD/RD-Resueltas-2d o-semestre-2024.zip"
download.file(url,destfile = "data.zip") unzip("data.zip")

folder1 <- "RD-Resueltas-2000-a-2024-2do-semestre"
im <- list.files(path=folder1,pattern="*.xlsx", full.names = T) |> lapply(read_xlsx) |>
  bind_rows()

imbar<- im |> group_by(AÑO,PAÍS) |> summarise(Count = n())

colnames(imbar) <- c("Year","Country", "Count")

imbartrue<- imbar |>
  mutate(Country= ifelse(Country %in% c("Venezuela","Perú","Colombia","Bolivia","Haití"),
                         Country,"Other"))

ggplot(imbartrue, aes(x=Year, y=Count, fill = Country))+
  geom_bar(stat="identity", position = "stack")+
  labs(x="Year", y="Applications resolved by country", title = "Amount of permanent residence
applications resolved by country in Chile 2000 - 2024")+ scale_fill_brewer(palette = "Set2")

ggsave("PRChile2000-2024.png", width=3000,height=2000,units="px")

#Plot percentage of contribution of the service market to real GDP of Chile 1996 - 2024 URLGDPnom <- "https://github.com/maxvarela1/GDPnominalchile/raw/refs/heads/main/gdpcontrubution.xlsx" download.file(URLGDPnom,destfile = "GDPnom.xlsx")
GDPnom <- read_excel("GDPnom.xlsx", skip = 1) GDPnom <- GDPnom[,-c(1,30)]

URLGDPdef <- "https://github.com/maxvarela1/GDPdeflatorChile/raw/refs/heads/main/GDP%20Deflator.xlsx" 
download.file(URLGDPdef,destfile = "GDPdef.xlsx")

GDPdef <- read_excel("GDPdef.xlsx", skip = 1) GDPdef <-GDPdef[,-c(1)]

GDPdeflatormarkets <- (GDPdef/100)

GDPrealmarkets <- (GDPnom/GDPdeflatormarkets) GDPrealmarkets <- GDPrealmarkets |>
  mutate(Years=1996:2023) |>
  mutate(servicemarket=(`23.Servicios financieros y empresariales`+`26.Servicios de vivienda e inmobiliarios`+`27.Servicios personales`+`28.Administración pública`))

#23+26+27+28
ggplot(GDPrealmarkets,aes(x=Years, y=(servicemarket/`31.PIB a precios corrientes`)))+
  geom_point()+ geom_line()+ scale_x_continuous(
    breaks = seq(1995, 2025, by = 5), labels = seq(1995, 2025, by = 5) )+
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.01), labels = seq(0, 1, by = 0.01)
  )+
  labs(title="Percentage of contribution of services to the real GDP of Chile (2018 base year)",y="Percentage of contribution (%)")

ggsave("GDPserviceChile1996-2024.png", width=3000,height=2000,units="px")
