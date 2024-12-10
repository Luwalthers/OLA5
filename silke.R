
#med punktum
regnskaber <- read.csv(
  file = "regnskaber.csv", 
  header = TRUE, 
  sep = ";",          # Separator ser ud til at være semikolon
  stringsAsFactors = FALSE, 
  na.strings = c("NA", ""), 
  fileEncoding = "latin1" # Brug latin1 for danske tegn
)

####Analysere data####
str(regnskaber) # Oversigt over datastruktur
summary(regnskaber) # Resumé-statistik for hver kolonne

#Tæller NA og går med 2020 regnskab istedet for 2021, da der er mange NA i 2021
na_counts <- colSums(is.na(regnskaber)) # Antal NA pr. kolonne

####Rens####
#ændrer første kolonnenavn
colnames(regnskaber)[1] <- "Mulighed"

table(regnskaber$Mulighed)
#[1] "Dårlig"        "Meget dårlige" "Dårlige"       "Neutrale"      "Gode"          "Meget gode"    "Ved ikke"  

# Erstat både "Dårlig" og "Dårlige" med "Dårlige"
regnskaber$Mulighed <- gsub("^(Dårlig|Dårlige)$", "Dårlige", regnskaber$Mulighed)

# Fjern rækker, hvor der står "Ved ikke" #51 stk. ved ik
regnskaber <- regnskaber[regnskaber$Mulighed != "Ved ikke", ]

regnskaber$Mulighed <- factor(regnskaber$Mulighed, 
                              levels = c("Meget dårlige","Dårlige","Neutrale","Gode","Meget gode"),
                              ordered = TRUE)


regnskaber <- regnskaber[order(regnskaber$Mulighed), ]

#gem som RDS
saveRDS(regnskaber, "regnskaber2.rds")


#####antal.plot#### 
antal <- as.data.frame(table(regnskaber$Mulighed))
colnames(antal) <- c("Mulighed", "Antal")
antal$Procent <- round(antal$Antal/sum(antal$Antal),4)*100 #udregner procentandel
sum(antal$Procent) #1


library(ggplot2)
# OBS: tilføj kilde
# Opret barplot med værdier, underoverskrift og procenttegn på y-aksen
ggplot(antal, aes(x = Mulighed, y = Procent)) +
  geom_bar(stat = "identity", fill = "skyblue") +  #identity = bruger faktiske værdier
  geom_text(aes(label = paste0(Procent, "%")), 
            vjust = -0.3, fontface = "bold") + 
  labs(
    title = "Over halvdelen af virksomheder ser positivt på muligheden for at låne penge",
    subtitle = "”Hvordan ser du mulighederne for at låne penge til din virksomhed?”",
    caption = "Kilde:XX",
    x = NULL,                           # X-akse etikette
    y = "Pct."
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_light() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),  # Gør titlen fed
    plot.subtitle = element_text(face = "italic", size = 8),
    axis.text.x = element_text(face = "bold", colour = "black")# Gør underoverskriften fed
  )

#flest i gode - benchmark

#andeling ud fra alder
etableringsdato <- data.frame(Muligheder = regnskaber$Mulighed,
                              Etableringsdato = regnskaber$Etableringsdato)

etableringsdato$Etableringsdato <- as.Date(regnskaber$Etableringsdato, format = "%d-%m-%Y")

# Beregn virksomhedens alder
etableringsdato$Alder <- as.numeric(difftime(as.Date("2021-12-31"), etableringsdato$Etableringsdato, units = "days")) / 365.25

# Runder til 2 decimaler for alder
etableringsdato$Alder <- round(etableringsdato$Alder, 2)

str(etableringsdato)


#opg 3.2
library(ordinal)

####OPG3.2####
#ændrer så det en numerisk kategorifaktor - OBS: dette ændrer i dataframen permanent
regnskaber$Mulighed <- factor(regnskaber$Mulighed,
                              levels = c("Meget dårlige","Dårlige", "Neutrale", "Gode", "Meget gode"), 
                              labels = c(1, 2, 3, 4, 5),
                              ordered = TRUE)

#soliditetsgrad
#likviditetsgrad
#samler i dataframe - Likviditetsgrad, Egenkapital, Varedebitor 

udvalgte.variabler <- data.frame(
  Mulighed = factor(regnskaber$Mulighed),
  Soliditetsgrads = as.vector(regnskaber$Soliditetsgrad.2020....),
  Balance = as.vector(regnskaber$Balance.2020..1.000.kr.),
  Langfristet = regnskaber$Langfristet.gæld.2020..1.000.kr.,
  Kortfristet = regnskaber$Kortfristet.gæld.2020..1.000.kr.,
  Egenkapital = as.vector(regnskaber$Egenkapital.2020..1.000.kr.))


#fjerner NA
udvalgte.variabler <- na.omit(udvalgte.variabler)

#udregner gearing
udvalgte.variabler$Gearing <- (udvalgte.variabler$Kortfristet + udvalgte.variabler$Langfristet) / udvalgte.variabler$Egenkapital

#udregner gældsforhold
udvalgte.variabler$Gældsforhold<- as.vector(udvalgte.variabler$Langfristet / udvalgte.variabler$Kortfristet)

#gør soliditetsgrad og balancen numerisk
udvalgte.variabler$Soliditetsgrads <- as.numeric(gsub(",",".", udvalgte.variabler$Soliditetsgrads))
udvalgte.variabler$Balance <- as.numeric(udvalgte.variabler$Balance)

#fjerner kortgæld,langgæld, egenkapital
udvalgte.variabler <- udvalgte.variabler[,-c(4:6)]

apply(udvalgte.variabler[, -1], 2, var)  # meget stor variation for hver variable - grundlag for skalering

str(udvalgte.variabler)

# Standardisér numeriske variabler 
udvalgte.variabler[,-1] <- scale(udvalgte.variabler[, -1])  # Undgå første kolonne, hvis den ikke skal skaleres

clm.all <- clm(Mulighed ~ ., data = udvalgte.variabler)
summary(clm.all)

