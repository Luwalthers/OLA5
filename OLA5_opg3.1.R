library(ggplot2)
library(skimr)

lines <- readLines("regnskaber.csv")
head(lines, 1) # Vis de første 10 linjer

lines <- readLines("regnskaber.csv", n = 10)
cat(lines[1]) # Header
cat(lines[2]) # Første datarække

#uden punktum
library(readr)
regnskaber <- read_delim(
  "regnskaber.csv", 
  delim = ";", 
  locale = locale(encoding = "ISO-8859-1"), # Tegnsætning
  na = c("NA", "")
)


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

# Opdel alder i intervaller
etableringsdato$Alder_interval <- cut(etableringsdato$Alder, 
                                      breaks = c(0, 15, 20, 25, 30, 40, 50, Inf), 
                                      labels = c("< 15", "16-20", "21-25", "26-30","31-40","41-50", "50+"),
                                      right = FALSE)  # right = FALSE gør intervallet inklusivt i venstre side

# Kontroller de opdelte aldersintervaller
table(etableringsdato$Alder_interval)
library(ggplot2)

ggplot(etableringsdato, aes(x = Alder_interval, fill = Muligheder)) +
  geom_bar(position = "stack", stat = "count") +  # Brug geom_bar til at plotte
  labs(
    title = "Virksomhedens alder i forhold til muligheder for at låne penge",
    x = "Virksomhedens alder (år)",
    y = "Antal virksomheder",
    fill = "Mulighed"
  ) +
  theme_minimal()
#giver måske ik mening


library(ordinal)
?ordinal::clm()

library(ordinal)

####OPG2####
#ændrer så det en numerisk kategorifaktor - OBS: dette ændrer i dataframen permanent
regnskaber$Mulighed <- factor(regnskaber$Mulighed,
                              levels = c("Meget dårlige","Dårlige", "Neutrale", "Gode", "Meget gode"), 
                              labels = c(1, 2, 3, 4, 5),
                              ordered = TRUE)

#1. udvælg variable vi vil undersøge
print(na_counts)
#efter undersøgelse af dataen, kan det ses at 2021 indeholder mange NA. der er derfor taget udgangspunkt i 2020
    #1.1 struktur undersøges ud fra hvad der har mange na
#2. fjern NA
#3. gør relevante varible numeriske hvis nødvendigt
#4. lav clm med svarmuligheder ~ .,

#samler i dataframe - Likviditetsgrad, Egenkapital, Varedebitor 
variable <- data.frame(
  Mulighed = factor(regnskaber$Mulighed),
  kortfristet_gæld_2020 = as.vector(regnskaber$Kortfristet.gæld.2020..1.000.kr.),
  Egenkapital_2020 = as.vector(regnskaber$Egenkapital.2020..1.000.kr.),
  Varedebitorer_2020 = as.vector(regnskaber$Varedebitorer.2020..1.000.kr.))

variable <- na.omit(variable)
str(variable)  # Tjek datatyper

apply(variable[, -1], 2, var)  # meget stor variation for hver variable - grundlag for skalering

# Standardisér numeriske variabler 
scaled.variable <- variable
scaled.variable[, 2:4] <- scale(variable[, 2:4]) #kun col 2-4

# Tjek resultaterne
apply(scaled.variable[, 2:4], 2, var)  # Variansen bør nu være ca. 1

library(ordinal)

# Brug de skalerede data
clm <- clm(Mulighed ~ kortfristet_gæld_2020 + Egenkapital_2020 + Varedebitorer_2020, 
             data = scaled.variable)

summary(clm)
str(scaled.variable)
clm2 <- clm(Mulighed ~ Egenkapital_2020, 
           data = scaled.variable)
summary(clm2)

#udvælg variblae til dataframe. 2. fjern na'er. 3. udregn relevante ting. 4. lav clm på mulighed mod udregnet tal

# Beregn forholdet mellem lang- og kortfristet gæld

Gældsforhold <- data.frame(
  Mulighed = regnskaber$Mulighed,
  Langfristet = regnskaber$Langfristet.gæld.2020..1.000.kr.,
  Kortfristet = regnskaber$Kortfristet.gæld.2020..1.000.kr.
)

Gældsforhold <- na.omit(Gældsforhold)

Gældsforhold$Forhold<- as.vector(Gældsforhold$Langfristet / Gældsforhold$Kortfristet)

clm.gæld <- clm(Mulighed ~ Forhold,data = Gældsforhold)
summary(clm.gæld)


#afkastningsgrad 

Afkastningsgrad <- data.frame(
  Mulighed = regnskaber$Mulighed,
  Afkastningsgrad20 = as.vector(regnskaber$Afkastningsgrad.2020....),
  Afkastningsgrad19 = as.vector(regnskaber$Afkastningsgrad.2019....)
)

Afkastningsgrad <- na.omit(Afkastningsgrad)

#clm.afkast <- clm(Mulighed ~ Afkastningsgrad20,data = Afkastningsgrad)
summary(clm.afkast)


#egenkapitalsforrening
#egenkapital.forrentnings <- data.frame(
 # Mulighed = regnskaber$Mulighed,
  Egenkapitalsforrentning20 = as.vector(regnskaber$Egenkapital.forrentning.2020....))

Afkastningsgrad <- na.omit(Afkastningsgrad)





























Afkastningsgrad$Afkastningsgrad20 <- gsub(",", ".", Afkastningsgrad$Afkastningsgrad20)
Afkastningsgrad$Afkastningsgrad19 <- gsub(",", ".", Afkastningsgrad$Afkastningsgrad19)

# Erstat ikke-numeriske værdier med NA
Afkastningsgrad$Afkastningsgrad20 <- as.numeric(Afkastningsgrad$Afkastningsgrad20)
Afkastningsgrad$Afkastningsgrad19 <- as.numeric(Afkastningsgrad$Afkastningsgrad19)

# Beregn væksten fra 2019 til 2020
#Afkastningsgrad$Vækst2 <- (Afkastningsgrad$Afkastningsgrad20 - Afkastningsgrad$Afkastningsgrad19) / Afkastningsgrad$Afkastningsgrad19*100

#En alternativ måde at beregne væksten på i sådanne tilfælde er at se på den absolutte ændring og derefter bruge en form for procentuel ændring.
# Hvis begge år er negative, så beregn væksten som den absolutte ændring
#Afkastningsgrad$Vækst <- ifelse(Afkastningsgrad$Afkastningsgrad19 < 0 & Afkastningsgrad$Afkastningsgrad20 < 0,
                                (Afkastningsgrad$Afkastningsgrad20 - Afkastningsgrad$Afkastningsgrad19) / abs(Afkastningsgrad$Afkastningsgrad19) * 100,
                                (Afkastningsgrad$Afkastningsgrad20 - Afkastningsgrad$Afkastningsgrad19) / abs(Afkastningsgrad$Afkastningsgrad19) * 100)




#tester numerisk på hele datasæt
#numeric_columns <- sapply(regnskaber, is.numeric)  # Find numeriske kolonner
#FALSE  TRUE 
# 31   197 








# Kør den ordinale regressionsmodel


#model <- clm(Mulighed ~ Branchekode.primær + Bruttofortjeneste.2021..1.000.kr., data = )





