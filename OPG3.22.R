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
udvalgte.variabler$Gearing <- round(((udvalgte.variabler$Kortfristet + udvalgte.variabler$Langfristet) / udvalgte.variabler$Egenkapital),2)

#udregner gældsforhold
udvalgte.variabler$Gældsforhold<- round(as.vector(udvalgte.variabler$Langfristet / udvalgte.variabler$Kortfristet),2)

#gør soliditetsgrad og balancen numerisk og egenkapital
udvalgte.variabler$Soliditetsgrads <- as.numeric(gsub(",",".", udvalgte.variabler$Soliditetsgrads))
udvalgte.variabler$Balance <- as.numeric(udvalgte.variabler$Balance)
udvalgte.variabler$Egenkapital <- as.numeric(udvalgte.variabler$Egenkapital)

#fjerner kortgæld,langgæld
udvalgte.variabler <- udvalgte.variabler[,-c(4:5)]

apply(udvalgte.variabler[, -1], 2, var)  # meget stor variation for hver variable - grundlag for skalering

str(udvalgte.variabler)

uden.scale <- udvalgte.variabler

# Standardisér numeriske variabler 
udvalgte.variabler[,-1] <- scale(udvalgte.variabler[, -1])  # Undgå første kolonne, hvis den ikke skal skaleres

clm.all <- clm(Mulighed ~ ., data = udvalgte.variabler)
summary(clm.all)


#korrelationsmatrice
korrelation.v <- udvalgte.variabler[,-1]
correlations <- cor(korrelation.v, use = "complete.obs")  # Brug 'complete.obs' for at ignorere rækker med NA
print(correlations)

library(corrplot)
corrplot(correlations, 
         method = "color", 
         type = "lower", 
         addCoef.col = "black", 
         tl.cex = 0.8,        # Størrelse på tekstetiketter
         tl.col = "black",    # Farve på tekstetikletter
         number.cex = 0.7,    # Størrelse på tal
         col = colorRampPalette(c("blue", "white", "red"))(200),  # Farveskala fra blå til rød
         diag = FALSE,        # Skjul diagonal for klarere visning
         tl.srt = 45,         # Roter tekstetiketterne, så de bliver skrå
         tl.offset = 1,     # Juster afstanden for at få etiketterne tættere på bunden
         cl.pos = "n",
         outline = T)        # Placer farveskalaen på højre side


library(ggcorrplot)

library(ordinal)
#clm på enketle
clm.soliditet <- clm(Mulighed ~ Soliditetsgrads, data = udvalgte.variabler)
summary(clm.soliditet)

clm.balance <- clm(Mulighed ~ Balance, data = udvalgte.variabler)
summary(clm.balance)

clm.gearing <- clm(Mulighed ~ Gearing, data = udvalgte.variabler)
summary(clm.gearing)

clm.gældsforhold <- clm(Mulighed ~ Gældsforhold, data = udvalgte.variabler)
summary(clm.gældsforhold)

clm.egenkapital <- clm(Mulighed ~ Egenkapital, data = udvalgte.variabler)
summary(clm.egenkapital)


#graf af en enkelt
#Balance
library(ggplot2)
library(tidyr)
library(dplyr)

uden.scale$Balance <- log(uden.scale$Balance)

# Gruppér data efter 'Mulighed' og beregn gennemsnit og antal for alle variabler
grupperet.mulighed <- uden.scale %>%
  group_by(Mulighed) %>%  # Gruppér efter 'Mulighed'
  summarise(across(everything(), list(gennemsnit = ~mean(. , na.rm = TRUE))))  # Beregn gennemsnit af alle variabler

grupperet.mulighed$svar <- c("Meget dårlige","Dårlige", "Neutrale", "Gode", "Meget gode")

grupperet.mulighed.long <- grupperet.mulighed %>%
  pivot_longer(cols = c(Soliditetsgrads_gennemsnit, Balance_gennemsnit, Egenkapital_gennemsnit),
               names_to = "Variable",
               values_to = "Værdi")


#balance plot - m log(balance)
ggplot(grupperet.mulighed, aes(x = factor(Mulighed), y = Balance_gennemsnit, fill = Mulighed)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Størrelsen på aktiverne har betydning for lånemulighederne",
    x = "",
    y = "Log(Balance)",
    fill = "Svarmulighed"
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Meget dårlige", 
      "2" = "Dårlige", 
      "3" = "Neutrale", 
      "4" = "Gode", 
      "5" = "Meget gode"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )

#gearing plot
ggplot(grupperet.mulighed, aes(x = factor(Mulighed), y = Gearing_gennemsnit, fill = Mulighed)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "x",
    x = "",
    y = "Gearing",
    fill = "Svarmulighed"
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Meget dårlige", 
      "2" = "Dårlige", 
      "3" = "Neutrale", 
      "4" = "Gode", 
      "5" = "Meget gode"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )


#solidietesgrad plot
ggplot(grupperet.mulighed, aes(x = factor(Mulighed), y = Soliditetsgrads_gennemsnit, fill = Mulighed)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "x",
    x = "",
    y = "Gearing",
    fill = "Svarmulighed"
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Meget dårlige", 
      "2" = "Dårlige", 
      "3" = "Neutrale", 
      "4" = "Gode", 
      "5" = "Meget gode"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )

#solidietesgrad plot
ggplot(grupperet.mulighed, aes(x = factor(Mulighed), y = Gældsforhold_gennemsnit, fill = Mulighed)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "x",
    x = "",
    y = "Gæld",
    fill = "Svarmulighed"
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Meget dårlige", 
      "2" = "Dårlige", 
      "3" = "Neutrale", 
      "4" = "Gode", 
      "5" = "Meget gode"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )





