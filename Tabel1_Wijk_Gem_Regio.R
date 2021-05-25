# SPSS Complex Samples in R

# Clear workspace
rm(list=ls())

# Clear console
# cat("\014")


# set working directory
setwd("G:\\Projecten\\Tabellenboek\\")

# load libraries
library(haven)
library(dplyr)
library(survey)
library(glue)


#########################
# Initialiseer een boel #
#########################

# Basisnaam voor output bestand
output_name <- "JM1218_2019_tabel1_20200217"

# Do not create factors from string variables
options(stringsAsFactors = FALSE)

# Strata met 1 waarneming toestaan. 
options(survey.lonely.psu="certainty") # Misschien adjust beter, want conservatiever?

# Open databestand (en sla evt op als Rdata)
# data_monitor <- read_spss("JM12-18 2019 HvB BZO DEF.sav")
# saveRDS(data_monitor, file="JM12-18 2019 HvB BZO DEF.rds") 
data_monitor <- readRDS("JM12-18 2019 HvB BZO DEF.rds")

# Neem alleen data van de eigen GGD
data <- data_monitor[data_monitor$GGD == 2, ]

# Hernoem kolomnaam van Wijk naar wijk (lowercase, R is hoofdlettergevoelig)
colnames(data)[colnames(data) == "Wijk"] <- "wijk"

# Subsetten van het survey design werkt niet goed als de gemeente een string is. Daarom omzetting naar numeric.
data$cbs <- as.numeric(data$cbs)

# Check of NA voorkomt bij gemeenten
sum(is.na(data$cbs))


#### Uit te draaien variabelen inlezen en checken of ze ook in spss bestand staan

# Lees lijst in met variabelen/indicatoren die je in het tabellenboek wilt hebben. Als je bestand een kolomnaam bevat, zet dan header = TRUE. 
# Heb je alleen een lijst met de namen van de indicatoren, zet dan header = FALSE. 
var_df <- read.csv("varlijst.csv", header = FALSE) 

# Het script verwacht dat de kolom "V1" heet. 
colnames(var_df) <- "V1"

# Check of alle variabelen uit varlijst.csv ook in het sav bestand staan. Vaak voorkomend probleem is dat de indicatoren in varlijst niet 
# met hoofdletters op de juiste plek zijn ingegeven (R is hoofdlettergevoelig)
if(FALSE %in% (var_df$V1 %in% names(data))) stop('Niet alle opgegeven variabelen komen voor in SPSS bestand')

# Welke variabele mist?
# var_df[which(var_df$V1 %in% names(data)==FALSE),1] 

# Voor testdoeleinden: niet alle indicatoren uitdraaien, maar een deel ervan.
# var_df <- data.frame(V1 = var_df[1:50,1])

# Survey design aanmaken. Strata en weights zijn de variabelen die je in SPSS in de/het planfile/csaplan op zou geven.
survey_design <- svydesign(ids = ~1, data = data, strata = ~wijk, weights = ~Wi_groot)


### Reken vooraf uit hoeveel antwoordopties de indicatoren samen hebben. Hiermee kan vooraf de grootte van het dataframe dat de uitgerekende
# waarden bevat worden ingesteld. Dit is sneller dan wanneer je in een loop steeds 1 regel aan een dataframe toevoegt.

aantal_variable_labels <- 0

for (var in var_df$V1){
  
  waarden <- unique(data[[var]])
  waarden_zonder_na <- length(waarden[!is.na(waarden)])
  
  aantal_variable_labels <- aantal_variable_labels + waarden_zonder_na
}

###########################
# Zonder crossings, regio #
###########################

# Maak een lege dataframe aan op de resultaten in op te slaan
df_regio = data.frame(matrix(NA, ncol = 9, nrow = aantal_variable_labels))

# Pas de kolomnamen aan
colnames(df_regio) <- c("varcode", "waarde", "label", "n", "percentage", "CIlower", "CIupper", "n_unweighted", "gebied")

# Initialiseer regelnummer op 1, voor het wegschrijven van de uitgerekende data
regelnummer <- 1

for (varcode in var_df$V1){
  
  if (!all(is.na(data[[varcode]]))){
    
    varlabels <- attr(data[[varcode]], "labels") # value labels
    
    tb <- svytable(formula = ~data[[varcode]] , design = survey_design)
    
    ct <- prop.table(tb) # ct bevat estimates als percentages
    
    for (j in 1:length(tb)){ # Voor het aantal niet-missing antwoordopties uit de vraag
      
      val <- names(tb)[j] # val is de numerieke code van de huidige antwoordoptie
      
      # loopen met survey package wil niet op normale manier. Daarom onderstaande methode om betrouwbaarheidsintervallen te krijgen.
      string <- "svyciprop(~I({varcode}=={val}), survey_design, method='xlogit', na.rm=TRUE, level = 0.95)"
      expr <- glue(string)
      ci <- eval(parse(text = expr)) # confidence intervals
      
      ## Schrijf info weg naar dataframe
      temp_varcode <- varcode # variabelenaam
      temp_waarde <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
      temp_label <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
      
      # Als variable label ontbreekt, zet dan naar lege string
      if (length(temp_label) == 0) {
        temp_label = ""
      } 
      
      temp_n <- round(tb[[j]])  # Populatie n / gewogen n <= is populatie van hele regio, niet van subset. 
      temp_percentage <- ci[[1]] * 100 # Estimate/percentage
      temp_CIlower <- attr(ci, "ci")[1] * 100 # CI lower
      temp_CIupper <- attr(ci, "ci")[2] * 100 # CI upper
      n_unweighted <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
      temp_gebied <- "Regio"
      
      # Schrijf data weg naar dataframe
      df_regio[regelnummer,] <- c(temp_varcode, temp_waarde, temp_label, temp_n, temp_percentage, temp_CIlower, temp_CIupper, temp_n_unweighted, temp_gebied)
      
      # Print huidige regelnummer om idee te krijgen hoe lang script nog zal runnen
      print(paste0(regelnummer, " van ", aantal_variable_labels))
      
      # Hoog regelnummer met 1 op om de volgende regel in de dataframe te vullen met de volgende indicator/antwoordoptie
      regelnummer <- regelnummer + 1
    }
  }  
}

# Zet naar numeric
df_regio$waarde <- as.numeric(df_regio$waarde)
df_regio$n <- as.numeric(df_regio$n)
df_regio$percentage <- as.numeric(df_regio$percentage)
df_regio$CIlower <- as.numeric(df_regio$CIlower)
df_regio$CIupper <- as.numeric(df_regio$CIupper)
df_regio$n_unweighted <- as.numeric(df_regio$n_unweighted)


# Tel aantal geldige antwoorden per vraag op.
df_regio <- df_regio %>%
  group_by(varcode) %>%
  mutate(n_vraag = sum(n_unweighted)) %>%
  ungroup()  

# Verwijder resultaten voor variabelen waarbij minder dan 30 respondenten antwoord hebben gegeven (zou bij regio niet nodig moeten zijn, maar ach)
df_regio$percentage[df_regio$n_vraag < 30] <- NA
df_regio$CIlower[df_regio$n_vraag < 30] <- NA
df_regio$CIupper[df_regio$n_vraag < 30] <- NA

# Maak koppelkolom aan
df_regio$varval <- paste0(df_regio$varcode, df_regio$waarde)

# Sla de data op als csv bestand
write.csv(df_regio, file = paste0(output_name, "_regio.csv"), row.names = FALSE)



##################################################
# Zonder crossing, gemeente niveau CI 90% en 95% #
##################################################

# Moet opgeschoond worden met subfunctie. Teveel code duplication.


# Bereken het aantal antwoordopties voor alle indicatoren, om te bepalen hoe groot de dataframe moet zijn waar alle data in wordt 
# opgeslagen. Dit is sneller dan wanneer je in een loop steeds 1 regel aan een dataframe toevoegt.
# Voor gemeenten is dit het aantal antwoordopties voor alle indicatoren samen keer het aantal gemeenten dat er in de data zit.

aantal_verwachte_rijen <- aantal_variable_labels * length(unique(data$cbs))

# Maak een lege dataframe aan op de resultaten in op te slaan
df_gem_ci95 = data.frame(matrix(NA, ncol = 8, nrow = aantal_verwachte_rijen))
df_gem_ci90 = data.frame(matrix(NA, ncol = 8, nrow = aantal_verwachte_rijen))

# Pas de kolomnamen aan
colnames(df_gem_ci95) <- c("varcode", "waarde", "label", "percentage", "CIlower", "CIupper", "n_unweighted", "gebied")
colnames(df_gem_ci90) <- c("varcode", "waarde", "label", "percentage", "CIlower", "CIupper", "n_unweighted", "gebied")

# Initialiseer regelnummer op 1, voor het wegschrijven van de uitgerekende data
regelnummer <- 1

for (gemeente in unique(data$cbs)) {

  # survey_design_sub <- subset(survey_design, cbs == gemeente) geeft hele rare output, doordat gemeente als R variabele wordt ingevoerd ipv tekst. 
  # Subset geeft geen foutmelding, maar wordt gedaan op verkeerde gemeente. Onderstaande werkt wel
  tekst <- "subset(survey_design, cbs == {gemeente})"
  survey_design_sub  <- eval(parse(text = glue(tekst)))
  
  data_sub <- data[data$cbs == gemeente & !is.na(data$cbs),] # Ook hier geen subset() gebruiken, geeft verkeerde data terug
  
  for (varcode in var_df$V1){
    
    if (!all(is.na(data[[varcode]]))){
  
      varlabels <- attr(data[[varcode]], "labels") # value labels. # value labels. Gebruik data ipv data_sub, omdat je anders labels mist als een bepaald antwoord niet voorkomt bij de eerste gemeente
  
      # tb <- svytable(formula = ~data_sub[[varcode]] , design = survey_design_sub) # Hiermee mis ik de antwoordopties die niemand heeft gekozen
      tb <- svytable(formula = ~data[[varcode]] , design = survey_design) # Deze bevat populatieaantallen. Prima als je tb alleen voor labels gebruikt en niet de gewogen n per gemeente wil weten
      
      for (j in 1:length(tb)){ # Voor het aantal niet-missing antwoordopties uit de vraag
        
        val <- names(tb)[j] # val is de numerieke code van de huidige antwoordoptie
        
        # loopen met survey package wil niet op normale manier. Daarom methode met glue en eval om betrouwbaarheidsintervallen te krijgen.
        
        # Eerst met confidence level 95% voor de vergelijking met regio, daarna met confidence level 90% voor vergelijking met wijk
        
        ## 95%
        string_95 <- "svyciprop(~I({varcode}=={val}), survey_design_sub, method='xlogit', na.rm=TRUE, level = 0.95)"
        expr_95 <- glue(string_95)
        ci_95 <- eval(parse(text = expr_95)) # confidence intervals
        
        # Schrijf info weg naar dataframe
        temp_varcode <- varcode # variabelenaam
        temp_waarde <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
        temp_label <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
        
        # Als variable label ontbreekt, zet dan naar lege string
        if (length(temp_label) == 0) {
          temp_label = ""
        } 
        
        # temp_n <- round(tb_regio[[j]])  # Populatie n / gewogen n  <= is populatie van hele regio, niet van subset.
        temp_percentage <- ci_95[[1]] * 100 # Estimate/percentage
        temp_CIlower <- attr(ci_95, "ci")[1] * 100 # CI lower
        temp_CIupper <- attr(ci_95, "ci")[2] * 100 # CI upper
        temp_n_unweighted <- sum(survey_design_sub[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
        temp_gebied <- gemeente
        
        # Schrijf data weg naar dataframe
        df_gem_ci95[regelnummer,] <- c(temp_varcode, temp_waarde, temp_label, temp_percentage, temp_CIlower, temp_CIupper, temp_n_unweighted, temp_gebied)
        
        
        ## 90%
        string_90 <- "svyciprop(~I({varcode}=={val}), survey_design_sub, method='xlogit', na.rm=TRUE, level = 0.90)"
        expr_90 <- glue(string_90)
        ci_90 <- eval(parse(text = expr_90)) # confidence intervals
        
        # Alleen betrouwbaarheidsintervallen hoeven opnieuw te worden uitgerekend, de rest blijft hetzelfde
        temp_CIlower <- attr(ci_90, "ci")[1] * 100 # CI lower
        temp_CIupper <- attr(ci_90, "ci")[2] * 100 # CI upper
        
        # Schrijf data weg naar dataframe
        df_gem_ci90[regelnummer,] <- c(temp_varcode, temp_waarde, temp_label, temp_percentage, temp_CIlower, temp_CIupper, temp_n_unweighted, temp_gebied)
        
        
        # Print huidige regelnummer om idee te krijgen hoe lang script nog zal runnen
        print(paste0(regelnummer, " van ", aantal_verwachte_rijen))
        
        # Hoog regelnummer met 1 op om de volgende regel in de dataframe te vullen met de volgende indicator/antwoordoptie
        regelnummer <- regelnummer + 1

      }
    }
  }  
}

# Zet naar numeric
df_gem_ci95$waarde <- as.numeric(df_gem_ci95$waarde)
df_gem_ci95$percentage <- as.numeric(df_gem_ci95$percentage)
df_gem_ci95$CIlower <- as.numeric(df_gem_ci95$CIlower)
df_gem_ci95$CIupper <- as.numeric(df_gem_ci95$CIupper)
df_gem_ci95$n_unweighted <- as.numeric(df_gem_ci95$n_unweighted)

# Zet naar numeric
df_gem_ci90$waarde <- as.numeric(df_gem_ci90$waarde)
df_gem_ci90$percentage <- as.numeric(df_gem_ci90$percentage)
df_gem_ci90$CIlower <- as.numeric(df_gem_ci90$CIlower)
df_gem_ci90$CIupper <- as.numeric(df_gem_ci90$CIupper)
df_gem_ci90$n_unweighted <- as.numeric(df_gem_ci90$n_unweighted)


# Tel aantal geldige antwoorden per vraag op.
df_gem_ci95 <- df_gem_ci95 %>%
  group_by(varcode, gebied) %>%
  mutate(n_vraag = sum(n_unweighted)) %>%
  ungroup()  

# Zelfde voor confidence level 90%
df_gem_ci90 <- df_gem_ci90 %>%
  group_by(varcode, gebied) %>%
  mutate(n_vraag = sum(n_unweighted)) %>%
  ungroup()  


df_gem_ci95$percentage[df_gem_ci95$n_vraag < 30] <- NA
df_gem_ci95$CIlower[df_gem_ci95$n_vraag < 30] <- NA
df_gem_ci95$CIupper[df_gem_ci95$n_vraag < 30] <- NA

df_gem_ci90$percentage[df_gem_ci90$n_vraag < 30] <- NA
df_gem_ci90$CIlower[df_gem_ci90$n_vraag < 30] <- NA
df_gem_ci90$CIupper[df_gem_ci90$n_vraag < 30] <- NA


# Maak cbsstr aan
df_gem_ci95$cbsstr <- as.character(df_gem_ci95$gebied)

df_gem_ci90$cbsstr <- as.character(df_gem_ci90$gebied)

# Maak koppelkolom aan
df_gem_ci95$varval <- paste0(df_gem_ci95$varcode, df_gem_ci95$waarde)
df_gem_ci90$varval <- paste0(df_gem_ci90$varcode, df_gem_ci90$waarde)

write.csv(df_gem_ci95, paste0(output_name, "_gem_ci95.csv"),  row.names = FALSE)
write.csv(df_gem_ci90, paste0(output_name, "_gem_ci90.csv"),  row.names = FALSE)


## Aan de gemeentetabel (ci 95%) wil ik de estimates, ci lowers en ci uppers van de regiotabel plakken

# # Lees df_regio in, als deze niet in de R omgeving staat
# df_regio <- read.csv(paste0(output_name, "_regio.csv"))

# Koppel aan regio
gem_vs_regio <- dplyr::left_join(df_gem_ci95, df_regio, by = "varval", suffix = c("_gem", "_regio"))

# Bij gemeente getallen naar 0 zetten ipv NA
gem_vs_regio$percentage_gem[is.na(gem_vs_regio$n_unweighted_gem)] <- 0

# Overblijfsel uit den ouden tijd, maar misschien nog handig voor latere sorteringen
# gem_vs_regio <- gem_vs_regio[order(gem_vs_regio$sorteervariabele),]

# Them stats.
gem_vs_regio$Sign <- "0"

for (i in 1:nrow(gem_vs_regio) ){
  # Bij binnenste 2 if-loops mag de conditie niet uitkomen op NA. Check daarom eerst of er NAs zijn in CI. Alleen lower checken is genoeg.
  if (!is.na(gem_vs_regio[i,'CIlower_gem']) & !is.na(gem_vs_regio[i,'CIlower_regio'])) {
    if (gem_vs_regio[i,'CIupper_gem'] < gem_vs_regio[i,'CIlower_regio']) {gem_vs_regio$Sign[i] = "1"}
    if (gem_vs_regio[i,'CIlower_gem'] > gem_vs_regio[i,'CIupper_regio']) {gem_vs_regio$Sign[i] = "1"}
  }
}


# Significanties waarbij n < 30 op NA zetten ipv 0
gem_vs_regio$Sign[gem_vs_regio$n_vraag_gem < 30] <- "n<30"

# Afspraak: Als regio en gemeente allebei een estimate van 1 (100%) of 0 (0%) hebben, dan significantie naar "Niet significant" zetten, bij andere percentages niet.
gem_vs_regio$Sign[gem_vs_regio$percentage_gem >= 99.5 & gem_vs_regio$percentage_regio >= 99.5] <- "0"
gem_vs_regio$Sign[gem_vs_regio$percentage_gem < 0.5 & gem_vs_regio$percentage_regio < 0.5] <- "0"

write.csv(gem_vs_regio, paste0(output_name, "_gem_vs_regio_def.csv"),  row.names = FALSE)


################################
# Zonder crossing, wijk niveau #
################################

# Vrijwel identiek aan gemeenteniveau. In de toekomst omzetten naar subfunctie.


# Bereken het aantal antwoordopties voor alle indicatoren, om te bepalen hoe groot de dataframe moet zijn waar alle data in wordt 
# opgeslagen. Dit is sneller dan wanneer je in een loop steeds 1 regel aan een dataframe toevoegt.
# Voor wijken is dit het aantal antwoordopties voor alle indicatoren samen keer het aantal wijken dat er in de data zit.

aantal_verwachte_rijen <- aantal_variable_labels * length(unique(data$wijk))


df_wijk = data.frame(matrix(NA, ncol = 8, nrow = aantal_verwachte_rijen))
colnames(df_wijk) <- c("varcode", "waarde", "label", "percentage", "CIlower", "CIupper", "n_unweighted", "gebied")

# Initialiseer regelnummer op 1, voor het wegschrijven van de uitgerekende data
regelnummer <- 1

for (wijk in unique(data$wijk)){
  
  # survey_design_sub <- subset(survey_design, cbs == gemeente) geeft hele rare output, doordat gemeente als R variabele wordt ingevoerd ipv tekst. 
  # Subset geeft geen foutmelding, maar wordt gedaan op verkeerde gemeente. Onderstaande werkt wel
  tekst <- "subset(survey_design, wijk == {wijk})"
  survey_design_sub  <- eval(parse(text = glue(tekst)))
  
  data_sub <- data[data$wijk == wijk & !is.na(data$wijk),] # Ook hier geen subset gebruiken, geeft verkeerde data terug
  
  for (varcode in var_df$V1){
    
    if (!all(is.na(data_sub[[varcode]]))){
    
      varlabels <- attr(data[[varcode]], "labels") # value labels. Gebruik data ipv data_sub, omdat je anders labels mist als een bepaald antwoord niet voorkomt bij de eerste wijk.
      
      tb <- svytable(formula = ~data[[varcode]] , design = survey_design) # Deze bevat populatieaantallen. Prima als je tb alleen voor labels gebruikt en niet de gewogen n per gemeente wil weten
      
      ct <- prop.table(tb) # ct bevat estimates als percentages
      
      for (j in 1:length(tb)){ # Voor het aantal niet-missing antwoordopties uit de vraag
        
        val <- names(tb)[j] # val is de numerieke code van de huidige antwoordoptie
        
        # loopen met survey package wil niet op normale manier. Daarom onderstaande methode om betrouwbaarheidsintervallen te krijgen.
        string <- "svyciprop(~I({varcode}=={val}), survey_design_sub, method='xlogit', na.rm=TRUE, level = 0.90)"
        expr <- glue(string)
        ci <- eval(parse(text = expr)) # confidence intervals
        
        ## Schrijf info weg naar dataframe
        temp_varcode <- varcode # variabelenaam
        temp_waarde <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
        temp_label <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
        
        # Als variable label ontbreekt, zet dan naar lege string
        if (length(temp_label) == 0) {
          temp_label = ""
        } 
        
        
        # df_wijk[idx, 4] <- round(tb[[j]])  # Populatie n / gewogen n <= is populatie van hele regio, niet van subset.
        temp_percentage <- ci[[1]] # Estimate/percentage
        temp_CIlower <- attr(ci, "ci")[1] # CI lower
        temp_CIupper <- attr(ci, "ci")[2] # CI upper
        temp_n_unweighted <- sum(survey_design_sub[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
        temp_gebied <- wijk
        
        # Schrijf data weg naar dataframe
        df_wijk[regelnummer,] <- c(temp_varcode, temp_waarde, temp_label, temp_percentage, temp_CIlower, temp_CIupper, temp_n_unweighted, temp_gebied)
        
        # Print huidige regelnummer om idee te krijgen hoe lang script nog zal runnen
        print(paste0(regelnummer, " van ", aantal_verwachte_rijen))
        
        # Hoog regelnummer met 1 op om de volgende regel in de dataframe te vullen met de volgende indicator/antwoordoptie
        regelnummer <- regelnummer + 1
        
      }
    }  
  }
}

# Zet naar numeric
df_wijk$waarde <- as.numeric(df_wijk$waarde)
df_wijk$percentage <- as.numeric(df_wijk$percentage)
df_wijk$CIlower <- as.numeric(df_wijk$CIlower)
df_wijk$CIupper <- as.numeric(df_wijk$CIupper)
df_wijk$n_unweighted <- as.numeric(df_wijk$n_unweighted)


# Tel aantal geldige antwoorden per vraag op.
df_wijk <- df_wijk %>%
  group_by(varcode, gebied) %>%
  mutate(n_vraag = sum(n_unweighted)) %>%
  ungroup()  


df_wijk$percentage[df_wijk$n_vraag < 30] <- NA
df_wijk$CIlower[df_wijk$n_vraag < 30] <- NA
df_wijk$CIupper[df_wijk$n_vraag < 30] <- NA


df_wijk$varval <- paste0(df_wijk$varcode, df_wijk$waarde)


write.csv(df_wijk, file = paste0(output_name, "_wijk_ci90.csv"), row.names = FALSE)


# Koppeltabel om gemeenten bij wijken te krijgen in df_wijk
wijk_gemeente_tabel <- data[,c('wijk', "cbs")]
wijk_gemeente_tabel2 <- wijk_gemeente_tabel[!duplicated(wijk_gemeente_tabel$wijk),]

wijk_gemeente_tabel2$gebied <- as.character(wijk_gemeente_tabel2$wijk)

df_wijk$wijk_char <- as.character(df_wijk$gebied)

# Koppel gemeenten aan df_wijk
df_wijk2 <- dplyr::left_join(df_wijk, wijk_gemeente_tabel2,  by = c("wijk_char"= "gebied"), suffix = c("", "_koppel"))
 
# Maak koppelcode in beide dataframes
df_wijk2$key <- paste0(df_wijk2$varval, df_wijk2$cbs)

df_gem_ci90$cbs <- as.numeric(df_gem_ci90$cbsstr)
df_gem_ci90$key <- paste0(df_gem_ci90$varcode, df_gem_ci90$waarde, df_gem_ci90$cbs)

# Koppel wijk en gem
wijk_vs_gem <- dplyr::left_join(df_wijk2, df_gem_ci90,  by = "key", suffix = c("_wijk", "_gem"))

# Them stats.
wijk_vs_gem$Sign <- "0"


for (i in 1:nrow(wijk_vs_gem) ){
  # Bij binnenste 2 if-loops mag de conditie niet uitkomen op NA. Check daarom eerst of er NAs zijn in CI. Alleen lower checken is genoeg.
  if (!is.na(wijk_vs_gem[i,'CIlower_wijk']) & !is.na(wijk_vs_gem[i,'CIlower_gem'])) {
    if (wijk_vs_gem[i,'CIupper_wijk'] < wijk_vs_gem[i,'CIlower_gem']) {wijk_vs_gem$Sign[i] = "1"}
    if (wijk_vs_gem[i,'CIlower_wijk'] > wijk_vs_gem[i,'CIupper_gem']) {wijk_vs_gem$Sign[i] = "1"}
  }
}

# Significanties waarbij n < 30 op NA zetten ipv 0
wijk_vs_gem$Sign[wijk_vs_gem$n_vraag_wijk < 30] <- "n<30"

# Afspraak: Als regio en gemeente allebei een estimate van 1 (100%) of 0 (0%) hebben, dan significantie naar "Niet significant" zetten, bij andere percentages niet.
wijk_vs_gem$Sign[wijk_vs_gem$percentage_wijk >= 99.5 & wijk_vs_gem$percentage_gem >= 99.5] <- "0"
wijk_vs_gem$Sign[wijk_vs_gem$percentage_wijk < 0.5 & wijk_vs_gem$percentage_gem < 0.5] <- "0"


write.csv(wijk_vs_gem, file = paste0(output_name, "wijk_vs_gem_def.csv"), row.names = FALSE)



