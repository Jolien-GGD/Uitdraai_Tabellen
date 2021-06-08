# Probeersel Complex Samples in R

# Clear workspace
rm(list=ls())

# Clear console
# cat("\014")

# Do not create factors from string variables
options(stringsAsFactors = FALSE)

# set working directory
setwd("F:\\Hvb_Onderzoek\\Projecten\\Tabellenboek\\Ouderen- en volwassenenmonitor 2020")

# load libraries
library(haven)
library(dplyr)
library(survey)
library(glue)
library(openxlsx)
library(beepr)

source('subfunctie_df_gebied.R')
source('subfunctie_significantie.R')

# Wat te vinden in bepaalde kolommen
# PrimaireEenheid = strata voor weging met weegfactor ewGGDlang
# wijk_excel = voor uitdraai wijkniveau
# Gemeentecode = voor uitdraai gemeente 
# Subregio = voor uitdraai subregio
# GGD = voor uitdraai GGD-regio


#########################
# Initialiseer een boel #
#########################

# Basisnaam voor output bestand
output_name <- "output/OM_2020_HvB_tabel1"

# Kies 1 varlijst
# varlijst <- "data/aangeleverd/varlijst_VM_regio.csv"
varlijst <- "data/aangeleverd/varlijst_OM_regio.csv"
# varlijst <- "data/aangeleverd/varlijst_VM_landelijk.csv"
# varlijst <- "data/aangeleverd/varlijst_OM_landelijk.csv"


# Strata met 1 waarneming toestaan. 
options(survey.lonely.psu="certainty") # Misschien adjust beter, want conservatiever?

# Open databestand (en sla evt op als Rdata)
# data_monitor <- read_spss("data/aangeleverd/Databestand GM2020 ZeeBra - compleet (jun).sav")
# saveRDS(data_monitor, file="data/aangeleverd/Databestand GM2020 ZeeBra - compleet (jun).rds") 
data_monitor <- readRDS("data/aangeleverd/Databestand GM2020 ZeeBra - compleet (jun).rds")

# Geen CBS respondenten meenemen, alleen GGD monitor
data_monitor <- data_monitor[data_monitor$Surveynummer == 1,]

# data <- data_monitor[data_monitor$GGD == 1, ] # West
data <- data_monitor[data_monitor$GGD == 2, ] # HvB
# data <- data_monitor[data_monitor$GGD == 3, ] # BZO
# data <- data_monitor[data_monitor$GGD == 4, ] # Zeeland

# MIBNV201 geeft aan of iets VM of OM is (en GGD of CBS, maar CBS was er al uit). Check of er missings zijn op deze variabele.
# sum(is.na(data$MIBNV201))

# Subset naar VM of OM
# data <- data[data$MIDGV201 == 1,] # VM
data <- data[data$MIDGV201 == 2,] # OM

# Check of variabelen numeric zijn
str(data$PrimaireEenheid)
str(data$WIJK)
str(data$Gemeentecode)
str(data$Subregio)
str(data$GGD)

# Check op NA 
sum(is.na(data$PrimaireEenheid))
sum(is.na(data$WIJK))
sum(is.na(data$Gemeentecode))
sum(is.na(data$Subregio))
sum(is.na(data$GGD))

# Hernoem kolomnamen naar namen waarop onderstaande code gebouwd is
colnames(data)[colnames(data) == "WIJK"] <- "wijk"
colnames(data)[colnames(data) == "Gemeentecode"] <- "cbs"


#### Uit te draaien variabelen inlezen en checken of ze ook in spss bestand staan
var_df <- read.csv(varlijst, header= TRUE) # Lees lijst in met variabelen/indicatoren die je in het tabellenboek wilt hebben

# Hernoem eerste kolom van var_df naar V1 (dat verwacht script)
colnames(var_df) <- "V1"

if(FALSE %in% (var_df$V1 %in% names(data))) stop('Niet alle opgegeven variabelen komen voor in SPSS bestand') # Check of alle variabelen uit varlijst.csv ook in het sav bestand staan
# var_df[which(var_df$V1 %in% names(data)==FALSE),1] # Welke variabele mist?



# # Voor testdoeleinden
# var_df <- data.frame(V1 = var_df[1:10,1])

# Subsetten van het survey design werkt niet goed als de gebied een string is. Daarom omzetting naar numeric.
data$cbs <- as.numeric(data$cbs)
# data$wijk <- as.numeric(data$wijk) # is al numeric
# data$Subregio <- as.numeric(data$Subregio) # is al numeric

# Survey design aanmaken
survey_design <- svydesign(ids = ~1, data = data, strata = ~PrimaireEenheid, weights = ~ewGGDlang) 


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
      
      # Schrijf info weg naar dataframe
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
      temp_n_unweighted <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
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

beep(sound = 3)

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

# Verwijder resultaten voor variabelen waarbij minder dan 50 respondenten antwoord hebben gegeven (zou bij regio niet nodig moeten zijn, maar ach)
df_regio$percentage[df_regio$n_vraag < 50] <- NA
df_regio$CIlower[df_regio$n_vraag < 50] <- NA
df_regio$CIupper[df_regio$n_vraag < 50] <- NA

# Maak koppelkolom aan
df_regio$varval <- paste0(df_regio$varcode, df_regio$waarde)

# write.csv(df_regio, file = paste0(output_name, "_regio.csv"), row.names = FALSE)
write.xlsx(df_regio, file = paste0(output_name, "_regio_ci95.xlsx"))


###########################################
# Zonder crossing, gemeente niveau CI 95% #
###########################################

# Bereken het aantal antwoordopties voor alle indicatoren, om te bepalen hoe groot de dataframe moet zijn waar alle data in wordt 
# opgeslagen. Dit is sneller dan wanneer je in een loop steeds 1 regel aan een dataframe toevoegt.
# Voor gemeenten is dit het aantal antwoordopties voor alle indicatoren samen keer het aantal gemeenten dat er in de data zit.

aantal_verwachte_rijen <- aantal_variable_labels * length(unique(data$cbs))

# Maak een lege dataframe aan op de resultaten in op te slaan
df_gem_ci95 = data.frame(matrix(NA, ncol = 8, nrow = aantal_verwachte_rijen))

# Pas de kolomnamen aan
colnames(df_gem_ci95) <- c("varcode", "waarde", "label", "percentage", "CIlower", "CIupper", "n_unweighted", "gebied")

# Bereken data met survey package via zelfgeschreven subfunctie
df_gem_ci95 <- subgebied(df_gebied = df_gem_ci95, gebiedskolom = "cbs", ci_level = 0.95, n_vraag_afkapwaarde = 50, data = data, var_df = var_df, survey_design = survey_design)

write.xlsx(df_gem_ci95, file = paste0(output_name, "_gem_ci95.xlsx"))

## Aan de gemeentetabel (ci 95%) wil ik de estimates, ci lowers en ci uppers van de regiotabel plakken

# # Lees in indien niet meer beschikbaar binnen R sessie
# df_gem_ci95 <- read.csv("data/output/wijk_excel als strata/BZO/ongekoppeld/VM_2020_BZO_tabel1_gem_ci95.csv")
# df_regio <- read.csv("data/output/wijk_excel als strata/BZO/ongekoppeld/VM_2020_BZO_tabel1_regio.csv")

# Koppel aan regio
gem_vs_regio <- dplyr::left_join(df_gem_ci95, df_regio, by = "varval", suffix = c("_gem", "_regio"))

# Bij gemeente getallen naar 0 zetten ipv NA
gem_vs_regio$percentage_gem[is.na(gem_vs_regio$n_unweighted_gem)] <- 0

# Test significantie
gem_vs_regio <- significantie_testen(df = gem_vs_regio, n_vraag_afkapwaarde = 50, gebied_klein = "gem", gebied_groot = "regio")

# write.csv(gem_vs_regio, paste0(output_name, "_gem95_vs_regio.csv"),  row.names = FALSE)
write.xlsx(gem_vs_regio, file = paste0(output_name, "_gem95_vs_regio95.xlsx"))



############
# Subregio #
############

# Bereken aantal benodigde rijen voor dataframe waarin cijfers worden opgeslagen
aantal_verwachte_rijen <- aantal_variable_labels * length(unique(data$Subregio))

# Maak dataframe aan en hernoem kolommen
df_subregio = data.frame(matrix(NA, ncol = 8, nrow = aantal_verwachte_rijen))
colnames(df_subregio) <- c("varcode", "waarde", "label", "percentage", "CIlower", "CIupper", "n_unweighted", "gebied")

# Initialiseer regelnummer op 1, voor het wegschrijven van de uitgerekende data
regelnummer <- 1

# Bereken data met survey package via zelfgeschreven subfunctie
df_subregio <- subgebied(df_gebied = df_subregio, gebiedskolom = "Subregio", ci_level = 0.95, n_vraag_afkapwaarde = 50, data = data, var_df = var_df, survey_design = survey_design)

# Sla op
write.xlsx(df_subregio, file = paste0(output_name, "_subregio_ci95.xlsx"))

# df_gem_ci95 <- read.xlsx("VM_2020_BZO_tabel1_gem_ci95.xlsx")

# Maak een tabel die bevat in welke subregio een gemeente valt. Koppel op die manier de info over subregio aan de dataframe met cijfers van de gemeente
gemeente_subregio_tabel <- data[,c('cbs', "Subregio")]
gemeente_subregio_tabel <- gemeente_subregio_tabel[!duplicated(gemeente_subregio_tabel$cbs),]
gemeente_subregio_tabel$gebied <- as.character(gemeente_subregio_tabel$cbs)

# Voeg de subregio toe aan de gemeentecijfers
# df_gem_ci95 <- read.xlsx("VM_2020_BZO_tabel1_gem_ci95.xlsx")
df_gem_ci95 <-  dplyr::left_join(df_gem_ci95, gemeente_subregio_tabel, by = c("gebied"), suffix = c("", "_koppel"))

# Beide bestanden hebben een koppelvariabele nodig, bestaande uit subregio en varval. In df_subregio zit deze al. In gemeentebestand moet deze nog aangemaakt.
df_gem_ci95$gebied_varval_koppel <- paste0(df_gem_ci95$Subregio, df_gem_ci95$varval)

# Koppel de cijfers van de hele subregio aan cijfers van de gemeenten
gemeente_vs_subregio <- dplyr::left_join(df_gem_ci95, df_subregio,  by = c("gebied_varval_koppel" = "gebied_varval"), suffix = c("_gem", "_subregio"))

# Test significantie
gemeente_vs_subregio <- significantie_testen(df = gemeente_vs_subregio, n_vraag_afkapwaarde = 50, gebied_klein = "gem", gebied_groot = "subregio")


write.xlsx(gemeente_vs_subregio, file = paste0(output_name, "_gem95_vs_subregio95.xlsx"))


################################
# Zonder crossing, wijk niveau #
################################

aantal_verwachte_rijen <- aantal_variable_labels * length(unique(data$wijk))

# Vrijwel identiek aan gemeenteniveau. In de toekomst omzetten naar subfunctie.

df_wijk = data.frame(matrix(NA, ncol = 8, nrow = aantal_verwachte_rijen))
colnames(df_wijk) <- c("varcode", "waarde", "label", "percentage", "CIlower", "CIupper", "n_unweighted", "gebied")

# Initialiseer regelnummer op 1, voor het wegschrijven van de uitgerekende data
regelnummer <- 1

# Bereken data met survey package via zelfgeschreven subfunctie
df_wijk <- subgebied(df_gebied = df_wijk, gebiedskolom = "wijk", ci_level = 0.90, n_vraag_afkapwaarde = 50, data = data, var_df = var_df, survey_design = survey_design)

# write.csv(df_wijk, file = paste0(output_name, "_wijk_ci90.csv"), row.names = FALSE)
write.xlsx(df_wijk, file = paste0(output_name, "_wijk_ci90.xlsx"))


# df_gem_ci95 <- read.xlsx("VM_2020_HvB_tabel1_gem_ci95.xlsx")
# df_wijk <- read.xlsx("VM_2020_HvB_tabel1_wijk_ci90.xlsx")

# Koppeltabel om gemeenten bij wijken te krijgen in df_wijk
wijk_gemeente_tabel <- data[,c('wijk', "cbs")]
wijk_gemeente_tabel2 <- wijk_gemeente_tabel[!duplicated(wijk_gemeente_tabel$wijk),]

wijk_gemeente_tabel2$gebied <- as.character(wijk_gemeente_tabel2$wijk)

df_wijk$wijk_char <- as.character(df_wijk$gebied)


# Koppel gemeenten aan df_wijk
df_wijk2 <- dplyr::left_join(df_wijk, wijk_gemeente_tabel2,  by = c("wijk_char"= "gebied"), suffix = c("", "_koppel"))

# Maak koppelcode in beide dataframes
df_wijk2$key <- paste0(df_wijk2$varval, df_wijk2$cbs)

# df_gem_ci95$cbs <- as.numeric(df_gem_ci95$cbsstr)
df_gem_ci95$key <- paste0(df_gem_ci95$varcode, df_gem_ci95$waarde, df_gem_ci95$gebied)


# Koppel wijk en gem
wijk_vs_gem <- dplyr::left_join(df_wijk2, df_gem_ci95,  by = "key", suffix = c("_wijk", "_gem"))

# Test significantie
wijk_vs_gem <- significantie_testen(df = wijk_vs_gem, n_vraag_afkapwaarde = 50, gebied_klein = "wijk", gebied_groot = "gem")

# write.csv(wijk_vs_gem, file = paste0(output_name, "wijk_ExtraTilburg_vs_gem_def.csv"), row.names = FALSE)
write.xlsx(wijk_vs_gem, file = paste0(output_name, "_wijk90_vs_gem95.xlsx"))



#############
# Landelijk #
#############


## Foei, weer veel code herhalingen. Later opschonen?

# Basisnaam voor outputbestand
output_name <- "output/OM_2020_tabel1"

# Open databestand (en sla evt op als Rdata)
# data_monitor <- read_spss("data/aangeleverd/Databestand GM2020 Landelijk met ZeeBra-indicatoren.sav")
# saveRDS(data_monitor, file="data/aangeleverd/Databestand GM2020 Landelijk met ZeeBra-indicatoren.rds") 
data_monitor <- readRDS("data/aangeleverd/Databestand GM2020 Landelijk met ZeeBra-indicatoren.rds")

# Geen CBS respondenten meenemen, alleen GGD monitor
data <- data_monitor[data_monitor$Surveynummer == 1,]

# MIBNV201 geeft aan of iets VM of OM is (en GGD of CBS, maar CBS was er al uit). Check op missings.
# sum(is.na(data$MIBNV201))

# Subset naar VM of OM
# data <- data[data$MIDGV201 == 1,] # VM
data <- data[data$MIDGV201 == 2,] # OM

# varlijst <- "data/aangeleverd/varlijst_VM_landelijk.csv"
varlijst <- "data/aangeleverd/varlijst_OM_landelijk.csv"

#### Uit te draaien variabelen inlezen en checken of ze ook in spss bestand staan
var_df <- read.csv(varlijst, header= TRUE) # Lees lijst in met variabelen/indicatoren die je in het tabellenboek wilt hebben

colnames(var_df) <- "V1" # Hernoem eerste kolom van var_df naar V1 (dat verwacht script)

if(FALSE %in% (var_df$V1 %in% names(data))) stop('Niet alle opgegeven variabelen komen voor in SPSS bestand') # Check of alle variabelen uit varlijst.csv ook in het sav bestand staan
# var_df[which(var_df$V1 %in% names(data)==FALSE),1] # Welke variabele mist?

# temp <- var_df[which(var_df$V1 %in% names(data)==FALSE),1]
# write.csv(temp, "varlijst_not_in_SPSS.csv", row.names = FALSE)

# Survey design aanmaken
survey_design <- svydesign(ids = ~1, data = data, strata = ~PrimaireEenheid, weights = ~ewGGDlang) 

aantal_variable_labels <- 0

for (var in var_df$V1){
  
  waarden <- unique(data[[var]])
  waarden_zonder_na <- length(waarden[!is.na(waarden)])
  
  aantal_variable_labels <- aantal_variable_labels + waarden_zonder_na
}

# Maak een lege dataframe aan op de resultaten in op te slaan
df_landelijk = data.frame(matrix(NA, ncol = 9, nrow = aantal_variable_labels))

# Pas de kolomnamen aan
colnames(df_landelijk) <- c("varcode", "waarde", "label", "n", "percentage", "CIlower", "CIupper", "n_unweighted", "gebied")

# Initialiseer regelnummer op 1, voor het wegschrijven van de uitgerekende data
regelnummer <- 1

# Test spul
# var_df <- data.frame(V1 = var_df[19:183,1])

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
      
      # Schrijf info weg naar dataframe
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
      temp_n_unweighted <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
      temp_gebied <- "Landelijk"
      
      # Schrijf data weg naar dataframe
      df_landelijk[regelnummer,] <- c(temp_varcode, temp_waarde, temp_label, temp_n, temp_percentage, temp_CIlower, temp_CIupper, temp_n_unweighted, temp_gebied)
      
      # Print huidige regelnummer om idee te krijgen hoe lang script nog zal runnen
      print(paste0(regelnummer, " van ", aantal_variable_labels))
      
      # Hoog regelnummer met 1 op om de volgende regel in de dataframe te vullen met de volgende indicator/antwoordoptie
      regelnummer <- regelnummer + 1
    }
  }  
}

beep(sound = 8)

# Zet naar numeric
df_landelijk$waarde <- as.numeric(df_landelijk$waarde)
df_landelijk$n <- as.numeric(df_landelijk$n)
df_landelijk$percentage <- as.numeric(df_landelijk$percentage)
df_landelijk$CIlower <- as.numeric(df_landelijk$CIlower)
df_landelijk$CIupper <- as.numeric(df_landelijk$CIupper)
df_landelijk$n_unweighted <- as.numeric(df_landelijk$n_unweighted)


# Tel aantal geldige antwoorden per vraag op.
df_landelijk <- df_landelijk %>%
  group_by(varcode) %>%
  mutate(n_vraag = sum(n_unweighted)) %>%
  ungroup()  

# Verwijder resultaten voor variabelen waarbij minder dan 50 respondenten antwoord hebben gegeven (zou bij regio niet nodig moeten zijn, maar ach)
df_landelijk$percentage[df_landelijk$n_vraag < 50] <- NA
df_landelijk$CIlower[df_landelijk$n_vraag < 50] <- NA
df_landelijk$CIupper[df_landelijk$n_vraag < 50] <- NA

# Maak koppelkolom aan
df_landelijk$varval <- paste0(df_landelijk$varcode, df_landelijk$waarde)

# write.csv(df_regio, file = paste0(output_name, "_regio.csv"), row.names = FALSE)
write.xlsx(df_landelijk, file = paste0(output_name, "_landelijk_ci95.xlsx"))

# Koppel regio en landelijk aan elkaar
# df_regio <- read.xlsx("VM_2020_Zeeland_tabel1_regio_ci95.xlsx")
# df_landelijk <- read.xlsx("VM_2020_tabel1_landelijk_ci95.xlsx")
regio_vs_landelijk <- dplyr::right_join(df_regio, df_landelijk,  by = c("varval"), suffix = c("_regio", "_landelijk"))

# Test significantie
regio_vs_landelijk <- significantie_testen(df = regio_vs_landelijk, n_vraag_afkapwaarde = 50, gebied_klein = "regio", gebied_groot = "landelijk")

write.xlsx(regio_vs_landelijk, file = paste0(output_name, "_regio95_vs_landelijk95.xlsx"))

