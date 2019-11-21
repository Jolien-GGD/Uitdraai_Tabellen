# Probeersel Complex Samples in R

# Clear workspace
rm(list=ls())

# Clear console
# cat("\014")


# set working directory
setwd("[Zet hier jouw working directory neer")

# load libraries
library(haven)
library(dplyr)
library(survey)
library(glue)


#########################
# Initialiseer een boel #
#########################

# Do not create factors from string variables
options(stringsAsFactors = FALSE)

# Strata met 1 waarneming toestaan. 
options(survey.lonely.psu="certainty") # Misschien adjust beter, want conservatiever?

# # Open databestand en sla op als Rdata
# data_volw <- expss::read_spss('landelijk en GGD_38.sav')
# saveRDS(data_volw, file="Probeersel_Volwmonitor_Tabellenboek_expss.rds") 

# Open Rdata
data_volw <- readRDS("Probeersel_Volwmonitor_Tabellenboek_expss.rds")

# Herschrijf naar data om code niet steeds aan te hoeven passen.
data <- data_volw 
# Initialiseer data_sub met volledige data. Dit omdat later in de code data_sub opgeroepen wordt, die anders niet gedefinieerd is.
data_sub <- data

# Er wordt gebruik gemaakt van zowel data (voor het opstellen van het volledige survey design) als data_sub. 
# data_sub wordt verderop gebruikt voor svytable(), om population counts van de subset te krijgen
# Als je een survey design bouwt met alleen de gesubsette data dan krijg je onbetrouwbare betrouwbaarheidsintervallen.

#survey designs opstellen (met volledige data, niet data_sub!). Er zijn twee verschillende weegfactoren (verschilt per variabele welke je nodig hebt,
# dus 2 designs aanmaken)
survey_design_ewGGD <- svydesign(ids = ~1,strata=data$PrimaireEenheid, weights=data$ewGGD, data=data) # ids = ~1 => Geen clustered sampling gebruikt
survey_design_ewCBSGGD <- svydesign(ids = ~1,strata=data$PrimaireEenheid, weights=data$ewCBSGGD, data=data)
# Initialiseer survey_design_sub met volledige data. Dit omdat later in de code survey_design_sub opgeroepen wordt, die anders niet gedefinieerd is.
survey_design_ewGGD_sub <- survey_design_ewGGD
survey_design_ewCBSGGD_sub <- survey_design_ewCBSGGD

## Subset op Iets. 
#data_sub <- subset(data, woonkern == 2) 
#survey_design_ewGGD_sub <- subset(survey_design_ewGGD, woonkern == 2) 
#survey_design_ewCBSGGD_sub <- subset(survey_design_ewCBSGGD, woonkern == 2) 

### leeg df voor cijfers
df <- data.frame(varcode = character(0), waarde = integer(0), label = character(0),
                 n = integer(0), CIest = integer(0), CIlower = integer(0), CIupper = integer(0), n_unweighted = integer(0))
# Dataframe voor crossings
df_cross <- data.frame(varcode = character(0), waarde = integer(0), label = character(0), crossing = character(0), crossing_var = character(0),
                       n = integer(0), CIest = integer(0), CIlower = integer(0), CIupper = integer(0), n_unweighted = integer(0))

#inladen lijst met de variabelen voor de tabellenboeken
var_df <- read.csv("varlijst_volw.csv", header=FALSE, sep=";")
# Lijst met crossings
var_cross <- read.csv("crosslijst_volw.csv", header=FALSE, sep=";")



####################
# Zonder crossings #
####################

for (varcode in var_df$V1){
  
  varlabels <- attr(data[[varcode]], "labels") # value labels
  
  # Maak table met juiste weegfactor. # Schrijf in beide gevallen survey design over naar survey_design, zodat voor de volgende loop maar 1 keer gedefinieerd hoeft te worden.
  if (var_df$V2[var_df$V1 == varcode] == "ewGGD") {
    survey_design <- survey_design_ewGGD_sub
  } else if (var_df$V2[var_df$V1 == varcode] == "ewCBSGGD"){
    survey_design <- survey_design_ewCBSGGD_sub
  }
  
  tb <- svytable(formula = ~data_sub[[varcode]] , design = survey_design)
  
  ct <- prop.table(tb) # ct bevat estimates als percentages
  
  for (j in 1:length(tb)){ # Voor het aantal niet-missing antwoordopties uit de vraag
    
    val <- names(tb)[j] # val is de numerieke code van de huidige antwoordoptie
    
    # loopen met survey package wil niet op normale manier. Daarom onderstaande methode om betrouwbaarheidsintervallen te krijgen.
    string <- "svyciprop(~I({varcode}=={val}), survey_design, method='xlogit', na.rm=TRUE)"
    expr <- glue(string)
    ci <- eval(parse(text = expr)) # confidence intervals
    
    # Schrijf info weg naar dataframe
    idx <- nrow(df) + 1
    df[idx, 1] <- varcode # variabelenaam
    df[idx, 2] <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
    df[idx, 3] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
    df[idx, 4] <- round(tb[[j]])  # Populatie n / gewogen n <= is populatie van hele regio, niet van subset. 
    df[idx, 5] <- ci[[1]] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
    df[idx, 6] <- attr(ci, "ci")[1] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
    df[idx, 7] <- attr(ci, "ci")[2] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
    df[idx, 8] <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
    
  }
}  

attr(data[[varcode]], "labels")

# Tel aantal geldige antwoorden per vraag op.
df <- df %>%
  group_by(varcode) %>%
  mutate(n_vraag = sum(n_unweighted)) %>%
  ungroup()  
  
  
#############
# CROSSINGS #
#############

for (varcode in var_df$V1){
  
  varlabels <- attr(data[[varcode]], "labels") # value labels
  
  # Maak table met juiste weegfactor
  if (var_df$V2[var_df$V1 == varcode] == "ewGGD") {
    survey_design <- survey_design_ewGGD_sub
  }
  else if (var_df$V2[var_df$V1 == varcode] == "ewCBSGGD"){
    survey_design <- survey_design_ewCBSGGD_sub
  }
  
  tb <- svytable(formula = ~data_sub[[varcode]] , design = survey_design) # tb bevat estimates van populatieaantallen
  
  ct <- prop.table(tb) # ct bevat estimates als percentages  
  
  for (crossvar in var_cross$V1){
    
    varlabels_cross <- attr(data[[crossvar]], "labels")
    
    for (j in 1:length(tb)){ # voor elke waarde van de variabele 
      
      val <- names(tb)[j] 
      
      string <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svyciprop, vartype='ci',method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
      expr <- glue(string)
      ci_cross <- eval(parse(text = expr))
      
      string2 <- "svyby(~I({varcode}=={val}), ~{crossvar}, survey_design, svytotal, vartype='ci', method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
      expr2 <- glue(string2)
      population_count <- eval(parse(text = expr2))
      
      for (k in 1:nrow(ci_cross)){
        
        idx <- nrow(df_cross) + 1
        df_cross[idx, 1] <- varcode # variabelenaam
        df_cross[idx, 2] <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
        df_cross[idx, 3] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
        df_cross[idx, 4] <- names(ci_cross)[1]  # Naam van crossing variabele
        df_cross[idx, 5] <- names(varlabels_cross[k]) # Level van crossing variabele
        df_cross[idx, 6] <- population_count[k,3] # Populatie n / gewogen n 
        df_cross[idx, 7] <- ci_cross[k,2] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 8] <- ci_cross[k,3] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 9] <- ci_cross[k,4] * 100 # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_cross[idx, 10] <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]) & survey_design[["variables"]][crossvar] == ci_cross[k,1], na.rm = TRUE)
        
      }
    }
  }
}


# Bij crossings aantallen per variabele en crossing tellen? Nog testen.
df_cross <- df_cross %>%
  group_by(varcode, crossing, crossing_var) %>%
  mutate(n_vraag = sum(n_unweighted)) %>%
  ungroup() 
# %>%
#  arrange(varcode, crossing, crossing_var)



write.csv2(df, file = "test.csv", row.names = FALSE)
write.csv2(df_cross, file = "test.csv", row.names = FALSE)
