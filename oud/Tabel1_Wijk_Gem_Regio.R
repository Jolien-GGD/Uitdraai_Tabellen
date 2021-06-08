# Probeersel Complex Samples in R

# Clear workspace
rm(list=ls())

# Clear console
# cat("\014")


# set working directory
setwd("F:\\Hvb_Onderzoek\\Projecten\\Tabellenboek\\Jongerenmonitor12-18 2019")

# Eenmalig: installeer packages
# Voorbeeld: install.packages("haven")

# load libraries
library(haven)
library(dplyr)
library(survey)
library(glue)


#########################
# Initialiseer een boel #
#########################

# Basisnaam voor output bestand
output_name <- "JM1218_2019_tabel1"

# Do not create factors from string variables
options(stringsAsFactors = FALSE)

# Strata met 1 waarneming toestaan. 
options(survey.lonely.psu="certainty") # Misschien adjust beter, want conservatiever?

# Open databestand (en sla evt op als Rdata)
data_monitor <- read_spss("JM12-18 2019 HvB BZO DEF.sav")
# data <- read_spss("JM1218 2019 BZO only.sav")



data <- data_monitor[data_monitor$GGD == 2, ]
# data <- data_monitor[data_monitor$GGD == 3, ] # BZO

# Hernoem kolomnaam van Wijk naar wijk (lowercase)
colnames(data)[colnames(data) == "Wijk"] <- "wijk"

# # Maak extra variabele aan voor 4 wijken in Tilburg die ook op samengevoegd niveau gerapporteerd moeten worden
# data$wijk_TB_samenvoeging <- NA
# data$wijk_TB_samenvoeging[data$wijk %in% c(85507, 85514)] <- 85530
# data$wijk_TB_samenvoeging[data$wijk %in% c(85509, 85515)] <- 85531


# Check of NA voorkomt bij gemeenten
sum(is.na(data$cbs))
# Proefbestand bevat lege gemeenten. Gooi deze voor gemak eruit
# data <- data[!is.na(data$cbs),]


# saveRDS(data_volw, file="Probeersel_monitor_Tabellenboek.rds") 
# Open Rdata
# data <- readRDS("Probeersel_monitor_Tabellenboek.rds")


#### Uit te draaien variabelen inlezen en checken of ze ook in spss bestand staan
var_df <- read.csv("varlijst.csv", header=FALSE) # Lees lijst in met variabelen/indicatoren die je in het tabellenboek wilt hebben
if(FALSE %in% (var_df$V1 %in% names(data))) stop('Niet alle opgegeven variabelen komen voor in SPSS bestand') # Check of alle variabelen uit varlijst.csv ook in het sav bestand staan
# var_df[which(var_df$V1 %in% names(data)==FALSE),1] # Welke variabele mist?

# Voor testdoeleinden
# var_df <- data.frame(V1 = var_df[1:10,1])


# Survey design aanmaken
survey_design <- svydesign(ids = ~1, data = data, strata = ~wijk, weights = ~Wi_groot) #HvB
# survey_design <- svydesign(ids = ~1, data = data, strata = ~key2, weights = ~Wi_groot) #BZO

###########################
# Zonder crossings, regio #
###########################

### leeg df voor cijfers
df_regio <- data.frame(varcode = character(0)
                       , waarde = integer(0)
                       , label = character(0)
                       , n = integer(0)
                       , percentage = integer(0)
                       , CIlower = integer(0)
                       , CIupper = integer(0)
                       , n_unweighted = integer(0)
                       , gebied = character(0))

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
      idx <- nrow(df_regio) + 1
      df_regio[idx, 1] <- varcode # variabelenaam
      df_regio[idx, 2] <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
      df_regio[idx, 3] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
      df_regio[idx, 4] <- round(tb[[j]])  # Populatie n / gewogen n <= is populatie van hele regio, niet van subset. 
      df_regio[idx, 5] <- ci[[1]] # Estimate/percentage
      df_regio[idx, 6] <- attr(ci, "ci")[1] # CI lower
      df_regio[idx, 7] <- attr(ci, "ci")[2] # CI upper
      df_regio[idx, 8] <- sum(survey_design[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
      df_regio[idx, 9] <- "Regio"
      
      print(idx)
      print(varcode)
      
      # write.csv(df_regio, file = paste0(output_name, "_regio_tussenbestand.csv", row.names = FALSE))
    }
  }  
}


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

df_regio$sorteervariabele <- 1:nrow(df_regio)

write.csv(df_regio, file = paste0(output_name, "_regio_def_testwebinar.csv"), row.names = FALSE)



##################################################
# Zonder crossing, gemeente niveau CI 90% en 95% #
##################################################

df_gem_ci95 <- data.frame(varcode = character(0)
                       , waarde = integer(0)
                       , label = character(0)
                       , percentage = integer(0)
                       , CIlower = integer(0)
                       , CIupper = integer(0)
                       , n_unweighted = integer(0)
                       , gebied = character(0))

df_gem_ci90 <- data.frame(varcode = character(0)
                          , waarde = integer(0)
                          , label = character(0)
                          , percentage = integer(0)
                          , CIlower = integer(0)
                          , CIupper = integer(0)
                          , n_unweighted = integer(0)
                          , gebied = character(0))



for (gemeente in unique(data$cbs)) {

  # survey_design_sub <- subset(survey_design, cbs == gemeente) geeft hele rare output, doordat gemeente als R variabele wordt ingevoerd ipv tekst. 
  # Subset geeft geen foutmelding, maar wordt gedaan op verkeerde gemeente. Onderstaande werkt wel
  tekst <- "subset(survey_design, cbs == {gemeente})"
  survey_design_sub  <- eval(parse(text = glue(tekst)))
  
  data_sub <- data[data$cbs == gemeente & !is.na(data$cbs),] # Ook hier geen subset() gebruiken, geeft verkeerde data terug
  
  for (varcode in var_df$V1){
    
    if (!all(is.na(data[[varcode]]))){
  
      varlabels <- attr(data[[varcode]], "labels") # value labels. Gebruik data ipv data_sub, omdat je anders labels mist als een bepaald antwoord niet voorkomt bij de eerste gemeente
  
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
        idx <- nrow(df_gem_ci95) + 1
        
        df_gem_ci95[idx, 1] <- varcode # variabelenaam
        df_gem_ci95[idx, 2] <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
        df_gem_ci95[idx, 3] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
        # df_gem_ci95[idx, 4] <- round(tb_regio[[j]])  # Populatie n / gewogen n 
        df_gem_ci95[idx, 4] <- ci_95[[1]] # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_gem_ci95[idx, 5] <- attr(ci_95, "ci")[1] # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_gem_ci95[idx, 6] <- attr(ci_95, "ci")[2]# vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_gem_ci95[idx, 7] <- sum(survey_design_sub[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
        df_gem_ci95[idx, 8] <- gemeente
        
        ## 90%
        string_90 <- "svyciprop(~I({varcode}=={val}), survey_design_sub, method='xlogit', na.rm=TRUE, level = 0.90)"
        expr_90 <- glue(string_90)
        ci_90 <- eval(parse(text = expr_90)) # confidence intervals


        df_gem_ci90[idx, 1] <- varcode # variabelenaam
        df_gem_ci90[idx, 2] <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
        df_gem_ci90[idx, 3] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
        # df_gem_ci90[idx, 4] <- round(tb[[j]])  # Populatie n / gewogen n <= is populatie van hele regio, niet van subset.
        df_gem_ci90[idx, 4] <- ci_90[[1]] # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_gem_ci90[idx, 5] <- attr(ci_90, "ci")[1] # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_gem_ci90[idx, 6] <- attr(ci_90, "ci")[2]# vermenigvuldigd met 100 om percentage te krijgen ipv proportie
        df_gem_ci90[idx, 7] <- sum(survey_design_sub[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
        df_gem_ci90[idx, 8] <- gemeente
        
        
        print(idx)
        print(gemeente)
        print(varcode)
  
        
        # write.csv(df_gem_ci95, file = paste0(output_name, "_gem_ci95_tussenbestand.csv"), row.names = FALSE)
        # write.csv(df_gem_ci90, file = paste0(output_name, "_gem_ci90_tussenbestand.csv"),  row.names = FALSE)
      }
    }
  }  
}


# Tel aantal geldige antwoorden per vraag op.
df_gem_ci95 <- df_gem_ci95 %>%
  group_by(varcode, gebied) %>%
  mutate(n_vraag = sum(n_unweighted)) %>%
  ungroup()  

# Geen zin in functie schrijven...
# Zelfde voor confidence level 90%
df_gem_ci90 <- df_gem_ci90 %>%
  group_by(varcode, gebied) %>%
  mutate(n_vraag = sum(n_unweighted)) %>%
  ungroup()  


# HvB 30, BZO 50
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

write.csv(df_gem_ci95, paste0(output_name, "_gem_ci95_tussenbestand_full.csv"),  row.names = FALSE)
write.csv(df_gem_ci90, paste0(output_name, "_gem_ci90_tussenbestand_full.csv"),  row.names = FALSE)

## Aan de gemeentetabel (ci 95%) wil ik de estimates, ci lowers en ci uppers van de regiotabel plakken

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
gem_vs_regio$Sign[gem_vs_regio$percentage_gem >= 0.995 & gem_vs_regio$percentage_regio >= 0.995] <- "0"
gem_vs_regio$Sign[gem_vs_regio$percentage_gem < 0.005 & gem_vs_regio$percentage_regio < 0.005] <- "0"

write.csv(gem_vs_regio, paste0(output_name, "_gem_vs_regio_def.csv"),  row.names = FALSE)


################################
# Zonder crossing, wijk niveau #
################################

# Vrijwel identiek aan gemeenteniveau, maar ik ben te lui om een subfunctie te schrijven

df_wijk <- data.frame(varcode = character(0)
                     , waarde = integer(0)
                     , label = character(0)
                     , percentage = integer(0)
                     , CIlower = integer(0)
                     , CIupper = integer(0)
                     , n_unweighted = integer(0)
                     , gebied = character(0))

###########################################
# Extra steekproefwijken


## Normale uitdraai 
 for (wijk in unique(data$wijk)){
# Uitdraai voor extra samenvoegingen Tilburg
# for (wijk in c(85530, 85531)){
  
  ## survey_design_sub <- subset(survey_design, cbs == gemeente) geeft hele rare output, doordat gemeente als R variabele wordt ingevoerd ipv tekst. 
  ## Subset geeft geen foutmelding, maar wordt gedaan op verkeerde gemeente. Onderstaande werkt wel
  
  # Normale uitdraai
  tekst <- "subset(survey_design, wijk == {wijk})"
  survey_design_sub  <- eval(parse(text = glue(tekst)))
  
  data_sub <- data[data$wijk == wijk & !is.na(data$wijk),] # Ook hier geen subset gebruiken, geeft verkeerde data terug
  
  # # Uitdraai voor extra samenvoegingen Tilburg
  # tekst <- "subset(survey_design, wijk_TB_samenvoeging == {wijk})"
  # survey_design_sub  <- eval(parse(text = glue(tekst)))
  # 
  # data_sub <- data[data$wijk_TB_samenvoeging == wijk & !is.na(data$wijk_TB_samenvoeging),]
  # 
  
  for (varcode in var_df$V1){
    
    if (!all(is.na(data_sub[[varcode]]))){
    
      varlabels <- attr(data[[varcode]], "labels") # value labels. Gebruik data ipv data_sub, omdat je anders labels mist als een bepaald antwoord niet voorkomt bij de eerste gemeente
      
      tb <- svytable(formula = ~data[[varcode]] , design = survey_design) # Deze bevat populatieaantallen. Prima als je tb alleen voor labels gebruikt en niet de gewogen n per gemeente wil weten
      
      ct <- prop.table(tb) # ct bevat estimates als percentages
      
      for (j in 1:length(tb)){ # Voor het aantal niet-missing antwoordopties uit de vraag
        
        val <- names(tb)[j] # val is de numerieke code van de huidige antwoordoptie
        
        # loopen met survey package wil niet op normale manier. Daarom onderstaande methode om betrouwbaarheidsintervallen te krijgen.
        string <- "svyciprop(~I({varcode}=={val}), survey_design_sub, method='xlogit', na.rm=TRUE, level = 0.90)"
        expr <- glue(string)
        ci <- eval(parse(text = expr)) # confidence intervals
        
        # Schrijf info weg naar dataframe
        idx <- nrow(df_wijk) + 1
        df_wijk[idx, 1] <- varcode # variabelenaam
        df_wijk[idx, 2] <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
        df_wijk[idx, 3] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
        # df_wijk[idx, 4] <- round(tb[[j]])  # Populatie n / gewogen n <= is populatie van hele regio, niet van subset. 
        df_wijk[idx, 4] <- ci[[1]] # Estimate/percentage
        df_wijk[idx, 5] <- attr(ci, "ci")[1] # CI lower
        df_wijk[idx, 6] <- attr(ci, "ci")[2] # CI upper
        df_wijk[idx, 7] <- sum(survey_design_sub[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
        df_wijk[idx, 8] <- wijk
        
        print(idx)
        print(wijk)
        print(varcode)
        
        
      }
    }  
  }
  write.csv(df_wijk, file = paste0(output_name, "_wijk_tussenbestand.csv"),  row.names = FALSE)
  
  }

# Inlezen als sessie bovenstaande niet in 1 dag gedraaid is
df_wijk <- read.csv("JM1218_2019_tabel1_20200217_wijk_tussenbestand_extraIndelingTilburg.csv", header=TRUE)
df_gem_ci90 <- read.csv("tabel 1/JM1218_2019_tabel1_20200217_gem_ci90_full.csv", header=TRUE)



# Tel aantal geldige antwoorden per vraag op.
df_wijk <- df_wijk %>%
  group_by(varcode, gebied) %>%
  mutate(n_vraag = sum(n_unweighted)) %>%
  ungroup()  


df_wijk$percentage[df_wijk$n_vraag < 30] <- NA
df_wijk$CIlower[df_wijk$n_vraag < 30] <- NA
df_wijk$CIupper[df_wijk$n_vraag < 30] <- NA


df_wijk$varval <- paste0(df_wijk$varcode, df_wijk$waarde)


write.csv(df_wijk, file = paste0(output_name, "_wijk_tussenbestand_full.csv"), row.names = FALSE)

## Normale uitdraai
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

## Stukje voor Tilburg extra wijken. Maar 2 regels code nodig ipv de +- 8 regels code hierboven, omdat de gemeente altijd Tilburg is
# df_wijk2 <- df_wijk
# df_wijk2$key <- paste0(df_wijk2$varval, "855")

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
wijk_vs_gem$Sign[wijk_vs_gem$percentage_wijk >= 0.995 & wijk_vs_gem$percentage_gem >= 0.995] <- "0"
wijk_vs_gem$Sign[wijk_vs_gem$percentage_wijk < 0.005 & wijk_vs_gem$percentage_gem < 0.005] <- "0"


write.csv(wijk_vs_gem, file = paste0(output_name, "wijk_ExtraTilburg_vs_gem_def.csv"), row.names = FALSE)







