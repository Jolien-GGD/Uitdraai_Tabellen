
rm(list=ls())

#### libraries ####
library(haven)
library(survey)
library(glue)
library(car) 
library(stringr)
library(dplyr)

# Working directory
setwd("G:\\Projecten\\Tabellenboek\\")

# Strings niet als factor inlezen
options(stringsAsFactors = FALSE)

# Strata met 1 waarneming toestaan. 
options(survey.lonely.psu="certainty") # Misschien adjust beter, want conservatiever?

#inladen jeugdmonitor, selecteren op GGD HvB
jm17 <- read_spss("G:\\Projecten\\Tabellenboek\\JM 0-11 2017 Brabant met weegfactoren en indicatoren.sav")
jm17 <- jm17[jm17$GGD == 2, ]

#### Uit te draaien variabelen inlezen en checken of ze ook in spss bestand staan
var_df <- read.csv("varlijst.csv", header=FALSE) # Lees lijst in met variabelen/indicatoren die je in het tabellenboek wilt hebben
if(FALSE %in% (var_df$V1 %in% names(jm17))) stop('Niet alle opgegeven variabelen komen voor in SPSS bestand') # Check of alle variabelen uit varlijst.csv ook in het sav bestand staan
# var_df[which(var_df$V1 %in% names(data)==FALSE),1] # Welke variabele mist?

# Voor testdoeleinden
# var_df <- data.frame(V1 = var_df[1:3,1])

# Hercodeer naar dichotome variabelen
jm17$ETNICAT_2CAT <- car::recode(jm17$ETNICAT, "1=2; 2=2; 3=1; 9=NA") #hercoderen naar dichotome variabele
jm17$MBGSO2S1_2CAT <- car::recode(jm17$MBGSO2S1, "1=0; 2=0; 3=0; 4=1; 5=0; 9=NA") #hercoderen naar dichotome variabele
jm17$ouderslaagmiddenopgeleid_2CAT <- car::recode(jm17$ouderslaagmiddenopgeleid, "0=0; 1=1; 8=NA; 9=NA")

# Zet labels goed
jm17$ETNICAT_2CAT <- labelled(jm17$ETNICAT_2CAT, c("Niet-westerse migratieachtergrond" = 1, "Nederlandse/westerse (migratie)achtergrond" = 2))
jm17$MBGSO2S1_2CAT <- labelled(jm17$MBGSO2S1_2CAT, c("Geen eenoudergezin" = 0, "Eenoudergezin" = 1))
jm17$ouderslaagmiddenopgeleid_2CAT <- labelled(jm17$ouderslaagmiddenopgeleid_2CAT, c("Niet alle aanwezige ouders laag/middenpgeleid" = 0, "Alle aanwezige ouders laag/middenopgeleid" = 1))


# Crossings variabelen
crossingvars <- c("geslacht", "leefcat", "ETNICAT_2CAT", "MBGSO2S1_2CAT", "ouderslaagmiddenopgeleid_2CAT", "buitenshuiswerkend")

# Survey design
jm17design <- svydesign(ids = ~1, data = jm17, strata = ~wijk, weights = jm17$Wi_groot)

# Dataframe om resultaten in op te slaan
df3 <- data.frame(gebied = character(0),
                   varnaam = character(0)
                   , varval = character(0)
                   , crossing = character(0)
                   , crossing_code = character(0)
                   , crossing_label = character(0)
                   , n = integer(0)
                   , n_unweighted = integer(0)
                   , percentage = integer(0)
                   , CIlower = integer(0)
                   , CIupper = integer(0))



####################
# Them script meat #
####################

idx = 0

# Voor alle variabelen in de varlijst (variabelen die je in het tabellenboek wilt hebben)
for (i in var_df$V1) {
    
    # Alleen uitvoeren voor variabelen waar niet alles NA is
    if (!all(is.na(jm17[[i]]))) {
        
        tb <- svytable(formula = ~ jm17[[i]] , design = jm17design) # tb bevat gewogen aantallen per waarde van de variabele
        ct <- prop.table(tb) # ct bevat estimates als percentages
        
        varlabels <- attr(jm17[[i]], "labels") # value labels
        
        for (crossvar in crossingvars){
            
            varlabels_cross <- attr(jm17[[crossvar]], "labels") # variabele van de crossing variabele
            
            # Voor het aantal niet-missing antwoordopties uit de vraag
            for (j in 1:length(tb)) {
                
                val <- names(tb)[j] # value labels van de variabele 
                
                # loopen met survey package wil niet op normale manier. Daarom glue en eval om betrouwbaarheidsintervallen te krijgen.
                #string <- "svyby(~I({var}=={val}), ~cbsstr, jm17design, svyciprop, vartype='ci',method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
                
                string <- "svyby(~I({i}=={val}), ~{crossvar}, jm17design, svyciprop, vartype='ci',method='xlogit', level = 0.95, na.rm=TRUE)"
                expr <- glue(string)
                ci_cross <- eval(parse(text = expr))
                
                string2 <- "svyby(~I({i}=={val}), ~{crossvar}, jm17design, svytotal, vartype='ci', method='xlogit', level = 0.95, na.rm=TRUE)"
                expr2 <- glue(string2)
                population_count <- eval(parse(text = expr2))
                
                # Voor elke waarde van de crossings variabele
                for (k in 1:nrow(ci_cross)){
                    
                    # Maak nieuwe rij(-index) aan
                    idx <- nrow(df3) + 1
                    
                    # Schrijf data weg
                    df3[idx, 2] <- i # variabele naam
                    df3[idx, 3] <- paste(i, names(tb)[j], sep = '') #varval: variabele naam + waarde van de huidige antwoordoptie
                    df3[idx, 4] <- names(ci_cross)[1]  # Naam van crossing variabele
                    df3[idx, 5] <- varlabels_cross[k] # numerieke code van crossing variabele
                    df3[idx, 6] <- names(varlabels_cross[k]) # label horend bij unieke code van crossing variabele
                    df3[idx, 7] <- population_count[k,3] # Gewogen n
                    df3[idx, 8] <- sum(jm17design[["variables"]][i]  == as.integer(names(tb)[j]) & jm17design[["variables"]][crossvar] == ci_cross[k,1], na.rm = TRUE) # ongewogen n
                    df3[idx, 9] <- ci_cross[k,2] # Percentage
                    df3[idx, 10] <- ci_cross[k,3] # CI lower
                    df3[idx, 11] <- ci_cross[k,4] # CI upper
                    
                    print(idx)
                    print(i)
                    print(names(ci_cross)[1])
                    print(varlabels_cross[k])
                    
                    write.csv(df3, file = "JM_tabel3_20191104_4.csv", row.names = FALSE)
                    
                }
            }
        }
    }
}

df3$gebied <- "Regio"

# Bij crossings aantallen per variabele en crossing tellen
df3 <- df3 %>%
    group_by(gebied, varnaam, crossing, crossing_code) %>% 
    mutate(n_vraag = sum(n_unweighted)) %>%
    ungroup() # %>%
    # arrange(gebied, crossing_label, varval)


# Verwijder resultaten voor vragen/indicatoren/variabelen waarop minder dan 30 respondenten antwoord hebben gegeven
# Loop is op regio, dus zeer waarschijnlijk niet van toepassing. Maar ach, better safe than sorry.
df3$percentage[df3$n_vraag < 30] <- NA
df3$CIlower[df3$n_vraag < 30] <- NA
df3$CIupper[df3$n_vraag < 30] <- NA


# Koppelcodes maken om vergelijking te kunnen maken. 
df3$crossval <- paste0(df3$crossing, df3$crossing_code)

# Key voor 'statische' linkertabel
df3$key <- paste0(df3$varval, df3$crossing, df3$crossing_code)

df3$koppelcode <- "geenKoppel"
df3$koppelcode[df3$crossval == 'geslacht2'] <- paste0(df3$varval[df3$crossval == 'geslacht2'], 'geslacht1') # geslacht; 1 = jongen, 2 = meisje
df3$koppelcode[df3$crossval == 'leefcat1'] <- paste0(df3$varval[df3$crossval == 'leefcat1'], 'leefcat2') # leefcat; 1 = 0-3 jaar, 2 = 4-7 jaar, 3 = 8-11 jaar
df3$koppelcode[df3$crossval == 'leefcat3'] <- paste0(df3$varval[df3$crossval == 'leefcat3'], 'leefcat2') 
df3$koppelcode[df3$crossval == 'ETNICAT_2CAT2'] <- paste0(df3$varval[df3$crossval == 'ETNICAT_2CAT2'], 'ETNICAT_2CAT1') # etniciteit; 1 = niet-westers, 2 = autochoon/westers
df3$koppelcode[df3$crossval == 'MBGSO2S1_2CAT1'] <- paste0(df3$varval[df3$crossval == 'MBGSO2S1_2CAT1'], 'MBGSO2S1_2CAT0') # gezinsstatus; 0 = niet eenoudergezin, 1 = eenoudergezin
df3$koppelcode[df3$crossval == 'ouderslaagmiddenopgeleid_2CAT1'] <- paste0(df3$varval[df3$crossval == 'ouderslaagmiddenopgeleid_2CAT1'], 'ouderslaagmiddenopgeleid_2CAT0') # opleiding ouders; 0 = niet alle ouders laag opgeleid, 1 = alle ouders laag opgeleid
df3$koppelcode[df3$crossval == 'buitenshuiswerkend1'] <- paste0(df3$varval[df3$crossval == 'buitenshuiswerkend1'], 'buitenshuiswerkend0') # werkende ouders; 0 = geen werkende, 1 = een of twee ouders werkend


# Koppel tabel aan zichzelf, om gegevens op 1 rij te kunnen vergelijken
df3_vergelijk <- dplyr::left_join(df3, df3, by = c("koppelcode" = "key"), suffix = c("_links", "_rechts"))

# Them stats.
df3_vergelijk$Sign <- "0"

for (i in 1:nrow(df3_vergelijk) ){
    # Bij binnenste 2 if-loops mag de conditie niet uitkomen op NA. Check daarom eerst of er NAs zijn in CI. Alleen lower checken is genoeg.
    if (!is.na(df3_vergelijk[i,'CIlower_links']) & !is.na(df3_vergelijk[i,'CIlower_rechts'])) {
        if (df3_vergelijk[i,'CIupper_links'] < df3_vergelijk[i,'CIlower_rechts']) {df3_vergelijk$Sign[i] = "1"}
        if (df3_vergelijk[i,'CIlower_links'] > df3_vergelijk[i,'CIupper_rechts']) {df3_vergelijk$Sign[i] = "1"}
    }
}

# Significanties waarbij n < 30 op NA zetten ipv 0
df3_vergelijk$Sign[df3_vergelijk$n_vraag_links < 30] <- "n<30"

# df3_vergelijk <- read.csv('tabel3_agk_20191104_full_4.csv', header = TRUE)

tabel_regio <- read.csv("tabel1_regio_full.csv", header = TRUE)

tabel3 <- dplyr::full_join(df3_vergelijk, tabel_regio, by = c("varval_links" = "varval")) # suffix wordt niet toegevoegd als tabel resultaat is van join?
# Dan maar zelf
colnames(tabel3)[(length(colnames(df3_vergelijk))+1):length(colnames(tabel3))] <- paste0(colnames(tabel3)[(length(colnames(df3_vergelijk))+1):length(colnames(tabel3))], '_regio')

write.csv(tabel3, 'tabel3_agk_20191104_full_4.csv', row.names = FALSE)





