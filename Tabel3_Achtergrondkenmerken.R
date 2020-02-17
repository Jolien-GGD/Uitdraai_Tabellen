
rm(list=ls())

#### libraries ####
library(haven)
library(survey)
library(glue)
library(car) 
library(stringr)
library(dplyr)

# Working directory
setwd("G:\\Projecten\\Tabellenboek\\Jongerenmonitor12-18 2019")

# Basisnaam voor output bestand
output_name <- "JM1218_2019_tabel3_20200217"

# Strings niet als factor inlezen
options(stringsAsFactors = FALSE)

# Strata met 1 waarneming toestaan. 
options(survey.lonely.psu="certainty") # Misschien adjust beter, want conservatiever?

#inladen jeugdmonitor, selecteren op GGD HvB
data_monitor <- read_spss("JM12-18 2019 HvB BZO DEF.sav")
data <- data_monitor[data_monitor$GGD == 2, ]

# Lees lijst in met variabelen/indicatoren die je in het tabellenboek wilt hebben
var_df <- read.csv("varlijst.csv", header=FALSE)

# Check of alle variabelen uit varlijst.csv ook in het sav bestand staan
if(FALSE %in% (var_df$V1 %in% names(data))) stop('Niet alle opgegeven variabelen komen voor in SPSS bestand')
# var_df[which(var_df$V1 %in% names(data)==FALSE),1] # Welke variabele mist?

# Voor testdoeleinden
# var_df <- data.frame(V1 = var_df[1:3,1])

# Hercodeer naar dichotome variabelen
data$ETNICAT_2CAT <- car::recode(data$ETNICAT, "1=2; 2=2; 3=1; 9=NA") #hercoderen naar dichotome variabele
data$MBGSO2S1_2CAT <- car::recode(data$MBGSO2S1, "1=0; 2=0; 3=0; 4=1; 5=0; 9=NA") #hercoderen naar dichotome variabele
data$ouderslaagmiddenopgeleid_2CAT <- car::recode(data$ouderslaagmiddenopgeleid, "0=0; 1=1; 8=NA; 9=NA")

# Zet labels goed
data$ETNICAT_2CAT <- labelled(data$ETNICAT_2CAT, c("Niet-westerse migratieachtergrond" = 1, "Nederlandse/westerse (migratie)achtergrond" = 2))
data$MBGSO2S1_2CAT <- labelled(data$MBGSO2S1_2CAT, c("Geen eenoudergezin" = 0, "Eenoudergezin" = 1))
data$ouderslaagmiddenopgeleid_2CAT <- labelled(data$ouderslaagmiddenopgeleid_2CAT, c("Niet alle aanwezige ouders laag/middenopgeleid" = 0, "Alle aanwezige ouders laag/middenopgeleid" = 1))


# Crossings variabelen
crossingvars <- c("geslacht", "leefcat", "ETNICAT_2CAT", "MBGSO2S1_2CAT", "ouderslaagmiddenopgeleid_2CAT", "buitenshuiswerkend")

# Check of crossing vars idd in spss bestand staan
if (FALSE %in%(c("geslacht", "leefcat", "etn", "opleiding", "eenoudergezin") %in% names(data))) stop ('Niet alle crossingsvariabelen staan in het SPSS bestand')

# Survey design
datadesign <- svydesign(ids = ~1, data = data, strata = ~wijk, weights = data$Wi_groot)

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
    if (!all(is.na(data[[i]]))) {
        
        tb <- svytable(formula = ~ data[[i]] , design = datadesign) # tb bevat gewogen aantallen per waarde van de variabele
        ct <- prop.table(tb) # ct bevat estimates als percentages
        
        varlabels <- attr(data[[i]], "labels") # value labels
        
        for (crossvar in crossingvars){
            
            varlabels_cross <- attr(data[[crossvar]], "labels") # variabele van de crossing variabele
            
            # Voor het aantal niet-missing antwoordopties uit de vraag
            for (j in 1:length(tb)) {
                
                val <- names(tb)[j] # value labels van de variabele 
                
                # loopen met survey package wil niet op normale manier. Daarom glue en eval om betrouwbaarheidsintervallen te krijgen.
                #string <- "svyby(~I({var}=={val}), ~cbsstr, datadesign, svyciprop, vartype='ci',method='xlogit', na.rm=TRUE, na.rm.all = TRUE)"
                
                string <- "svyby(~I({i}=={val}), ~{crossvar}, datadesign, svyciprop, vartype='ci',method='xlogit', level = 0.99, na.rm=TRUE)"
                expr <- glue(string)
                ci_cross <- eval(parse(text = expr))
                
                string2 <- "svyby(~I({i}=={val}), ~{crossvar}, datadesign, svytotal, vartype='ci', method='xlogit', level = 0.99, na.rm=TRUE)"
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
                    df3[idx, 8] <- sum(datadesign[["variables"]][i]  == as.integer(names(tb)[j]) & datadesign[["variables"]][crossvar] == ci_cross[k,1], na.rm = TRUE) # ongewogen n
                    df3[idx, 9] <- ci_cross[k,2] # Percentage
                    df3[idx, 10] <- ci_cross[k,3] # CI lower
                    df3[idx, 11] <- ci_cross[k,4] # CI upper
                    
                    print(idx)
                    print(i)
                    print(names(ci_cross)[1])
                    print(varlabels_cross[k])

                }
            }
        }
    }
    
    write.csv(df3, file = paste0(output_name, "_tussenbestand.csv"), row.names = FALSE)
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

tabel_regio <- read.csv("JM1218_2019_regio_20200217.csv", header = TRUE)

tabel3 <- dplyr::full_join(df3_vergelijk, tabel_regio, by = c("varval_links" = "varval")) # suffix wordt niet toegevoegd als tabel resultaat is van join?
# Dan maar zelf
colnames(tabel3)[(length(colnames(df3_vergelijk))+1):length(colnames(tabel3))] <- paste0(colnames(tabel3)[(length(colnames(df3_vergelijk))+1):length(colnames(tabel3))], '_regio')

write.csv(tabel3, file = paste0(output_name, "_def.csv"), row.names = FALSE)





