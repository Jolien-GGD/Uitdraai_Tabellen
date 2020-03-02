##########
# Trends #
##########

rm(list=ls())

#### libraries ####
library(haven)
library(survey)
library(glue)
library(dplyr)


#### wd ####
setwd("G:\\Projecten\\Tabellenboek\\")

options(stringsAsFactors = FALSE)

# Strata met 1 waarneming toestaan. 
options(survey.lonely.psu="certainty") # Misschien adjust beter, want conservatiever?

#### inladen data ####
jm17 <- read_spss("JM 0-11 2017 Brabant met weegfactoren en indicatoren.sav")
jm17 <- jm17[jm17$GGD == 2, ]

jm13 <- read_spss("JM 0-11 2013 Definitief_trend (met 2017).sav")
jm13 <- jm13[jm13$GGD == 2, ]

jm08 <- read_spss("JM011 2008 trend deel 1 en 2 samen.sav")
jm08 <- jm08[jm08$GGD == 2, ]

# Check dat Xsteekproef op 0 staat bij bestand 2008.
jm08 <- jm08[jm08$Xsteekproef == 0, ]


var_df <- read.csv("varlijst_trend.csv", header=FALSE)
#var_df <- data.frame(V1 = var_df[1:5,1])


# Survey designs
survey_design17 <- svydesign(ids = ~1, data = jm17, strata = ~wijk, weights = ~Wi_groot)
survey_design13  <- svydesign(ids = ~1, data = jm13, strata = ~cbs, weights = ~Wi_groot)
survey_design08  <- svydesign(ids = ~1, data = jm08, strata = ~cbs, weights = ~Wi_groot)

# Zet in lijst om over te kunnen loopen
lijst <- list(
    data = list(jm17, jm13, jm08),
    survey_design = list(survey_design17, survey_design13, survey_design08),
    outputnaam = list("JM2017_trend_20191107", "JM2013_trend_20191107", "JM2008_trend_20191107")
)


for (iter in 1:length(lijst$data)) {
    
#############
# Regio HvB #
#############
    
data = as.data.frame(lijst$data[[iter]])
survey_design = lijst$survey_design[[iter]]
outputnaam = lijst$outputnaam[[iter]]

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
            
            write.csv(df_regio, file = paste0(outputnaam, "_regio.csv"), row.names = FALSE)
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

write.csv(df_regio, file = paste0(outputnaam, "_regio_full.csv"), row.names = FALSE)



####################################
# Zonder crossing, gemeente niveau #
####################################

df_gem <- data.frame(varcode = character(0)
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
            
            varlabels <- attr(data_sub[[varcode]], "labels") # value labels
            
            # tb <- svytable(formula = ~data_sub[[varcode]] , design = survey_design_sub) # Hiermee mis ik de antwoordopties die niemand heeft gekozen
            tb <- svytable(formula = ~data[[varcode]] , design = survey_design) # Deze bevat populatieaantallen. Prima als je tb alleen voor labels gebruikt en niet de gewogen n per gemeente wil weten
            
            for (j in 1:length(tb)){ # Voor het aantal niet-missing antwoordopties uit de vraag
                
                val <- names(tb)[j] # val is de numerieke code van de huidige antwoordoptie
                
                # loopen met survey package wil niet op normale manier. Daarom methode met glue en eval om betrouwbaarheidsintervallen te krijgen.
                
                # Eerst met confidence level 95% voor de vergelijking met regio, daarna met confidence level 90% voor vergelijking met wijk

                string <- "svyciprop(~I({varcode}=={val}), survey_design_sub, method='xlogit', na.rm=TRUE, level = 0.95)"
                expr <- glue(string)
                ci <- eval(parse(text = expr)) # confidence intervals
                
                # Schrijf info weg naar dataframe
                idx <- nrow(df_gem) + 1
                
                df_gem[idx, 1] <- varcode # variabelenaam
                df_gem[idx, 2] <- names(tb)[j] # numerieke waarde van huidige antwoordoptie
                df_gem[idx, 3] <- names(varlabels)[varlabels == as.numeric(names(tb)[j])] # tekstlabel van huidige antwoordoptie
                # df_gem[idx, 4] <- round(tb_regio[[j]])  # Populatie n / gewogen n 
                df_gem[idx, 4] <- ci[[1]] # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
                df_gem[idx, 5] <- attr(ci, "ci")[1] # vermenigvuldigd met 100 om percentage te krijgen ipv proportie
                df_gem[idx, 6] <- attr(ci, "ci")[2]# vermenigvuldigd met 100 om percentage te krijgen ipv proportie
                df_gem[idx, 7] <- sum(survey_design_sub[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
                df_gem[idx, 8] <- gemeente
                
                write.csv(df_gem, file = paste0(outputnaam, "_gem.csv"), row.names = FALSE)
                
                print(idx)
                print(gemeente)
                print(varcode)
            }
        }
    }  
}

# Tel aantal geldige antwoorden per vraag op.
df_gem <- df_gem %>%
    group_by(varcode, gebied) %>%
    mutate(n_vraag = sum(n_unweighted)) %>%
    ungroup()  


df_gem$percentage[df_gem$n_vraag < 30] <- NA
df_gem$CIlower[df_gem$n_vraag < 30] <- NA
df_gem$CIupper[df_gem$n_vraag < 30] <- NA


# Maak cbsstr aan
df_gem$cbsstr <- as.character(df_gem$gebied)


# Maak koppelkolom aan
df_gem$varval <- paste0(df_gem$varcode, df_gem$waarde)
df_gem$key <- paste0(df_gem$varcode, df_gem$waarde, df_gem$cbsstr)

write.csv(df_gem, file = paste0(outputnaam, "_gem_full.csv"), row.names = FALSE)

}




########## Gemeenten

# In de loop wordt alles df_regio genoemd. Daarom naar aparte variabele via inlezen van csv
tabel_jm17_gem <- read.csv("JM2017_trend_20191107_gem_full.csv", header = TRUE)
tabel_jm13_gem <- read.csv("JM2013_trend_20191107_gem_full.csv", header = TRUE)
tabel_jm08_gem <- read.csv("JM2008_trend_20191107_gem_full.csv", header = TRUE)

tabel2_gem_temp <- dplyr::full_join(tabel_jm17_gem, tabel_jm13_gem, by = "key", suffix = c("_17", "_13"))
tabel2_gem <- dplyr::full_join(tabel2_gem_temp, tabel_jm08_gem, by = "key", suffix = c("", "_08")) # Suffix werkt niet? 
# Dan maar zelf
colnames(tabel2_gem)[(length(colnames(tabel2_gem_temp))+1):length(colnames(tabel2_gem))] <- paste0(colnames(tabel2_gem)[(length(colnames(tabel2_gem_temp))+1):length(colnames(tabel2_gem))], '_08')



# Them stats.
tabel2_gem$Sign_17_13 <- 0

for (i in 1:nrow(tabel2_gem) ){
    # Bij binnenste 2 if-loops mag de conditie niet uitkomen op NA. Check daarom eerst of er NAs zijn in CI. Alleen lower checken is genoeg.
    if (!is.na(tabel2_gem[i,'CIlower_17']) & !is.na(tabel2_gem[i,'CIlower_13'])) {
        if (tabel2_gem[i,'CIupper_17'] < tabel2_gem[i,'CIlower_13']) {tabel2_gem$Sign_17_13[i] = 1}
        if (tabel2_gem[i,'CIlower_17'] > tabel2_gem[i,'CIupper_13']) {tabel2_gem$Sign_17_13[i] = 1}
    }
}


# Them stats 2.
tabel2_gem$Sign_17_08 <- 0

for (i in 1:nrow(tabel2_gem) ){
    # Bij binnenste 2 if-loops mag de conditie niet uitkomen op NA. Check daarom eerst of er NAs zijn in CI. Alleen lower checken is genoeg.
    if (!is.na(tabel2_gem[i,'CIlower_17']) & !is.na(tabel2_gem[i,'CIlower_08'])) {
        if (tabel2_gem[i,'CIupper_17'] < tabel2_gem[i,'CIlower_08']) {tabel2_gem$Sign_17_08[i] = 1}
        if (tabel2_gem[i,'CIlower_17'] > tabel2_gem[i,'CIupper_08']) {tabel2_gem$Sign_17_08[i] = 1}
    }
}



#### Regio

# In de loop wordt alles df_regio genoemd. Daarom naar aparte variabele via inlezen van csv
tabel_jm17_regio <- read.csv("JM2017_trend_20191107_regio_full.csv", header = TRUE)
tabel_jm13_regio <- read.csv("JM2013_trend_20191107_regio_full.csv", header = TRUE)
tabel_jm08_regio <- read.csv("JM2008_trend_20191107_regio_full.csv", header = TRUE)


# Join alle tabellen aan elkaar
tabel2_regio_temp <- dplyr::full_join(tabel_jm17_regio, tabel_jm13_regio, by = "varval", suffix = c("_17", "_13"))
tabel2_regio <- dplyr::full_join(tabel2_regio_temp, tabel_jm08_regio, by = "varval", suffix = c("", "_08")) # Suffix werkt niet? 
# Dan maar zelf
colnames(tabel2_regio)[(length(colnames(tabel2_regio_temp))+1):length(colnames(tabel2_regio))] <- paste0(colnames(tabel2_regio)[(length(colnames(tabel2_regio_temp))+1):length(colnames(tabel2_regio))], '_08')


# Them stats.
tabel2_regio$Sign_17_13 <- 0

for (i in 1:nrow(tabel2_regio) ){
    # Bij binnenste 2 if-loops mag de conditie niet uitkomen op NA. Check daarom eerst of er NAs zijn in CI. Alleen lower checken is genoeg.
    if (!is.na(tabel2_regio[i,'CIlower_17']) & !is.na(tabel2_regio[i,'CIlower_13'])) {
        if (tabel2_regio[i,'CIupper_17'] < tabel2_regio[i,'CIlower_13']) {tabel2_regio$Sign_17_13[i] = 1}
        if (tabel2_regio[i,'CIlower_17'] > tabel2_regio[i,'CIupper_13']) {tabel2_regio$Sign_17_13[i] = 1}
    }
}


# Them stats 2.
tabel2_regio$Sign_17_08 <- 0

for (i in 1:nrow(tabel2_regio) ){
    # Bij binnenste 2 if-loops mag de conditie niet uitkomen op NA. Check daarom eerst of er NAs zijn in CI. Alleen lower checken is genoeg.
    if (!is.na(tabel2_regio[i,'CIlower_13']) & !is.na(tabel2_regio[i,'CIlower_08'])) {
        if (tabel2_regio[i,'CIupper_13'] < tabel2_regio[i,'CIlower_08']) {tabel2_regio$Sign_17_08[i] = 1}
        if (tabel2_regio[i,'CIlower_13'] > tabel2_regio[i,'CIupper_08']) {tabel2_regio$Sign_17_08[i] = 1}
    }
}




#### WEGSCHRIJVEN ####
write.csv(tabel2_gem, 'tabel2_gem.csv', row.names = FALSE)
write.csv(tabel2_regio, 'tabel2_regio.csv', row.names = FALSE)

# tabel2_gem <- read.csv("tabel2_gem_Jolien.csv", header = TRUE)
# tabel2_regio <- read.csv("tabel2_regio_Jolien.csv", header = TRUE)

tabel2 <- dplyr::full_join(tabel2_gem, tabel2_regio, by = c("varval_17" = "varval"), suffix = c("_gem", "_regio"))

write.csv(tabel2, 'tabel2_volledig.csv', row.names = FALSE)



