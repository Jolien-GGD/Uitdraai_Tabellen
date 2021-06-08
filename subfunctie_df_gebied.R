
subgebied <- function(df_gebied, gebiedskolom, ci_level, n_vraag_afkapwaarde, data, var_df, survey_design){
  
  # Initialiseer regelnummer op 1, voor het wegschrijven van de uitgerekende data
  regelnummer <- 1
  
  for (gebied in unique(data[[gebiedskolom]])) {
    
    # survey_design_sub <- subset(survey_design, cbs == gemeente) geeft hele rare output, doordat gemeente als R variabele wordt ingevoerd ipv tekst. 
    # Subset geeft geen foutmelding, maar wordt gedaan op verkeerde gemeente. Onderstaande werkt wel
    tekst <- paste0("subset(survey_design, ", gebiedskolom, " == {gebied})")
    survey_design_sub  <- eval(parse(text = glue(tekst)))

    data_sub <- data[data[[gebiedskolom]] == gebied & !is.na(data[[gebiedskolom]]),] # Ook hier geen subset() gebruiken, geeft verkeerde data terug
    
    for (varcode in var_df$V1){
      
      if (!all(is.na(data[[varcode]]))){
        
        varlabels <- attr(data[[varcode]], "labels") # value labels. # value labels. Gebruik data ipv data_sub, omdat je anders labels mist als een bepaald antwoord niet voorkomt bij de eerste gemeente
        
        # loopen met survey package wil niet op normale manier. Daarom methode met glue en eval om tb en betrouwbaarheidsintervallen te krijgen.
        # tb <- svytable(formula = ~data_sub[[varcode]] , design = survey_design_sub) # Hiermee mis ik de antwoordopties die niemand heeft gekozen
        
        string_tb <- paste0("svytable(formula = ~data[['{varcode}']], design = survey_design)") # Deze bevat populatieaantallen. Prima als je tb alleen voor labels gebruikt en niet de gewogen n per gemeente wil weten
        expr_tb <- glue(string_tb)
        tb <- eval(parse(text = expr_tb)) 
        
        # tb <- svytable(formula = ~data[[varcode]] , design = survey_design) 
        
        for (j in 1:length(tb)){ # Voor het aantal niet-missing antwoordopties uit de vraag
          
          val <- names(tb)[j] # val is de numerieke code van de huidige antwoordoptie
          
          ## bereken betrouwbaarheidsintervallen
          string <- paste0("svyciprop(~I({varcode}=={val}), survey_design_sub, method='xlogit', na.rm=TRUE, level = ", ci_level, ")")
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
          
          # temp_n <- round(tb_regio[[j]])  # Populatie n / gewogen n  <= is populatie van hele regio, niet van subset.
          temp_percentage <- ci[[1]] * 100 # Estimate/percentage
          temp_CIlower <- attr(ci, "ci")[1] * 100 # CI lower
          temp_CIupper <- attr(ci, "ci")[2] * 100 # CI upper
          temp_n_unweighted <- sum(survey_design_sub[["variables"]][varcode]  == as.integer(names(tb)[j]), na.rm = TRUE) # sample n / ongewogen n
          temp_gebied <- gebied
          
          # Schrijf data weg naar dataframe
          df_gebied[regelnummer,] <- c(temp_varcode, temp_waarde, temp_label, temp_percentage, temp_CIlower, temp_CIupper, temp_n_unweighted, temp_gebied)
          
          # Print huidige regelnummer om idee te krijgen hoe lang script nog zal runnen
          print(paste0(regelnummer, " van ", aantal_verwachte_rijen))
          
          # Hoog regelnummer met 1 op om de volgende regel in de dataframe te vullen met de volgende indicator/antwoordoptie
          regelnummer <- regelnummer + 1
          
        }
      }
    }  
  }
  
  # Zet naar numeric
  df_gebied$waarde <- as.numeric(df_gebied$waarde)
  df_gebied$percentage <- as.numeric(df_gebied$percentage)
  df_gebied$CIlower <- as.numeric(df_gebied$CIlower)
  df_gebied$CIupper <- as.numeric(df_gebied$CIupper)
  df_gebied$n_unweighted <- as.numeric(df_gebied$n_unweighted)
  
  # Tel aantal geldige antwoorden per vraag op.
  df_gebied <- df_gebied %>%
    group_by(varcode, gebied) %>%
    mutate(n_vraag = sum(n_unweighted)) %>%
    ungroup()  
  
  df_gebied$percentage[df_gebied$n_vraag < n_vraag_afkapwaarde] <- NA
  df_gebied$CIlower[df_gebied$n_vraag < n_vraag_afkapwaarde] <- NA
  df_gebied$CIupper[df_gebied$n_vraag < n_vraag_afkapwaarde] <- NA
  
  # Maak combi van variable en value aan
  df_gebied$varval <- paste0(df_gebied$varcode, df_gebied$waarde)
  
  # Maak koppelkolom aan
  df_gebied$gebied_varval <- paste0(df_gebied$gebied, df_gebied$varval)
  
  beep(sound = 3)
  
  return(df_gebied)
}