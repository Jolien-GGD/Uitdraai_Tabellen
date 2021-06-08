
significantie_testen <- function(df, n_vraag_afkapwaarde, gebied_klein, gebied_groot){

# Them stats.
df$Sign <- "0"

for (i in 1:nrow(df) ){
  # Bij binnenste 2 if-loops mag de conditie niet uitkomen op NA. Check daarom eerst of er NAs zijn in CI. Alleen lower checken is genoeg.
  if (!is.na(df[i,paste0('CIlower_', gebied_klein)]) & !is.na(df[i,paste0('CIlower_', gebied_groot)])) {
    if (df[i,paste0('CIupper_', gebied_klein)] < df[i,paste0('CIlower_', gebied_groot)]) {df$Sign[i] = "1"}
    if (df[i,paste0('CIlower_', gebied_klein)] > df[i,paste0('CIupper_', gebied_groot)]) {df$Sign[i] = "1"}
  }
}


# Significanties waarbij n < 50 op NA zetten ipv 0
df$Sign[df[paste0("n_vraag_", gebied_klein)] < n_vraag_afkapwaarde] <- paste0("n<", n_vraag_afkapwaarde)

# Afspraak: Als regio en gemeente allebei een estimate van 1 (100%) of 0 (0%) hebben, dan significantie naar "Niet significant" zetten, bij andere percentages niet.
# Anders krijg je in tabellenboek estimate van 100% (of 0%) bij gemeente en zelfde getal bij regio, maar alsnog een significant verschil. Vinden ambtenaren lastig.
df$Sign[df[paste0("percentage_", gebied_klein)] >= 99.5 & df[paste0("percentage_", gebied_groot)] >= 99.5] <- "0"
df$Sign[df[paste0("percentage_", gebied_klein)] < 0.5 & df[paste0("percentage_", gebied_groot)] < 0.5] <- "0"

return(df)

}

