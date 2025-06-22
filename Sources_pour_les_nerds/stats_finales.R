library(readODS)

data <- read_ods("~/Bureau/allemand/Sources_pour_les_nerds/german_stats.ods", sheet = 1)

# Afficher les noms de colonnes pour les adapter ensuite
print(names(data))

# Remplacer les valeurs non numériques par NA
data[data == "unk"] <- NA
data[data == "#VALUE!"] <- NA

# Convertir les colonnes en numériques
data$page_number <- as.integer(data[[1]])
data$new_words <- as.integer(data[[2]]) #nb de mots découverts sur cette page
data$total_words <- as.integer(data[[3]]) #nb mots total page
data$time_minutes <- as.numeric(data[[4]]) #temps pour lire la page

# Calculer nouveaux mots/total de mots
data$new_words_on_total <- with(data, 100 * new_words / total_words)
# Calculer les mots par minute
data$words_per_minute <- with(data, total_words / time_minutes)
# Calculer temps moyen par mot
data$time_per_word <- with(data, time_minutes / total_words)

# Ajuster une régression linéaire : vitesse en fonction du numéro de page
model <- lm(words_per_minute ~ page_number, data = data[valid, ])

plot_with_slope <- function(x, y, xlab, ylab, main, legend_pos = "topleft", color = "blue", data_name = "") {
  valid <- !is.na(x) & !is.na(y)
  
  # Ajustement modèle linéaire
  model <- lm(y[valid] ~ x[valid])
  slope <- coef(model)[2]
  
  # Tracé des points
  plot(x[valid], y[valid],
       type = "b",
       col = color,
       pch = 19,
       xlab = xlab,
       ylab = ylab,
       main = main,
       cex.main = 0.8,
       cex.lab = 0.7,
       cex.axis = 0.7,
       cex = 0.7)
  
  # Droite de régression
  abline(model, col = "red", lwd = 2)
  
  # Légende avec la pente
  legend_text <- paste0("Pente = ", ifelse(slope >= 0, "+", ""), round(slope, 3), " unités/page")
  legend("topleft", legend = legend_text, bty = "n", cex = 0.8, col = "red")
  
  # Ajout d’une sous-légende optionnelle (ex: nom du livre)
  if (data_name != "") {
    mtext(paste0("*(", data_name, ")"), side = 1, line = 3.5, cex = 0.5)
  }
  
  return(slope)
}
# Exemple avec vitesse de lecture (mots par minute)
slope1 <- plot_with_slope(
  x = data$page_number,
  y = data$words_per_minute,
  xlab = "Numéro de page",
  ylab = "Mots par minute",
  main = "Figure 1. Progression de la vitesse de lecture (mots lus par minute)",
  data_name = "Livre: Außenseiter 1 - Fauxhumain1"
)

# Exemple avec temps par mot
slope2 <- plot_with_slope(
  x = data$page_number,
  y = data$time_per_word,
  xlab = "Page",
  ylab = "Minutes par mot",
  main = "Figure 2. Vitesse de lecture 2 (temps par mot)",
  data_name = "Livre: Außenseiter 1 - Fauxhumain1"
)

# Exemple avec taux de mots nouveaux
slope3 <- plot_with_slope(
  x = data$page_number,
  y = data$new_words_on_total,
  xlab = "Page",
  ylab = "Mots nouveaux (%)",
  main = "Figure 3. Taux de mots découverts par page",
  data_name = "Livre: Außenseiter 1 - Fauxhumain1"
)
