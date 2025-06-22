library(readODS)

# Charger les données
data <- read_ods("~/Bureau/allemand/Sources_pour_les_nerds/german_stats.ods", sheet = 1)

# Remplacer les valeurs non numériques par NA
data[data == "unk"] <- NA
data[data == "#VALUE!"] <- NA

# Convertir les colonnes en numériques
data$page_number <- as.integer(data[[1]])
data$new_words <- as.integer(data[[2]])
data$total_words <- as.integer(data[[3]])
data$time_minutes <- as.numeric(data[[4]])

# Calculs dérivés
data$words_per_minute <- with(data, total_words / time_minutes)
data$time_per_word <- with(data, time_minutes / total_words)
data$new_words_on_total <- with(data, 100 * new_words / total_words)

# Fonction pour tracer et afficher pente
plot_with_slope <- function(x, y, xlab, ylab, main, data_name = "") {
  valid <- !is.na(x) & !is.na(y)
  model <- lm(y[valid] ~ x[valid])
  slope <- coef(model)[2]
  
  plot(x[valid], y[valid],
       xlab = xlab, ylab = ylab, main = main,
       pch = 19, col = "blue",
       cex.main = 0.8, cex.lab = 0.7, cex.axis = 0.7, cex = 0.7)
  
  abline(model, col = "red", lwd = 2)
  
  legend_text <- paste0("Pente = ", round(slope, 3))
  legend("topleft", legend = legend_text, bty = "n", col = "red", cex = 0.8)
  
  if (data_name != "") {
    mtext(paste0("*(", data_name, ")"), side = 1, line = 3.5, cex = 0.5)
  }
  
  cat(main, "\n")
  cat("Pente =", round(slope, 5), "\n\n")
}

# Tracer et calculer les pentes pour chaque graphique

plot_with_slope(
  data$page_number,
  data$words_per_minute,
  xlab = "Numéro de page",
  ylab = "Mots par minute",
  main = "Figure 1. Progression de la vitesse de lecture",
  data_name = "Livre: Außenseiter 1 - Fauxhumain1"
)

plot_with_slope(
  data$page_number,
  data$time_per_word,
  xlab = "Numéro de page",
  ylab = "Minutes par mot",
  main = "Figure 2. Vitesse de lecture (temps par mot)",
  data_name = "Livre: Außenseiter 1 - Fauxhumain1"
)

plot_with_slope(
  data$page_number,
  data$new_words_on_total,
  xlab = "Numéro de page",
  ylab = "Mots nouveaux (%)",
  main = "Figure 3. Taux de mots découverts par page",
  data_name = "Livre: Außenseiter 1 - Fauxhumain1"
)
