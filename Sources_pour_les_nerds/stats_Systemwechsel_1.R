# ---- Chargement des bibliothèques ----
library(readODS)
library(ggplot2)
library(lubridate)
library(dplyr)

# ---- Paramètres généraux ----
dir.create("graphes_export", showWarnings = FALSE)
fichier_ods <- "~/Bureau/allemand/Sources_pour_les_nerds/german_stats_all.ods"

# ---- Chargement et nettoyage ----
data <- read_ods(fichier_ods, sheet = 1)

# Remplacer les valeurs non numériques par NA
data[data == "unk"] <- NA
data[data == "#VALUE!"] <- NA

# Conversion en numérique
data$page_number <- as.integer(data[[1]])
data$total_words <- as.integer(data[[2]])
data$time_minutes <- as.numeric(data[[3]])

# ---- Nettoyage des outliers ----
data$words_per_minute <- with(data, total_words / time_minutes)
data$outlier_status<- as.character(data[[7]])
mask_keep <- is.na(data$outlier_status) | tolower(trimws(data$outlier_status)) != "yes"
data_no_outliers <- data[mask_keep, ]

# ---- Données propres ----
wpm_clean <- data_no_outliers$words_per_minute[
  !is.na(data_no_outliers$words_per_minute) & data_no_outliers$words_per_minute != 0
]
time_clean <- data_no_outliers$time_minutes[
  !is.na(data_no_outliers$time_minutes) & data_no_outliers$time_minutes != 0
]
cumulative_hours <- cumsum(time_clean) / 60
mean_last20_clean <- mean(tail(wpm_clean, 20), na.rm = TRUE)

# ---- Fonction de tracé avec régression ----
plot_with_slope <- function(x, y, xlab, ylab, main, data_name = "", y_tick_step = 1) {
  valid <- !is.na(x) & !is.na(y)
  model <- lm(y[valid] ~ x[valid])
  slope <- coef(model)[2]
  
  plot(x[valid], y[valid],
       xlab = xlab, ylab = ylab, main = main,
       pch = 19, col = "blue",
       cex.main = 0.8, cex.lab = 0.7, cex.axis = 0.7, cex = 0.7,
       yaxt = "n",
       ylim = c(floor(min(y[valid])), ceiling(max(y[valid])) + 2) 
  )
  
  y_ticks <- seq(floor(min(y[valid])), ceiling(max(y[valid])), by = y_tick_step)
  axis(side = 2, at = y_ticks, las = 1, cex.axis = 0.7)
  abline(model, col = "red", lwd = 2)
  
  legend_text <- paste0("Pente = ", round(slope, 3))
  legend("topleft", legend = legend_text, bty = "n", col = "red", cex = 0.8)
  
  if (data_name != "") {
    mtext(paste0("*", data_name), side = 1, line = 4, cex = 0.6)
  }
  
  cat(main, "\nPente =", round(slope, 5), "\n\n")
}

# ---- Figure 1 : vitesse d'étude sans outliers ----
png("graphes_export/fig1_vitesse_etude_SANS_outliers.png", width = 800, height = 600)
plot_with_slope(
  data_no_outliers$page_number,
  data_no_outliers$words_per_minute,
  xlab = "Numéro de page",
  ylab = "Mots par minute",
  main = "Figure 1. Progression de la vitesse d'étude (sans outliers)",
  data_name = "Livre: Systemwechsel 1 - Fauxhumain1 (p9 - pXXXX)",
  y_tick_step = 3
)
abline(h = mean_last20_clean, col = "darkgreen", lwd = 2, lty = 2)
legend("bottomright",
       legend = paste0("Moyenne derniers 20 points = ", round(mean_last20_clean, 1), " mpm"),
       col = "darkgreen", lwd = 2, lty = 2, cex = 0.9, bty = "n")
dev.off()

# ---- Figure 2 : vitesse selon le temps cumulé ----
png("graphes_export/fig2_vitesse_etude_total.png", width = 800, height = 600)
plot_with_slope(
  cumulative_hours,
  wpm_clean,
  xlab = "Heures additionnées d'étude (total)",
  ylab = "Mots par minute",
  main = "Figure 2. Progression de la vitesse d'étude (total, sans outliers)",
  data_name = "Livre: Systemwechsel 1 - Fauxhumain1 (p9 - pXXX)",
  y_tick_step = 3
)
abline(h = mean_last20_clean, col = "darkgreen", lwd = 2, lty = 2)
legend("bottomright",
       legend = paste0("Moyenne derniers 20 points = ", round(mean_last20_clean, 1), " mpm"),
       col = "darkgreen", lwd = 2, lty = 2, cex = 0.9, bty = "n")
dev.off()

# ---- Statistiques finales ----
valid_values <- data$words_per_minute[!is.na(data$words_per_minute) & data$words_per_minute != 0]
mean_last50 <- mean(tail(valid_values, 50), na.rm = TRUE)

cat("Moyenne des 50 derniers points (toutes données) :", round(mean_last50, 1), "mpm\n")
cat("Moyenne des 20 derniers points (sans outliers) :", round(mean_last20_clean, 1), "mpm\n\n")

res <- data %>%
  filter(!is.na(page_number), !is.na(words_per_minute), words_per_minute > 0) %>%
  mutate(intervalle = cut(page_number, breaks = c(0, 50, 100, 200, 300, 380), right = FALSE)) %>%
  group_by(intervalle) %>%
  summarise(mean_wpm = mean(words_per_minute, na.rm = TRUE))
print(res)


#Stats globales (toute lecture) ---------------------------------------------------------

# ---- Chargement et nettoyage ----
data_global <- read_ods(fichier_ods, sheet = 5)

# Remplacer les valeurs non numériques par NA
data_global[data_global == "unk"] <- NA
data_global[data_global == "#VALUE!"] <- NA

# Remplacement des valeurs invalides
data_global[data_global %in% c("unk", "#VALUE!")] <- NA

# Conversion en numérique
data_global$page_number <- as.integer(data_global[[1]])
data_global$total_words <- as.integer(data_global[[2]])
data_global$time_minutes <- as.numeric(data_global[[3]])
data_global$outlier_status<- as.character(data_global[[7]])
data_global$words_per_minute <- with(data_global, total_words / time_minutes)
# ---- Nettoyage des outliers ----
mask_keep_global <- is.na(data_global$outlier_status) | tolower(trimws(data_global$outlier_status)) != "yes"
data_no_outliers_global <- data_global[mask_keep_global, ]

# ---- Données propres ----
wpm_clean_global <- data_no_outliers_global$words_per_minute[
  !is.na(data_no_outliers_global$words_per_minute) & data_no_outliers_global$words_per_minute != 0
]
time_clean_global <- data_no_outliers_global$time_minutes[
  !is.na(data_no_outliers_global$time_minutes) & data_no_outliers_global$time_minutes != 0
]
cumulative_hours_global <- cumsum(time_clean_global) / 60
mean_last20_clean_global <- mean(tail(wpm_clean_global, 20), na.rm = TRUE)

# ---- Temps total AVEC outliers ----
time_with_outliers <- data_global$time_minutes[
  !is.na(data_global$time_minutes) & data_global$time_minutes != 0
]
total_hours_with_outliers <- sum(time_with_outliers) / 60


# On affiche ça :

# ---- Figure 1 : vitesse selon le temps cumulé ----
png("graphes_export/fig2_study_speed_merged_total.png", width = 800, height = 600)
plot_with_slope(
  cumulative_hours_global,
  wpm_clean_global,
  xlab = "Heures additionnées d'étude (total)",
  ylab = "Mots par minute",
  main = "Figure 2. Progression de la vitesse d'étude (total, mpm de pages <180 mots non pris en compte)",
  data_name = "Livre: Aussenseiter+Systemwechsel  - Fauxhumain1 (p9 - pXXX)",
  y_tick_step = 2
)

# Ligne horizontale moyenne
abline(h = mean_last20_clean_global, col = "darkgreen", lwd = 2, lty = 2)

# Légende
legend("bottomright",
       legend = paste0("Moyenne derniers 20 points = ", round(mean_last20_clean, 1), " mpm"),
       col = "darkgreen", lwd = 2, lty = 2, cex = 0.9, bty = "n")

# ---- Ajout du texte rouge ----
total_hours <- max(cumulative_hours_global)
text(
  x = max(cumulative_hours_global) * 0.8,  # position horizontale (70 % du max)
  y = min(wpm_clean_global) + 1,            # position verticale
  labels = paste0("Temps total (outliers exclus) : ", round(total_hours, 1), " h"),
  col = "red",
  cex = 1.2,
  font = 2
)
text(
  x = max(cumulative_hours_global) * 0.79,
  y = min(wpm_clean_global) + 3,  # un peu plus haut que le texte rouge précédent
  labels = paste0("Temps total (avec outliers) : ", round(total_hours_with_outliers, 1), " h"),
  col = "red",
  cex = 1.2,
  font = 2
)
dev.off()
