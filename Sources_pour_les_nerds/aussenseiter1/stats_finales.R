library(readODS)
library(ggplot2)
library(lubridate)
library(dplyr)

# Créer un dossier de sortie (si nécessaire)
dir.create("graphes_export", showWarnings = FALSE)

# Charger les données
data <- read_ods("~/Bureau/allemand/Sources_pour_les_nerds/german_stats.ods", sheet = 1)

# Remplacer les valeurs non numériques par NA
data[data == "unk"] <- NA
data[data == "#VALUE!"] <- NA

# Convertir les colonnes en numériques
data$page_number <- as.integer(data[[1]])
data$new_words <- as.integer(data[[2]])
data$consult_words <- as.integer(data[[3]])
data$total_words <- as.integer(data[[4]])
data$time_minutes <- as.numeric(data[[5]])

# Dates
dates_raw <- as.character(data[[6]])
dates_parsed <- dmy(dates_raw)
print(dates_parsed)

#Récupération du statut du batch (outlier = "yes")
#Les outliers sont les batchs de pages où les moyennes de temps sont sous-estimées à cause de pages <180 mots
data$outlier_status<- as.character(data[[9]])

# Calculs dérivés
data$words_per_minute <- with(data, total_words / time_minutes)
data$time_per_word <- with(data, time_minutes / total_words)
data$new_words_on_total <- with(data, 100 * new_words / total_words)
data$consult_words_on_total <- with(data, 100 * consult_words / total_words)
total_dur_hour <- (sum(data$time_minutes, na.rm = TRUE))/60

# --- OUTLIERS: masque et sous-ensemble propre
# Créer dataset sans batchs outliers :
mask_keep <- is.na(data$outlier_status) | tolower(trimws(data$outlier_status)) != "yes"
data_no_outliers <- data[mask_keep, ]

#Moyenne des 20 derniers points NON-OUTLIERS :
valid_values_clean <- data_no_outliers$words_per_minute[
  !is.na(data_no_outliers$words_per_minute) & data_no_outliers$words_per_minute != 0
]
mean_last20_clean <- mean(tail(valid_values_clean, 20))

# Fonction pour tracer et afficher pente
plot_with_slope <- function(x, y, xlab, ylab, main, data_name = "", y_tick_step = 1) {
  valid <- !is.na(x) & !is.na(y)
  model <- lm(y[valid] ~ x[valid])
  slope <- coef(model)[2]
  
  plot(x[valid], y[valid],
       xlab = xlab, ylab = ylab, main = main,
       pch = 19, col = "blue",
       cex.main = 0.8, cex.lab = 0.7, cex.axis = 0.7, cex = 0.7,
       yaxt = "n"  # désactive l’axe y automatique
  )
  
  # Axe y personnalisé
  y_ticks <- seq(floor(min(y[valid])), ceiling(max(y[valid])), by = y_tick_step)
  axis(side = 2, at = y_ticks, las = 1, cex.axis = 0.7)
  
  abline(model, col = "red", lwd = 2)
  
  legend_text <- paste0("Pente = ", round(slope, 3))
  legend("topleft", legend = legend_text, bty = "n", col = "red", cex = 0.8)
  
  if (data_name != "") {
    mtext(paste0("*", data_name, ""), side = 1, line = 4, cex = 0.6)
  }
  
  cat(main, "\n")
  cat("Pente =", round(slope, 5), "\n\n")
}

# Tracer et calculer les pentes pour chaque graphique

# Filtrer les valeurs valides (ni NA ni 0)
valid_values <- data$words_per_minute[!is.na(data$words_per_minute) & data$words_per_minute != 0]

# Moyenne des 20 derniers points valides
mean_last20 <- mean(tail(valid_values, 20))

# Figure 1 avec régression + ligne horizontale de la moyenne
png("graphes_export/fig1_vitesse_etude.png", width = 800, height = 600)
plot_with_slope(
  data$page_number,
  data$words_per_minute,
  xlab = "Numéro de page",
  ylab = "Mots par minute",
  main = "Figure 1. Progression de la vitesse d'étude (avec batchs w/ outliers)",
  data_name = "Livre: Außenseiter 1 - Fauxhumain1 (p9 - p378)",
  y_tick_step = 3
)

# Ligne horizontale = moyenne des 20 derniers points
abline(h = mean_last20, col = "darkgreen", lwd = 2, lty = 2)

# Légende en bas à droite
legend("bottomright", 
       legend = paste0("Moyenne derniers 20 points = ", round(mean_last20, 1), " mpm"),
       col = "darkgreen", lwd = 2, lty = 2, cex = 0.9, bty = "n")

dev.off()

# --- Figure 1 SANS outliers
png("graphes_export/fig1_vitesse_etude_SANS_outliers.png", width = 800, height = 600)
plot_with_slope(
  data_no_outliers$page_number,
  data_no_outliers$words_per_minute,
  xlab = "Numéro de page",
  ylab = "Mots par minute",
  main = "Figure 1. Progression de la vitesse d'étude (sans batchs w/ outliers)",
  data_name = "Livre: Außenseiter 1 - Fauxhumain1 (p9 - p378)",
  y_tick_step = 3
)

abline(h = mean_last20_clean, col = "darkgreen", lwd = 2, lty = 2)
legend(
  "bottomright",
  legend = paste0("Moyenne derniers 20 points = ", round(mean_last20_clean, 1), " mpm"),
  col = "darkgreen", lwd = 2, lty = 2, cex = 0.9, bty = "n"
)
dev.off()

#New words per page
png("graphes_export/fig2_mots_nouveaux.png", width = 800, height = 600)
plot_with_slope(
  data$page_number,
  data$new_words_on_total,
  xlab = "Numéro de page",
  ylab = "Mots nouveaux (%)",
  main = "Figure 2. Taux de mots découverts par page (sans outliers)",
  data_name = "Livre: Außenseiter 1 - Fauxhumain1 (p9 - p378)",
  y_tick_step = 4
)
dev.off()

#Superposition

#data_no_outliers$page_number,
#data_no_outliers$words_per_minute,
#data_no_outliers

plot_superpose_fig1_fig2 <- function(data) {
  valid1 <- !is.na(data_no_outliers$page_number) & !is.na(data_no_outliers$words_per_minute)
  valid3 <- !is.na(data_no_outliers$page_number) & !is.na(data_no_outliers$new_words_on_total)
  
  # Tracer figure 1 : mots par minute (axe Y gauche)
  plot(data_no_outliers$page_number[valid1], data_no_outliers$words_per_minute[valid1],
       type = "l", pch = 3, col = "blue",
       xlab = "Numéro de page", ylab = "Mots par minute",
       main = "Figure 3. Figure 1 & 2 superposées (sans batchs w/ outliers)",
       cex.main = 0.8, cex.lab = 0.7, cex.axis = 0.7, cex = 0.4)
  mtext("*Livre: Außenseiter 1 - Fauxhumain1 (p9 - p378)", side = 1, line = 4, cex = 0.6, col = "black")
  # Calcul et affichage pente pour figure 1
  model1 <- lm(words_per_minute ~ page_number, data = data_no_outliers[valid1, ])
  #abline(model1, col = "blue", lwd = 2)
  legend_text1 <- paste0("Vitesse étude mots/min (pente = ", round(coef(model1)[2], 3), ")")
  
  # Ajouter figure 3 : mots nouveaux en % (axe Y droit)
  par(new = TRUE)
  plot(data_no_outliers$page_number[valid3], data_no_outliers$new_words_on_total[valid3],
       type = "l", pch = 17, col = "red",
       axes = FALSE, xlab = "", ylab = "", cex = 0.4)
  axis(side = 4, col = "red", col.axis = "red", cex.axis = 0.7)
  mtext("Mots nouveaux (%)", side = 4, line = 2, col = "red", cex = 0.7)
  
  # Calcul et affichage pente pour figure 3
  model3 <- lm(new_words_on_total ~ page_number, data = data_no_outliers[valid3, ])
  #abline(model3, col = "red", lwd = 2)
  legend_text3 <- paste0("Mots nouveaux/total % (pente = ", round(coef(model3)[2], 3), ")")
  
  
  # Ajouter légende
  legend("top", legend = c(legend_text1, legend_text3),
         col = c("blue", "red"), pch = c(19, 19), bty = "n", cex = 0.7)
  
  #bty = n sinon
  # Afficher pentes dans console
  cat("Figure 1 - pente mots/minute:", round(coef(model1)[2], 5), "\n")
  cat("Figure 2 - pente mots nouveaux %:", round(coef(model3)[2], 5), "\n")
}

#Génère figure superposée
png("graphes_export/fig3_superposition_fig1_fig2.png", width = 800, height = 600)
plot_superpose_fig1_fig2(data)
dev.off()

#Consult dico
png("graphes_export/fig4_mots_consultes.png", width = 800, height = 600)
plot_with_slope(
  data$page_number,
  data$consult_words_on_total,
  xlab = "Numéro de page",
  ylab = "Mots consultés (%)",
  main = "Figure 5. Taux de mots consultés (usage dico) par page",
  data_name = "Livre: Außenseiter 1 - Fauxhumain1 (p9 - p378)",
  y_tick_step = 2
)
dev.off()

# Histo pour temps total
p <- ggplot(data, aes(x = page_number, y = time_minutes)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(breaks = seq(10, 380, by = 20)) +
  labs(x = "Numéro de page", y = "Durée (min)", 
       title = "Figure 5. Temps total d'étude effectif",
       caption = "*Livre: Außenseiter 1 - Fauxhumain1 (p9 - p378)") +
  theme_minimal() +
  theme(
    plot.title = element_text(
    face = "bold",        # texte en gras
    size = 10,            # taille moyenne (par défaut autour de 11-12, 14 est un bon compromis)
    hjust = 0.5,          # centrage horizontal
    color = "black"       # couleur noire sobre
    ),
    axis.title.y = element_text(size=9),
    axis.title.x = element_text(size=9),
    plot.caption = element_text(hjust = 0.5, size = 7.2))

# Calcul du total
total_time <- (sum(data$time_minutes, na.rm = TRUE))/60

# Ajout du total en annotation
p <- p + annotate("text", x = Inf, y = Inf, 
                  label = paste("Total:", round(total_time, 1), "heures (sans pause)"),
                  hjust = 1.1, vjust = 1.5, size = 3.5, color = "red")
print(p)

# Capture temps total (ggplot)
ggsave(
  filename = "graphes_export/fig5_temps_total_etude.png",
  plot = p,
  width = 6,
  height = 4
)

# Vitesse sur les 50 dernières pages :
valid_values <- data$words_per_minute[!is.na(data$words_per_minute) & data$words_per_minute != 0]
mean_last50 <- mean(tail(valid_values, 50))
print(mean_last50)

# --- Moyenne des 20 derniers points valides SANS outliers
print("Last 20 words per minute, no outliers:")
print(mean_last20_clean)
print("End no outliers")


#Vitesse intervalles 

res <- with(subset(data, !is.na(page_number) & !is.na(words_per_minute) & words_per_minute > 0),
            tapply(words_per_minute,
                   cut(page_number, breaks = c(0, 50, 100, 200, 300, 380), right = FALSE),
                   mean))
print(res)
