## ----echo=FALSE----------------------------------------------------------------------
library(tidyverse)
library(glmnet)
source ("Funktion_Residuenanalyse.R") # Datei mit Code

#setwd("~/GitHub/HockeyAnalytics/scripts")

# Select your prefered Season
Run_RAPM <- readRDS("../data/temp/Run_RAPM_L1_S2021.RData")

row_names <- rownames(Run_RAPM[[2]])
# Create a logical vector to identify rows without 'offense' or 'defense' in their names
rows_to_keep <- !grepl("offense|defense", tolower(row_names))
filtered_data <- Run_RAPM[[2]][rows_to_keep, ] # Subset of dataframe
Run_RAPM[[2]] <- filtered_data # Update the list with the filtered dataframe
coefficients = Run_RAPM[[2]]

# Einfacher Darstellung zum ablesen der Koeffizienten
coefficients_long <- data.frame(Prädiktor = names(coefficients), 
                                Schätzwert = coefficients, 
                                row.names = NULL)

coefficients_long

## ------------------------------------------------------------------------------------
# Make a validation with help of a lienar Regression to see if the same variables are stat. significant

# Load data from /temp folder
season_info <- sub(".*/RAPM_xGF_(L\\d+_S\\d+).RData", "\\1", temp_file_path)
xGF60 <- readRDS(paste("../data/temp/xGF_60_", season_info, ".RData", sep = ""))
shift_length <- readRDS(paste("../data/temp/shift_length_", season_info, ".RData", sep = ""))
RAPM_xGF <- readRDS(temp_file_path)
#RAPM_xGF <- readRDS("../data/temp/RAPM_xGF_L1_S2021.RData")

RAPM_xGF_df <- as.data.frame(RAPM_xGF) # lm braucht ein dataframe und kann den Typ Matrix nicht umwandeln.
Run_RAPM_lm <- lm(xGF60 ~ ., data=RAPM_xGF_df, weights=shift_length)
model_summary <- summary(Run_RAPM_lm) # Summary zuweisen
coefficients <- model_summary$coefficients

# Filtern der Koeffizienten nach p-Wert, somit sind folgende Koeffizienten signifikant:
significant_coefficients <- coefficients[coefficients[, "Pr(>|t|)"] < 0.05, ]
significant_coefficients

coef_names <- rownames(coefficients)
filtered_coefficients <- coefficients[!grepl("offense|defense", coef_names, ignore.case = TRUE), ]
write.csv(filtered_coefficients, file = paste("../data/output/filtered_coefficients_",season_info,".csv", sep =""), row.names = TRUE)

filtered_coefficients


par(mfrow = c(2,3))
#plot(Run_RAPM_lm)
#plot.lmSim(Run_RAPM_lm)


#create horizontal bar chart to display each VIF value
#barplot(vif_values, main = "VIF Werte", horiz = TRUE, col = "steelblue")

#add vertical line at 5
#abline(v = 5, lwd = 3, lty = 2)

toi_data <- shifts_grouped_ev %>%
  select(h1.num:h6.num,a1.num:a6.num,shift_length)

toi_data <- toi_data %>%
  gather(key = "Player_position", value = "Player", h1.num:h6.num, a1.num:a6.num) %>%
  select(Player, shift_length)

# Calculate TOI for each player
toi <- toi_data %>%
  group_by(Player) %>%
  summarise(TimeOnIce = sum(shift_length, na.rm = TRUE) / 60) %>% # from seconds -> minutes
  arrange(desc(TimeOnIce))
# Only Player Coefficients:
filtered_coefficients <- coefficients

# Create a data frame with coef_names and values
filtered_coefficients <- data.frame(coef_names, values = coefficients)

# Extract player names using regular expression and add as a new column
filtered_coefficients$Player <- gsub(".*__(\\S+)", "\\1", filtered_coefficients$coef_names)

filtered_coefficients <- merge(filtered_coefficients, toi, by = "Player", all.x = TRUE)

# Remove duplicate rows based on the player_name column
filtered_linear <- filtered_coefficients %>%
  select(-c(coef_names,values.Std..Error,values.t.value,values.Pr...t..)) %>%
  distinct(Player, .keep_all = TRUE)

# Create unique variable names based on season_info and assign the filtered data
assign(paste("filtered_linear_", season_info, sep = ""), filtered_linear, envir = .GlobalEnv)
