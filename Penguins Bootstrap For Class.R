#Install these packages
install.packages(c("palmerpenguins", "ggplot2", "dplyr", "ggpubr"))

# Load required libraries
library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(ggpubr)

##### Histogram of body mass for all penguins #### 
# Clean data: filter non-missing body masses
penguins_clean <- penguins |>
  filter(!is.na(body_mass_g)) 

# Plot original distribution of body mass for all penguins
ggplot(penguins_clean, aes(x = body_mass_g)) +
  geom_histogram(fill = "#0072B2", color = "white", bins = 30) +
  labs(
    title = "Distribution of Body Mass in All Penguins",
    subtitle = "Data from palmerpenguins package",
    x = "Body Mass (grams)",
    y = "Count"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", face = "bold")
  )

#### Bootstrapped Mean CI for All Penguins #####

bootstrap_penguin_mean <- function(data = penguins_clean$body_mass_g, 
                                   R = 10000, 
                                   alpha = 0.05) {
 set.seed(123)
  
  bootstrap_means <- numeric(R)  # Preallocate vector to store results
  
  for (i in 1:R) {
    sample_i <- sample(data, size = length(data), replace = TRUE)  # Resample with replacement
    bootstrap_means[i] <- mean(sample_i)  # Store the sample mean
  }
  
  # Compute confidence interval
  ci_bounds <- quantile(bootstrap_means, probs = c(alpha / 2, 1 - alpha / 2))
  
  # Prepare data for plotting
  boot_df <- data.frame(mean_body_mass = bootstrap_means)
  boot_df$ci_region <- case_when(
    boot_df$mean_body_mass < ci_bounds[1] ~ "Outside CI",
    boot_df$mean_body_mass > ci_bounds[2] ~ "Outside CI",
    TRUE ~ "Inside CI"
  )
  
  # Plot bootstrap histogram
  ggplot(boot_df, aes(x = mean_body_mass, fill = ci_region)) +
    geom_histogram(bins = 50, color = "black", alpha = 0.8) +
    scale_fill_manual(values = c(
      "Inside CI" = "#dc5318",
      "Outside CI" = "#5c89a6"
    )) +
    labs(
      title = "Bootstrap Distribution of Mean Body Mass (All Penguins)",
      subtitle = paste0((1 - alpha) * 100, "% CI: [", 
                        round(ci_bounds[1], 2), ", ", round(ci_bounds[2], 2), "] grams"),
      x = "Bootstrapped Mean Body Mass (grams)",
      y = "Frequency"
    ) +
    theme_classic(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black", face = "bold"),
      plot.title = element_text(color = "black", face = "bold", size = 16),
      plot.subtitle = element_text(color = "black", size = 12),
      legend.position = "none"
    )
}

# Run it!
bootstrap_penguin_mean(alpha = 0.05)  # 95% CI

#Compare to parametric test! 
t.test(penguins$body_mass_g)

##### Types of Penguins Weight #####

# Clean the data
penguins_clean <- penguins |>
  filter(!is.na(body_mass_g), !is.na(species))

# Create individual histograms
p_adelie <- ggplot(filter(penguins_clean, species == "Adelie"), aes(x = body_mass_g)) +
  geom_histogram(bins = 20, fill = "#E69F00", color = "white") +
  labs(title = "Adelie Penguins", x = "Body Mass (g)", y = "Count") +
  theme_classic(base_size = 14)

p_chinstrap <- ggplot(filter(penguins_clean, species == "Chinstrap"), aes(x = body_mass_g)) +
  geom_histogram(bins = 20, fill = "#56B4E9", color = "white") +
  labs(title = "Chinstrap Penguins", x = "Body Mass (g)", y = "Count") +
  theme_classic(base_size = 14)

p_gentoo <- ggplot(filter(penguins_clean, species == "Gentoo"), aes(x = body_mass_g)) +
  geom_histogram(bins = 20, fill = "#009E73", color = "white") +
  labs(title = "Gentoo Penguins", x = "Body Mass (g)", y = "Count") +
  theme_classic(base_size = 14)

ggarrange(
  p_adelie, p_chinstrap, p_gentoo,
  ncol = 3, nrow = 1,
  common.legend = FALSE
)
