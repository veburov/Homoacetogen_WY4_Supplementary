# ======================================================================
# Gompertz_model.R
# ----------------------------------------------------------------------
# This script fits experimental methane production data
# to the modified Gompertz model and calculates:
#   - Gm: potential methane yield
#   - Rm: maximum methane production rate
#   - lambda: lag phase duration
# It also estimates confidence intervals, R², and generates a plot.
# ======================================================================

# --- Load required packages ---
library(ggplot2)
library(dplyr)

# --- 1. Input data (replace with your own) ---
# Provide two vectors: 
#   t     = time (days)
#   G_obs = cumulative methane production (e.g., mL or mol CH4 / kg VS)
#
# Example dataset (toy data for demonstration only):
t <- rep(0:10, each = 3)  # triplicates
set.seed(123)
G_obs <- 8 * (1 - exp(-0.25 * t)) + rnorm(length(t), sd = 0.2)

# --- 2. Fit the modified Gompertz model ---
start_vals <- list(Gm = max(G_obs), Rm = 1, lambda = 0)

fit <- nls(
  G_obs ~ Gm * exp(-exp((Rm * exp(1) / Gm) * (lambda - t) + 1)),
  start = start_vals,
  control = nls.control(maxiter = 200, tol = 1e-6)
)

# --- 3. Estimated parameters ---
coef_fit <- coef(fit)

# --- 4. 95% confidence intervals & SD approximation ---
ci <- confint(fit)
cat("Parameter estimates (± SD):\n")
for(i in 1:nrow(ci)){
  est <- coef_fit[i]
  sd <- (ci[i,2] - ci[i,1]) / (2 * 1.96)  # SD approx. from 95% CI
  cat(sprintf("  %s = %.3f ± %.3f\n", rownames(ci)[i], est, sd))
}

# --- 5. Model performance (R²) ---
G_pred <- predict(fit)
SS_res <- sum((G_obs - G_pred)^2)
SS_tot <- sum((G_obs - mean(G_obs))^2)
R2 <- 1 - SS_res/SS_tot
cat(sprintf("\nR² = %.4f\n\n", R2))

# --- 6. Summary table (mean ± SD per day) ---
df <- data.frame(t = t, G_obs = G_obs, G_pred = G_pred)
df_summary <- df %>%
  group_by(t) %>%
  summarise(
    G_mean = mean(G_obs),
    G_sd   = sd(G_obs),
    .groups = "drop"
  )

cat("Daily mean ± SD:\n")
for(i in 1:nrow(df_summary)){
  cat(sprintf("  Day %d: %.2f ± %.2f\n",
              df_summary$t[i],
              df_summary$G_mean[i],
              df_summary$G_sd[i]))
}

# --- 7. Plot ---
ggplot() +
  geom_point(data = df_summary, aes(x = t, y = G_mean), 
             shape = 4, size = 3, color = "black") +
  geom_errorbar(data = df_summary, aes(x = t, ymin = G_mean - G_sd, ymax = G_mean + G_sd), 
                width = 0.2, color = "black") +
  geom_line(data = df, aes(x = t, y = G_pred), 
            linetype = "dashed", size = 1, color = "blue") +
  labs(title = "Modified Gompertz model fit",
       x = "Time (days)",
       y = "Methane production") +
  theme_minimal()

