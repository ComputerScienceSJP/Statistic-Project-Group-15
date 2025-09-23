df <- read.csv("Shift to Solar Power in Colombo and Gampaha (2022-2024).csv")

usage_raw <- df[["Average.Monthly.Electricity.Usage..in.Units."]]

get_mid <- function(x) {
  parts <- strsplit(x, "-")[[1]]
  nums <- as.numeric(trimws(parts))
  if (length(nums) == 2) mean(nums) else NA
}

usage <- na.omit(sapply(usage_raw, get_mid))

hist(usage, prob = TRUE, col = "lightgray", main = "Electricity Usage Distribution", xlab = "Monthly Units")
lines(density(usage), col = "blue", lwd = 2)
xfit <- seq(min(usage), max(usage), length = 100)
yfit <- dnorm(xfit, mean = mean(usage), sd = sd(usage))
lines(xfit, yfit, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Empirical Density", "Normal Distribution"), col = c("blue", "red"), lty = c(1,2), lwd = 2)
