df <- read.csv("Shift to Solar Power in Colombo and Gampaha (2022-2024).csv")

clean <- function(x) {
  y <- gsub("Rs\\.?\\s*", "", x)
  y <- gsub(",", "", y)
  y <- gsub("–", "-", y)
  y <- trimws(y)
  if (grepl("-", y)) {
    p <- strsplit(y, "-", fixed = TRUE)[[1]]
    mean(as.numeric(trimws(p)))
  } else {
    as.numeric(y)
  }
}

df$BillMid <- sapply(df$Average.Monthly.Electricity.Bill..in.LKR., clean)
df$UsageMid <- sapply(df$Average.Monthly.Electricity.Usage..in.Units., clean)

dat <- na.omit(df[, c("BillMid", "UsageMid")])

cor.test(dat$UsageMid, dat$BillMid)

fit <- lm(BillMid ~ UsageMid, data = dat)
summary(fit)

plot(dat$UsageMid, dat$BillMid, pch = 19,
     xlab = "Usage (Units)", ylab = "Bill (LKR)")
abline(fit, col = "red", lwd = 2)
