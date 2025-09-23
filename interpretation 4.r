df <- read.csv("Shift to Solar Power in Colombo and Gampaha (2022-2024).csv")

bills <- df[["Average.Monthly.Electricity.Bill..in.LKR."]]
counts <- table(bills)
barplot(counts,
        main = "Monthly Electricity Bill Ranges",
        col = "skyblue",
        ylab = "Number of Respondents",
        las = 1)

get_mid <- function(x) {
  x <- gsub(",", "", x)
  parts <- strsplit(x, "-")[[1]]
  nums <- as.numeric(trimws(parts))
  if (length(nums) == 2) mean(nums) else NA
}

bill_vals <- na.omit(sapply(bills, get_mid))

mean(bill_vals)
t.test(bill_vals, conf.level = 0.95)

if (!dir.exists("figures")) dir.create("figures")
pdf("figures/monthly_bill_ranges.pdf", width = 7, height = 4)
barplot(counts,
        main = "Monthly Electricity Bill Ranges",
        col = "skyblue",
        ylab = "Number of Respondents",
        las = 1)
dev.off()
