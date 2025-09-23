df <- read.csv("Shift to Solar Power in Colombo and Gampaha (2022-2024).csv", stringsAsFactors = FALSE)
df$Adoption <- df[["Have.you.installed.a.solar.power.system.at.your.premises."]]
df <- subset(df, Adoption == "Yes")
bill_col <- grep("monthly.electricity.bill.change", colnames(df), value = TRUE)
df$BillChange <- df[[bill_col]]
df$BillChangeClean <- trimws(tolower(df$BillChange))
map <- c("increased" = -1, "no change" = 0, "decreased slightly" = 1, "decreased significantly" = 2)
df$BillChangeProxy <- map[df$BillChangeClean]
proxy_scores <- na.omit(df$BillChangeProxy)
t.test(proxy_scores, mu = 0, alternative = "greater")
barplot(
  table(df$BillChange),
  main = "Reported Bill Change After Installing Solar",
  ylab = "Number of Respondents",
  col = c("red", "gray", "skyblue", "green")
)

