df <- read.csv("Shift to Solar Power in Colombo and Gampaha (2022-2024).csv")
df$Adoption <- df[["Have.you.installed.a.solar.power.system.at.your.premises."]]
df <- subset(df, Adoption %in% c("Yes", "No"))

table(df$Adoption)
x <- sum(df$Adoption == "Yes")
n <- nrow(df)
prop.test(x, n, conf.level = 0.95)

barplot(table(df$Adoption), main="Solar Adoption", ylab="Count")
