# ======================================
# Código
# ======================================


library(readr)
library(dplyr)
library(PerformanceAnalytics)


titan_class <- read_csv("dados/titan-Class.csv")
titan_file <- read_csv("dados/titan-File.csv")


names(titan_class)[names(titan_class) == "Number of bugs"] <- "bugs"


titan_class$path_join <- gsub("\\\\", "/", titan_class$Path)
titan_file$path_join <- gsub("\\\\", "/", titan_file$LongName)


titan_file_sel <- titan_file %>%
  select(
    path_join,
    `Number of previous fixes`,
    `Number of committers`,
    `Number of previous modifications`,
    `Number of developer commits`
  )


dados <- titan_class %>%
  select(path_join, LOC, LLOC, WMC, CBO, RFC, DIT, LCOM5, bugs) %>%
  left_join(titan_file_sel, by = "path_join")


dados$bug_bin <- ifelse(dados$bugs > 0, 1, 0)


summary(dados)


mean(dados$LOC, na.rm = TRUE)
mean(dados$WMC, na.rm = TRUE)
mean(dados$CBO, na.rm = TRUE)

median(dados$LOC, na.rm = TRUE)
median(dados$WMC, na.rm = TRUE)
median(dados$CBO, na.rm = TRUE)


moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


moda_LOC   <- moda(dados$LOC)
moda_WMC   <- moda(dados$WMC)
moda_CBO   <- moda(dados$CBO)
moda_RFC   <- moda(dados$RFC)
moda_DIT   <- moda(dados$DIT)
moda_LCOM5 <- moda(dados$LCOM5)

cat("Moda LOC:", moda_LOC, "\n")
cat("Moda WMC:", moda_WMC, "\n")
cat("Moda CBO:", moda_CBO, "\n")
cat("Moda RFC:", moda_RFC, "\n")
cat("Moda DIT:", moda_DIT, "\n")
cat("Moda LCOM5:", moda_LCOM5, "\n")


quantile(dados$LOC, probs = c(0.25, 0.50, 0.75, 0.90, 0.95), na.rm = TRUE)
quantile(dados$WMC, probs = c(0.25, 0.50, 0.75, 0.90, 0.95), na.rm = TRUE)
quantile(dados$CBO, probs = c(0.25, 0.50, 0.75, 0.90, 0.95), na.rm = TRUE)


sd(dados$LOC, na.rm = TRUE)
var(dados$LOC, na.rm = TRUE)
range(dados$LOC, na.rm = TRUE)


sd(dados$WMC, na.rm = TRUE)
var(dados$WMC, na.rm = TRUE)
range(dados$WMC, na.rm = TRUE)


sd(dados$CBO, na.rm = TRUE)
var(dados$CBO, na.rm = TRUE)
range(dados$CBO, na.rm = TRUE)


hist(dados$LOC, main = "Histograma LOC", xlab = "LOC")
hist(dados$WMC, main = "Histograma WMC", xlab = "WMC")
hist(dados$CBO, main = "Histograma CBO", xlab = "CBO")


boxplot(dados$LOC, main = "Boxplot LOC", ylab = "LOC")
boxplot(dados$WMC, main = "Boxplot WMC", ylab = "WMC")


boxplot(dados$CBO,
        main = "Boxplot da métrica CBO",
        ylab = "CBO",
        col = "lightblue",
        outline = TRUE)


plot(dados$LOC, dados$WMC,
     main = "Dispersão entre LOC e WMC",
     xlab = "LOC",
     ylab = "WMC")


chart.Correlation(dados[, c("LOC", "WMC", "CBO", "RFC", "DIT", "LCOM5", "bugs")])


shapiro.test(dados$LOC)
shapiro.test(dados$WMC)
shapiro.test(dados$CBO)
shapiro.test(dados$RFC)


modelo <- glm(bug_bin ~ LOC + WMC + CBO + RFC + DIT + LCOM5,
              data = dados,
              family = binomial)

summary(modelo)