install.packages('corrr')
install.packages('corrplot')
library(corrr)
library(corrplot)
library(tidyverse)

# 1. Read in your summary table of counts
data.temp <- read.csv("/Users/YOUR USER NAME/Downloads/YOUR DATA.csv", 
                      stringsAsFactors = FALSE)

# 2. Select the numeric columns in your data
data <- data.temp[c(INSERT COLUMN NUMBERS HERE AS A COMMA SEPARATED LIST)]

# 2. Run this to create a function for correlations
corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficients on the diagonal
           diag = diag
  )
} 

# 3. Create a matrix of correlations
correlate.counts <- correlate(data)

# 4. Create a correlation chart
corrplot2(
  data = data,
  method = "pearson",
  sig.level = 0.01, # adjust accordingly, 0.05 is the social science standard
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)

# 5. Create a correlation network
correlate.counts %>% network_plot(min_cor = .01, colors = c("red","blue"))

