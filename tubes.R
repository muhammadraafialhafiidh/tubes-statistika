library(data.table)
data <- fread("daun2.csv")

summary(data[data$jenis == "arumanis"])
summary(data[data$jenis == "manalagi"])

library(moments)
skewness(data$Panjang[data$jenis == "arumanis"])
skewness(data$Lebar[data$jenis == "arumanis"])
skewness(data$diameter[data$jenis == "arumanis"])
skewness(data$Panjang[data$jenis == "manalagi"])
skewness(data$Lebar[data$jenis == "manalagi"])
skewness(data$diameter[data$jenis == "manalagi"])


par(mfrow = c(1, 3))
hist(data$Panjang[data$jenis == "arumanis"], 
     main = "Panjang Daun Mangga Arumanis", 
     xlab = "Panjang (cm)", 
     col = "blue", 
     border = "black")
hist(data$Lebar[data$jenis == "arumanis"], 
     main = "Lebar Daun Mangga Arumanis", 
     xlab = "Lebar (cm)", 
     col = "green", 
     border = "black")
hist(data$diameter[data$jenis == "arumanis"], 
     main = "Diameter Daun Mangga Arumanis", 
     xlab = "Diameter (cm)", 
     col = "coral", 
     border = "black")

par(mfrow = c(1, 3))
hist(data$Panjang[data$jenis == "manalagi"], 
     main = "Panjang Daun Mangga Manalagi", 
     xlab = "Panjang (cm)", 
     col = "red", 
     border = "black")
hist(data$Lebar[data$jenis == "manalagi"], 
     main = "Lebar Daun Mangga Manalagi", 
     xlab = "Lebar (cm)", 
     col = "navy", 
     border = "black")
hist(data$diameter[data$jenis == "manalagi"], 
     main = "Diameter Daun Mangga Manalagi", 
     xlab = "Diameter (cm)", 
     col = "orange", 
     border = "black")



par(mfrow = c(1, 3))
plot(data$Panjang[data$jenis == "arumanis"], 
     data$Lebar[data$jenis == "arumanis"], 
     main = "Panjang dan Lebar Daun Arumanis",
     xlab = "Panjang Daun (cm)", 
     ylab = "Lebar Daun (cm)", 
     col = "blue", 
     pch = 16)
model1 <- lm(Lebar ~ Panjang, data = data[data$jenis == "arumanis", ])
abline(model1, col = "red", lwd = 2)
plot(data$diameter[data$jenis == "arumanis"], 
     data$Panjang[data$jenis == "arumanis"], 
     main = "Diameter dan Panjang Daun Arumanis",
     xlab = "Diameter Daun (cm)", 
     ylab = "Panjang Daun (cm)", 
     col = "green", 
     pch = 16)
model2 <- lm(Panjang ~ diameter, data = data[data$jenis == "arumanis", ])
abline(model2, col = "red", lwd = 2)
plot(data$diameter[data$jenis == "arumanis"], 
     data$Lebar[data$jenis == "arumanis"], 
     main = "Diameter dan Lebar Daun Arumanis",
     xlab = "Diameter Daun (cm)", 
     ylab = "Lebar Daun (cm)", 
     col = "coral", 
     pch = 16)
model3 <- lm(Lebar ~ diameter, data = data[data$jenis == "arumanis", ])
abline(model3, col = "red", lwd = 2)

par(mfrow = c(1, 3))
plot(data$Panjang[data$jenis == "manalagi"], 
     data$Lebar[data$jenis == "manalagi"], 
     main = "Panjang dan Lebar Daun Manalagi",
     xlab = "Panjang Daun (cm)", 
     ylab = "Lebar Daun (cm)", 
     col = "red", 
     pch = 16)
model1 <- lm(Lebar ~ Panjang, data = data[data$jenis == "manalagi", ])
abline(model1, col = "green", lwd = 2)
plot(data$diameter[data$jenis == "manalagi"], 
     data$Panjang[data$jenis == "manalagi"], 
     main = "Diameter dan Panjang Daun Manalagi",
     xlab = "Diameter Daun (cm)", 
     ylab = "Panjang Daun (cm)", 
     col = "navy", 
     pch = 16)
model2 <- lm(Panjang ~ diameter, data = data[data$jenis == "manalagi", ])
abline(model2, col = "green", lwd = 2)
plot(data$diameter[data$jenis == "manalagi"], 
     data$Lebar[data$jenis == "manalagi"], 
     main = "Diameter dan Lebar Daun Manalagi",
     xlab = "Diameter Daun (cm)", 
     ylab = "Lebar Daun (cm)", 
     col = "orange", 
     pch = 16)
model3 <- lm(Lebar ~ diameter, data = data[data$jenis == "manalagi", ])
abline(model3, col = "green", lwd = 2)

cor(data$diameter[data$jenis == "arumanis"],data$diameter[data$jenis == "manalagi"])

par(mfrow = c(1, 3))
boxplot(Panjang ~ jenis, 
        data = data, 
        main = "Panjang Daun Arumanis dan Manalagi", 
        xlab = "Jenis Daun", 
        ylab = "Panjang Daun (cm)", 
        col = c("blue", "red"), 
        border = "black")
boxplot(Lebar ~ jenis, 
        data = data, 
        main = "Lebar Daun Arumanis dan Manalagi", 
        xlab = "Jenis Daun", 
        ylab = "Lebar Daun (cm)", 
        col = c("navy", "green"), 
        border = "black")
boxplot(diameter ~ jenis, 
        data = data, 
        main = "Diameter Daun Arumanis dan Manalagi", 
        xlab = "Jenis Daun", 
        ylab = "Diameter Daun (cm)", 
        col = c("coral", "orange"), 
        border = "black")








library(ggplot2)

























contoh

summary(data)


hist_data <-hist(data$Panjang[data$jenis == "arumanis"], 
                 main = "Panjang Daun Mangga Arumanis", 
                 xlab = "Panjang (cm)", 
                 col = "blue", 
                 border = "black")
text(hist_data$mids, hist_data$counts, 
     labels = hist_data$counts, 
     pos = 3, cex = 0.8, col = "blue")


