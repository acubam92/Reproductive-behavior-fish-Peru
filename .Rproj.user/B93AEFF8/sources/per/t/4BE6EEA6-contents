
# Fechas ------------------------------------------------------------------
fecha <- read.csv(file = "data/fecha.csv", stringsAsFactors = FALSE, sep = ";")
fecha <- fecha[301:324,]

par(mar = c(0,4,0,0), oma = c(5,1,1,1), mfrow = c(3,1))
# AICEN -------------------------------------------------------------------
dataICEN <- read.csv(file = "data/ICEN 1950_2023.csv", stringsAsFactors = FALSE, sep = ";")
rangos <- c(-1,-1.2,-1.4)

dataICEN_2017_2018 <- dataICEN[dataICEN$year %in% c(2017,2018),]

anoma.to.use.numeric <- as.numeric(dataICEN_2017_2018$ICEN)

xlim <- c(1, length(anoma.to.use.numeric))
ylim1 <- c(-2, 2)

umbral <- 0

plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
abline(h = -3:3, v = seq(1,36, 12), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
abline(h = rangos, lty = "dotted", col = "blue")

axis(side = 2, at = -3:3, las = 2, cex.axis = 1.3)
mtext(text = "ICEN", side = 2, line = 2.5, cex = 1.3)
box()

# AIGS --------------------------------------------------------------------
anoma.to.use <- matrixIGS[rownames(matrixIGS) %in% c(2017:2018),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))

xlim <- c(1, length(anoma.to.use.numeric))
ylim1 <- c(-3, 3)

deciles.use.1 <- rep(rangosIGS[[3]][,2], 2)
deciles.use.2 <- rep(rangosIGS[[3]][,1], 2)


umbral <- 0

plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.1, 
              rev(deciles.use.1*-1)), 
        col ="grey50", border = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.2, rev(deciles.use.2*-1)), 
        col ="grey70", border = FALSE)

abline(h = -3:3, v = seq(1,36, 12), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
axis(side = 2, at = -3:3, las = 2, cex.axis = 1.3)
mtext(text = "AIGS", side = 2, line = 2.5, cex = 1.3)
legend(x = 1, y = -2, legend = c("Anomalía positiva", "Anomalía negativa"), 
       fill = c("blue", "red"), cex = 0.8, bty = "n")
legend(x = 6, y = -2, legend = c("Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
       fill = c("grey70", "grey50", "white"), cex = 0.8, bty = "n")

box()

# AFD ---------------------------------------------------------------------
anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2017:2018),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))

xlim <- c(1, length(anoma.to.use.numeric))
ylim1 <- c(-20, 20)

deciles.use.1 <- c(rep(decilesFD[,2], 2))
deciles.use.2 <- c(rep(decilesFD[,1], 2))

umbral <- 0

plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.1, 
              rev(deciles.use.1*-1)), 
        col ="grey50", border = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.2, rev(deciles.use.2*-1)), 
        col ="grey70", border = FALSE)

abline(h = seq(-30, 30, 5), v = seq(1,36, 12), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)

axis(side = 1, at = 1:length(fecha$label), labels = fecha$label, cex.axis = 1.3, las = 2)
axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
mtext(text = "AFD", side = 2, line = 2.5, cex = 1.3)
mtext(text = "Meses", side = 1, line = 3.5, cex = 1.3, outer = TRUE)
legend(x = 1, y = -12, legend = c("Anomalía positiva", "Anomalía negativa"), 
       fill = c("blue", "red"), cex = 0.8, bty = "n")
legend(x = 6, y = -12, legend = c("Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
       fill = c("grey70", "grey50", "white"), cex = 0.8, bty = "n")
box()

dev.copy(png, 
         filename = paste0("figures/Anomalia ICEN, AIGS, AFD.png"),
         width = 1800, height = 2500, res = 300)
dev.off()
