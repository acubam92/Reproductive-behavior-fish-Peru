par(mfrow = c(3,1), mar = c(1,6,0.5,1), oma = c(5,1,1,1))
#IGS 2021 - 2023
anoma.to.use <- matrixIGS[rownames(matrixIGS) %in% c(2021:2023),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
anoma.to.use.numeric <- anoma.to.use.numeric[1:27]

xlim <- c(1, 27)
ylim1 <- c(-3, 3)

deciles.use.1 <- rep(rangosIGS[[3]][,2], nrow(anoma.to.use))[1:27]
deciles.use.2 <- rep(rangosIGS[[3]][,1], nrow(anoma.to.use))[1:27]

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
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 5)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 5)

axis(side = 2, at = round(-3:3,2), las = 2, cex.axis = 1.3)
mtext(text = "AIGS", side = 2, line = 3.5, cex = 1.3)

box()
#FD 2021 - 2023
anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2021:2023),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))

xlim <- c(1, 27)
ylim1 <- c(-20, 20)

deciles.use.1 <- c(rep(decilesFD[,2], 2), decilesFD[1:3,2])
deciles.use.2 <- c(rep(decilesFD[,1], 2), decilesFD[1:3,1])

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
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 5)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 5)

axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
mtext(text = "AFD", side = 2, line = 3.5, cex = 1.3)

box()

#FC 2021 - 2023
anoma.to.use <- matrixFC[rownames(matrixFC) %in% c(2021:2023),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))

xlim <- c(1, 27)
ylim1 <- c(-0.09, 0.09)

deciles.use.1 <- c(rep(decilesFC[,2], 2), decilesFC[1:3,2]) 
deciles.use.2 <- c(rep(decilesFC[,1], 2), decilesFC[1:3,1])

umbral <- 0

plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.1, 
              rev(deciles.use.1*-1)), 
        col ="grey50", border = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.2, rev(deciles.use.2*-1)), 
        col ="grey70", border = FALSE)

abline(h = round(seq(-0.1,0.1,0.025),2), v = seq(1,36, 12), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", 
      col = "red", lwd = 5)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", 
      col = "blue", lwd = 5)

axis(side = 1, at = 1:27, labels = fecha$label, cex.axis = 1.3, las = 2)
axis(side = 2, at = round(seq(-0.1,0.1,0.025),2), las = 2, cex.axis = 1.3)
mtext(text = "AFC", side = 2, line = 3.5, cex = 1.3)
legend("bottomleft", legend = c("Anomalía positiva", "Anomalía negativa", 
                                "Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
       fill = c("red", "blue", "grey70", "grey50", "white"), cex = 0.8)

box()


mtext(text = "Meses", side = 1, line = 4, cex = 1.3)


dev.copy(png, 
         filename = paste0("figures/Anomalia FD, IGS, FC 2021-2023.png"),
         width = 2500, height = 3000, res = 300)
dev.off()

