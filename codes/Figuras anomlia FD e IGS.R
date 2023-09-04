#FD 2020 - 2021
anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2020:2021),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))

xlim <- c(1, 24)
ylim1 <- c(-20, 20)

deciles.use.1 <- rep(decilesFD[,2], 2)
deciles.use.2 <- rep(decilesFD[,1], 2)


par(mar = c(0,0,0,0), mfrow = c(2,1), oma = c(4,4,1,1))
#par(mar = c(4,4,1,1))
umbral <- 0

plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.1, 
              rev(deciles.use.1*-1)), 
        col ="grey50", border = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.2, rev(deciles.use.2*-1)), 
        col ="grey70", border = FALSE)

abline(h = seq(-30, 30, 5), v = seq(1,length.xlim, 12), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)

axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
mtext(text = "Anomalía de FD", side = 2, line = 2.5, cex = 1.3)
# legend("top", legend = c("Anomalía positiva", "Anomalía negativa"), 
#        col = c("red", "blue"), cex = 0.7, lwd = 4, horiz = TRUE)
# legend("top", legend = c("Anomalía positiva", "Anomalía negativa", 
#                                 "Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
#        fill = c("red", "blue", "grey70", "grey50", "white"), cex = 1, horiz = TRUE,
#        bty = "n")

box()

#IGS 2020 - 2021
anoma.to.use <- matrixIGS[rownames(matrixIGS) %in% c(2020:2021),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))

xlim <- c(1, 24)
ylim1 <- c(-3, 3)

deciles.use.1 <- rep(decilesIGS[,2], 2)
deciles.use.2 <- rep(decilesIGS[,1], 2)


umbral <- 0

plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.1, 
              rev(deciles.use.1*-1)), 
        col ="grey50", border = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.2, rev(deciles.use.2*-1)), 
        col ="grey70", border = FALSE)

abline(h = -3:3, v = seq(1,24, 12), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)

axis(side = 1, at = 1:24, labels = fecha$label, cex.axis = 1.3)
axis(side = 2, at = -3:3, las = 2, cex.axis = 1.3)
mtext(text = "Anomalía de IGS", side = 2, line = 2.5, cex = 1.3)
mtext(text = "Meses", side = 1, line = 2.5, cex = 1.3)
# legend("bottomright", legend = c("Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
#        fill = c("grey70", "grey50", "white"), cex = 0.7, horiz = TRUE)

box()


dev.copy(png, 
         filename = paste0("figures/Anomalia FD_IGs 2020-2021.png"),
         width = 2500, height = 2500, res = 300)
dev.off()
