
#FC ----------------------------------------------------------------------

#Anomalia por periodo interanual 
FC <- read.csv(file = "Data/FC DE ANCHOVETA 1986-2023.csv", stringsAsFactors = FALSE, check.names = FALSE, sep = ";")
FC <- FC[FC$Year > 1991,]
patronesFC <- read.csv(file = "data/patrones FC.csv", stringsAsFactors = FALSE, row.names = 1)

#FC por periodos
FC_92_00 <- FC[FC$Year < 2001,]
FC_01_09 <- FC[FC$Year < 2010 & FC$Year > 2000,]
FC_10_22 <- FC[FC$Year > 2009,]

#patrones FC por periodos
patronFC92_00 <- rep(as.numeric(patronesFC[1,]), length(sort(unique(FC_92_00$Year))))
patronFC01_09 <- rep(as.numeric(patronesFC[2,]), length(sort(unique(FC_01_09$Year))))
patronFC10_22 <- rep(as.numeric(patronesFC[3,]), length(sort(unique(FC_10_22$Year))))

#serie de tiempo FC
STFC <- ts(data = FC$FC, start = 1992, frequency = 12)

#serie de tiempo FC patrones
rep_patronesFC <- c(patronFC92_00, patronFC01_09, patronFC10_22)
STpatronesFC <- ts(data = rep_patronesFC, start = 1992, frequency = 12)

#Calculo de anomalías
anomaliasFC<- round((STFC - STpatronesFC), 4)
write.csv(x = anomaliasFC, file = "output/AFC 200323.csv")

#matrix de FC
years_FC <- sort(unique(FC$Year))
matrixFC <- matrix(data = c(as.numeric(anomaliasFC), rep(NA, length(rep_patronesFC) - length(STFC))), 
                    nrow = length(years_FC), ncol = 12, byrow = TRUE)

dimnames(matrixFC) <- list(sort(unique(FC$Year)), month.abb)

# Rangos FC------------------------------------------------------------------

matrixFC_92_99 <- matrixFC[rownames(matrixFC) %in% c(1992:1999),]
matrixFC_00_10 <- matrixFC[rownames(matrixFC) %in% c(2000:2009),]
matrixFC_11_22 <- matrixFC[rownames(matrixFC) %in% c(2010:2022),]
listFC <- list(matrixFC_92_99, matrixFC_00_10, matrixFC_11_22)

rangosFC <- list()

for(i in 1:3){
  anomaliaFC_year <- listFC[[i]]
  anomaliaFC <- as.data.frame(abs(anomaliaFC_year))
  decilesFC <- NULL
  for (j in 1:12) {
    rangoFC <- round(quantile(anomaliaFC[,j], probs = c(0.5, 0.9), na.rm = TRUE),4)
    decilesFC <- rbind(decilesFC, rangoFC)
    
  }
  rangosFC[[i]] <- decilesFC
  row.names(decilesFC) <- month.abb
  write.csv(x = decilesFC, file = paste0("output/deciles FC ", i, " periodo.csv"))
}

# Fechas ------------------------------------------------------------------

fecha <- read.csv(file = "data/fecha.csv", stringsAsFactors = FALSE, sep = ";")
fecha <- fecha[fecha$year > 2020,]

# Grafico FC --------------------------------------------------------------
#FC 2010 - 2022
anoma.to.use <- matrixFC[rownames(matrixFC) %in% c(2011:2023),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))

xlim <- c(1, length(anoma.to.use.numeric))
ylim1 <- c(-0.1, 0.1)

deciles.use.1 <- rep(rangosFC[[3]][,2], nrow(anoma.to.use), length.out = length(anoma.to.use))
deciles.use.2 <- rep(rangosFC[[3]][,1], nrow(anoma.to.use))


par(mar = c(4,6,1,1))
umbral <- 0

plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.1, 
              rev(deciles.use.1*-1)), 
        col ="grey50", border = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.2, rev(deciles.use.2*-1)), 
        col ="grey70", border = FALSE)

abline(h = seq(-0.2,0.2,0.05), v = seq(1,156, 12), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)

axis(side = 1, at = seq(1,156, 12), labels = 2011:2023, cex.axis = 1.3)
axis(side = 2, at = seq(-0.2,0.2,0.05), las = 2, cex.axis = 1.3)
mtext(text = "Anomalía de FC", side = 2, line = 4.5, cex = 1.3)
mtext(text = "Años", side = 1, line = 2.5, cex = 1.3)
legend(x = 1, y = -0.07, legend = c("Anomalía positiva", "Anomalía negativa"), 
       fill = c("blue", "red"), cex = 0.8, bty = "n")
legend(x = 30, y = -0.07, legend = c("Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
       fill = c("grey70", "grey50", "white"), cex = 0.8, bty = "n")

box()


dev.copy(png, 
         filename = paste0("figures/Anomalia FC_2011_2023.png"),
         width = 2500, height = 1500, res = 300)
dev.off()


#FC 2021 - 2023
anoma.to.use <- matrixFC[rownames(matrixFC) %in% c(2021:2023),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))

xlim <- c(1, length(anoma.to.use.numeric))
ylim1 <- c(-0.1, 0.1)

deciles.use.1 <- rep(decilesFC[,2], nrow(anoma.to.use), length.out = length(anoma.to.use.numeric))
deciles.use.2 <- rep(decilesFC[,1], nrow(anoma.to.use), length.out = length(anoma.to.use.numeric))

par(mar = c(4,6,1,1))
umbral <- 0

plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.1, 
              rev(deciles.use.1*-1)), 
        col ="grey50", border = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.2, rev(deciles.use.2*-1)), 
        col ="grey70", border = FALSE)

abline(h = seq(-0.1,0.1,0.025), v = seq(1,36, 12), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", 
      col = "red", lwd = 5)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", 
      col = "blue", lwd = 5)

axis(side = 1, at = 1:length(anoma.to.use.numeric), labels = fecha$label, cex.axis = 1.3, las = 2)
axis(side = 2, at = seq(-0.2,0.2,0.05), las = 2, cex.axis = 1.3)
mtext(text = "AFC", side = 2, line = 4.5, cex = 1.3)
mtext(text = "Meses", side = 1, line = 3, cex = 1.3)
legend(x = 1, y = -0.07, legend = c("Anomalía positiva", "Anomalía negativa"), 
       fill = c("red", "blue"), cex = 0.8, bty = "n")
legend(x = 10, y = -0.07, legend = c("Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
       fill = c("grey70", "grey50", "white"), cex = 0.8, bty = "n")

box()


dev.copy(png, 
         filename = paste0("figures/Anomalia FC 2020-2023.png"),
         width = 3200, height = 2000, res = 300)
dev.off()
