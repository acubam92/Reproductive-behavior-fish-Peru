
#FD ----------------------------------------------------------------------

#Anomalia por periodo interanual 95-09 y 10-17
FD <- read.csv(file = "Data/FD_95_21.csv", stringsAsFactors = FALSE, sep = ";")

FD9509 <- FD[FD$year < 2010,]
FDpatron9509 <- tapply(FD9509$fd, list(FD9509$month), mean, na.rm = TRUE)
patronfd9509 <- rep(FDpatron9509, length(sort(unique(FD9509$year))))

FD1017 <- FD[FD$year > 2009 & FD$year < 2018,]
FDpatron1017 <- tapply(FD1017$fd, list(FD1017$month), mean, na.rm = TRUE)
FD1020 <- FD[FD$year > 2009,]
patronfd1017 <- rep(FDpatron1017, (length(sort(unique(FD1020$year)))))

STFD <- ts(data = FD$fd, start = 1995, frequency = 12)

patronesFD <- c(as.numeric(patronfd9509), as.numeric(patronfd1017))
patronesFD2 <- patronesFD[1:length(FD$fd)]
STpatrones <- ts(data = patronesFD2, start = 1995, frequency = 12)

anomaliasFD <- round((STFD - STpatrones), 2)

years <- sort(unique(FD$year))
matrixFD <- matrix(data = c(as.numeric(anomaliasFD), rep(NA, length(patronesFD) - length(STFD))), 
                   nrow = length(years), ncol = 12, byrow = TRUE)

dimnames(matrixFD) <- list(sort(unique(FD$year)), month.abb)

# Rangos FD------------------------------------------------------------------

matrix9517 <- matrixFD[1:23,]

decilesprueba <- NULL

for(i in seq_along(colnames(matrixFD))){
  anoFD96191 <- matrix9517[,i]
  anomalia <- as.data.frame(abs(anoFD96191))
  rangoFD <- round(quantile(anomalia$`abs(anoFD96191)`, probs = c(0.4, 0.8), na.rm = TRUE),1)
  
  decilesprueba <- rbind(decilesprueba, rangoFD)
  
}

row.names(decilesprueba) <- month.abb
write.csv(x = decilesprueba, file = "output/deciles FD_antiguos.csv")


# Fechas ------------------------------------------------------------------

fecha <- read.csv(file = "data/fecha.csv", stringsAsFactors = FALSE, sep = ";")
fecha <- fecha[fecha$year > 2019,]

# Grafico FD --------------------------------------------------------------
#FD 2010 - 2020
mes.actual <- 8 #colocar mes del año
meses.year <- 12 - mes.actual 
length.xlim <- length(matrixFD[rownames(matrixFD) %in% c(2011:2021),]) - meses.year
anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2011:2021),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
anoma.to.use.numeric <- anoma.to.use.numeric[1:length.xlim]

xlim <- c(1, length.xlim)
ylim1 <- c(-20, 20)

repe <- trunc(length.xlim/12)

deciles.use.1 <- c(rep(decilesprueba[,2], repe),decilesprueba[,2][1:mes.actual])
deciles.use.2 <- c(rep(decilesprueba[,1], repe),decilesprueba[,1][1:mes.actual])


par(mar = c(4,4,1,1))
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

axis(side = 1, at = seq(1,repe*12+12, 12), labels = 2011:2021, cex.axis = 1.3)
axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
mtext(text = "Anomalía de FD", side = 2, line = 2.5, cex = 1.3)
mtext(text = "Años", side = 1, line = 2.5, cex = 1.3)
legend("bottomleft", legend = c("Anomalía positiva", "Anomalía negativa", 
                                "Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
       fill = c("red", "blue", "grey70", "grey50", "white"), cex = 0.8)

box()


dev.copy(png, 
         filename = paste0("figures/Anomalia FD.png"),
         width = 2500, height = 1500, res = 300)
dev.off()


#FD 2020 - 2021
  mes.actual <- 8 #colocar mes del año
  meses.year <- 12 - mes.actual 
  length.xlim <- length(matrixFD[rownames(matrixFD) %in% c(2020:2021),]) - meses.year
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2020:2021),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  anoma.to.use.numeric <- anoma.to.use.numeric[1:length.xlim]
  
  xlim <- c(1, length.xlim)
  ylim1 <- c(-20, 20)
  
  repe <- trunc(length.xlim/12)
  
  deciles.use.1 <- c(rep(decilesprueba[,2], repe),decilesprueba[,2][1:mes.actual])
  deciles.use.2 <- c(rep(decilesprueba[,1], repe),decilesprueba[,1][1:mes.actual])
  
  
  par(mar = c(4,4,1,1))
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
  
  axis(side = 1, at = 1:length.xlim, labels = fecha$label, cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  mtext(text = "Anomalía de FD", side = 2, line = 2.5, cex = 1.3)
  mtext(text = "Meses", side = 1, line = 2.5, cex = 1.3)
  legend("bottomleft", legend = c("Anomalía positiva", "Anomalía negativa", 
                                  "Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
         fill = c("red", "blue", "grey70", "grey50", "white"), cex = 0.8)
  
  box()
  
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD 2019-2021.png"),
           width = 3200, height = 2000, res = 300)
  dev.off()
