
#FD ----------------------------------------------------------------------

#Anomalia por periodo interanual 95-09 y 10-17
FD <- read.csv(file = "Data/FD.csv", stringsAsFactors = FALSE, sep = ";", check.names = FALSE)
patronesFD <- read.csv(file = "data/patrones FD.csv", stringsAsFactors = FALSE, sep = ";", 
                       row.names = 1)

#FD por periodos
FD_92_99 <- FD[FD$year < 1999,]
FD_00_11 <- FD[FD$year < 2011 & FD$year > 1998,]
FD_12_21 <- FD[FD$year > 2010,]

#patrones FD por periodos
patronFD_92_99 <- rep(as.numeric(patronesFD[1,]), length(sort(unique(FD_92_99$year))))
patronFD_00_11 <- rep(as.numeric(patronesFD[2,]), length(sort(unique(FD_00_11$year))))
patronFD_12_21 <- rep(as.numeric(patronesFD[3,]), length(sort(unique(FD_12_21$year))))

#serie de tiempo FD
STFD <- ts(data = FD$fd, start = 1992, frequency = 12)

#serie de tiempo FD patrones
rep_patronesFD <- c(patronFD_92_99, patronFD_00_11, patronFD_12_21)
STpatronesFD <- ts(data = rep_patronesFD, start = 1992, frequency = 12)

#Calculo de anomalías
anomaliasFD <- round((STFD - STpatronesFD), 2)

#matrix de FD
years_FD <- sort(unique(FD$year))
matrixFD <- matrix(data = c(as.numeric(anomaliasFD), rep(NA, length(rep_patronesFD) - length(STFD))), 
                   nrow = length(years_FD), ncol = 12, byrow = TRUE)

dimnames(matrixFD) <- list(sort(unique(FD$year)), month.abb)

# Rangos FD------------------------------------------------------------------

matrixFD_92_20 <- matrixFD[-30,]
matrixFD_92_98 <- matrixFD_92_20[rownames(matrixFD_92_20) %in% c(1992:1998),]
matrixFD_99_10 <- matrixFD_92_20[rownames(matrixFD_92_20) %in% c(1999:2010),]
matrixFD_11_20 <- matrixFD_92_20[rownames(matrixFD_92_20) %in% c(2011:2020),]
listFD <- list(matrixFD_92_98, matrixFD_99_10, matrixFD_11_20)

rangosFD <- list()

for(i in 1:3){
  anomaliaFD_year <- listFD[[i]]
  anomaliaFD <- as.data.frame(abs(anomaliaFD_year))
  decilesFD <- NULL
  for (j in 1:12) {
    rangoFD <- round(quantile(anomaliaFD[,j], probs = c(0.4, 0.8), na.rm = TRUE),1)
    decilesFD <- rbind(decilesFD, rangoFD)
    
  }
  rangosFD[[i]] <- decilesFD
  row.names(decilesFD) <- month.abb
  write.csv(x = decilesFD, file = paste0("output/deciles ", i, " periodo.csv"))
}



# Fechas ------------------------------------------------------------------

fecha <- read.csv(file = "data/fecha.csv", stringsAsFactors = FALSE, sep = ";")
fecha <- fecha[349:372,]

# Grafico FD --------------------------------------------------------------
#FD 2011 - 2021
# mes.actual <- 12 #colocar mes del año
# meses.year <- 12 - mes.actual 
# length.xlim <- length(matrixFD[rownames(matrixFD) %in% c(2012:2021),]) - meses.year
# anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2012:2021),]
# anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
# anoma.to.use.numeric <- anoma.to.use.numeric[1:length.xlim]

xlim <- c(1, 120)
ylim1 <- c(-20, 20)

# repe <- trunc(length.xlim/12)

deciles.use.1 <- rep(decilesFD[,2], 10)
deciles.use.2 <- rep(decilesFD[,1], 10)


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

abline(h = seq(-30, 30, 5), v = seq(1,10, 12), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)

axis(side = 1, at = seq(1,120, 12), labels = 2012:2021, cex.axis = 1.3)
axis(side = 2, at = seq(-20, 20, 5), las = 2, cex.axis = 1.3)
mtext(text = "Anomalía de FD", side = 2, line = 2.5, cex = 1.3)
mtext(text = "Años", side = 1, line = 2.5, cex = 1.3)
legend("bottomleft", legend = c("Anomalía positiva", "Anomalía negativa", 
                                "Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
       fill = c("red", "blue", "grey70", "grey50", "white"), cex = 0.8)

box()


dev.copy(png, 
         filename = paste0("figures/Anomalia FD_final.png"),
         width = 2500, height = 1500, res = 300)
dev.off()

# par(mfrow = c(3,1), mar = c(1,6,1,1), oma = c(4,1,1,1))
#FD 2021 - 2023
anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2021:2022),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))

xlim <- c(1, length(anoma.to.use.numeric))
ylim1 <- c(-20, 20)
  

# deciles.use.1 <- c(rep(decilesFD[,2], 2), decilesFD[1:3,2])
# deciles.use.2 <- c(rep(decilesFD[,1], 2), decilesFD[1:3,1])
  
deciles.use.1 <- c(rep(decilesFD[,2], 2))
deciles.use.2 <- c(rep(decilesFD[,1], 2))

par(mar = c(5,4,1,1))
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
  mtext(text = "Meses", side = 1, line = 3, cex = 1.3)
  legend("bottomleft", legend = c("Anomalía positiva", "Anomalía negativa", 
                                  "Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
         fill = c("red", "blue", "grey70", "grey50", "white"), cex = 0.8)
  
  box()
  
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD 2017_2018.png"),
           width = 2500, height = 1500, res = 300)
  dev.off()

  

# Grafico FD 1992-2020 ----------------------------------------------------
  periodo1_FD <- 1992:1998
  periodo2_FD <- 1999:2010
  periodo3_FD <- 2011:2020
  periodo_FD <- list(periodo1_FD, periodo2_FD, periodo3_FD)
  
  par(mar = c(1.5,1.5,1,1), mfrow = c(3,1), oma = c(3,3,1,1))
  for (i in 1:3) {
    anoma.to.use <- matrixFD[rownames(matrixFD) %in% periodo_FD[[i]],]
    anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
    
    xlim <- c(1, length(anoma.to.use.numeric))
    ylim1 <- c(-20, 20)
    
    rangos_use <- rangosFD[[i]]
    
    deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
    deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
    
    
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
    
    abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
    par(new=TRUE)
    lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
    lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
    
    axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = periodo_FD[[i]], 
         cex.axis = 1.3)
    axis(side = 2, at = seq(-20, 20, 5), las = 2, cex.axis = 1.3)
     # legend("bottomleft", legend = c("Anomalía positiva", "Anomalía negativa", 
    #                                 "Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
    #        fill = c("red", "blue", "grey70", "grey50", "white"), cex = 0.8)
    # 
    box()
   
  }
  
  mtext(text = "SF anomalies", side = 2, line = 1, cex = 1.3, outer = TRUE)
  mtext(text = "Years", side = 1, line = 1, cex = 1.3, outer = TRUE)
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD 1992-2020.png"),
           width = 3200, height = 2500, res = 300)
  dev.off() 
  

# Gráfico del 1992 a 1998 -------------------------------------------------
  par(mar = c(1.5,1,1,1), mfrow = c(3,1), oma = c(3,4.5,1,1))
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% 1992:1998,]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[1]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
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
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = periodo_FD[[1]], 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  # mtext(text = "Años", side = 1, line = 3, cex = 1.3)
  # mtext(text = "Anomalías de FD (%)", side = 2, line = 3, cex = 1.3)
  # legend("bottom", legend = c("Anomalía positiva", "Anomalía negativa",
  #                                 "Sin efecto", "Efecto moderado", "Efecto Fuerte"),
  #        fill = c("red", "blue", "grey70", "grey50", "white"), cex = 0.8, hor = TRUE)

  box()
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD 1992-1998.png"),
           width = 3200, height = 2000, res = 300)
  dev.off() 
  
# Gráfico del 1999 a 2010 -------------------------------------------------
  
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% 1999:2010,]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[2]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
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
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = 1999:2010, 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  # mtext(text = "Años", side = 1, line = 3, cex = 1.3)
  # mtext(text = "Anomalía de FD", side = 2, line = 3, cex = 1.3)
  # # legend("bottom", legend = c("Anomalía positiva", "Anomalía negativa",
  #                                 "Sin efecto", "Efecto moderado", "Efecto Fuerte"),
  #        fill = c("red", "blue", "grey70", "grey50", "white"), cex = 0.8, hor = TRUE)
  
  box()
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD 2005-2010.png"),
           width = 3200, height = 2000, res = 300)
  dev.off() 
  
# Gráfico del 2011 a 2020 -------------------------------------------------
  
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% 2011:2020,]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[3]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
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
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = 2011:2020, 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  mtext(text = "Años", side = 1, line = 2, cex = 1.3, outer = TRUE)
  mtext(text = "Anomalía de FD", side = 2, line = 3, cex = 1.3, outer = TRUE)
  # legend("bottom", legend = c("Anomalía positiva", "Anomalía negativa",
  #                                 "Sin efecto", "Efecto moderado", "Efecto Fuerte"),
  #        fill = c("red", "blue", "grey70", "grey50", "white"), cex = 0.8, hor = TRUE)
  
  box()
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD 1992-2020.png"),
           width = 3200, height = 2500, res = 300)
  dev.off() 
  
# Niño debil --------------------------------------------------------------
#Primer periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(1994,1995),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[1]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  par(mar = c(2,1,1,1), mfrow = c(3,1), oma = c(3,4.5,1,1))
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(1994,1995), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  box()

  #Segundo periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2002,2006,2007,2008,2009,2010),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[2]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(2002,2006,2007,2008,2009,2010), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  box()
  
  #Tercer periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2012,2014,2018,2019),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[3]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(2012,2014,2018,2019), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  
  box()
  
  mtext(text = "Years", side = 1, line = 1.5, cex = 1.3, outer = TRUE)
  mtext(text = "SF anomalies", side = 2, line = 3, cex = 1.3, outer = TRUE)
  
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD niño debil.png"),
           width = 3200, height = 2500, res = 300)
  dev.off() 
  
  

# Niño moderado -----------------------------------------------------------
  #Primer periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(1993,1994),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[1]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  par(mar = c(2,1,1,1), mfrow = c(2,1), oma = c(3,4.5,1,1))
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(1993,1994), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  box()
  
  #Tercer periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2016,2017),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[3]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(2016,2017), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  
  box()
  
  mtext(text = "Years", side = 1, line = 1.5, cex = 1.3, outer = TRUE)
  mtext(text = "SF anomalies", side = 2, line = 3, cex = 1.3, outer = TRUE)
  
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD niño moderado.png"),
           width = 3200, height = 2500, res = 300)
  dev.off() 
  
  
  
# Niño fuerte -------------------------------------------------------------
  #Primer periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(1997,1998),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[1]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  par(mar = c(2,1,1,1), mfrow = c(2,1), oma = c(3,4.5,1,1))
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(1997,1998), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  box()
  
  #Tercer periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2015,2016),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[3]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(2015,2016), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  
  box()
  
  mtext(text = "Years", side = 1, line = 1.5, cex = 1.3, outer = TRUE)
  mtext(text = "SF anomalies", side = 2, line = 3, cex = 1.3, outer = TRUE)
  
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD niño fuerte.png"),
           width = 3200, height = 2500, res = 300)
  dev.off() 
  
  
  
# Niña debil --------------------------------------------------------------
  #Segundo periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2001,2002),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[2]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  par(mar = c(2,1,1,1), mfrow = c(2,1), oma = c(3,4.5,1,1))
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(2001,2002), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  box()
  
  #Tercer periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2017,2018),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[3]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(2017,2018), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  
  box()
  
  mtext(text = "Years", side = 1, line = 1.5, cex = 1.3, outer = TRUE)
  mtext(text = "SF anomalies", side = 2, line = 3, cex = 1.3, outer = TRUE)
  
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD niña debil.png"),
           width = 3200, height = 2500, res = 300)
  dev.off() 
  
  
# Niña moderada -----------------------------------------------------------
  #Primer periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(1996,1997),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[1]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  par(mar = c(2,1,1,1), mfrow = c(2,1), oma = c(3,4.5,1,1))
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(1996,1997), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  box()
  
  #Segundo periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2007,2008,2010,2011),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[2]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(2007,2008,2010,2011), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  
  box()
  
  mtext(text = "Years", side = 1, line = 1.5, cex = 1.3, outer = TRUE)
  mtext(text = "SF anomalies", side = 2, line = 3, cex = 1.3, outer = TRUE)
  
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD niña moderado.png"),
           width = 3200, height = 2500, res = 300)
  dev.off()   
  

# Niña fuerte -------------------------------------------------------------
  #Tercer periodo
  anoma.to.use <- matrixFD[rownames(matrixFD) %in% c(2013,2014),]
  anoma.to.use.numeric <- as.numeric(t(anoma.to.use))
  
  xlim <- c(1, length(anoma.to.use.numeric))
  ylim1 <- c(-30, 30)
  
  rangos_use <- rangosFD[[3]]
  
  deciles.use.1 <- rep(rangos_use[,2], length(anoma.to.use.numeric)/12)
  deciles.use.2 <- rep(rangos_use[,1], length(anoma.to.use.numeric)/12)
  
  par(mar = c(3,4.5,1,1))
  umbral <- 0
  
  plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.1, 
                rev(deciles.use.1*-1)), 
          col ="grey50", border = FALSE)
  polygon(x = c(1:xlim[2], xlim[2]:1), 
          y = c(deciles.use.2, rev(deciles.use.2*-1)), 
          col ="grey70", border = FALSE)
  
  abline(h = seq(-30, 30, 5), v = seq(1,length(anoma.to.use.numeric), 12), lty = "dashed", col = "grey50")
  par(new=TRUE)
  lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", col = "red", lwd = 3)
  lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", col = "blue", lwd = 3)
  
  axis(side = 1, at = seq(1,length(anoma.to.use.numeric), 12), labels = c(2013,2014), 
       cex.axis = 1.3)
  axis(side = 2, at = seq(-30, 30, 5), las = 2, cex.axis = 1.3)
  box()
  mtext(text = "Years", side = 1, line = 2, cex = 1.3)
  mtext(text = "SF anomalies", side = 2, line = 3, cex = 1.3)
  
  dev.copy(png, 
           filename = paste0("figures/Anomalia FD niña fuerte.png"),
           width = 3200, height = 2500, res = 300)
  dev.off() 
  
  
  