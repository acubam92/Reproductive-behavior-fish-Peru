# FD ----------------------------------------------------------------------
data_FDquincenal <- read.csv(file = "data/FD quincenal ene22_mar23.csv", stringsAsFactors = FALSE,
                             sep = ";")
patronesFD 

meses <- 1:12

patronFDquincenal <- NULL

for (i in meses) {
  patronFD_quincena <- rep(patronesFD[3,i],2)
  
  patronFDquincenal <- c(patronFDquincenal, patronFD_quincena)
  
}


patronquincenal_FD <- rep(patronFDquincenal, length(sort(unique(data_FDquincenal$year))))

STFD_quincenal <- ts(data = data_FDquincenal$FD, start = 2022, frequency = 24) 

STpatronFDquincenal <- ts(data = (patronquincenal_FD), start = 2022, frequency = 24)

AFD_quincenal <- round(STFD_quincenal- STpatronFDquincenal, 1)

matrix_AFD_quincenal <- matrix(data = AFD_quincenal, nrow = length(sort(unique(data_FDquincenal$year))), 
                               ncol = 24, byrow = TRUE)

dimnames(matrix_AFD_quincenal) <- list(sort(unique(data_FDquincenal$year)), data_FDquincenal$month[1:24])

write.csv(x = matrix_AFD_quincenal, file = "output/AFD quincenal ene 22 a mar 23.csv")

# Rangos ------------------------------------------------------------------
#rangos son extraidos del code Anomalias FD_Final
rangoFD_quincena <- NULL

for (i in meses) {
  rangosFD_quincenca_decilcUatro <- rep(rangosFD[[3]][i,1],2)
  rangosFD_quincenca_decilocho <- rep(rangosFD[[3]][i,2],2)
  
  rangoFD_quincena <- rbind(rangoFD_quincena, cbind(rangosFD_quincenca_decilcUatro, rangosFD_quincenca_decilocho))
}

colnames(rangoFD_quincena) <- c("decil4", "decil8")


# Grafico AFC quincena ----------------------------------------------------

#AIGS 2022 - 2023 quincena
anoma.to.use <- matrix_AFD_quincenal[rownames(matrix_AFD_quincenal) %in% c(2022:2023),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))

xlim <- c(1, 30)
ylim1 <- c(-20, 20)

deciles.use.1 <- c(rep(rangoFD_quincena[,2], 1), rangoFD_quincena[1:6,2]) 
deciles.use.2 <- c(rep(rangoFD_quincena[,1], 1), rangoFD_quincena[1:6,1])


par(mar = c(6,4,1,1))
umbral <- 0

plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.1, 
              rev(deciles.use.1*-1)), 
        col ="grey50", border = FALSE)
polygon(x = c(1:xlim[2], xlim[2]:1), 
        y = c(deciles.use.2, rev(deciles.use.2*-1)), 
        col ="grey70", border = FALSE)

abline(h = seq(-20,20,5), v = seq(1,36, 24), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", 
      col = "red", lwd = 5)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", 
      col = "blue", lwd = 5)

axis(side = 1, at = xlim[1]:xlim[2], labels = data_FDquincenal$Labels[1:30], 
     cex.axis = 1.3, las = 2)
axis(side = 2, at = seq(-20,20,5), las = 2, cex.axis = 1.3)
mtext(text = "AFD", side = 2, line = 2.5, cex = 1.3)
mtext(text = "Meses", side = 1, line = 4.5, cex = 1.3)
legend("bottomleft", legend = c("Anomalía positiva", "Anomalía negativa", 
                                "Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
       fill = c("red", "blue", "grey70", "grey50", "white"), cex = 0.8)

box()


dev.copy(png, 
         filename = paste0("figures/Anomalia FD quincenal 2022-2023.png"),
         width = 2500, height = 1500, res = 300)
dev.off()

