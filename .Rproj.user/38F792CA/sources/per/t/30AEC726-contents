# IGS ----------------------------------------------------------------------
data_IGSquincenal <- read.csv(file = "data/IGS quincenal ene16_mar17.csv", stringsAsFactors = FALSE,
                             sep = ";")
patronesIGS 

meses <- 1:12

patronIGSquincenal <- NULL

for (i in meses) {
  patronIGS_quincena <- rep(patronesIGS[3,i],2)
  
  patronIGSquincenal <- c(patronIGSquincenal, patronIGS_quincena)
  
}


patronquincenal_IGS <- rep(patronIGSquincenal, length(sort(unique(data_IGSquincenal$year))))

STIGS_quincenal <- ts(data = data_IGSquincenal$IGS, start = 2022, frequency = 24) 

STpatronIGSquincenal <- ts(data = (patronquincenal_IGS), start = 2022, frequency = 24)

AIGS_quincenal <- round(STIGS_quincenal- STpatronIGSquincenal, 1)

matrix_IGS_quincenal <- matrix(data = AIGS_quincenal, nrow = length(sort(unique(data_IGSquincenal$year))), 
                               ncol = 24, byrow = TRUE)

dimnames(matrix_IGS_quincenal) <- list(sort(unique(data_IGSquincenal$year)), data_IGSquincenal$month[1:24])

write.csv(x = matrix_IGS_quincenal, file = "output/AIGS quincenal ene 16 a mar 17.csv")

# Rangos ------------------------------------------------------------------
#rangos son extraidos del code Anomalias IGS_Final
rangoIGS_quincena <- NULL

for (i in meses) {
  rangosIGS_quincenca_decilcUatro <- rep(rangosIGS[[3]][i,1],2)
  rangosIGS_quincenca_decilocho <- rep(rangosIGS[[3]][i,2],2)
  
  rangoIGS_quincena <- rbind(rangoIGS_quincena, cbind(rangosIGS_quincenca_decilcUatro, rangosIGS_quincenca_decilocho))
}

colnames(rangoIGS_quincena) <- c("decil4", "decil8")

# Grafico AFC quincena ----------------------------------------------------

#AIGS 2022 - 2023 quincena
anoma.to.use <- matrix_IGS_quincenal[rownames(matrix_IGS_quincenal) %in% c(2016:2017),]
anoma.to.use.numeric <- as.numeric(t(anoma.to.use))

xlim <- c(1, 30)
ylim1 <- c(-3, 3)

deciles.use.1 <- c(rep(rangoIGS_quincena[,2], 1), rangoIGS_quincena[1:6,2]) 
deciles.use.2 <- c(rep(rangoIGS_quincena[,1], 1), rangoIGS_quincena[1:6,1])


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

abline(h = seq(-3,3,1), v = seq(1,36, 24), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(anoma.to.use.numeric > 0, anoma.to.use.numeric, NA), type = "h", 
      col = "red", lwd = 5)
lines(ifelse(anoma.to.use.numeric < 0, anoma.to.use.numeric, NA), type = "h", 
      col = "blue", lwd = 5)

axis(side = 1, at = xlim[1]:xlim[2], labels = data_IGSquincenal$Labels[1:30], 
     cex.axis = 1.3, las = 2)
axis(side = 2, at = -3:3, las = 2, cex.axis = 1.3)
mtext(text = "AIGS", side = 2, line = 2.5, cex = 1.3)
mtext(text = "Meses", side = 1, line = 4.5, cex = 1.3)
legend("bottomleft", legend = c("Anomalía positiva", "Anomalía negativa", 
                                "Sin efecto", "Efecto moderado", "Efecto Fuerte"), 
       fill = c("red", "blue", "grey70", "grey50", "white"), cex = 0.8)

box()


dev.copy(png, 
         filename = paste0("figures/Anomalia IGS quincenal 2016-2017"),
         width = 2500, height = 1500, res = 300)
dev.off()

