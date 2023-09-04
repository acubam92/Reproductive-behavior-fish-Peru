dataATSM <- read.csv("data/ATSM JUL 2020.csv", stringsAsFactors = FALSE)
dataATSM <- dataATSM[-(1:120),c(1,2,11)]
STATSM <- ts(data = dataATSM$NORTE.CENTRO, start = 2010, frequency = 12)

xlim <- c(1, 132)
ylim1 <- c(-5, 5)

par(mar = c(4,4,1,1))
umbral <- 0

plot(1, 1, type = "n", xlim = xlim, ylim = ylim1, xlab = NA, ylab = NA, axes  = FALSE)

abline(h = seq(-5, 5, 1), v = seq(1,132, 12), lty = "dashed", col = "grey50")
par(new=TRUE)
lines(ifelse(as.numeric(STATSM) > 0, as.numeric(STATSM), NA), type = "h", col = "red", lwd = 3)
lines(ifelse(as.numeric(STATSM) < 0, as.numeric(STATSM), NA), type = "h", 
      col = "blue", lwd = 3)

axis(side = 1, at = seq(1,144, 12), labels = c(2010:2021), cex.axis = 1.3)
axis(side = 2, at = seq(-5, 5, 1), las = 2, cex.axis = 1.3)
mtext(text = "ATSM", side = 2, line = 2.5, cex = 1.3)
mtext(text = "Años", side = 1, line = 2.5, cex = 1.3)

box()

dev.copy(png, 
         filename = paste0("figures/ATSM.png"),
         width = 2500, height = 1500, res = 300)
dev.off()

