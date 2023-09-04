library(WaveletComp)
dataFD <- read.csv(file = "data/FD.csv", stringsAsFactors = FALSE, sep = ";")
dataFD$day <- rep(1, times = length(dataFD$month))
dataFD$fecha <- as.POSIXct(paste0(dataFD$year,"-",dataFD$month, "-", dataFD$day), format = c("%Y-%m-%d"))

analyze.wavelet(my.data = dataFD$fecha, my.series = dataFD$fd, loess.span = 0, dt = 1)

dataFD_kluster <- dataFD[-which(is.na(dataFD$fd)),]
dataFD_kluster <- dataFD_kluster[,-2]
dataFD_kluster_mean <- tapply(dataFD_kluster$fd, list(dataFD_kluster$year), mean)
km.fd <- kmeans(dataFD_kluster_mean, centers = 3)
km.fd$cluster
plot(km.fd$cluster)
data_IG = data.frame(Anio = 1960:2019, IG = c(rnorm(n = 20, mean = 2),
                                              rnorm(n = 20, mean = 2.5),
                                              rnorm(n = 20, mean = 1.5)))
km.res1 = kmeans(data_IG, centers = 3)
km.res1$cluster
