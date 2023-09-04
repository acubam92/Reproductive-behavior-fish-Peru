rangosFD

resumen_rangosFD_1 <- cbind(rbind(var(rangosFD[[1]][,1]), var(rangosFD[[1]][,2])), 
                            colMeans(x = rangosFD[[1]]))
resumen_rangosFD_2 <- cbind(rbind(var(rangosFD[[2]][,1]), var(rangosFD[[2]][,2])), 
                            colMeans(x = rangosFD[[2]]))
resumen_rangosFD_3 <- cbind(rbind(var(rangosFD[[3]][,1]), var(rangosFD[[3]][,2])), 
                            colMeans(x = rangosFD[[3]]))
resumen_rangosFD <- rbind(resumen_rangosFD_1, resumen_rangosFD_2, resumen_rangosFD_3)

write.csv(x = resumen_rangosFD, file = "output/resumen rangos FD.csv")

rangosIGS

resumen_rangosIGS_1 <- cbind(rbind(var(rangosIGS[[1]][,1]), var(rangosIGS[[1]][,2])), 
                            colMeans(x = rangosIGS[[1]]))
resumen_rangosIGS_2 <- cbind(rbind(var(rangosIGS[[2]][,1]), var(rangosIGS[[2]][,2])), 
                            colMeans(x = rangosIGS[[2]]))
resumen_rangosIGS_3 <- cbind(rbind(var(rangosIGS[[3]][,1]), var(rangosIGS[[3]][,2])), 
                            colMeans(x = rangosIGS[[3]]))
resumen_rangosIGS <- rbind(resumen_rangosIGS_1, resumen_rangosIGS_2, resumen_rangosIGS_3)

write.csv(x = resumen_rangosIGS, file = "output/resumen rangos IGS.csv")
