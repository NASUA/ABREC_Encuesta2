myCSV <- read.csv("data/Encuesta_2020_clean.csv", header = T)
names(myCSV)


myvec <- rownames(table(myCSV$Tiempo.de.paro.antes.de.empleo,myCSV$Ano.de.convocatoria..mayor.grado.))[c(3,4,5,1,2,7)]
myTab <- table(myCSV$Tiempo.de.paro.antes.de.empleo,myCSV$Ano.de.retorno)
myTab <- sweep(myTab, 2, colSums(myTab), FUN = "/")

heatmap(myTab[myvec, ], Colv = NA, Rowv = NA, scale = "row")


boxplot(myCSV$Uso.de.habilidades~myCSV$Cual.es.su.ultimo.nivel.de.estudio.culminado...con.o.sin.beca.Senescyt.)


heatmap(table(myCSV$Tiempo.de.paro.antes.de.empleo, myCSV$Cual.es.su.ultimo.nivel.de.estudio.culminado...con.o.sin.beca.Senescyt.)[myvec,],  Colv = NA, Rowv = NA, scale = "column")

barplot(table(myCSV$Tiempo.de.paro.antes.de.empleo))



par(las = 1)
tab <- table(myCSV$StatusBecario)/sum(table(myCSV$StatusBecario))

barplot(tab[c(1,3,2)],
        col = "orange", xaxt = "n",
        ylim = c(0,1), cex.axis = 2)


#################


increaseStudy <- myCSV$Cual.es.su.ultimo.nivel.de.estudio.culminado...con.o.sin.beca.Senescyt. - (myCSV$NivelEstudioMadre+myCSV$MaximoEstudiosPadre)/2

par(mfrow = c(1,2))
boxplot(increaseStudy~myCSV$El.lugar.donde.vivia.era,
        col = "orange", frame = F, cex.axis = 1.5,
        ylab = "",
        xlab = "")
abline(h = 0, lty = 2)
boxplot(increaseStudy~myCSV$Su.colegio.era, 
        col = "orange", cex.axis = 1.5,
        ylab = "",
        frame = F,
        xlab = "")
abline(h = 0, lty = 2)




dev.off()
barplot(table(myCSV$Si.Senescyt.decidiese.terminar.su.contrato.de.manera.unilateral..estaria.usted.y.su.familia.en.condiciones.de.devolver.los.costos.de.su.beca.), col = "orange")

