library("stargazer")
library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]

pdf("crabs.pdf")
boxplot(crabsquant[which(crabs$sex == "M"),], col=c("cyan"),main="Caracteristiques des crabes de sexe M",ylim=c(0, 60))
boxplot(crabsquant[which(crabs$sex == "F"),], col=c("pink"),main="Caracteristiques des crabes de sexe F",ylim=c(0, 60))
boxplot(crabsquant[which(crabs$sex == "M" & crabs$sp == "O"),],col=c("green"),main="Caracteristiques des crabes de sexe M et d'espece O",ylim=c(0, 60))
boxplot(crabsquant[which(crabs$sex == "M" & crabs$sp == "B"),],col=c("green3"),main="Caracteristiques des crabes de sexe M et d'espece B",ylim=c(0, 60))
boxplot(crabsquant[which(crabs$sex == "F" & crabs$sp == "O"),],col=c("orange"),main="Caracteristiques des crabes de sexe F et d'espece O",ylim=c(0, 60))
boxplot(crabsquant[which(crabs$sex == "F" & crabs$sp == "B"),],col=c("orange3"),main="Caracteristiques des crabes de sexe F et d'espece B",ylim=c(0, 60))

pairs(crabsquant,main="Graphique des Paires de Variables")
pairs(crabsquant,col=c('blue','red')[crabs$sp],main="Caracteristiques des especes de crabe B (bleu) et O (rouge)")
pairs(crabsquant,col=c('green3','brown')[crabs$sex],main="Caracteristiques des crabes de sexe M (vert) et F (marron)")
pairs(crabsquant[which(crabs$sp=="O"),],col=c('green3','brown')[crabs$sex],main="Caracteristiques des crabes d'espece O de sexe M (vert) et F (marron)")
pairs(crabsquant[which(crabs$sp=="B"),],col=c('green3','brown')[crabs$sex],main="Caracteristiques des crabes d'espece B de sexe M (vert) et F (marron)")
dev.off()

cat("\nDESCRIPTION DES CRABES DE SEXE M\n")
print(summary(crabs[which(crabs$sex == "M"),])[,4:8])

cat("\nDESCRIPTION DES CRABES DE SEXE F\n")
print(summary(crabs[which(crabs$sex == "F"),])[,4:8])

cat("\nDESCRIPTION DES CRABES DE SEXE M ET D'ESPECE B\n")
print(summary(crabs[which(crabs$sex == "M" & crabs$sp == "B"),])[,4:8])

cat("\nDESCRIPTION DES CRABES DE SEXE M ET D'ESPECE O\n")
print(summary(crabs[which(crabs$sex == "M" & crabs$sp == "O"),])[,4:8])

cat("\nDESCRIPTION DES CRABES DE SEXE F ET D'ESPECE B\n")
print(summary(crabs[which(crabs$sex == "F" & crabs$sp == "B"),])[,4:8])

cat("\nDESCRIPTION DES CRABES DE SEXE F ET D'ESPECE O\n")
print(summary(crabs[which(crabs$sex == "F" & crabs$sp == "O"),])[,4:8])


##########
# Suite à l'analyse visuelle des diagrammes à moustache, il semble exister des différences physiques entre les deux espèces.
# En effet, l'espèce O apparait généralement plus grande que l'espèce B. 
# La différence de taille est très marquée entre l'espèce O et B pour les crabes de sexe F: les valeurs des caracteristiques des crabes O-F sont toutes supérieures à celles des crabes B-F. (FL,RW,CL,CW,BD), permettant de différencier ces deux espèces.
# Cette différence est moins flagrante pour les crabes de sexe M: les valeurs des caractéristiques des crabes O sont légèrements plus grandes mais ne permettraient pas de différencier simplement et rapidement les deux espèces.
# 
# A continuer
# 
# 
# stargazer(cor(crabsquant))

# cov(crabsquant)
# scale(crabsquant, center=TRUE, scale=TRUE)
# 