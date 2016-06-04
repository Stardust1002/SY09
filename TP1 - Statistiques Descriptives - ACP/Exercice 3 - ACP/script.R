theorique = function(){
	tab = matrix(c(3,4,3,1,4,3,2,3,6,4,1,2),nrow=4,byrow=TRUE)
	tab = scale(tab,scale=FALSE)

	V = 1/nrow(tab)*t(tab)%*%tab
	eig = eigen(V)

	p_inertie_explique = 100*eig$values/sum(diag(V))
	cump_inertie_explique = 100*cumsum(eig$values)/sum(diag(V))

	res = tab%*%eig$vectors

	# pdf("premier_plan_factoriel.pdf")
	par(pty="s")
	# plot(tab,xlim=c(-3,3),ylim=c(-3,3),main="Premier plan factoriel des données initiales")
	plot(res,xlim=c(-3,3),ylim=c(-3,3),main="Representation des individus dans le premier plan factoriel",xlab="Axe 1",ylab="Axe 2")
	abline(h=0,v=0,col="black")
	# plot(res[,c(1,3)],xlim=c(-3,3),ylim=c(-3,3),main="Premier plan factoriel de à l'ACP",xlab="Axe 1",ylab="Axe 3")
	text(res, pos = 1, labels = c("Individu 1", "Individu 2", "Individu 3", "Individu 4"))

	require(plotrix)
	correlation = cor(tab,res)
	par(pty="s")
	plot(correlation,xlim=c(-1,1),ylim=c(-1,1),asp=1,ylab="Axe 2",xlab="Axe 1",main="Representation des variables dans le premier plan factoriel")
	draw.circle(0,0,1,border="red")
	abline(h=0,v=0)
	text(correlation, pos = 1, labels = c("Variable 1", "Variable 2", "Variable 3"))

	# dev.off()

	list(tab,V,res,eig,p_inertie_explique,cump_inertie_explique)

	# stargazer(correlation)

	# Pourcentages d'inertie expliquée par chaque axe:
	#  63.977844 29.369721  6.652434
	# Pourcentages d'inertie expliquée par les sous-espaces principaux sont:
	# 63.97784  93.34757 100.00000
	# On en déduit que le nuage est pratiquement dans un espace de dimension 2.
	# Composantes principales: res

	#K=1: res[,1]%*%t(eig$vectors[,1])
	#K=2: res[,1:2]%*%t(eig$vectors[,1:2])
	#K=3: res%*%t(eig$vectors)
	#
	# Lorsque K = 3, cette somme correspond à: C x t(U) = X, soit la matrice initiale.

}

utilisation_outils_R = function(){
	tab <- read.table('notes.txt')
	# tab = scale(tab,scale=FALSE)

	# V = 1/nrow(tab)*t(tab)%*%tab
	# eig = eigen(V)

	# p_inertie_explique = 100*eig$values/sum(diag(V))
	# cump_inertie_explique = 100*cumsum(eig$values)/sum(diag(V))

	# res = tab%*%eig$vectors
	# 
	r <- princomp(tab)
	pdf("utilisation_outils.pdf")

	require(plotrix)
	correlation = cor(scale(tab,scale=FALSE),r$scores)

	par(pty="s")
	plot(correlation,xlim=c(-1,1),ylim=c(-1,1),asp=1,ylab="Axe 2",xlab="Axe 1")
	draw.circle(0,0,1,border="red")
	abline(h=0,v=0)
	text(correlation, pos = 1, labels = c("math", "scie", "fran", "lati", "d-m"))

	par(pty="s")
	plot(correlation[,c(1,3)],xlim=c(-1,1),ylim=c(-1,1),asp=1,ylab="Axe 3",xlab="Axe 1")
	draw.circle(0,0,1,border="red")
	abline(h=0,v=0)
	text(correlation[,c(1,3)], pos = 1, labels = c("math", "scie", "fran", "lati", "d-m"))

	plot(princomp(tab))
	biplot(princomp(tab))

	dev.off()
	correlation

}


traitement_crabs = function(){
	library(MASS)
	data(crabs)
	crabsquant = crabs[,4:8]
	# crabsquant = as.matrix(crabsquant)
	# crabsquant = scale(crabsquant,scale=FALSE)
	# V = 1/nrow(crabsquant)*t(crabsquant)%*%crabsquant
	# eig = eigen(V)
	# p_inertie_explique = eig$values/sum(diag(V))*100
	# cump_inertie_explique = cumsum(eig$values)/sum(diag(V))*100

	# cat("Pourcentages cumulés des inerties expliquées\n")
	# print(cump_inertie_explique)

	# C = crabsquant %*% eig$vectors
	# 
	###### SANS PRETRAITEMENT
	C = princomp(crabsquant)$scores
	id_O_M = which(crabs$sex == "M" & crabs$sp == "O")
	id_O_F = which(crabs$sex == "F" & crabs$sp == "O")
	id_B_M = which(crabs$sex == "M" & crabs$sp == "B")
	id_B_F = which(crabs$sex == "F" & crabs$sp == "B")

	pdf("crabs.pdf")
	# boxplot(C[which(crabs$sex == "M"),], col=c("cyan"),main="Caracteristiques des crabes de sexe M",ylim=c(-10,10))
	# boxplot(C[which(crabs$sex == "F"),], col=c("pink"),main="Caracteristiques des crabes de sexe F",ylim=c(-10,10))
	# boxplot(C[which(crabs$sex == "M" & crabs$sp == "O"),],col=c("green"),main="Caracteristiques des crabes de sexe M et d'espece O",ylim=c(-10,10))
	# boxplot(C[which(crabs$sex == "M" & crabs$sp == "B"),],col=c("green3"),main="Caracteristiques des crabes de sexe M et d'espece B",ylim=c(-10,10))
	# boxplot(C[which(crabs$sex == "F" & crabs$sp == "O"),],col=c("orange"),main="Caracteristiques des crabes de sexe F et d'espece O",ylim=c(-10,10))
	# boxplot(C[which(crabs$sex == "F" & crabs$sp == "B"),],col=c("orange3"),main="Caracteristiques des crabes de sexe F et d'espece B",ylim=c(-10,10))
	
	# pairs(C,col=c('blue','red')[crabs$sp],main="Caracteristiques des especes de crabe B (cyan) et O (rouge)")
	# pairs(C,col=c('green3','brown')[crabs$sex],main="Caracteristiques des crabes de sexe M (vert) et F (marron)")
	# pairs(C[which(crabs$sp=="O"),],col=c('green3','brown')[crabs$sex],main="Caracteristiques des crabes d'espece O de sexe M (vert) et F (marron)")
	# pairs(C[which(crabs$sp=="B"),],col=c('green3','brown')[crabs$sex],main="Caracteristiques des crabes d'espece B de sexe M (vert) et F (marron)")
	# pairs(C[which(crabs$sp=="B"),],col=c('green3','brown')[crabs$sex],main="Caracteristiques des crabes d'espece B de sexe M (vert) et F (marron)")
	
	crabs$category = 0
	crabs$category[id_O_M] = 1
	crabs$category[id_O_F] = 2
	crabs$category[id_B_M] = 3
	crabs$category[id_B_F] = 4
	as.factor(crabs$category)

	pairs(C,col=c('blue','purple','cyan','red')[crabs$category],main="Caracteristiques des crabes de tout sexe et toute espece")
	biplot(princomp(crabsquant))
	plot(C,col=c('blue','purple','cyan','red')[crabs$category],main="Premier plan factoriel de l'A.C.P.")
	# abline(h=0,v=0)

	############# PRETRAITEMENT
	crabsquant = crabsquant / crabsquant[,1]
	C = princomp(crabsquant)$scores
	id_O_M = which(crabs$sex == "M" & crabs$sp == "O")
	id_O_F = which(crabs$sex == "F" & crabs$sp == "O")
	id_B_M = which(crabs$sex == "M" & crabs$sp == "B")
	id_B_F = which(crabs$sex == "F" & crabs$sp == "B")

	# boxplot(C[which(crabs$sex == "M"),], col=c("cyan"),main="Caracteristiques des crabes de sexe M",ylim=c(-10,10))
	# boxplot(C[which(crabs$sex == "F"),], col=c("pink"),main="Caracteristiques des crabes de sexe F",ylim=c(-10,10))
	# boxplot(C[which(crabs$sex == "M" & crabs$sp == "O"),],col=c("green"),main="Caracteristiques des crabes de sexe M et d'espece O",ylim=c(-10,10))
	# boxplot(C[which(crabs$sex == "M" & crabs$sp == "B"),],col=c("green3"),main="Caracteristiques des crabes de sexe M et d'espece B",ylim=c(-10,10))
	# boxplot(C[which(crabs$sex == "F" & crabs$sp == "O"),],col=c("orange"),main="Caracteristiques des crabes de sexe F et d'espece O",ylim=c(-10,10))
	# boxplot(C[which(crabs$sex == "F" & crabs$sp == "B"),],col=c("orange3"),main="Caracteristiques des crabes de sexe F et d'espece B",ylim=c(-10,10))

	# pairs(C,col=c('blue','red')[crabs$sp],main="Caracteristiques des especes de crabe B (cyan) et O (rouge)")
	# pairs(C,col=c('green3','brown')[crabs$sex],main="Caracteristiques des crabes de sexe M (vert) et F (marron)")
	# pairs(C[which(crabs$sp=="O"),],col=c('green3','brown')[crabs$sex],main="Caracteristiques des crabes d'espece O de sexe M (vert) et F (marron)")
	# pairs(C[which(crabs$sp=="B"),],col=c('green3','brown')[crabs$sex],main="Caracteristiques des crabes d'espece B de sexe M (vert) et F (marron)")
	# pairs(C[which(crabs$sp=="B"),],col=c('green3','brown')[crabs$sex],main="Caracteristiques des crabes d'espece B de sexe M (vert) et F (marron)")
	
	crabs$category = 0
	crabs$category[id_O_M] = 1
	crabs$category[id_O_F] = 2
	crabs$category[id_B_M] = 3
	crabs$category[id_B_F] = 4
	as.factor(crabs$category)

	pairs(C[,1:4],col=c('blue','purple','cyan','red')[crabs$category],main="Caracteristiques des crabes de tout sexe et toute espece")
	biplot(princomp(crabsquant))
	plot(C[,1:4],col=c('blue','purple','cyan','red')[crabs$category],main="Premier plan factoriel de l'A.C.P.")
	# abline(h=0,v=0)
	dev.off()

	C
	# r <- princomp(crabsquant)
	# pairs(r$scores,col=c('blue','red')[crabs$sex])
	# indices_B <- which(crabs$sp == "B")
	# indices_O <- which(crabs$sp == "O")
	# pairs(r$scores[indices_O,],col=c('blue','red')[crabs$sex])
	# pairs(r$scores[indices_O,],col=c('blue','red')[crabs$sex])
	# 
	# pairs(r$scores,col=c('blue','red')[crabs$sex])
	# pairs(r$scores,col=c('blue','red')[crabs$sex])

}