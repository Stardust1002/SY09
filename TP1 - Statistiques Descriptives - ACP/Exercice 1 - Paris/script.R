######
#
matches = unique(books.sel[c('winner','loser','match_uid')])

### ATTENTION
### NE PAS SE FIER AUX LEVELS QUI COMPTENT LES ENTREES SUPPRIMEES DANS LE PRETRAITEMENT
# Nombre de Matchs:
# length(unique(books.sel$match_uid))
# length(unique(matches$match_uid))
# 25993
# 
# #length(levels(matches$match_uid))
# #26532
# 

# Nombre de joueurs:
# length(unique(abind(books.sel$winner, books.sel$loser, along=1)))
# OR
# length(union(books.sel$winner,books.sel$loser))
# 1523
# Prouvé par la suite avec la fonction playersStats
# 
# # length(levels(matches$winner))
# # length(levels(matches$loser))
# # 1527
# 
# P??riode de temps
# range(books.sel$year)
# [1] 2OO9 2015

# Quels sont les diff??rents matchbookers ?
# unique(books.sel$book)
# [1] B D C F A G E
# levels(books.sel$book)
# A B C D E F G

# Nombre de matchs gagn??s et perdus par joueur:

playersStats = function(X){
	matches = unique(X[c('winner','loser','match_uid')])
	players = unique(union(matches$winner,matches$loser))
	result = data.frame(
		player_uid = factor(levels(matches$winner),labels=levels(matches$winner)),
		win = table(matches$winner),
		loss = table(matches$loser)
		)
	### ON RETIRE LES JOUEURS ENLEVES DANS LE PRETRAITEMENT MAIS TOUJOURS PRESENTS DANS TABLE (utilisant levels)
	result = result[which(result$player_uid %in% players),]
	### ON SELECTIONNE LES COLONNES QUI NOUS IMPORTENT
	result = result[c('player_uid','win.Freq','loss.Freq')]
	### ON CALCULE LE RATIO
	result$ratio = result$win / (result$win+result$loss)
	result$ratio[which(is.nan(result$ratio))] = 0
	result$positive = as.factor(result$ratio >= 0.5)

	### ON TRIE PAR ORDRE DECROISSANT DES RATIO
	result = result[order(result$ratio,na.last=TRUE,decreasing=TRUE),] 
	pdf("players_levels.pdf")
	plot(result$loss,result$win,pch=21,col=c("red","green")[result$positive],main="Representation du Niveau des Joueurs",xlab="Nombre de Defaites",ylab="Nombre de Victoires")
	abline(a=1,b=1,col="black",lty=2)
	text(mean(183),(183),"Ratio = 0.5",srt=16,pos=3)
	dev.off()
	result

}


#### QUESTION 3
#
#
#
matchesSuspects = function(X){

	matches = data.frame(
		match_uid = as.factor(X$match_uid),
		book = X$book,
		winner = X$winner,
		loser = X$loser,
		match_book_uid = X$match_book_uid,
		#evolution_winner = X$implied_prob_winner_open - X$implied_prob_winner_close,
		#evolution_loser = X$implied_prob_loser_open - X$implied_prob_loser_close,
		evolution_winner = X$implied_prob_winner_close - X$implied_prob_winner_open,
		evolution_loser = X$implied_prob_loser_close - X$implied_prob_loser_open,
		evolution = abs(X$implied_prob_winner_close - X$implied_prob_winner_open),
		suspect = FALSE
	)
	matches$suspect[which(matches$evolution > 0.1)] = TRUE
	
	# matches suspects & bookmakers impliqués:
	matches_suspects = matches[which(matches$suspect == TRUE),]
	bookmakers_suspects = table(matches_suspects$book)
	bookmakers_ratio = bookmakers_suspects / table(matches$book) *100
	# winners & losers suspectés d'être associés à des malversations:
	winners_suspects = unique(matches_suspects$winner[which(matches_suspects$evolution_loser > 0.1)])
	losers_suspects = matches_suspects$loser[which(matches_suspects$evolution_winner < -0.1)]
	players_suspects = as.factor(union(winners_suspects,losers_suspects))

	# joueurs impliqués dans un grand nombre de défaites:
	loss_suspects = sort(table(losers_suspects),decreasing=TRUE)
	loss_suspects = loss_suspects[which(loss_suspects > 10)]

	losers_suspects = unique(losers_suspects)
	# matches_suspects = unique(matches_suspects$match_uid)
	liste = list(matches_suspects,bookmakers_suspects,winners_suspects,losers_suspects,players_suspects,loss_suspects)

	### STATS
	cat("\n",length(matches_suspects$match_uid),"paris suspects\n")
	cat("\n",length(unique(matches_suspects$match_uid)),"matches suspects /",length(unique(matches$match_uid)),"\n")
	cat("\n # Nombre de matches joués par bookmaker:\n")
	print(table(matches$book))
	cat("\n # Bookmakers impliqués et nombre de matchs suspects correspondants:\n")
	print(bookmakers_suspects)
	cat("\n # Ratio des matchs suspects pour les Bookmakers impliqués:\n")
	print(bookmakers_ratio)
	cat("\n # Winners suspects:",length(liste[[3]]))
	cat("\n # Losers suspects:",length(liste[[4]]))
	cat("\n # Total players suspects:",length(liste[[5]]))
	cat("\n # Joueurs impliqués dans un grand nombre de défaites (>10):",length(liste[[6]]))

	liste

	#i=1: matches_suspects: dim(unique(m[[1]]$match_uid) = 2798
	#i=2: bookmakers_suspects: 7
	#i=3: winners_suspects: length(m[[3]]) = 333
	#i=4: losers_suspects: length(m[[4]]) = 365
	#i=5: players_suspects: length(m[[5]]) = 462
	#i=6: loss_suspects (> 10): length(m[[6]]) = 37
	
	#joueurs suspects: 39
}


