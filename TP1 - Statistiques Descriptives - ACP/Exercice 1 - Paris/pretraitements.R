
# conversion de type : booleens definis comme "logicals" 
books$moved_towards_winner <- as.logical(books$moved_towards_winner)
books$is_cancelled_or_walkover <- as.logical(books$is_cancelled_or_walkover)

# simplification des identifiants de joueurs, paris et matches 
shorten <- function (hash)
{
	substr(hash, 1, 10)
}
books$match_book_uid <- as.character(books$match_book_uid)
books$match_uid <- as.character(books$match_uid)
books$winner <- as.character(books$winner)
books$loser <- as.character(books$loser)
books <- transform(books, loser=shorten(loser), winner=shorten(winner), match_uid=shorten(match_uid), match_book_uid=shorten(match_book_uid))
players.levels <- unique(c(books$winner,books$loser))
books$match_book_uid <- as.factor(books$match_book_uid)
books$match_uid <- as.factor(books$match_uid)
books$winner <- factor(books$winner, levels=players.levels)
books$loser <- factor(books$loser, levels=players.levels)
rm(players.levels)

# suppression des matches pour lesquels la probabilite initiale de victoire est significative (differente de plus de 0.1 de la probabilite de gain mediane sur tous les bookmakers) 
# => la "normalisation" de ces cotes (retour a une cote "normale") pourrait etre interpretee comme signe de match arrange 
medianes <- aggregate(implied_prob_winner_open ~ match_uid, books, median)$implied_prob_winner_open[books$match_uid]
indices <- which(abs(books$implied_prob_winner_open-medianes)>0.1)
books.sel <- books[-indices,]
books.sel <- books.sel[-which(books.sel$is_cancelled_or_walkover),]
books.sel <- books.sel[,-which(names(books.sel)=="is_cancelled_or_walkover")]
rm(medianes,indices)

