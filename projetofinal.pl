/****************************************************************************************************

	Predicado:	propaga
 	Argumentos:	Puz -		Uma lista que contem 3 listas, que representam respetivamente, uma lista 
 							com	listas que representam diferentes termometros, uma lista com o numero 
 							de posicoes que tem de estar preenchidas por linha e uma lista com o 
 							numero de posicoes que tem de estar preenchidas por coluna
 				Pos - 		Uma posicao do puzzle que e representada na forma de um tuple (x, y)
 				Posicoes - 	Uma lista com as posicoes que precisam de estar preenchidas para Pos 
 							estar preenchido
 	Descricao:	Dado um puzzle(Puz) e uma posicao(Pos), o preenchimento da posicao , Pos 
 				implica o preenchimento de todas as posicoes da lista de posicoes(Posicoes)
 
*****************************************************************************************************/

propaga([T|_], Pos, Posicoes):- propagaAux(T, Pos, Posicoes1), sort(Posicoes1,Posicoes).

propagaAux([T|_], Pos, Posicoes):- propagaT(T, Pos, Posicoes),!.
propagaAux([_|Resto], Pos, Posicoes):- propagaAux(Resto, Pos, Posicoes).

propagaT([Pos|_], Pos, [Pos]).
propagaT([P|Resto], Pos, Posicoes) :- 
	propagaT(Resto, Pos, Posicoes1),
	append([P], Posicoes1, Posicoes).

/****************************************************************************************************

	Predicado: 	nao_altera_linhas_anteriores
 	Argumentos:	Posicoes - 			A lista de posicoes que vai ser testada
 				L -					O numero da linha onde se vai comecar a verificar
 				Ja_preenchidas -	Uma lista de posicoes cujas posicoes ja se encontram
 									preenchidas no puzzle
 	Descricao:	Dada a lista de posicoes Posicoes, representando uma possibilidade de 
 				preenchimento para a linha L, todas as posicoes desta lista pertencendo a linhas 
 				anteriores a L, pertencem a lista de posicoes Ja_Preenchidas. Como o nome indica,
 				esta lista contem todas as posicoes ja preenchidas nas linhas anteriores a L
 
*****************************************************************************************************/

nao_altera_linhas_anteriores(_, 1, _).
nao_altera_linhas_anteriores(Posicoes, L, Ja_Preenchidas):-
 	L1 is L - 1,
	nao_altera_linha(Posicoes,L1,Ja_Preenchidas),
	nao_altera_linhas_anteriores(Posicoes, L1, Ja_Preenchidas).

nao_altera_linha([], _, _).
nao_altera_linha([Elemento|Posicoes], Linha, Ja_Preenchidas):-
	elemento_n_altera_linha(Elemento, Linha, Ja_Preenchidas),
	nao_altera_linha(Posicoes, Linha, Ja_Preenchidas).		

elemento_n_altera_linha(Elemento, _, Posicoes):- member(Elemento, Posicoes),!.
elemento_n_altera_linha(Elemento, Linha, _):- \+pertence_linha(Linha, Elemento),!.	
pertence_linha(Linha, (Linha,_)).	

/****************************************************************************************************

	Predicado:	verifica_parcial
 	Argumentos:	Puz - 				Uma lista que contem 3 listas, que representam respetivamente, 
 									uma lista com listas que representam diferentes termometros, 
 									uma lista com o numero de posicoes que tem de estar preenchidas
 									por linha e uma lista com o numero de posicoes que tem de 
 									estar preenchidas por coluna
 				Ja_Preenchidas - 	Uma lista de posicoes cujas posicoes ja se encontram
 									preenchidas no puzzle
 				Dim - 				A dimensao do puzzle
 				Poss - 				Uma lista de posicoes representando uma potencial possibilidade 
 									para preencher uma determinada linha
 	Descricao:	Dada a lista Poss e a lista Ja_Preenchidas verifica se estas nao ultrapassao o 
 				total das colunas do puzzle
 
*****************************************************************************************************/

verifica_parcial([_, _|[Limites]], Ja_Preenchidas, _, Poss):- 
	verifica_parcialaux(Limites, Ja_Preenchidas, Poss, 1).

verifica_parcialaux([], _, _, _).
verifica_parcialaux([Inicio|Fim], Ja_Preenchidas, Pos, Col):-
	Col1 is Col + 1,
 	verifica_coluna(Inicio, Ja_Preenchidas, Col, Pos),
 	verifica_parcialaux(Fim, Ja_Preenchidas, Pos, Col1).

verifica_coluna(Limite, Ja_Preenchidas, Col, Pos):- 
	union(Ja_Preenchidas, Pos, Todas_posicaos),
	verifica_coluna_aux(Todas_posicaos, Col, Qntd),
	Qntd =< Limite.

verifica_coluna_aux([], _, 0).
verifica_coluna_aux([Elemento|Resto], Col, Qntd) :-
	pertence_coluna(Elemento, Col),
	verifica_coluna_aux(Resto, Col, Qntd1),
	Qntd is Qntd1 + 1,!.

verifica_coluna_aux([Elemento|Resto], Col, Qntd) :-
	\+pertence_coluna(Elemento, Col),
	verifica_coluna_aux(Resto, Col, Qntd1),
	Qntd is Qntd1.

pertence_coluna((_,Coluna), Coluna). 

/****************************************************************************************************

	Predicado:	possibilidades_linha
 	Argumentos:	Puz - 				Uma lista que contem 3 listas, que representam respetivamente, 
 									uma lista com listas que representam diferentes termometros, 
 									uma lista com o numero de posicoes que tem de estar preenchidas
 									por linha e uma lista com o numero de posicoes que tem de 
 									estar preenchidas por coluna
 				Posicoes_linha -	Uma lista com todos os elementos de uma determinada linha
 				Total -				Quantidade de elementos que uma linha pode ter
 				Ja_Preenchidas -	Uma lista com as posicoes que ja se encontram preenchidas no puzzle
 				Possibilidades_L -	Uma lista com as posicoes da linha que podem estar preenchidas
 									de acordo com as regras do puzzle
 	Descricao:	Este predicado unifica a variavel Possibilidades_L com uma lista com possiveis solucoes
 				para uma determinada linha
 
*****************************************************************************************************/	

possibilidades_linha(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L):-
	possibilidades_linha_aux(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L1, []),!, 
	sort(Possibilidades_L1, Possibilidades_L).

possibilidades_linha_aux(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L, Ja_testadas):-
	combinacao(Posicoes_linha, Total, Combinacao, Ja_testadas),
	union([Combinacao], Ja_testadas, Ja_testadas1),
	propaga_combinacao(Puz, Combinacao, Propagada),
	sort(Propagada, Propagada1),
	verifica_combinacao(Puz, Total, Ja_Preenchidas, Propagada1, Posicoes_linha),
	possibilidades_linha_aux(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L1, Ja_testadas1),	
	append([Propagada1], Possibilidades_L1, Possibilidades_L),!.
possibilidades_linha_aux(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L, Ja_testadas):-
	combinacao(Posicoes_linha, Total, Combinacao, Ja_testadas),
	union([Combinacao], Ja_testadas, Ja_testadas1),
	possibilidades_linha_aux(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possibilidades_L, Ja_testadas1).
possibilidades_linha_aux(_, _, _, _, [], _).

verifica_combinacao(Puz, Total, Ja_Preenchidas, Propagada, Posicoes_linha):-
	Posicoes_linha = [(Linha,_)|_],
	length(Posicoes_linha, Dim),
	nao_altera_linhas_anteriores(Propagada, Linha, Ja_Preenchidas),!,
	verifica_parcial(Puz, Ja_Preenchidas, Dim, Propagada),
	union(Ja_Preenchidas, Propagada, Preenchidas),
	ve_linha_atual(Preenchidas, Total, Linha, 0).

ve_linha_atual([], Total, _, Total).
ve_linha_atual([Elemento|Resto], Total, Linha, Meutotal):-
	pertence_linha(Linha, Elemento),
	Meutotal1 is Meutotal + 1,
	ve_linha_atual(Resto, Total, Linha, Meutotal1).
ve_linha_atual([Elemento|Resto], Total, Linha, Meutotal):-
	\+pertence_linha(Linha, Elemento),
	ve_linha_atual(Resto, Total, Linha, Meutotal).

propaga_combinacao(_, [], []).
propaga_combinacao(Puz, [Pos|Resto], Propagada):-
	propaga(Puz, Pos, Propagacao),
	propaga_combinacao(Puz, Resto, Propagada1),
	union(Propagacao, Propagada1, Propagada).

combinacao(Lista, Tamanho, Combinacao, Ja_testadas):-
	comb(Lista, Combinacao), length(Combinacao, Tamanho),
	\+member(Combinacao, Ja_testadas).	

comb(_,[]). 
comb([Inicio|Resto],[Inicio|Combo]) :-
	comb(Resto,Combo).
comb([_|Resto],[Inicio|Combo]):-
    comb(Resto,[Inicio|Combo]).

/****************************************************************************************************

	Predicado:	resolve
 	Argumentos:	Puz - 			Uma lista que contem 3 listas, que representam respetivamente, 
 								uma lista com listas que representam diferentes termometros, 
 								uma lista com o numero de posicoes que tem de estar preenchidas
 								por linha e uma lista com o numero de posicoes que tem de 
 								estar preenchidas por coluna
 				Solucao -		Uma lista com as posicoes que devem estar preenchidas para o puzzle
 								poder ser declarado como resolvido
 	Descricao:	Este predicado resolve o puzzle, ou seja unifica a variavel Solucao com uma possivel
 				solucao para o puzzle se esta existir	
 
*****************************************************************************************************/

resolve(Puz, Solucao):- 
	
	Puz = [_, LimitesL, LimitesC],
	length(LimitesL, MaxL), 
	length(LimitesC, MaxC),
	nth1(1, LimitesL, Total),
	gera_posicoes(1, MaxC, Posicoes_linha),
	possibilidades_linha(Puz, Posicoes_linha, Total, [], Possibilidades),!,
	resolve_linha(Puz, 1, MaxL, MaxC, Possibilidades, [],  Solucao1),
	sort(Solucao1,Solucao).

resolve_linha(_, MaxL, MaxL, _, [Possibilidade|_], Ja_Preenchidas, Solucao):-
	union(Possibilidade, Ja_Preenchidas, Solucao).
resolve_linha(Puz, Linha, MaxL, MaxC, Lst_Possibilidades, Ja_Preenchidas, Solucao):-
	Linha < MaxL,
	Puz = [_, LimitesL, _],
	Linha1 is Linha + 1,
	nth1(Linha1, LimitesL, Total),
	member(Possibilidade, Lst_Possibilidades),
	union(Ja_Preenchidas, Possibilidade, Ja_Preenchidas1),
	gera_posicoes(Linha1, MaxC, Posicoes_linha),
	possibilidades_linha(Puz, Posicoes_linha, Total, Ja_Preenchidas1, Possibilidades_L),
	resolve_linha(Puz, Linha1, MaxL, MaxC, Possibilidades_L, Ja_Preenchidas1, Solucao).

gera_posicoes(_, 0, []).
gera_posicoes(Linha, MaxColuna, Posicoes_linha):-
	MaxColuna1 is MaxColuna -1,
	gera_posicoes(Linha, MaxColuna1, Posicoes_linha1),
	append(Posicoes_linha1, [(Linha, MaxColuna)], Posicoes_linha), !.

