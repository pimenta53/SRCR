:- op( 900,xfy,'::' ).
/* permitir adicionar a base de conhecimento	*/
:-dynamic caminho/6.
:-dynamic caminhoEntreLocais/2.
:-dynamic pais/1,cidade/1,regiao/1,servico/1,servicoLocal/2.
:-dynamic regiaoDaCidade/2,paisRegiao/2.

/* para não imprimir warnings	*/
:-prolog_flag(single_var_warnings,_,off).


%cidade(nome).

cidade(braga).
cidade(barcelos).
cidade(madrid).
cidade(porto).

%pais(nome).
pais(portugal).
pais(espanha).

%regiao(nome).
regiao(minho).
regiao(douro).

%regiaoDaCidade(cidade,regiao).
regiaoDaCidade(braga,minho).
%regiaoDaCidade(barcelos,minho).
%regiaoDaCidade(porto, douro).

%paisRegiao(regiao,pais).
paisRegiao(minho,portugal).
paisRegiao(douro,portugal).

%servicos(serviço).
servico(restaurante).
servico(hospital).
servico(bombeiros).
servico(ctt).
servico(pizzaria).
servico(cinema).


%caminho(tipo, origem, destino, kms, tempo, custo).

caminho(autoestrada, braga, porto, 50, 30, 17).
caminho(viamunicipal, braga, barcelos, 25, 20, 10).
caminho(viamunicipal, porto, lisboa, 25, 20, 10).
caminho(viamunicipal, porto, barcelos, 30,30,12).
caminho(viamunicipal, lisboa, setubal, 30,30,12).
caminho(viamunicipal, setubal, faro, 30,30,12).
caminho(viamunicipal, barcelos, faro, 30,30,12).

%servicoLocal(serviço,local).
servicoLocal(padaria,minho).
servicoLocal(restaurante,braga).
servicoLocal(ctt,barcelos).
servicoLocal(pizzaria,braga).
servicoLocal(ctt,braga).
servicoLocal(cinema,porto).
servicoLocal(supermercado,barcelos).
servicoLocal(talho,porto).
servicoLocal(talho,faro).
servicoLocal(talho,barcelos).

%funçoes gerais

cidadePais(C,P) :- regiaoDaCidade(C,N), paisRegiao(N,P).

printLista([]):-write('').
printLista([H|T]):-
	write(H),nl,
	printLista(T).

concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).


not( Question) :-
	Question, !,fail.
not(Question).

difList([],_,[]).
difList([H1|T1],L2,[H1|L3]):-
	not(member(H1,L2)), difer(T1,L2,L3).
difList([_|T1],L2,L3):-
	difList(T1,L2,L3).

%Remover repetidos de uma lista
removerT(_, [] , []).
removerT(X, [X | L1], L) :-
   removerT(X,L1,L).
removerT(X, [Y | L1], [Y | L]) :-
	removerT( X,L1,L).

remRep([],[]).
remRep([H|T],[H|TL]) :-
	removerT(H,T,Lista),
	remRep(Lista,TL).


%1-Identificar os serviços existentes num determinado local, região ou país;


servicosCidade(Cidade, L):-
	findall(X,servicoLocal(X,Cidade),L).

servicosRegiaoAux([],[]).
servicosRegiaoAux([H|T],L):-
	servicosCidade(H,L1),servicosRegiaoAux(T,L2),concat(L1,L2,L).

servicosRegiao(Regiao,L):-
	findall(C,regiaoDaCidade(C,Regiao),Cidades),servicosRegiaoAux(Cidades,L).	
	
servicosPaisAux([],[]).
servicosPaisAux([H|T],L) :-
	servicosRegiao(H,L1),
	servicosPaisAux(T,L2),
	concat(L1,L2,L).

servicosPais(Pais,L):-
	findall(P,paisRegiao(R,Pais),Regs),servicosPaisAux(Regs,L).

/* 2- Identificar os locais onde esteja presente um determinado serviço ou conjunto de serviços; */

existeServico(Serv,L):-
	findall(C,servicoLocal(Serv,C),L).

existemServicos([],[]).
existemServicos([H|T],L):-
	existeServico(H,L1),
	existemServicos(T,L2),concat(L1,L2,L).

/* 3- serviçoes não existentes numa determinada cidade,região,Pais */


todosServicos(L):-
	findall(S,servico(S),L),printLista(L).

servicosForaRegiao(Regiao,L):-
	todosServicos(Todos),
	servicosRegiao(Regiao,ServReg),
	difList(Todos,ServReg,L).

servicosForaCidade(Cidade,L):-
        todosServicos(Todos),
        servicosCidade(Cidade,ServCid),
        difList(Todos,ServCid,L).

servicosForaPais(Pais,L):-
        todosServicos(Todos),
        servicosPais(Pais,ServPais),
        difList(Todos,ServPais,L).


/* 4-Determinar o caminho entre dois locais; 
%caminho(tipo, origem, destino, kms, tempo, custo).*/

caminhoEntreLocais(A, Z, C) :-
	caminhoAux(A, [Z], C).

caminhoAux(A, [A | C1], [A | C1]). 
caminhoAux(A, [Y | C1], C) :-
	adjacente(X, Y), not(member(X, C1)), 
	caminhoAux(A, [X, Y | C1], C).

adjacente(X, Y) :- 
	caminho(_,X,Y,_,_,_).	

%5-Determinar o caminho entre dois serviços;
/* caminho mais curto falta fazer */

caminhoEntreServicos(S1,S2,C) :- 
	servicoLocal(S1,L1), 
	servicoLocal(S2,L2),
	caminhoEntreLocais(L1,L2,C).

%6-Determinar o caminho que permite percorrer uma sequencia de servicos

caminhoSeqServicos([],C).
caminhoSeqServicos([H],C).
caminhoSeqServicos([H1,H2|T],C) :-
	caminhoEntreServicos(H1,H2,C),
	caminhoSeqServicos([H2|T],C).

/*	ADICIONAR E REMOVER SERVIÇOS LOCALIZAÇÕES E CAMINHOS */
%insercoes
novaCidade(Cidade):-
	inserir(cidade(Cidade)).

novoPais(Pais):-
   inserir(pais(Pais)).

novaRegiao(Reg):-
   inserir(regiao(Reg)).

novoServico(Serv) :-
	inserir(servico(Serv)).

novoServicoLocal(Serv,Local):-
	inserir(servicoLocal(Serv,Local)).

novoCaminho(Tip,Org,Dst,Km,Tim,Cst) :-
	inserir(caminho( Tip,Org,Dst,Km,Tim,Cst )).

novaRegiaoCidade(C,R) :-
	inserir(regiaoDaCidade( C,R )).

novoPaisRegiao(R,P) :-
	inserir(paisRegiao( R,P )).

%remocoes
removeCidade(Cidade):-
	remover(cidade(Cidade)).

removerPais(Pais):-
   remover(pais(Pais)).

removerRegiao(Reg):-
  	remover(regiao(Reg)).


/*	listar BD */
cidadesRegiao(Regiao,L):-
        findall(C,regiaoDaCidade(C,Regiao),L).


/*	EXTRAS de SERCIÇOS	*/

existeLocal(L) :-
	cidade(L);
	regiao(L);
	pais(L).

totalServicosCidade(Cidade,R):-
	findall(S,servicoLocal(S,Cidade),L), length(L,R).

/* total de cidades de um serviço	*/
totalCidadesServico(Srv,R):-
        findall(C,servicoLocal(Srv,C),L), length(L,R).


totalCidadesRegiao(Regiao,R):-
	findall(C,regiaoDaCidade(C,Regiao),L), length(L,R).

totalRegioesDoPais(Pais,R):-
        findall(R,paisRegiao(R,Pais),L), length(L,R).

max2((Cid1,X),(Cid2,Y),(Cid1,X)) :- X>Y.
max2((Cid1,X),(Cid2,Y),(Cid2,Y)) :- X=<Y.

maior2([A],A).
maior2([H|T],R) :- maior2(T,R1), max2(H,R1,R).

add2((X,Y),L,[(X,Y)|L]).

/* lista com o local e o toal de serviços que este possui */
totServL([],[]).
totServL([H1|T1],L):-
	totalServicosCidade(H1,R),
        add2((H1,R),[],L1),
	totServL(T1,L2),concat(L1,L2,L).

/* local com maior numero de servicos */
localMaisServicos(R):-
	findall(Cidade,servicoLocal(X,Cidade),L1),
	totServL(L1,L2),
	maior2(L2,R).

/* 	TOTAL DE CIDADES COM UM DETERMINADO SERVIÇO	*/
totCidadeS([],[]).
totCidadeS([H1|T1],L):-
        totalCidadesServico(H1,R1),
        add2((H1,R1),[],L1),
        totCidadeS(T1,L2),concat(L1,L2,L).

servicoMaisRegioes(R):-
	findall(S,servicoLocal(S,X),L1),
	totServL(L1,L2),
        maior2(L2,R).
	

/* ################# MECANISMOS ####################*/
inserir( Termo ) :-
   findall( Invariante,+Termo::Invariante,Lista ),
	insercao( Termo ),
	teste( Lista ).

remover(Termo):-
	findall(Inv,-Termo::Inv,LInv),
	remocao(Termo),
	teste(LInv).


insercao(Termo) :-
	assert(Termo).
insercao(Termo) :-
	retract( Termo ),!,fail.

remocao(Termo):-
	retract(Termo).
remocao(Termo):-
	assert(Termo),!,fail.


teste( [] ).
	teste( [R|LR] ) :-
	R,
	teste( LR ).

comprimento( [],0 ).
comprimento( [_|XS],R1 ) :-
	comprimento( XS,R ),
	R1 is R+1.


%Invariantes
%Só podemos remover uma cidade se esta nao esta referenciada
-cidade( Cid ) :: (not(servicoLocal(_,Cid)),
						not(caminho(_,Cid,_,_,_,_)),
						not(caminho(_,_,Cid,_,_,_)),
						not(regiaoDaCidade(Cid,_))
					).
						

%Só podemos remover uma regiao se esta nao esta referenciada
-regiao( Reg ) :: (not(regiaoDaCidade(_,Reg)),
						not(caminho(_,Reg,_,_,_,_)),
						not(caminho(_,_,Reg,_,_,_)),
						not(paisRegiao(Reg,_)),
						not(servicoLocal(_,Reg))
					).
	
%Só podemos remover um pais se este nao esta referenciado
-pais( Pais ) :: (not(caminho(_,Pais,_,_,_,_)),
						not(caminho(_,_,Pais,_,_,_)),
						not(paisRegiao(_,Pais))
						).


%Só podemos inserir uma cidade se esta ainda n existe
+cidade( C ) :: (findall( C, (cidade( C )),S ),
                  comprimento( S,N ), N==1
						).

%Só podemos inserir um pais se este ainda n existe
+pais( P ) :: (findall( P, (pais( P )),S ),
                  comprimento( S,N ), N==1
						).

%Só podemos inserir uma regiao se esta ainda n existe
+regiao( R ) :: (findall( R, (regiao( R )),S ),
                  comprimento( S,N ), N==1
						).

%Só podemos inserir uma serviço se esta ainda n existe
+servico( Serv ) :: (findall( Serv, (servico( Serv )),S ),
                  comprimento( S,N ), N==1
						).

%Só podemos associar um serviço a um local se esta ainda n existe e o local já existe
+servicoLocal( Serv,L ) :: (findall( (Serv,L), (servicoLocal( Serv,L )),S ),
                  			comprimento( S,N ), N==1
                  			).
+servicoLocal( Serv,L ) :: (existeLocal(L),
									servico(Serv)
									).


%Só podemos adicionar uma rota se esta ainda n existe
+caminho( Tip,Org,Dst,Km,Tim,Cst ) :: (findall( (Tip,Org,Dst,Km,Tim,Cst), (caminho(Tip,Org,Dst,Km,Tim,Cst)), S),
                  comprimento( S,N ), N==1,
						existeLocal(Org),
						existeLocal(Dst)
                  ).

%Só podemos relacionar cidade e regiao se ambos existem 
+regiaoDaCidade( C,R ) :: (findall( (C,R), (regiaoDaCidade( C,R )),S ),
                  comprimento( S,N ), N==1,
						cidade(C),
						regiao(R)
                  ).

%Só podemos relacionar cidade e regiao se ambos existem 
+paisRegiao( R,P ) :: (findall( (R,P), (paisRegiao( R,P )),S ),
                  comprimento( S,N ), N==1,
						regiao(R),
						pais(P)
                  ).



