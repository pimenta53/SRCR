:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( discontiguous_warnings,off ).
:- op( 900,xfy,'::' ).

:- dynamic localidade/1.
:- dynamic hospital/1.
:- dynamic farmacia/2.
:- dynamic appClinica/1.
:- dynamic medicamento/4.
:- dynamic estatuto/1.
:- dynamic data/4.
:- dynamic medicDisponivel/4.
:- dynamic excepcaoAppClinica/1.
:- dynamic excepcaoFarm/2.
:- dynamic remove/2.

%---------------------------------
%		Hospital(nome)
%---------------------------------
hospital('S. Marcos').


%---------------------------------
%		Local(nome)
%---------------------------------
localidade('perelhal').
localidade('braga').
localidade('barcelos').


%---------------------------------
%		Farmacia(nome,localidade/hospital).
%---------------------------------
farmacia(central,'S. Marcos').
farmacia(oliveira,'perelhal').
farmacia(nova,'braga').
-farmacia(peralhal,'braga').
	
	%VALOR NULO TIPO I
	%Caso em que não se sabe em que sitio está a farmacia pimentel
farmacia(pimentel,desc).
excepcaoFarm(Nome,Sitio) :-
	farmacia(Nome,desc).
	
-farmacia(Nome,Sitio) :-
   nao(farmacia(Nome,Sitio)),
	nao(excepcaoFarm(Nome,Sitio)).


%---------------------------------
%		Aplicaçao Clinica
%---------------------------------
appClinica(constipacao).
appClinica(alergia).
appClinica(calvicie).

-appClinica( A ) :-
   nao(appClinica(A)),
   nao(excepcaoAppClinica(A)).


%---------------------------------
%       Estatuto(tipo)
%---------------------------------
estatuto(reformado).
estatuto(publico).

	%CONHECIMENTO NEGATIVO
-estatuto(jovem).
-estatuto(pensionista).

	%Representacao de informacao negativa pelo pressoposto do mundo fechado -- NEGACAO POR FALHA NA PROVA
-estatuto(E) :-
	nao(estatuto(E)).


%---------------------------------
%  	Data( medicamento, ano, mes, validade meses)
%---------------------------------
data(epizen, 1997, 01, 12).
data(relistor, 1997, 02, 18).
data(tobi, 2002, 10, 24).

	%VALOR NULO TIPO II
	%Nao se sabe se o medicamento rennan foi lançado em 2003 ou 2004
excepcaoDT(rennan, 2003, 10, 1).
excepcaoDT(rennan, 2004, 10, 1).

	%VALOR NULO TIPO II
	%Nao se sabe a data de lancamento do kineret. Apenas se conhece a decada
excepcaoDT(kineret,A,M,V) :-
	A >= 1980, A =< 1990, M >= 1, M =< 12.


-data( D,A,M,V ) :-
   nao(data( D,A,M,V )),
	nao(excepcaoDT( D,A,M,V )).


%---------------------------------
%     Medicamento(nome,doença,especialidade,principio activo)
%---------------------------------
medicamento(relistor,constipacao,hematologia,methylnaltrexone).
medicamento(epizen,alergia,pneumologia,ephinephrine).
medicamento(tobi,constipacao,hematologia,tobramycin).
medicamento(rennan,alergia,pneumologia,rennamycin).
medicamento(kineret,reumatismo,ortopedia,anakinra).
medicamento(cenas,alergia,cenas,cenas).

	%VALOR NULO TIPO 1
	%E desconhecido medicamento para a calvice
medicamento(algum,calvicie,algum,algum).
excepcaoMedic(M,D,E,PA) :-
	medicamento(algum,D,algum,algum).

	%VALOR NULO TIPO III
	%Caso em que não se permite conhecer o medicamento que trate o cancro
medicamento(algum,cancro,alguma,algum).
excepcaoMedic(M,D,E,PA) :-
	medicamento(algum,D,alguma,algum).
nulo(cancro).

	
-medicamento( N,A,S,PA ) :-
   nao(medicamento( N,A,S,PA )),
	nao(excepcaoMedic( N,A,S,PA )).


%EXTRA 1
%Encontra medicamentos para determinada doenca
medicParaDoenca(D,M) :- 
	findall( (N,D,E,PA), (medicamento(N,D,E,PA), nao(excepcaoMedic(algum,D,algum,algum))), M ),
			 comprimento(M,T), T>=1.

-medicParaDoenca(D,M) :- 
	nao(medicamento(N,D,E,PA)),
	nao(excepcaoMedic(algum,D,algum,algum)).


%EXTRA 2
%Encontra o medicamento com mais validade para uma doenca
medicDoencaMaiorValidade(D,Med) :-
	findall( (N,D,Validade), 
				(medicamento(N,D,E,PA),	nao(excepcaoMedic(algum,D,algum,algum)), 
				 data(N,Ano,Mes,Validade), nao(excepcaoDT( N,_,_,_))),
				R ),
	comprimento(R,T), T>=1,
	maiorValid(R,Med).		

-medicDoencaMaiorValidade(D,Med) :-
	nao(medicDoencaMaiorValidade(D,Med)),
	nao(excepcaoMedic(_,D,_,_)),
	findall( (N,D), (medicamento(N,D,E,PA), excepcaoDT(rennan,Ano,Mes,Valid)), R),
	comprimento(R,T), T==0.


maiorV((N1,D1,V1),(N2,D2,V2),(N1,D1,V1)):- V1>=V2.
maiorV((N1,D1,V1),(N2,D2,V2),(N2,D2,V2)):- V2>V1.
	
maiorValid([R],R).
maiorValid([(N,D,Validade)|T],R):- 
	maiorValid(T,R1), 
	maiorV((N,D,Validade),R1,R).


%---------------------------------
%	 Disponibilidades(farmacia, medicamento, preco publico, preco reformado)
%---------------------------------
medicDisponivel(central, relistor, 12.5, 3.5).
medicDisponivel(oliveira, tobi, 6.5, nulo).
medicDisponivel(central, tobi, 7.5, 2.3).
medicDisponivel(oliveira, kineret, 20.3, 15.3).
medicDisponivel(central, epizen, 10.3, 7.3).
medicDisponivel(central, kineret, 5.3, 2.3).

	%Desconhece-se se existe epizen na farmacia central ou na nova
excepcaoMDisp(central,epizen, algum, algum).
excepcaoMDisp(nova,epizen, algum, algum).
	%Desconhece-se se existe rennan na farmacia nova
excepcaoMDisp(nova,rennan, algum, algum).

	%A farmacia central nao disponibiliza medicamentos para a calvice --CONHECIMENTO NEGATIVO
-medicDisponivel(central,M,D,PP,PR) :-
	medicamento(M,calvice,E,PA).

-medicDisponivel( F,M,PP,PR ) :-
   nao(medicDisponivel( F,M,PP,PR )),
	nao(excepcaoMDisp(F,M,algum,algum)).

%EXTRA 3
%em que farmacia se encontra o medicamento pretendido ao melhor preco
medicMaisBaratoPublic(M,R) :- 
	findall((M,F,PP), (medicDisponivel(F,M,PP,PR)), S),
							  comprimento(S,N), N>=1,
							  menorL(S,R).

-medicMaisBaratoPublic(M,R) :- 
	nao(medicDisponivel( F,M,PP,PR )),
	nao(excepcaoMDisp(F,M,algum,algum)).


menor((F1,PP1),(F2,PP2),(F1,PP1)):- PP1=<PP2.
menor((F1,PP1),(F2,PP2),(F2,PP2)):- PP2<PP1.

menorL([(M,F,PP)],(F,PP)).
menorL([(M,F,PP)|T],R):- menorL(T,R1), menor((F,PP),R1,R).


%EXTRA 4
%Que aplicacoes clinicas sao disponibilizadas por uma farmacia
appClinicasFarmacia(F,R) :-
	findall( (D), (medicDisponivel(F,N,PP,PR), medicamento(N,D,E,PA)), R1),
  	comprimento(R1,T), T>=1,
	removeRep(R1,R).

-appClinicasFarmacia(F,R) :-
	nao(appClinicasFarmacia(F,R)),
	nao(excepcaoMDisp(F,_,_,_)).

remove(_,[],[]).
remove(X,[X|T],R1) :-
	remove(X,T,R1).
remove(X,[Y|T],[Y|R1]) :-
	remove(X,T,R1).

removeRep([],[]).
removeRep([H|T],[H|R]) :-
	remove(H,T,R1),
	removeRep(R1,R).


%---------------------------------
%%Funcoes de insercao
%---------------------------------
evolucao( Termo ) :-
   solucoes( Invariante,+Termo::Invariante,Lista ),
	insercao( Termo ),
	teste( Lista ).

insercao( Termo ) :-
   assert( Termo ).
insercao( Termo ) :-
   retract( Termo ),!,fail.

remover( Termo ):-
   findall( Inv,-Termo::Inv,LInv),
	remocao(Termo),
	teste(LInv).

remocao(Termo):-
   retract(Termo).
remocao(Termo):-
	assert(Termo),!,fail.


teste( [] ).
teste( [R|LR] ) :-
   R,
   teste( LR ).

solucoes(X,Y,Z) :-
	findall(X,Y,Z).

comprimento([],0).
comprimento([X|XS],R) :-
	comprimento(XS,R1),
	R is R1+1.




%---------------------------------
%%Funções auxiliares utilizadas
%---------------------------------

demo(Questao, verdadeiro) :-
	Questao.
demo(Questao, falso) :-
	-Questao.
demo(Questao, desconhecido) :-
	nao( Questao ), 
	nao( -Questao ).

nao( Questao ) :-
	Questao, !, fail.
nao( Questao ).



%%Manipulação de invariantes que designem restrições à inserção e à remoção de conhecimento

%Só podemos remover um hospital se este não se encontrar associado
-hospital(Hosp) :: (nao(farmacia(_,Hosp))).

%Só podemos remover uma localidade se este não se encontrar associado
-localidade(Loc) :: (nao(farmacia(_,Loc))).

%Só podemos remover uma farmácia se este não se encontrar associado
-farmacia(Nome,_) :: (nao(medicDisponivel(Nome,_,_,_)),
					  nao(excepcaoMDisp(Nome,_,_,_))).

%Só podemos remover uma aplicação clínica se este não se encontrar associado
-appClinica(Ac) :: (nao(medicamento(_,Ac,_,_))).

%Só podemos remover um medicamento se este não se encontrar associado
-medicamento(Med,_,_,_) :: (nao(data(Med,_,_,_)),
							nao(excepcaoDT(Med,_,_,_)),
							nao(medicDisponivel(_,Med,_,_)),
							nao(excepcaoMDisp(_,Med,_,_))).

%Só podemos adicionar um hospital se este ainda não existe
+hospital(Hosp) :: (findall(Hosp, (hospital(Hosp)), S),
					comprimento(S,N), N==1).
+hospital :: 
%Só podemos adicionar uma localidade se este ainda não existe
+localidade(Loc) :: (findall(Loc, (localidade(Loc)), S),
				comprimento(S,N), N==1).

%Só podemos adicionar uma farmácia se o nome ainda não existe e o localidade/hospital existe
+farmacia(Nome,LH) :: (findall((Nome,LH), (farmacia(Nome,LH)), S),
					   comprimento(S,N), N==1,
					   localidade(LH);
					   hospital(LH)).
					   
%Só podemos adicionar uma aplicação clínica se esta ainda não existe
+appClinica(Ac) :: (findall(Ac, (appClinica(Ac)), S),
					comprimento(S,N), N==1).

%Só podemos adicionar um medicamento se o nome é único e a aplicação clínica existe
+medicamento(Nome,Ac,X,Z) :: (findall((Nome,Ac,_,_), (medicamento(Nome,Ac,_,_)), S),
							  comprimento(S,N), N==1,
							  appClinica(Ac),
							  nao(nulo(Ac))).

%Só podemos adicionar um estatuto se este ainda não existe
+estatuto(Tipo) :: (findall(Tipo, (estatuto(Tipo)), S),
					comprimento(S,N), N==1).

%Só podemos associar uma data a um medicamento se este existe e a data que se pretende associar, ainda não está associada a este
+data(Med,A,M,MV) :: (findall((Med,A,M,MV), (data(Med,A,M,MV)), S),
					   comprimento(S,N), N==1,
					   medicamento(Med,_,_,_)).

%Só podemos associar um medicamento a um farmácia se ambos existem e ainda não se encontram associados
+medicDisponivel(Farm,Med,_,_) :: (findall((Farm,Med,_,_), (medicDisponivel(Farm,Med,_,_)), S),
								  	 comprimento(S,N), N==1,
									 farmacia(Farm,_),
									 medicamento(Med,_,_,_)).
