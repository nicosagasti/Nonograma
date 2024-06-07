:- module(proylcc,
	[  
		put/8
	]).

:-use_module(library(lists)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY is the result of replacing the occurrence of X in position XIndex of Xs by Y.
replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkCluesRecursivo(+listaPista, +listaGrilla, +Contador, +Retorno)
%
% Verifica si la lista de pistas se satisface
checkCluesRecursivo([], [], 0, 1). % Primer Caso Base
checkCluesRecursivo([P | _], [X |_ ], L, 0):- % Segundo Caso Base
    X \== "#",
    L \== 0,
    L \== P.

checkCluesRecursivo([], [X | _], L, 0):- % Tercer Caso Base => cuando hay marcados de mas
    X == "#",
    L == 0.
checkCluesRecursivo([P],[],L,1):- % Cuarto Caso Base => Cuando era el ultimo y L es P 
    P == L.

checkCluesRecursivo([P | _],[],L,0):- % Quinto Caso Base => Cuando nos quedamos sin lista para consumir, pero seguimos teniendo Pistas, L != P
    P \== L.
checkCluesRecursivo([P | _],[],L,0):- % Sexto Caso Base => Cuando nos quedamos sin lista para consumir, pero seguimos teniendo Pistas, L == P
    P == L.

% Casos Recursivos:
checkCluesRecursivo([],[X |Xs],0, Return):-
    X \== "#",
    checkCluesRecursivo([],Xs,0,Return).

checkCluesRecursivo([P|Ps], [X | Xs], Count, Return) :-
    X \== "#", 
    P \== Count,
    checkCluesRecursivo([P|Ps], Xs, 0, Return).

% Caso en el que X no es #
checkCluesRecursivo([P | Ps], [X | Xs], Count, Return) :-
    X \== "#", 
    P == Count,
    checkCluesRecursivo(Ps, Xs, 0, Return).

% Caso en el que X es #
checkCluesRecursivo([P | Ps], [X | Xs], Count, Return) :-
    X == "#", 
    CountN is Count + 1,
    checkCluesRecursivo([P | Ps], Xs, CountN, Return).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkClues(+ClueList, +GridList, -Satisfied)
%
% Metodo cascara: Verifica si la lista de pistas se satisface
checkClues(ClueList, GridList, Satisfied) :-
    checkCluesRecursivo(ClueList, GridList, 0, Satisfied).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% addElement(+Element, +List, -Result)
%
% agrega un elemento al inicio de la lista
addElement(X,[],[X]).
addElement(X,[Head|Tail],R):- 
    R = [X,Head|Tail].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getValuesFromColumn(+GridList, +ColumnIndex, -ColumnValues)
%
% obtiene los valores de una columna en la grilla representada como lista de listas
getValuesFromColumn([],_Pos,[]).
getValuesFromColumn([H|T],Pos,List):- 
    nth0(Pos,H,Element),
    getValuesFromColumn(T,Pos,ListAux),
    addElement(Element,ListAux,List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkGrid(+Grid, +RowClues, +ColsClues, +[RowN, ColN], -RowSat, -ColSat)
% 
% Chequea si la fila[RowN] y la columna[[ColN] verifican a las respectivas pistas
checkGrid(Grid, RowsClues, ColsClues, [RowN, ColN], RowSat, ColSat):-
    % Obtenemos las pistas de la lista de Pistas
    nth0(RowN, RowsClues, RowNElement),
    nth0(ColN, ColsClues, ColNElement),
    
    % Obtenemos la fila de la Grilla  
    nth0(RowN, Grid, NewRow),
  	
    %Obtenemos la columna de la Grilla
    getValuesFromColumn(Grid, ColN, NewCol),

    % Chequeamos si verifica con las Rows
    checkClues(RowNElement, NewRow, RowSat),

    % Chequeamos si verifica con las Cols
    checkClues(ColNElement, NewCol, ColSat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%
put(Content, [RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, RowSat, ColSat):- 
	% NewGrid is the result of replacing the row Row in position RowN of Grid by a new row NewRow (not yet instantiated).
	replace(Row, RowN, NewRow, Grid, NewGrid),

	% NewRow is the result of replacing the cell Cell in position ColN of Row by _,
	% if Cell matches Content (Cell is instantiated in the call to replace/5).	
	% Otherwise (;)
	% NewRow is the result of replacing the cell in position ColN of Row by Content (no matter its content: _Cell).

	(replace(Cell, ColN, _, Row, NewRow),
	Cell == Content
		;
	replace(_Cell, ColN, Content, Row, NewRow)),

    %% Ver si se verifican RowSat y ColSat
    checkGrid(NewGrid, RowsClues, ColsClues, [RowN, ColN], RowSat, ColSat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkOnes(+List, -Return)
%
% Verifica si la lista pasada por parametro esta completamente formada por unos.

% Casos bases:
checkOnes([1],1).
checkOnes([0],0).
checkOnes([0|_],0).

%Caso Recursivo :
checkOnes([X|Xs],R):-
    X==1,
    checkOnes(Xs,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkWon(+RowList, +ColList, -Return)
%
% Verifica si ambas listas son formadas completamente por unos.
checkWon([X|Xs], [Y|Ys],Res):-
    checkOnes([X|Xs],Res1),
    checkOnes([Y|Ys],Res2),
    (Res1 == 0 -> Res = 0 ;  
	(Res1 == Res2 -> Res = 1 ; Res = 0)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Obtiene una posicion de una lista.
%
%getPos(+Pos, +List, -Return)
getPos(0,L,R):-
	L = [H|_T], R = H.
getPos(N,L,R):-
	L = [_H|T], N > 0, 
	S is N-1, 
	getPos(S,T,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Lee una matriz y devuelve una columna en forma de lista.
%
% listarCol(+Grid, +Pos, -List).
listarCol([],_Pos,[]).
	%Lista = [].  %%podria ser un hecho
listarCol([H|T],Pos,Lista):- 
	nth0(Pos,H,Elemento),
    listarCol(T,Pos,ListaAux),
    addElement(Elemento,ListaAux,Lista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
generateClue([],N,Pista,Resto):- 
	Resto = [],Pista = [N].
generateClue([H|T],N,Pista,Resto):- 
	H == "#",
	S is N+1, generateClue(T,S,Pista,Resto);
    Resto = T,Pista = [N].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Genera una lista de pistas a traves de una fila de una matriz.
%
%generateCluesList(+Row, -CluesList).
generateCluesList([],[]):- !. 
	%%ListaPista = [],!.
generateCluesList(Fila,ListaPista):- 
	Fila \==[], generateClue(Fila,0,Pista,Resto),
	generateCluesList(Resto,ListaAux),
    Pista = [H], 
	(H \== 0, addElement(H,ListaAux,ListaPista);ListaPista = ListaAux).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Agrega un elemento al final de una lista
%
%addAtEnd(+Elememt, +List, -newList).
addAtEnd(X,[],[X]).
	%R = [X].
addAtEnd(X,[H|T],R):- 
	addAtEnd(X,T,Raux), 
	R = [H|Raux].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Resuelve una grilla dada.
%
%solveGrid(+RowClues, +ColClues, +Grid, -SolvedGrid, +numRows, +numCols).
solveGrid(PistasF,PistasC,Grilla,GrillaRes,CantF,CantC):-  
    solveRows(PistasF,Grilla,GrillaAux,CantF),
    transposed(GrillaAux,GrillaTraspuesta,CantC),
    solveRows(PistasC,GrillaTraspuesta,GrillaAux2,CantC),
    transposed(GrillaAux2,GrillaResAux2,CantF),
    verifyRows(PistasF,GrillaResAux2,Sat,CantF),
    (Sat = true,GrillaRes = GrillaResAux2;
	solveGrid(PistasF,PistasC,GrillaResAux2,GrillaRes,CantF,CantC)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Verifica que cada fila de una grilla respeta las pistas dadas.
%Idealmente recibe: Una lista de pistas, una grilla y la cantidad de filas.
%Salida esperada: True en caso de que las filas verifiquen las pistas, false en caso contrario.
%verifyRows(+CluesList, +Grid, +numRows, -Return ).
verifyRows([],[],Sat,-1):- 
	Sat = true,!.
verifyRows([P|RestoP],[F|RestoF],Sat,N):- 
	S is N-1, 
	generateCluesList(F,Pista),
	P = Pista, 
	verifyRows(RestoP,RestoF,Sat,S); 
	Sat = false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                               
%Genera la mejor respuesta posible de una grilla.
%Idealmente recibe: Pistas de filas, una grilla y la cantidad de filas.
%Salida esperada: Una grilla con la mayor cantidad de pistas resueltas en una iteracion.
%solveRows(+CluesList, +Grid, -ResGrid, +numRows/Cols).
solveRows(_PistasF,Grilla,GrillaRes,-1):-
	GrillaRes = Grilla,!.
solveRows(PistasF,Grilla,GrillaRes,Pos):-
	nth0(Pos,PistasF,Pista),
	nth0(Pos,Grilla,Fila),
    generatePosibilitiesList(Fila,Pista,ListaPosibilidades),
	longitud(Fila,Long),
    solveLine(ListaPosibilidades,LineaModificada,Long),
    replace(_Fila,Pos,LineaModificada,Grilla,GrillaModificada),
    S is Pos-1,
	solveRows(PistasF,GrillaModificada,GrillaRes,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Genera la grilla traspuesta de una grilla dada.
%Idealmente recibe: Una grilla y la cantidad de filas.
%Salida esperada: La grilla traspuesta a la dada.
%transposed(+Grid, -transposedGrid, +numRows).
transposed(_Matriz,MatrizTraspuesta,-1):-
	MatrizTraspuesta = [],!.
transposed(Matriz,MatrizTraspuesta,Pos):-
	listarCol(Matriz,Pos,Col),
	S is Pos-1,
	transposed(Matriz,MatrizAux,S), 
	addAtEnd(Col,MatrizAux,MatrizTraspuesta).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Resuelve una linea dada de la forma mas completa posible.
%Idealmente recibe: Una lista de posibles combinaciones validas y la longitud de la linea.
%Salida esperada: Una linea con la mayor cantidad de casilleros resueltos.
%solveLine(+PosibilitiesList, -solvedLine, +lineLength)
solveLine(_ListaPosibilidades,LineaResuelta,-1):- 
	LineaResuelta = [],!.
solveLine(ListaPosibilidades,LineaResuelta,Long):-
	S is Long -1,
	solveLine(ListaPosibilidades,LineaAux,S),
	(forall(member(Posibilidad,ListaPosibilidades),
	nth0(Long,Posibilidad,"#")),
    append(LineaAux,["#"],LineaResuelta);
    forall(member(Posibilidad,ListaPosibilidades),
	nth0(Long,Posibilidad,"X")),
    append(LineaAux,["X"],LineaResuelta);
    append(LineaAux,[_],LineaResuelta)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Genera una lista con las posibilidades validas de una Fila.
%Idealmente recibe: Una fila y su pista asociada.
%Salida esperada: Una lista con las diferentes posibilidades validas de la fila.
%generatePosibilitiesList(+Row, +rowClue, -posibilitiesList)
generatePosibilitiesList(Fila,Pista,ListaPosibilidades):-
	findall(Fila,generatePosibility(Fila,Pista),ListaPosibilidades).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Genera las posibles combinaciones de una fila.
%Idealmente recibe: Una fila y una pista.
%Salida esperada: Distintas combinaciones validas de la fila dada.
%generatePosibility(+Row, +rowClue, -Return)
generatePosibility([],[]):-!.
generatePosibility(Fila,[PistaActual|Resto]):- 
	espaciar(Fila,FilaAux),
	addClue(FilaAux,FilaConPista,PistaActual),
    (Resto\=[], agregarEspacio(FilaConPista,FilaConPistayEspacio);
	Resto==[], espaciar(FilaConPista,FilaConPistayEspacio)),
    generatePosibility(FilaConPistayEspacio,Resto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Los siguientes tres predicados se encargan de generar distintas combinaciones dentro de la fila dada.
%Agrega a la fila un secuencia de '#' correspondientes a la pista.
addClue(Linea,Linea, 0).
addClue(["#"|Linea], RestoLinea, N):-
	N > 0,
	S is N - 1,
    addClue(Linea, RestoLinea, S).

%Genera espacios dentro la fila.
espaciar(Linea, Linea).
espaciar(["X"|Linea],RestoLinea) :- 
	espaciar(Linea, RestoLinea).

%Agrega un espacio individual dentro de la fila luego de haber agregado una pista.
agregarEspacio(["X"|T],T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Determina la longitud de una lista.
%
%longitud(+List,-Length).
longitud([],R):- 
	R is -1.
longitud([_H|T],R):- 
	longitud(T,Raux), 
	R is Raux + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Predicado para inicializar pistas: %
% Predicado principal
%
%compareGrid(+Grid, +SolvedGrid, -RowsClues, -ColumnsClues, +CantCol)
compareGrid(Grid, SolvedGrid, RowsClues, ColumnsClues, CantCol) :-
    compareRows(Grid, SolvedGrid, RowsClues),
    transposed(Grid, TransposedGrid, CantCol),
    transposed(SolvedGrid, TransposedSolvedGrid, CantCol),
    compareRows(TransposedGrid, TransposedSolvedGrid, ColumnsClues).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Comparar filas
%
%compareRows(+Row1, +Row2, +CluesList).
compareRows([], [], []).
compareRows([Row1|Rest1], [Row2|Rest2], [Clue|CluesRest]) :-
    (rows_equal(Row1, Row2) -> Clue = 1 ; Clue = 0),
    compareRows(Rest1, Rest2, CluesRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Comparar dos filas considerando "X" y "_" como equivalentes
%
%rows_equal(+row1,+row2).
rows_equal([], []).
rows_equal([H1|T1], [H2|T2]) :-
    equal_cells(H1, H2),
    rows_equal(T1, T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Comparar dos celdas considerando "X" y "_" como equivalentes
%
%equal_cells(+cell1, +cell2).
equal_cells(X, Y):- X == "#", Y =="#", !.
equal_cells(X, Y) :- X \== "#", Y\=="#".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
