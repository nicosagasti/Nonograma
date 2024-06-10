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


% Primer Caso Base:
%Este caso se cumple cuando ambas listas están vacías, el resultado es 1.
checkCluesRecursivo([], [], 0, 1). 

%Segundo Caso Base:
%Este caso se activa cuando hay una diferencia entre las pistas y la grilla, el resultado es 0.
checkCluesRecursivo([P | _], [X |_ ], L, 0):- 
    X \== "#",
    L \== 0,
    L \== P.

%Tercer Caso Base: 
%Este caso refleja cuando hay celdas marcadas con "#" de mas, si la lista de pistas está vacía, 
%el primer elemento de la cuadrícula es "#", y L es 0. El resultado es 0.
checkCluesRecursivo([], [X | _], L, 0):- 
    X == "#",
    L == 0.

%Cuarto Caso Base: 
%Este caso se cumple cuando hay solo un elemento en la lista de pistas, y la lista de la grilla está vacía.
checkCluesRecursivo([P],[],L,1):- 
    P == L.

%Quinto Caso Base:
%Cuando nos quedamos sin lista para consumir, pero seguimos teniendo Pistas, L != P
checkCluesRecursivo([P | _],[],L,0):- 
    P \== L.

%Sexto Caso Base:
%Cuando nos quedamos sin lista para consumir, seguimos teniendo Pistas pero L == P
checkCluesRecursivo([P | _],[],L,0):- 
    P == L.

% Casos Recursivos:

%Primer Caso Recursivo:
%Si la lista de pistas está vacía y el primer elemento de la lista no es "#", 
%se continúa con el resto de la lista.
checkCluesRecursivo([],[X |Xs],0, Return):-
    X \== "#",
    checkCluesRecursivo([],Xs,0,Return).

%Segundo Caso Recursivo:
%Si el primer elemento de la lista no es "#", P no coincide con el contador, 
%se continúa con el resto de las pistas y la lista reiniciando el contador.
checkCluesRecursivo([P|Ps], [X | Xs], Count, Return) :-
    X \== "#", 
    P \== Count,
    checkCluesRecursivo([P|Ps], Xs, 0, Return).

%Tercer Caso Recursivo:
%Si el primer elemento de la lista no es "#", P coincide con el contador, 
%se continúa con el resto de las pistas y la lista reiniciando el contador.
checkCluesRecursivo([P | Ps], [X | Xs], Count, Return) :-
    X \== "#", 
    P == Count,
    checkCluesRecursivo(Ps, Xs, 0, Return).

% Caso en el que X es #
%Cuarto Caso Recursivo:
%Si el primer elemento de la lista es "#", 
%se incrementa el contador y se continúa con el resto de las pistas y la lista.
checkCluesRecursivo([P | Ps], [X | Xs], Count, Return) :-
    X == "#", 
    CountN is Count + 1,
    checkCluesRecursivo([P | Ps], Xs, CountN, Return).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkClues(+ClueList, +GridList, -Satisfied)
%
%Método cascara: Verifica si la lista de pistas se satisface
%llama al predicado recursivo con la lista de pistas y una lista de la grilla, 
%se inicia el contador en 0 y se devuelve el resultado en Satisfied
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
%Lee una grilla y devuelve una columna en forma de lista.
%
% listarCol(+Grid, +Pos, -List).
listarCol([],_Pos,[]).

listarCol([H|T],Pos,Lista):- 
	nth0(Pos,H,Elemento),
    listarCol(T,Pos,ListaAux),
    addElement(Elemento,ListaAux,Lista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Caso base: 
%se utiliza para finalizar la generación de la pista cuando no quedan más celdas por procesar.
generateClue([],N,Clue,Rest):- 
	Rest = [],Clue = [N].

%Caso Recursivo: 
%se utiliza para procesar cada celda de la lista, incrementando el contador cuando encuentra un "#" 
%y finalizando la pista cuando encuentra cualquier otro símbolo.
generateClue([H|T],N,Clue,Rest):- 
	H == "#",
	S is N+1, generateClue(T,S,Clue,Rest);
    Rest = T, Clue = [N].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Genera una lista de pistas a traves de una fila de una matriz.
%
%generateCluesList(+Row, -CluesList).

%Caso Base: 
%se utiliza para finalizar la generación de pistas cuando no quedan más celdas por procesar.
generateCluesList([],[]):- !. 

generateCluesList(Row,ClueList):- 
	Row \==[], generateClue(Row,0,Clue,Rest),
	generateCluesList(Rest,AuxList),
    Clue = [H], 
	(H \== 0, addElement(H,AuxList,ClueList); ClueList= AuxList).

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
solveGrid(RClues,CCLues,Grid,ResGrid,CantR,CantC):-  
    solveRows(RClues,Grid,AuxGrid,CantR),
    transposed(AuxGrid,TransposedGrid,CantC),
    solveRows(CCLues,TransposedGrid,AuxGrid2,CantC),
    transposed(AuxGrid2,ResGridAux2,CantR),
    verifyRows(RClues,ResGridAux2,Sat,CantR),
    (Sat = true,ResGrid = ResGridAux2;
	solveGrid(RClues,CCLues,ResGridAux2,ResGrid,CantR,CantC)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Verifica que cada fila de una grilla respeta las pistas dadas.
%
%verifyRows(+CluesList, +Grid, +numRows, -Return ).
verifyRows([],[],Sat,-1):- 
	Sat = true,!.
verifyRows([P|RestP],[F|RestF],Sat,N):- 
	S is N-1, 
	generateCluesList(F,Clue),
	P = Clue, 
	verifyRows(RestP,RestF,Sat,S); 
	Sat = false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                               
%Genera la mejor respuesta posible de una grilla.
%
%solveRows(+CluesList, +Grid, -ResGrid, +numRows/Cols).

%Caso Base
solveRows(_RClues,Grid,ResGrid,-1):-
	ResGrid = Grid,!.

%Caso Recursivo:
%obtiene la pista y la fila actual, genera las posibles combinaciones válidas, resuelve la fila, 
%reemplaza la fila en la grilla.
solveRows(RClues,Grid,ResGrid,Pos):-
	nth0(Pos,RClues,Clue),
	nth0(Pos,Grid,Row),
    generatePosibilitiesList(Row,Clue,PosibilitiesList),
	longitud(Row,Long),
    solveLine(PosibilitiesList,ModifiedList,Long),
    replace(_Row,Pos,ModifiedList,Grid,ModifiedGrid),
    S is Pos-1,
	solveRows(RClues,ModifiedGrid,ResGrid,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Genera la grilla traspuesta de una grilla dada.
%
%transposed(+Grid, -transposedGrid, +numRows).
transposed(_Grid,TransposedGrid,-1):-
	TransposedGrid = [],!.
transposed(Grid,TransposedGrid,Pos):-
	listarCol(Grid,Pos,Col),
	S is Pos-1,
	transposed(Grid,AuxGrid,S), 
	addAtEnd(Col,AuxGrid,TransposedGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%resuelve una línea dada una lista de posibles combinaciones válidas para esa línea.
%
%solveLine(+PosibilitiesList, -solvedLine, +lineLength)

%Caso Base
solveLine(_PosibilitiesList,SolvedLine,-1):- 
	SolvedLine = [],!.

%Caso Recursivo: 
solveLine(PosibilitiesList,SolvedLine,Length):-
	S is Length -1,
	solveLine(PosibilitiesList,AuxLine,S),
	(forall(member(Posibility,PosibilitiesList),
	nth0(Length,Posibility,"#")),
    append(AuxLine,["#"],SolvedLine);   %Si todas las combinaciones en PosibilitiesList tienen "#", agrega "#"
    forall(member(Posibility,PosibilitiesList),
	nth0(Length,Posibility,"X")),
    append(AuxLine,["X"],SolvedLine);   %Si todas las combinaciones en PosibilitiesList tienen "X", agrega "X"
    append(AuxLine,[_],SolvedLine)).   %Si no se puede determinar, agrega "_"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Genera una lista con las posibilidades validas de una Fila.
%utiliza findall/3 para encontrar todas las configuraciones de Row que son generadas por generatePosibility/3 
%y las almacena en PosibilitiesList.
%generatePosibilitiesList(+Row, +rowClue, -posibilitiesList)
generatePosibilitiesList(Row,Clue,PosibilitiesList):-
	findall(Row,generatePosibility(Row,Clue),PosibilitiesList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Genera las posibles combinaciones de una fila de acuerdo a una pista.
%
%generatePosibility(+Row, +rowClue, -Ret)

%Caso Base: 
%Se activa cuando ambas listas están vacías (se alcanzó el final)
generatePosibility([],[]):-!.

%Caso Recursivo:
generatePosibility(Row,[ActualCLue|Rest]):- 
	toSpace(Row,AuxRow),
	addClue(AuxRow,RowWithClue,ActualCLue),  %agrega una secuencia de celdas marcadas
    (Rest\=[], addSpace(RowWithClue,RowWithClueAndSpace); %si hay mas pistas, agrega un espacio
	Rest==[], toSpace(RowWithClue,RowWithClueAndSpace)),
    generatePosibility(RowWithClueAndSpace,Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Los siguientes tres predicados se encargan de generar distintas combinaciones dentro de la fila dada.
%Agrega a la fila un secuencia de '#' correspondientes a la pista.
addClue(Line,Line, 0).
addClue(["#"|Line], LineRest, N):-
	N > 0,
	S is N - 1,
    addClue(Line, LineRest, S).

%Genera espacios dentro la fila.
toSpace(Line, Line).
toSpace(["X"|Line],LineRest) :- 
	toSpace(Line, LineRest).

%Agrega un espacio individual dentro de la fila luego de haber agregado una pista.
addSpace(["X"|T],T).

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
% Predicado para inicializar pistas: 
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
