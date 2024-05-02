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
% Adds an element to the beginning of a list
addElement(X,[],R)   :- 
    R = [X].
addElement(X,[Head|Tail],R):- 
    R = [X,Head|Tail].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getValuesFromColumn(+GridList, +ColumnIndex, -ColumnValues)
%
% Extracts the values from a specific column in a grid represented as a list of lists.
getValuesFromColumn([],_Pos,List)  :- 
    List = [].
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
     (Res1 == 0 -> Res = 0 ; (Res1 == Res2 -> Res = 1 ; Res = 0)).