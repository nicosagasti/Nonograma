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

% getClue(+Index, +List, -Element)
% Selecciona el elemento en la posición Index de la lista List.
getClues(Index, List, Element) :-
    nth0(Index, List, Element).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
checkCluesRecursivo([], [], 0, 1). % Primer Caso Base
checkCluesRecursivo([P | _], [X |_ ], L, 0):- % Segundo Caso Base
    X \== #,
    L \== 0,
    L \== P.

checkCluesRecursivo([], [X | _], L, 0):- % Tercer Caso Base => cuando hay marcados de mas
    X == #,
    L == 0.

checkCluesRecursivo([], [X | _], L, 0):-  % Caso base ,que no funciona sin el L == 0
    X \== #,
    L == 0.
    
checkCluesRecursivo([P | _],[],L,1):- % Cuarto Caso Base => Cuando era el ultimo y L es P 
    P == L.

checkCluesRecursivo([P | _],[],L,0):- % Quinto Caso Base => Cuando nos quedamos sin lista para consumir, pero seguimos teniendo Pistas, L != P
    P \== L.

% Casos Recursivos:
checkCluesRecursivo([P|Ps], [X | Xs], Count, Return) :-
    X \== #, 
    P \== Count,
    checkCluesRecursivo([P|Ps], Xs, 0, Return).

% Caso en el que X no es #
checkCluesRecursivo([P | Ps], [X | Xs], Count, Return) :-
    X \== #, 
    P == Count,
    checkCluesRecursivo(Ps, Xs, 0, Return).

% Caso en el que X es #
checkCluesRecursivo([P | Ps], [X | Xs], Count, Return) :-
    X == #, 
    CountN is Count + 1,
    checkCluesRecursivo([P | Ps], Xs, CountN, Return).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
checkClues(Element, NewList, Satisfied) :-
    checkCluesRecursivo(Element, NewList, 0, Satisfied).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkGrid(+Grid, +RowNElement, +ColNElement, +[RowN, ColN], -RowSat, -ColSat)
% 
checkGrid(Grid, RowNElement, ColNElement, [RowN, ColN], RowSat, ColSat):-
    % Obtenemos las filas de la Grilla  
    getClues(RowN, Grid, NewRow),
    getClues(ColN, Grid, NewCol),

    % Chequeamos si verifica con las Rows
    checkClues(RowNElement, NewRow, RowSat),

    % Chequeamos si verifica con las Cols
    checkClues(ColNElement, NewCol, ColSat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%
put(Content, [RowN, ColN], _RowsClues, _ColsClues, Grid, NewGrid, RowSat, ColSat):-
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

    % Obtenemos las pistas de la lista de Pistas
    getClues(RowN, RowsClues, RowNElement),
    getClues(ColN, ColsClues, ColNElement),

    %% Ver si se verifican RowSat y ColSat
    checkGrid(NewGrid, RowNElement, ColNElement, [RowN, ColN], RowSat, ColSat).
