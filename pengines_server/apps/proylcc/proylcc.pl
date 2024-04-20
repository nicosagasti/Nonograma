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

    % Obtenemos las pistas REALES
    getClues(RowN, RowsClues, RowElement),
    getClues(ColN, ColsClues, ColElement),
    %% Ver si se verifican RowSat y ColSat
    checkGrid(NewGrid, RowElement, ColElement, [RowN, ColN], RowSat, ColSat).

checkGrid(Grid, RowElement, ColElement, [RowN, ColN], RowSat, ColSat):-
    % Obtenemos las filas de la Grilla  
    getClues(RowN, Grid, NewRow),
    getClues(ColN, Grid, NewCol),

    % Chequeamos si verifica con las Rows
    checkClues(RowElement, NewRow, RowSat),

    % Chequeamos si verifica con las Cols
    checkClues(ColElement, NewCol, ColSat).

checkClues(Element, NewList, Satisfied) :-
    checkCluesRecursivo(Element, NewList, 0, Satisfied).

checkCluesRecursivo([], [], 0, 1). % Primer Caso Base
checkCluesRecursivo([P | ], [X | ], L, 0):- % Segundo Caso Base
      X == #,
    L == 0,
    L == P.

checkCluesRecursivo([], [X], L, 0):- % Tercer Caso Base => cuando hay marcados de mas
    X == "#",
    L == 0.

checkCluesRecursivo([P],[],L,1):-
    P==L.

% Casos Recursivos:
checkCluesRecursivo([P|Ps], [X | Xs], Count, Return) :-
    X == #, 
    P == Count,
    checkCluesRecursivo([P|Ps], Xs, 0, Return).

% Caso en el que X no es #
checkCluesRecursivo([P | Ps], [X | Xs], Count, Return) :-
    X == #, 
    P == Count,
    checkCluesRecursivo(Ps, Xs, 0, Return).

% Caso en el que X es #
checkCluesRecursivo([P | Ps], [X | Xs], Count, Return) :-
    X == #, 
    CountN is Count + 1,
    checkCluesRecursivo([P | Ps], Xs, CountN, Return).
