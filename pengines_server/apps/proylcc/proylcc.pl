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

% choose(+Index, +List, -Element)
% Selecciona el elemento en la posición Index de la lista List.
choose(Index, List, Element) :-
    nth1(Index, List, Element).

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

    %% ver si se verfican rowSat y colSat
    choose(RowN, RowsClues, RC),
    choose(ColN, ColsClues, CC),
    checkMatches(NewGrid, RC, CC, [RowN, ColN], RowSat, ColSat).


checkMatches(Grid, RClues, CClues, [RowN, ColN], RowSat, ColSat):-
    choose(RowN, Grid, NewRow),
    checkClues(RClues, NewRow, RowSat),

    transpose(Grid, TransposedGrid),
    choose(ColN,TransposedGrid, NewCol),
    checkClues(CClues, NewCol, ColSat).

% Verificar si las pistas de una fila se cumplen
checkClues(Clues, ListClues, Satisfied) :-
    (Clues = ListClues ->
        Satisfied is 1
    ;
        Satisfied is 0
    ).
    % length(Clues, NumClues),
    % length(ListClues, NumCompressedCells),
    % NumClues =:= NumCompressedCells,
    % checkClues(ListClues, Clues, 1, RowSat).

checkClues([], [], _, yes)
checkClues([Cell|RestRow], [Clue|RestClues], N, RowSat) :-
    length(Cell, Clue),
    NextN is N + 1,
    checkClues(RestRow, RestClues, NextN, RowSat).

% Base case: Transposing an empty list results in an empty list.
transpose([], []).

% Transpose non-empty grid
transpose([[]|_], []) :- !. % Ensure the transposed grid has no empty rows
transpose(Grid, [FirstCol|RestTransposed]) :-
    transpose_rows(Grid, FirstCol, RestGrid),
    transpose(RestGrid, RestTransposed).

% Helper predicate to transpose rows
transpose_rows([], [], []).
transpose_rows([[X|Xs]|RestRows], [X|FirstCol], [Xs|RestCols]) :-
    transpose_rows(RestRows, FirstCol, RestCols).
