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
	replace(_Cell, ColN, Content, Row, NewRow)).


% Ejemplo chatgpt

/* put(Content, [RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, FilaSat, ColSat) :-
    % NewGrid es el resultado de reemplazar la fila Row en la posición RowN de Grid por una nueva fila NewRow.
    replace(Row, RowN, NewRow, Grid, NewGrid),

    % NewRow es el resultado de reemplazar la celda Cell en la posición ColN de Row por Content si Cell coincide con Content,
    % de lo contrario, reemplaza la celda en la posición ColN de Row por Content sin importar su contenido.
    (   replace(Cell, ColN, _, Row, NewRow),
        Cell == Content ->
        FilaSat = 0
    ;   replace(_, ColN, Content, Row, NewRow),
        FilaSat = 1
    ),

    % Determinar la satisfacción de la fila RowN en la nueva cuadrícula.
    row_satisfied(NewRow, RowN, RowsClues, FilaSat),

    % Determinar la satisfacción de la columna ColN en la nueva cuadrícula.
    nth1(ColN, NewRow, NewCell),
    col_satisfied(NewCell, ColN, ColsClues, ColSat). */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Este es el put/7 => debemos implementar
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -RowSat, -ColSat).
%

put(Content, [RowN, ColN], RowsClues, ColsClues, Grid, RowSat, ColSat):-
	row_sat_satisfied(RowN, RowsClues, Grid, RowSat), % Verificamos si se cumplen la propiedad de las filas
	col_sat_satisfied(ColN, ColsClues, Grid, ColSat). % Verificamos si se cumplen la propiedad de las columnas

% Verificar si RowSat se cumple para una fila específica
row_sat_satisfied(RowN, RowClues, Grid, RowSat) :-
    nth1(RowN, Grid, Row), % Obtener la fila correspondiente
    row_satisfied(Row, RowClues, RowSat).

% Verificar si ColSat se cumple para una columna específica
col_sat_satisfied(ColN, ColClues, Grid, ColSat) :-
    transpose(Grid, TransposedGrid), % Transponer la cuadrícula para tratar las columnas como filas
    row_sat_satisfied(ColN, ColClues, TransposedGrid, ColSat).

% Verificar si las pistas de una fila se cumplen
row_satisfied(Row, RowClues, RowSat) :-
    compressRow(Row, CompressRow),
    check_clues(CompressRow, RowClues, RowSat).

% Verificar si las pistas de una fila se cumplen
check_clues(CompressRow, Clues, RowSat) :-
    length(Clues, NumClues),
    length(CompressRow, NumCompressedCells),
    NumClues =:= NumCompressedCells,
    check_clues(CompressRow, Clues, 1, RowSat).

check_clues([], [], _, yes).
check_clues([Cell|RestRow], [Clue|RestClues], N, RowSat) :-
    length(Cell, Clue),
    NextN is N + 1,
    check_clues(RestRow, RestClues, NextN, RowSat).

% Caso base: comprimir una lista vacía resulta en una lista vacía.
compressRow([], []).

% Caso en el que la cabeza de la lista es un espacio vacío ('_').
% Ignoramos la secuencia de espacios vacíos y continuamos con el resto de la fila.
compressRow(['_' | Resto], Comprimida) :-
    compressRow(Resto, Comprimida).

% Caso en el que la cabeza de la lista es una celda llena ('#').
% Comenzamos una nueva secuencia de celdas llenas.
compressRow(['#' | Resto], Comprimida) :-
    append([ '#' | Secuencia ], Resto, RestoSinEncabezado),
    length([ '#' | Secuencia ], Longitud),
    Comprimida = [(Longitud, '#') | RestoComprimida],
    compressRow(RestoSinEncabezado, RestoComprimida). 

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

