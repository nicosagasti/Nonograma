:- module(init, [ init/3 ]).

/**
 * init(-RowsClues, -ColsClues, Grid).
 * Predicate specifying the initial grid, which will be shown at the beginning of the game,
 * including the rows and columns clues.
 */

init(
[[3], [1,2], [4], [5], [5]], % PistasFilas
[[2], [5], [1,3], [5], [4]], % PistasColumnas
[["X", _ , _ , _ , _ ],
[_ , _ ,"X" , _ , _ ],
["X", _ , _ , _ , _ ], % Grilla
["#","#","#", _ , _ ],
[ _ , _ ,"#","#","#"]
]
).  