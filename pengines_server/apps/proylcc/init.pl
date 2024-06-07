:- module(init, [ init/3 ]).

/**
 * init(-RowsClues, -ColsClues, Grid).
 * Predicate specifying the initial grid, which will be shown at the beginning of the game,
 * including the rows and columns clues.
 */
/*init(
    [[7], [9], [3,2], [2,2], [2], [3,2], [4,4], [3,2], [1,1], [3]], % PistasFilas
    [[2], [2,5], [3,3,1], [4,4], [4], [2], [2,1], [2,1], [7], [7]], % PistasColumnas
    [[ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ]]
). */

/*init(
	[[3,3],[1,3,3,1],[3,10,2],[3,2,2,2],[5,4],[3,2],[1,1],[1,1],[2,1],[2,1,1,1],[3,3,3,1],[1,1,1,1,1,1,1],[1,1,3,3,1],[1,2,2,1],[1,2,2],[1,2,3,2],[1,2,3,3],[1,2,1,4],[1,1,2,4],[3,1,10]],
[[2,1,1,2],[1,1,1,1],[4,7,1],[1,4,4,2,1],[1,5,2,1],[2,2,2],[4,2],[3,4,1],[1,2,2,1],[1,3,1],[1,2,1],[1,3,1],[1,2,1],[1,3,1],[3,2,2,2],[5,4,3],[2,2,4],[1,5,5],[4,4,4],[4]],
			[[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
		 	 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
		 	 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
			 [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]


). */

/*init(
[[1,1], [2,2], [3,2,3], [8], [8], [1,2,1], [2,2,2], [8], [2,2], [4]],    % PistasFilas

[[3], [4,2], [7], [2,3], [6,1], [6,1], [2,3], [7], [4,2], [3]],     % PistasColumnas

[[ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
 [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
 [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],        % Grilla
 [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
 [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
 [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
 [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
 [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
 [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
 [ _ , _ , _ , _ , _ , _ , _ , _ , _ , _ ]
]
).*/

init(
    [[3], [1,2], [4], [5], [5]], % PistasFilas
    [[2], [5], [1,3], [5], [4]], % PistasColumnas
    [["X","#","#","#", _ ],
    [ _ ,"#","X","#","#"],
    ["X","#","#","#","#"], % Grilla
    ["#","#","#","#","#"],
    ["#","#","#", _ ,"#"]]
    ).