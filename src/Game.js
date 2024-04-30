import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [toggleChecked, setToggleChecked] = useState(false);

  const [completedColumnsClues, setCompletedColumnsClues] = useState([]);
  const [completedRowsClues, setCompletedRowsClues] = useState([]);

  const [gameWonStatus, setGameWonStatus] = useState(false);

  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);
  }, []);

  function handleServerReady(instance) {
    pengine = instance;
    const queryS = 'init(RowClues, ColumClues, Grid)';

    // Recibe la instancia de Prolog y utiliza la consulta init(RowsClues, ColsClues, Grid) para obtener las pistas de filas y columnas del tablero del juego
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);

        // Inicializar las completedRowsClues con ceros
        const initialCompletedRowsClues = Array(response['RowClues'].length).fill(0);
        setCompletedRowsClues(initialCompletedRowsClues);

        // Inicializar las completedColumnsClues con ceros
        const initialCompletedColumnsClues = Array(response['ColumClues'].length).fill(0);
        setCompletedColumnsClues(initialCompletedColumnsClues);

        initializeClues(response['Grid'], response['RowClues'], response['ColumClues']);
      }
    });
  }

  function initializeClues(Grid, RowClues, ColumnClues) {
    let rowsLength = RowClues.length;
    let colsLength = ColumnClues.length;
    let diagonalLength = Math.min(rowsLength, colsLength);

    const squaresS = JSON.stringify(Grid).replaceAll('""', '');
    const rowCluesS = JSON.stringify(RowClues);
    const colCluesS = JSON.stringify(ColumnClues);

    let rowAux = new Array(rowsLength).fill(0);
    let colAux = new Array(colsLength).fill(0);

    // Recorrer la diagonal de la matriz cuadrada
    for (let i = 0; i < diagonalLength; i++) {
      const queryA = `checkGrid(${squaresS}, ${rowCluesS}, ${colCluesS}, [${i}, ${i}], RowSat, ColSat)`;

      setWaiting(true);
      pengine.query(queryA, (succes, response) => {
        if (succes) {
          rowAux[i] = response['RowSat'];
          colAux[i] = response['ColSat'];

          setCompletedRowsClues([...rowAux]);
          setCompletedColumnsClues([...colAux]);
        }
        setWaiting(false);
      });
    }

    // Continuar recorriendo el resto de la matriz
    if (rowsLength > colsLength) {
      // Iterar sobre las filas restantes
      for (let i = diagonalLength; i < rowsLength; i++) {
        for (let j = 0; j < colsLength; j++) {
          const queryA = `checkGrid(${squaresS}, ${rowCluesS}, ${colCluesS}, [${i}, ${j}], RowSat, ColSat)`;
          setWaiting(true);

          pengine.query(queryA, (success, response) => {
            if (success) {
              rowAux[i] = response['RowSat'];
              colAux[j] = response['ColSat'];
              setCompletedRowsClues([...rowAux]);
              setCompletedColumnsClues([...colAux]);
            }
            setWaiting(false);
          });
        }
      }
    } else if (rowsLength < colsLength) { 
      // Iterar sobre las columnas restantes
      for (let j = diagonalLength; j < colsLength; j++) {
        for (let i = 0; i < rowsLength; i++) {
          const queryA = `checkGrid(${squaresS}, ${rowCluesS}, ${colCluesS}, [${i}, ${j}], RowSat, ColSat)`;
          setWaiting(true);

          pengine.query(queryA, (success, response) => {
            if (success) {
              rowAux[i] = response['RowSat'];
              colAux[j] = response['ColSat'];
              setCompletedRowsClues([...rowAux]);
              setCompletedColumnsClues([...colAux]);
            }
            setWaiting(false);
          });
        }
      }
    }
  }

  function gameWon(completedRows, completedCols) {
    const RowAuxValues = JSON.stringify(completedRows);
    const ColAuxValues = JSON.stringify(completedCols);

    const queryS1 = `checkWon(${RowAuxValues},${ColAuxValues},Result)`;
    setWaiting(true);
    pengine.query(queryS1, (success, response) => {
      if (success) {
        setGameWonStatus(response['Result']);
      }
      setWaiting(false);
    });
  }

  function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting || gameWonStatus) {
      return;
    }

    const content = toggleChecked ? "#" : "X";

    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_');

    const rowCluesS = JSON.stringify(rowsClues);
    const colCluesS = JSON.stringify(colsClues);

    const queryS = `put("${content}", [${i},${j}], ${rowCluesS}, ${colCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`;
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {

        let rowAux = [...completedRowsClues];
        let colAux = [...completedColumnsClues];

        rowAux[i] = response['RowSat'];
        colAux[j] = response['ColSat'];

        //Si se cumplen todas las pistas se gana el juego.
        gameWon(rowAux, colAux);

        setCompletedRowsClues(rowAux);
        setCompletedColumnsClues(colAux);

        // updateCompletedClues("row", i, response['RowSat']);
        // updateCompletedClues("col", j, response['ColSat']);

        setGrid(response['ResGrid']);
      }
      setWaiting(false);
    });

  }

  function updateCompletedClues(type, i, completed) {
    let cluesAux = type === "row" ? [...completedRowsClues] : [...completedColumnsClues];
    cluesAux[i] = completed;
    type === "row" ? setCompletedRowsClues(cluesAux) : setCompletedColumnsClues(cluesAux);

  }

  if (!grid) {
    return null;
  }

  const statusText = gameWonStatus ? 'You Won!' : 'Keep playing!';

  return (
    <div className="game">
      <Board
        grid={grid}
        rowsClues={rowsClues}
        colsClues={colsClues}
        completedColumnsClues={completedColumnsClues}
        completedRowsClues={completedRowsClues}
        onClick={(i, j) => handleClick(i, j)}
        gameWon={gameWonStatus}
      />
      <div className="game-info">
        <button className={`toggle-btn ${toggleChecked ? 'toggled' : ''}`}
          onClick={() => setToggleChecked(!toggleChecked)}>
          <div className="thumb"></div>
        </button>
      </div>
      {statusText}
    </div>
  );

}

export default Game;