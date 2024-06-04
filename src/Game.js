import React, { useEffect, useState } from "react";
import PengineClient from "./PengineClient";
import Board from "./Board";

let pengine;

function Game() {
  // State
  const [grid, setGrid] = useState(null);
  const [solvedGrid, setSolvedGrid] = useState(null);
  const [originalGrid, setOriginalGrid] = useState([]);

  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);

  const [waiting, setWaiting] = useState(false);
  const [toggleChecked, setToggleChecked] = useState(false);

  const [showHintMode, setShowHintMode] = useState(false);

  const [showSolvedGridMode, setShowSolvedGridMode] = useState(false);

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
    const queryS = "init(RowClues, ColumClues, Grid)";
    setWaiting(true);
    // Recibe la instancia de Prolog y utiliza la consulta init(RowsClues, ColsClues, Grid) para obtener las pistas de filas y columnas del tablero del juego
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response["Grid"]);
        setRowsClues(response["RowClues"]);
        setColsClues(response["ColumClues"]);

        // Inicializar las completedRowsClues con ceros
        const initialCompletedRowsClues = Array(
          response["RowClues"].length
        ).fill(0);
        setCompletedRowsClues(initialCompletedRowsClues);

        // Inicializar las completedColumnsClues con ceros
        const initialCompletedColumnsClues = Array(
          response["ColumClues"].length
        ).fill(0);
        setCompletedColumnsClues(initialCompletedColumnsClues);
        const squaresS = JSON.stringify(response["Grid"]).replaceAll(
          '"_"',
          "_"
        );

        const rowCluesS = JSON.stringify(response["RowClues"]);
        const colCluesS = JSON.stringify(response["ColumClues"]);

        const numFilas = JSON.stringify(response["RowClues"].length - 1); 
        const numCols = JSON.stringify(response["ColumClues"].length - 1);

        //Hacemos la query para conseguir la grilla resuelta
        const queryA = `solveGrid(${rowCluesS}, ${colCluesS}, ${squaresS}, GrillaResueltaAux, ${numFilas}, ${numCols})`;
        pengine.query(queryA, (success, response) => {
          if (success) {
            setSolvedGrid(response["GrillaResueltaAux"]);

            //Hacemos la query para veirficar que pistas se encuentran resueltas
            const solvedGridAux = JSON.stringify(response["GrillaResueltaAux"]);
            const queryB = `compareGrid(${squaresS}, ${solvedGridAux}, RowsClues, ColumnsClues)`;
            pengine.query(queryB, (succes, response) => {
              if (succes) {
                setCompletedRowsClues(response["RowsClues"]);
                setCompletedColumnsClues(response["ColumnsClues"]);

                //Vericamos si el juego ya esta ganado
                gameWon(response["RowsClues"], response["ColumnsClues"]);
              }
              setWaiting(false);
            });
          } 
          setWaiting(false);
        });
      }
    });
  }

  function gameWon(completedRows, completedCols) {
    const RowAuxValues = JSON.stringify(completedRows);
    const ColAuxValues = JSON.stringify(completedCols);

    const queryS1 = `checkWon(${RowAuxValues},${ColAuxValues},Result)`;
    setWaiting(true);
    pengine.query(queryS1, (success, response) => {
      if (success) {
        setGameWonStatus(response["Result"]);
      }
      setWaiting(false);
    });
  }

  function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting || gameWonStatus || showSolvedGridMode) {
      return;
    }

    if (showHintMode) {
      if (grid[i][j] == null) {
        const hintValue = solvedGrid[i][j];

        // Actualizamos la grilla con el valor de pista
        const newGrid = [...grid];
        newGrid[i][j] = hintValue;
        setGrid(newGrid);
      }
      // Desactivamos el modo de pistas
      setShowHintMode(false);
      return;
    }

    const content = toggleChecked ? "#" : "X";

    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.
    const squaresS = JSON.stringify(grid).replaceAll('"_"', "_");

    const rowCluesS = JSON.stringify(rowsClues);
    const colCluesS = JSON.stringify(colsClues);

    const queryS = `put("${content}", [${i},${j}], ${rowCluesS}, ${colCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`;
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        let rowAux = [...completedRowsClues];
        let colAux = [...completedColumnsClues];

        rowAux[i] = response["RowSat"];
        colAux[j] = response["ColSat"];

        //Si se cumplen todas las pistas se gana el juego.
        gameWon(rowAux, colAux);

        setCompletedRowsClues(rowAux);
        setCompletedColumnsClues(colAux);

        setGrid(response["ResGrid"]);
      }
      setWaiting(false);
    });
  }

  function handleShowHint() {
    setShowHintMode(true);
  }

  function handleSolvedGrid() {
    if (showSolvedGridMode) {
      // Si el modo de mostrar la grilla resuelta está activado, ocultamos la grilla resuelta
      setGrid(originalGrid); // Volvemos a la grilla original
    } else {
      // Si el modo de mostrar la grilla resuelta está desactivado, mostramos la grilla resuelta
      const gridCopy = grid.map((row) => [...row]); // Hacer una copia profunda de la grilla actual
      setOriginalGrid(gridCopy);
      const newGrid = [...grid];

      for (let i = 0; i < rowsClues.length; i++) {
        for (let j = 0; j < colsClues.length; j++) {
          newGrid[i][j] = solvedGrid[i][j];
        }
      }
      setGrid(newGrid); // Establecemos la grilla resuelta
    }
    setShowSolvedGridMode(!showSolvedGridMode);
  }

  if (!grid) {
    return null;
  }

  const statusText = gameWonStatus ? "You´ve Won!" : "Keep playing!";

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
        <button
          className={`toggle-btn ${toggleChecked ? "toggled " : ""}`}
          onClick={() => setToggleChecked(!toggleChecked)}
        >
          <div className="thumb"></div>
        </button>
        <button className="hint-button" onClick={handleShowHint}>
          Show Hint
        </button>
        <button
          className="solve-button"
          onClick={() => {
            handleSolvedGrid();
          }}
        ></button>
      </div>
      <div
        style={{
          fontSize: "30px",
          textAlign: "center",
          margin: "5px",
          position: "relative",
          left: "5px",
          marginTop: "150px",
        }}
      >
        {statusText}
      </div>
    </div>
  );
}

export default Game;
