import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

import Switch from '@mui/material/Switch';
import Box from '@mui/material/Box';

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

        //Inicializar las completedColumnsClues
        for(let i = 0 ; i< response['RowClues'].length ; i++){
          completedRowsClues[i] = 0;
        }

        //Inicializar las completedRowsClues
        for(let i=0; i<response['ColumClues'].length; i++){
          completedColumnsClues[i] = 0;
        }

        procesarPreGrilla(response['Grid'], response['RowClues'], response['ColumClues']);
      }  
    });
  }

    function procesarPreGrilla(Grid, RowClues, ColumnClues){
      let rowsLength = RowClues.length;
      let colsLength = ColumnClues.length;
      let diagonalLength = Math.min(rowsLength, colsLength);

      // Recorrer la diagonal de la matriz
      for (let i = 0; i < diagonalLength; i++) {
        seCumplePista(Grid, RowClues, ColumnClues, i,i);
      }

      // Continuar recorriendo el resto de la matriz
      for (let i = diagonalLength; i < rowsLength; i++) {
        for (let j = diagonalLength; j < colsLength; j++) {
          seCumplePista(Grid, RowClues, ColumnClues, i,j);
        }
      }
    }

  function seCumplePista(Grid, RowsClues, ColsClues, i, j){
    if(waiting){
      return;
    }

    const squaresS = JSON.stringify(Grid).replaceAll('"_"', '_');
    const rowCluesS = JSON.stringify(RowsClues);
    const colCluesS = JSON.stringify(ColsClues);

    const queryA = `checkGrid(${squaresS}, ${rowCluesS}, ${colCluesS}, [${i}, ${j}], RowSat, ColSat)`;

    setWaiting(true);
    pengine.query(queryA, (success, response) => {
      if (success) {
        updateCompletedClues("row", i, response['RowSat']);
        updateCompletedClues("col", j, response['ColSat']);
      }
      setWaiting(false);
    });
  }

  function gameWon(RowAux,ColAux){
    const RowAuxValues= JSON.stringify(RowAux);
    const ColAuxValues= JSON.stringify(ColAux);

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

    const queryS = `put("${content}", [${i},${j}], ${rowCluesS}, ${colCluesS},${squaresS}, ResGrid, RowSat, ColSat)`; 
    
    setWaiting(true);

    pengine.query(queryS, (success, response) => {
      if (success) {
        updateCompletedClues("row",i, response['RowSat']);
        updateCompletedClues("col",j, response['ColSat']);
        
        //Si se cumplen todas las pistas se gana el juego.
        gameWon(completedRowsClues,completedColumnsClues);
        setGrid(response['ResGrid']);

      } 
      setWaiting(false);
    });
    
  }

  function updateCompletedClues(type, i, completed){
    let cluesAux;
    if(type === "row"){
      cluesAux = [...completedRowsClues];
    }
    else{
      cluesAux = [...completedColumnsClues];
    }

    cluesAux[i] = completed;

    if (type === "row"){
      setCompletedRowsClues(cluesAux);
    }
    else{
      setCompletedColumnsClues(cluesAux);
    }
  }

  if (!grid) {
    return null;
  }

  const statusText = gameWonStatus? 'You Won!' :'Keep playing!';
  return (
    <div className="game">
      <Board
        grid={grid}
        rowsClues={rowsClues}
        colsClues={colsClues}
        completedColumnsClues={completedColumnsClues}
        completedRowsClues={completedRowsClues}
        onClick={(i, j) => handleClick(i, j)}
      />
      <div className="game-info">
        <Box display="flex" alignItems="center">
          <div style={{ marginRight: '10px', backgroundColor: toggleChecked ? 'black' : 'transparent', color: toggleChecked ? 'black' : 'black' }}>{toggleChecked ? '#' : 'X'}</div>
          <Switch toggleSwitch// Agregar el componente de interruptor de palanca
            checked = {toggleChecked}
            onChange = {() => setToggleChecked(!toggleChecked)}
            color = "primary"
            inputProps = {{ 'aria-label': 'toggle checkbox' }}
          />
        </Box>
        {statusText}
      </div>
    </div>
  );

}

export default Game;