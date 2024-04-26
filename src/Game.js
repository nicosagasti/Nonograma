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
    const queryS = 'init(RowClues, ColumClues, Grid)'; // TODO Ver comillas 
    // Recibe la instancia de Prolog y utiliza la consulta init(RowsClues, ColsClues, Grid) para obtener las pistas de filas y columnas del tablero del juego
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);

        //inicializar las completedColumnsClues
        for(let i = 0 ; i< response['RowClues'].length ; i++){
          completedRowsClues[i] = 0;
        }

        //inicializar las completedRowsClues
        for(let i=0; i<response['ColumClues'].length; i++){
          completedColumnsClues[i]=0;
        }

        procesarPreGrilla(response['Grid'], response['RowsClues'], response['ColsClues']);
      }
      
    });
  }

  function procesarPreGrilla(Grid, RowsClues, ColsClues){
    let rowsLength = rowsClues.length;
    let colsLength = colsClues.length;
    let diagonalLength = Math.min(rowsLength, colsLength);

    // Recorrer la diagonal de la matriz
    for (let i = 0; i < diagonalLength; i++) {
      // Aquí puedes realizar tu lógica para verificar la pista en la diagonal
      console.log("Elemento en la diagonal:", grid[i][i]);
    }

    // Continuar recorriendo el resto de la matriz
    for (let i = diagonalLength; i < rowsLength; i++) {
      for (let j = diagonalLength; j < colsLength; j++) {
        // Aquí puedes realizar tu lógica para procesar el resto de la matriz
        console.log("Elemento en la posición no diagonal:", grid[i][j]);
      }
    }
    
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
        gameWon(RowAux,ColAux);
        setGrid(response['ResGrid']);

      } 
      setWaiting(false);
    });
    
  }

  function updateCompletedClues(type, i,completed){
    const cluesAux = type === "row" ? [...completedRowsClues] : [...completedColumnsClues]; //copia superficial del arreglo
    CluesAux[i] = completed;
    if (type == "row")
      setCompletedRowsClues(CluesAux);
    else
    setCompletedColumnsClues(CluesAux);
  }

  function gameWon(RowAux,ColAux){
    const RowAuxValues= JSON.stringify(RowAux);
    const ColAuxValues= JSON.stringify(ColAux);
    
    // console.log("Rows" + RowAuxValues);
    // console.log("Cols" + ColAuxValues);

    const queryS1 = `checkWon(${RowAuxValues},${ColAuxValues},Result)`;

    setWaiting(true);
    pengine.query(queryS1, (success, response) => {
      if (success) {

        console.log(response['Result']); 
        setGameWonStatus(response['Result']);
        
      }
      setWaiting(false);
    });
        
  }

  if (!grid) {
    return null;
  }

  const statusText = 'Keep playing!';
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