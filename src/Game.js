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

  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);
  }, []);

  function handleServerReady(instance) {
    pengine = instance;
    const queryS = 'init(RowClues, ColumClues, Grid)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);
      }
    });
  }


 /* function checkCluesAfterMove(grid, rowsClues, colsClues, i, j) {
    // 1. Extract relevant row and column segments:
    const row = grid[i]; // Extract the clicked row
    const currentCol = grid.map(row => row[j]); // Extract the clicked column
  
    // 2. Check row clues:
    const rowClueIndex = i;
    const currentRowClue = rowsClues[rowClueIndex];
    let rowCluesMet = true; // Assume true initially
  
    let count = 0;
    let filledCells = 0; // Track filled cells for clue comparison
    for (let cellIndex = 0; cellIndex < row.length; cellIndex++) {
      if (row[cellIndex] === '#') {
        count++;
        filledCells++; // Increment filled cells
      } else if (count > 0) {
        if (count !== currentRowClue[filledCells - 1]) {
          rowCluesMet = false;
          break; // Early exit if clue is not met
        }
        count = 0;
      }
    }
  
    // Check for remaining filled cells at the end:
    if (count > 0 && count !== currentRowClue[filledCells]) {
      rowCluesMet = false;
    }
  
    // 3. Check column clues (similar logic):
    const colClueStartIndex = i * grid.length; // Starting index for column clues
    const colCluesMet = checkColumnClues(currentCol, colsClues, colClueStartIndex);
  
    // 4. Return results:
    return { rowCluesMet, colCluesMet };
  }
  
  function checkColumnClues(column, clues, startIndex) {
    let count = 0;
    let filledCells = 0;
    for (let i = 0; i < column.length; i++) {
      if (column[i] === '#') {
        count++;
        filledCells++;
      } else if (count > 0) {
        const expectedCount = clues[(startIndex + i) % clues.length];
        if (count !== expectedCount) {
          return false; // Early exit if clue is not met
        }
        count = 0;
      }
    }
  
    // Check for remaining filled cells at the end:
    if (count > 0 && count !== clues[(startIndex + column.length - 1) % clues.length]) {
      return false;
    }
  
    return true;
  }*/

  function checkCluesAfterMove(grid, rowsClues, colsClues, i, j, type) {
    const newClues = [];
  
    // Iterar sobre cada fila o columna
    for (let k = 0; k < (type === 'row'? grid.length : grid[0].length); k++) {
      let count = 0;
      const currentClue = [];
  
      // Iterar sobre cada celda de la fila o columna
      for (let l = 0; l < (type === 'row'? grid[k].length : grid.length); l++) {
        const cell = type === 'row'? grid[k][l] : grid[l][k];
        if (cell === '#') {
          count++;
        } else if (count > 0) {
          currentClue.push(count);
          count = 0;
        }
      }
      
      // Agregar el recuento final
      if (count > 0) {
        currentClue.push(count);
      }

      console.log(currentClue + type);
  
      // Comprobar si la pista se cumple
      const clueIndex = type === 'row'? k : j;
      const originalClue = type === 'row'? rowsClues[clueIndex] : colsClues.slice(clueIndex * grid.length, (clueIndex + 1) * grid.length);
      const areCluesIdentical = currentClue.every((currentClue, index) => currentClue === originalClue[index]);
  
      newClues.push(areCluesIdentical); //Agrega las pistas a las pistas actualizadas
    }
  
    // Devolver el objeto con la información de si las pistas se cumplieron o no
    return newClues;
  }
  

  function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting) {
      return;
    }

    const content = toggleChecked ? '#' : 'X';

    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
    const rowCluesS = JSON.stringify(rowsClues);
    const colCluesS = JSON.stringify(colsClues);
    const queryS = `put("${content}", [${i},${j}], ${rowCluesS}, ${colCluesS},${squaresS}, ResGrid, RowSat, ColSat)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);
        //verificar si se cumplen las pistas
        const newRowClues =checkCluesAfterMove(grid,rowsClues, colsClues, i, j, 'row');
        const newColsClues =checkCluesAfterMove(grid,rowsClues, colsClues, i, j, 'cols');
        console.log(newRowClues);
        console.log(newColsClues);
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
        onClick={(i, j) => handleClick(i, j)}
      />
      <div className="game-info">
        <Box display="flex" alignItems="center">
          <div style={{ marginRight: '10px', backgroundColor: toggleChecked ? 'black' : 'transparent', color: toggleChecked ? 'white' : 'black' }}>{toggleChecked ? '#' : 'X'}</div>
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