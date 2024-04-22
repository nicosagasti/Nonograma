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
    const queryS = 'init(RowClues, ColumClues, Grid)'; // TODO Ver comillas 
    // Recibe la instancia de Prolog y utiliza la consulta init(RowsClues, ColsClues, Grid) para obtener las pistas de filas y columnas del tablero del juego
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);
      }
    });
  }

  function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting) {
      return;
    }

    const content = toggleChecked ? "#" : "X";
    
    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
  
    const rowCluesS = JSON.stringify(rowsClues);
    const colCluesS = JSON.stringify(colsClues);
    const queryS = `put("${content}", [${i},${j}], ${rowCluesS}, ${colCluesS},${squaresS}, ResGrid, RowSat, ColSat)`; 
    
    setWaiting(true);

    console.log(queryS);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);
        
        console.log(response);

        console.log(response['RowSat']);
        console.log(response['ColSat']);

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