import React from 'react';

function Square({ value, onClick,gameWon }) {
    const squareStyle = {
        backgroundColor: gameWon && value === '#' ? '#15b58e' : (value === '#' ? 'black' : 'white'),
    };

    const displayValue = gameWon && value === '#' ? null : (value !== '_' ? value : null);

    return (
        <button className="square" style={squareStyle} onClick={onClick}>
            {displayValue}
        </button>
    );
}

export default Square;
/*
/*{value !== '_' ? value : null}
*/
