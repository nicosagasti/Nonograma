import React from 'react';

function Square({ value, onClick }) {
    const squareStyle = {
        backgroundColor: value === '#' ? 'black' : 'white',
    };

    return (
        <button className="square" style={squareStyle} onClick={onClick}>
            {value !== '_' ? value : null}
        </button>
    );
}

export default Square;
