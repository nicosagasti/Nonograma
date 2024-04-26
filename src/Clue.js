import React from 'react';

function Clue({ clue, completed }) {
    return (
        <div className={`clue ${completed ? 'completed' : ''}`}>
            {clue.map((num, i) => (
                <div key={i}>
                    {num}
                </div>
            ))}
        </div>
    );
}

export default Clue;