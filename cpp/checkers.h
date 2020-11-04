//
// Created by jon on 10/21/20.
//

#ifndef PROJ1_CHECKERS_H
#define PROJ1_CHECKERS_H

#include <cstdint>
#include <cstring>

// type casting to prevent CLion complaining
#define BOARD_ENCODE(row, col) \
	(((uint64_t) 1u) << ((((uint64_t) row) << 3u) | (col)))

#define PIECE_ENCODE(row, col, type) \
	((uint8_t) (((row) << 3u) | (col) | ((type) << 7u)))

#define PIECE_SETTYPE(type) \
	((type) << 7u)

#define PIECE_PRESENT(piece) \
	!!(piece)

#define PIECE_TYPE(piece) \
	((piece) >> 7u)

#define PIECE_ROW(piece) \
	(((piece) & 0x38u) >> 3u)

#define PIECE_COL(piece) \
	((piece) & 0x07u)

#define BLK	0u
#define RED	1u

#define MAN	0u
#define KNG	1u

namespace checkers {

    typedef uint8_t Player;
    typedef uint8_t Type;

    typedef uint64_t Board;
    typedef uint8_t Piece;

    class State {
    public:
	// 32 bytes
	Board board[2][2];

	// 24 bytes
	Piece pieces[2][12];

	// 1 byte
	Player turn;

	State &operator=(const State &state) {
		turn = state.turn;
		memcpy(board, state.board, sizeof(board));
		memcpy(pieces, state.pieces, sizeof(pieces));
		return *this;
	}

	// empty state
	// TODO: don't do memsets if not going to be used
	State()
		: turn{0} {
		memset(board, 0, sizeof(board));
		memset(pieces, 0, sizeof(pieces));
	}
    };

    class Move {
    public:
        State state;

        Board path;
        Piece start, pos;

        Move *prev;

        // empty move
        Move() = default;

        // initialize fields
        Move(State &state, Piece start)
        	: state{state}, start{start}, path{0}, pos{start}, prev{nullptr} { }

        // move copy constructor (sort-of)
        Move(Move &move, Piece pos)
        	: state{move.state}, path{move.path}, start{move.start}, pos{pos}, prev{nullptr} { }

	Move &operator=(const Move &move) {
        	state = move.state;
        	path = move.path;
        	pos = move.pos;
        	start = move.start;
        	prev = move.prev;
        	return *this;
        }
    };

}

#endif //PROJ1_CHECKERS_H
