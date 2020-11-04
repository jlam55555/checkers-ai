//
// Created by jon on 10/21/20.
//

#include <climits>
#include <iostream>
#include <fstream>
#include <chrono>
#include "checkers.h"

// for testing; remove later
//#include <time.h>

void load_from_file(std::string &fname, checkers::State &state, unsigned int &time_limit) {
	uint8_t i, j, col, pieces_count[2] = {0};
	checkers::Player player;
	checkers::Type type;
	unsigned int piece;

	// if no filename, use default board
	if (fname.empty()) {
		// fill in board
		state.board[BLK][0] = 0b101010100101010110101010u;
		state.board[RED][0] = (uint64_t) 0b010101011010101001010101u << 40u;
		state.board[BLK][1] = state.board[RED][1] = 0;

		// got lazy with this part
		for (i = 0; i < 3; ++i) {
			for (j = 0; j < 8; ++j) {
				if (i % 2 != j % 2) {
					state.pieces[BLK][pieces_count[BLK]++] = PIECE_ENCODE(i, j, MAN);
				}
			}
		}
		for (i = 5; i < 8; ++i) {
			for (j = 0; j < 8; ++j) {
				if (i % 2 != j % 2) {
					state.pieces[RED][pieces_count[RED]++] = PIECE_ENCODE(i, j, MAN);
				}
			}
		}

		// black starts the game
		state.turn = BLK;

		// read in time limit
		std::string in;
		std::cout << "Enter time limit (s) [3]: ";
		std::getline(std::cin, in);
		try {
			time_limit = std::stoi(in);
		} catch (std::exception &e) {
			time_limit = 3;
		}
		return;
	}

	// read in file and make sure it exists; if it doesn't, throw a fit and use default board
	std::ifstream is{fname};
	if (is.fail()) {
		std::cerr << "Error loading from file " << fname << "; loading default board instead." << std::endl;
		fname = "";
		load_from_file(fname, state, time_limit);
		return;
	}

	for (i = 0; i < 8; ++i) {
		for (j = 0; j < 4; ++j) {
			is >> piece;

			// 0 indicates nothing here
			if (!piece)
				continue;

			// get piece info
			player = static_cast<checkers::Player>(!(piece & 0x01u));
			type = static_cast<checkers::Type>(piece > 2);

			// calculate column
			col = 2 * j + !(i % 2);

			// add player to board
			state.board[player][0] |= BOARD_ENCODE(i, col);
			if (type == KNG) {
				state.board[player][1] |= BOARD_ENCODE(i, col);
			}

			// add to player list of pieces
			state.pieces[player][pieces_count[player]++] = PIECE_ENCODE(i, col, type);
		}
	}

	// read in next player's turn
	is >> piece;
	state.turn = static_cast<checkers::Player>(piece == 2);

	// read in time limit
	is >> time_limit;
	is.close();
}

void print_board(checkers::State &state) {
	checkers::Board mask = 0x1u, pieces = state.board[BLK][0] | state.board[RED][0];
	unsigned char i, j;

	std::cout << std::endl << std::endl << std::endl;
	std::cout << "Player " << (int)(state.turn + 1) << "'s turn" << std::endl;
	std::cout << "   0  1  2  3  4  5  6  7" << std::endl;
	for (i = 0; i < 8; ++i) {
		std::cout << ((int)i) << " ";
		for (j = 0; j < 8; ++j, mask <<= 1u) {
			// if not a valid board piece, ignore
			if (i % 2 == j % 2) {
				std::cout << "\033[30;107m   \33[0m";
				continue;
			}

			// if no piece here, ignore
			if (!(mask & pieces)) {
				std::cout << "\33[40;97m   \33[0m";
				continue;
			}

			// print out correct piece
			std::cout << "\33[40;97m ";
			if (mask & state.board[BLK][0]) {
				std::cout << ((mask & state.board[BLK][1]) ? "\u2654" : "\u25ef");
			} else {
				std::cout << ((mask & state.board[RED][1]) ? "\u265a" : "\u2b24");
			}
			std::cout << " \33[0m";
		}
		std::cout << std::endl;
	}
}

inline bool attempt_capture(uint8_t row, uint8_t col, int8_t dir_row, int8_t dir_col, checkers::State &state, checkers::Move *prev_move,
	checkers::Move *move_stack, int8_t &stack_pos, checkers::Move *intermediate_moves,
	checkers::Move &move, checkers::Player &player, checkers::Type &type, uint8_t &piece_pos) {

	uint8_t j;

	if ((dir_row == 1 ? row < 6 : row > 1)
		&& (dir_col == 1 ? col < 6 : col > 1)
		&& (type == KNG || player == (dir_row == 1 ? BLK : RED))
		&& state.board[!player][0] & BOARD_ENCODE(row+dir_row, col+dir_col)
		&& !((state.board[player][0] | state.board[!player][0]) & BOARD_ENCODE(row+2*dir_row, col+2*dir_col))
		&& !(move.path & BOARD_ENCODE(row+dir_row, col+dir_col))) {

		// create new move instance, copy state
		checkers::Move new_move{move, PIECE_ENCODE(row+dir_row*2, col+dir_col*2, type)};

		// update turn in state
		new_move.state.turn = !player;

		// update board
		new_move.state.board[player][0] &= ~BOARD_ENCODE(row, col);
		new_move.state.board[player][0] |= BOARD_ENCODE(row+dir_row*2, col+dir_col*2);
		if (type == KNG) {
			new_move.state.board[player][1] &= ~BOARD_ENCODE(row, col);
			new_move.state.board[player][1] |= BOARD_ENCODE(row+dir_row*2, col+dir_col*2);
		}
		new_move.state.board[!player][0] &= ~BOARD_ENCODE(row+dir_row, col+dir_col);
		new_move.state.board[!player][1] &= ~BOARD_ENCODE(row+dir_row, col+dir_col);

		// move current player's piece
		new_move.state.pieces[player][piece_pos] = PIECE_ENCODE(row+2*dir_row, col+2*dir_col, type);

		// remove opponent piece; tight loop is okay (maximum 12 iterations)
		checkers::Piece *opponent_pieces = new_move.state.pieces[!player];
		for (j = 0; PIECE_ROW(opponent_pieces[j]) != row+dir_row || PIECE_COL(opponent_pieces[j]) != col+dir_col; ++j);
		for (; j < 11; ++j) {
			opponent_pieces[j] = opponent_pieces[j+1];
		}
		opponent_pieces[11] = 0;

		// add captured piece to path
		new_move.path |= BOARD_ENCODE(row+dir_row, col+dir_col);

		// promote to king if necessary
		if (type == MAN && (row+2*dir_row == 0 || row+2*dir_row == 7)) {
			new_move.state.board[player][1] |= BOARD_ENCODE(row+dir_row*2, col+dir_col*2);
			new_move.state.pieces[player][piece_pos] |= PIECE_SETTYPE(KNG);
		}

		// add reference to previous move (if move history enabled)
		if (intermediate_moves) {
			new_move.prev = prev_move;
		}

		// add new move instance to stack
		move_stack[++stack_pos] = new_move;

		return true;
	}
	return false;
}

inline bool attempt_nocapture(uint8_t row, uint8_t col, int8_t dir_row, int8_t dir_col, checkers::Player player,
	checkers::Player type, checkers::State &state, checkers::Move *intermediate_moves, checkers::Move *prev_move,
	uint8_t piece_pos, checkers::Move *moves, uint8_t &num_moves, checkers::Move &move) {

	if ((dir_row == 1 ? row < 7 : row > 0)
		&& (dir_col == 1 ? col < 7 : col > 0)
		&& (type == KNG || player == (dir_row == 1 ? BLK : RED))
		&& !((state.board[player][0] | state.board[!player][0]) & BOARD_ENCODE(row+dir_row, col+dir_col))) {

		checkers::Move new_move{move, PIECE_ENCODE(row+dir_row, col+dir_col, type)};

		// update turn in state
		new_move.state.turn = !player;

		// update board
		new_move.state.board[player][0] &= ~BOARD_ENCODE(row, col);
		new_move.state.board[player][0] |= BOARD_ENCODE(row+dir_row, col+dir_col);
		if (type == KNG) {
			new_move.state.board[player][1] &= ~BOARD_ENCODE(row, col);
			new_move.state.board[player][1] |= BOARD_ENCODE(row+dir_row, col+dir_col);
		}

		// update pieces
		new_move.state.pieces[player][piece_pos] = PIECE_ENCODE(row+dir_row, col+dir_col, type);

		// promote to king if necessary
		if (type == MAN && (row+dir_row == 0 || row+dir_row == 7)) {
			new_move.state.board[player][1] |= BOARD_ENCODE(row+dir_row, col+dir_col);
			new_move.state.pieces[player][piece_pos] |= PIECE_SETTYPE(KNG);
		}

		// add reference to previous move (if move history defined
		if (intermediate_moves) {
			new_move.prev = prev_move;
		}

		// add to valid moves
		moves[num_moves++] = new_move;

		return true;
	}

	return false;
}

// returns number of valid moves
unsigned int valid_moves(checkers::State &state, checkers::Move *moves, checkers::Move *intermediate_moves = nullptr) {
	checkers::Move move_stack[100], move{}, *prev_move;
	checkers::Player player = state.turn;
	checkers::Type type;
	checkers::Piece piece;
	checkers::Board start_bitmask;
	uint8_t i, j, start_row, start_col, row, col, num_moves = 0;
	int8_t stack_pos, history_pos = 0;
	bool found_move;

	// find valid capture moves, if any
	for (i = 0; i < 12; ++i) {
		// skip if piece is gone
		piece = state.pieces[player][i];
		if (!PIECE_PRESENT(piece))
			break;

		// find moves starting from this piece
		start_row = PIECE_ROW(piece);
		start_col = PIECE_COL(piece);
		type = PIECE_TYPE(piece);

		// add current element to stack
		stack_pos = 0;
		move_stack[0] = checkers::Move{state, piece};

		// hide current element on board temporarily
		start_bitmask = BOARD_ENCODE(start_row, start_col);
		state.board[player][0] &= ~start_bitmask;
		state.board[player][1] &= ~start_bitmask;

		// find capture positions!
		while (stack_pos >= 0) {
			move = move_stack[stack_pos--];
			row = PIECE_ROW(move.pos);
			col = PIECE_COL(move.pos);

			// for keeping track of whether further captures can be made or not
			found_move = false;

			// if saving move history, save it there as well
			if (intermediate_moves) {
				intermediate_moves[history_pos++] = move;
				prev_move = intermediate_moves + history_pos - 1;
			}

			// attempt diagonal up left capture
			found_move |= attempt_capture(row, col, -1, -1, state, prev_move, move_stack,
				stack_pos, intermediate_moves, move, player, type, i);
			found_move |= attempt_capture(row, col, -1, 1, state, prev_move, move_stack,
				stack_pos, intermediate_moves, move, player, type, i);
			found_move |= attempt_capture(row, col, 1, -1, state, prev_move, move_stack,
				stack_pos, intermediate_moves, move, player, type, i);
			found_move |= attempt_capture(row, col, 1, 1, state, prev_move, move_stack,
				stack_pos, intermediate_moves, move, player, type, i);

			// if no further moves and not initial state, add
			if (!found_move && move.path) {
				moves[num_moves++] = move;
			}
		}

		// restore current element on board
		state.board[player][0] |= start_bitmask;
		if (type == KNG) {
			state.board[player][1] |= start_bitmask;
		}
	}
	if (num_moves)
		return num_moves;

	// find valid non-capture moves, if any
	for (i = 0; i < 12; ++i) {
		// skip if piece is gone
		piece = state.pieces[player][i];
		if (!PIECE_PRESENT(piece))
			break;

		row = PIECE_ROW(piece);
		col = PIECE_COL(piece);
		type = PIECE_TYPE(piece);

		move = checkers::Move(state, piece);

		if (intermediate_moves) {
			intermediate_moves[history_pos++] = move;
			prev_move = intermediate_moves + history_pos - 1;
		}

		attempt_nocapture(row, col, -1, -1, player, type, state, intermediate_moves, prev_move,
			i, moves, num_moves, move);
		attempt_nocapture(row, col, -1, 1, player, type, state, intermediate_moves, prev_move,
			i, moves, num_moves, move);
		attempt_nocapture(row, col, 1, -1, player, type, state, intermediate_moves, prev_move,
			i, moves, num_moves, move);
		attempt_nocapture(row, col, 1, 1, player, type, state, intermediate_moves, prev_move,
			i, moves, num_moves, move);
	}

	// return number of moves or 0
	return num_moves;
}

void print_valid_moves(checkers::Move *moves, unsigned int move_count, unsigned int num_game_moves) {
	checkers::Move *curr;
	std::string path{};
	uint i;

	std::cout << "Number of moves made so far: " << num_game_moves << std::endl;

	if (num_game_moves) {
		std::cout << "-1: Undo last move" << std::endl;
	}

	std::cout << "Number of valid moves: " << move_count << std::endl;
	for (i = 0; i < move_count; ++i) {
		std::cout << ((int)i) << ": ";

		curr = moves + i;
		path = "";
		do {
			path =  "(" + std::to_string(PIECE_ROW(curr->pos)) + "," + std::to_string(PIECE_COL(curr->pos)) + ")->" + path;
		} while ((curr = curr->prev));

		std::cout << path << std::endl;
	}
}

// some references:
// https://www.wikihow.com/Win-at-Checkers
// https://github.com/kevingregor/Checkers/blob/master/Final%20Project%20Report.pdf
// http://www.cs.columbia.edu/~devans/TIC/AB.html
int32_t score(checkers::State &state, checkers::Player player) {
	uint8_t i, j, type, row, col;
	checkers::Piece *piece, *piece2;

	// metric 1: counting kings and men
	int32_t kng_men_count = 0;

	// metric 2: seeing how far pieces are (but also allowing for back row)
	int32_t men_dist_to_kng = 0;
	int32_t back_row = 0;

	// metric 3: allowing for trades if "ahead" ("ahead" based on score on first two)
	// higher affinity for trades based on higher number of kings and men on current side
	int32_t trade_affinity = 0;
	uint8_t player1_pieces = 0;
	uint8_t player2_pieces = 0;

	// metric 4: closeness of pieces to opponent's pieces
	uint32_t total_piece_count = 0;
	int32_t sum_distances = 0;
	uint32_t dist_to_corner1 = 0;
	uint32_t dist_to_corner2 = 0;
	bool king_corner1_player1 = false;
	bool king_corner2_player1 = false;
	bool king_corner1_player2 = false;
	bool king_corner2_player2 = false;

	// metric 5: proximity to center of board
	int32_t board_center = 0;

	for (i = 0, piece = state.pieces[player]; i < 12 && PIECE_PRESENT(*piece); ++i, ++piece) {
		type = PIECE_TYPE(*piece);
		row = PIECE_ROW(*piece);
		col = PIECE_COL(*piece);

		kng_men_count += (type == MAN) ? 3 : 5;

		if (type == MAN) {
			men_dist_to_kng += (player == BLK) ? 7 - row : row;
		}

		if (row == (player == BLK ? 0 : 7) && type == MAN) {
			++back_row;
		}

		if (col >= 2 && col <= 5
			&& row >= 2 && row <= 5) {
			++board_center;
		}

		dist_to_corner1 += row + col;
		dist_to_corner2 += (7 - row) + (7 - col);

		if (row + col == 1)
			king_corner1_player1 = true;
		if ((7 - row) + (7 - col) == 1)
			king_corner2_player1 = true;

		// should be added either way, not directly part of minimax result
		++total_piece_count;
		++player1_pieces;
	}

	for (i = 0, piece = state.pieces[!player]; i < 12 && PIECE_PRESENT(*piece); ++i, ++piece) {
		type = PIECE_TYPE(*piece);
		row = PIECE_ROW(*piece);
		col = PIECE_COL(*piece);

		kng_men_count -= (type == MAN) ? 3 : 5;

		if (type == MAN) {
			men_dist_to_kng -= ((!player) == BLK) ? 7 - row : row;
		}

		if (row == ((!player) == BLK ? 0 : 7) && type == MAN) {
			--back_row;
		}

		if (col >= 2 && col <= 5
		    && row >= 2 && row <= 5) {
			--board_center;
		}

		dist_to_corner1 -= row + col;
		dist_to_corner2 -= (7 - row) + (7 - col);

		if (row + col == 1)
			king_corner1_player2 = true;
		if ((7 - row) + (7 - col) == 1)
			king_corner2_player2 = true;

		// should be added either way, not directly part of minimax result
		++total_piece_count;
		++player2_pieces;
	}

	// attempt trades if winning, try not to if losing
	// trade affinity tries to maximize the ratio of winner to loser number of pieces
	if (kng_men_count > 0) {
		trade_affinity = player1_pieces * 10 / player2_pieces;
	} else {
		trade_affinity = -player2_pieces * 10 / player1_pieces;
	}

	// calculate distances to double corners; both winner and loser wants
	// to get closer to this position;
	// 6 arbitrarily chosen to indicate near-endgame position
	if (total_piece_count < 6 && kng_men_count) {
		checkers::Board loser_board = state.board[kng_men_count > 0 ? !player : player][0];
		if (loser_board & 0b0000000000000000000000000000000000000001000000100000010100001010u && dist_to_corner1 > 0) {
			sum_distances = -dist_to_corner1;
		} else if (loser_board & 0b0101000010100000010000001000000000000000000000000000000000000000u) {
			sum_distances = -dist_to_corner2;
		}
	}

	return kng_men_count * 100000
		+ (-1 * men_dist_to_kng + 2 * back_row) * 1000
		+ trade_affinity * 500
		+ sum_distances * 100
		+ board_center * 10
		+ rand() % 10;
}

#define TIMEOUT		INT32_MAX
int32_t min_value(checkers::State &, int32_t, int32_t, uint8_t, uint8_t, uint8_t, std::chrono::time_point<std::chrono::system_clock >);
int32_t max_value(checkers::State &, int32_t, int32_t, uint8_t, uint8_t, uint8_t, std::chrono::time_point<std::chrono::system_clock>, checkers::Move * = nullptr);
checkers::Move minimax_search(checkers::State &state, uint8_t depth_limit, uint8_t time_limit,
	std::chrono::time_point<std::chrono::system_clock> start, bool &is_timeout, bool &is_guaranteed_win) {
	int32_t v = 0;
	checkers::Move best_move;

	v = max_value(state, INT32_MIN, INT32_MAX, 0, depth_limit, time_limit, start, &best_move);
	if (v == TIMEOUT) {
		is_timeout = true;
	} else if (abs(v) > 99000000) {
		is_guaranteed_win = true;
	}
	return best_move;
}

int32_t max_value(checkers::State &state, int32_t alpha, int32_t beta, uint8_t depth, uint8_t depth_limit, uint8_t time_limit, std::chrono::time_point<std::chrono::system_clock> start, checkers::Move *move) {
	int32_t v = INT_MIN, tmp;
	checkers::Move moves[20];
	uint8_t i, move_count = valid_moves(state, moves);

	// terminal test
	if (move_count == 0) {
		return -100000000 + depth;
	}
	if (depth == depth_limit) {
		return score(state, state.turn);
	}
	// timeout
	if (std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now() - start).count() > time_limit * 1000 - 10) {
		return TIMEOUT;
	}

//	if (depth == 0)
//		std::cout << std::endl;
	for (i = 0; i < move_count; ++i) {
		// v = max(v, min_value(...))
		if (v < (tmp = min_value(moves[i].state, alpha, beta, depth+1, depth_limit, time_limit, start))) {
			v = tmp;
			if (move)
				*move = moves[i];
		}

		if (tmp == TIMEOUT)
			return TIMEOUT;

		// if v >= beta return v (+ 1)
		if (v >= beta)
			return v + 1;
		// alpha = max(alpha, v)
		if (v > alpha)
			alpha = v;

//		if (depth == 0)
//			std::cout << v << std::endl;
	}
	return v;
}

int32_t min_value(checkers::State &state, int32_t alpha, int32_t beta, uint8_t depth, uint8_t depth_limit, uint8_t time_limit, std::chrono::time_point<std::chrono::system_clock> start) {
	int32_t v = INT_MAX, tmp;
	checkers::Move moves[20];
	uint8_t i, move_count = valid_moves(state, moves);

	// terminal test
	if (move_count == 0) {
		return 100000000 - depth;
	}
	if (depth == depth_limit) {
		return score(state, !state.turn);
	}
	// timeout
	if (std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now() - start).count() > time_limit * 1000 - 10) {
		return TIMEOUT;
	}

	for (i = 0; i < move_count; ++i) {
		// v = min(v, max_value(...))
		if (v > (tmp = max_value(moves[i].state, alpha, beta, depth+1, depth_limit, time_limit, start)))
			v = tmp;

		if (tmp == TIMEOUT)
			return TIMEOUT;

		// if v <= alpha return v (- 1)
		if (v <= alpha)
			return v - 1;
		// beta = min(beta, v)
		if (v < beta)
			beta = v;
	}
	return v;
}

checkers::Move iterative_deepening_search(checkers::State &state, uint8_t time_limit) {
	checkers::Move best_move{}, next_move{};
	uint8_t depth_limit = 5;
	auto start = std::chrono::system_clock::now();
	bool is_timeout = false, is_guaranteed_win = false;

	for (; ; ++depth_limit) {
		next_move = minimax_search(state, depth_limit, time_limit, start, is_timeout, is_guaranteed_win);

		if (is_timeout || is_guaranteed_win) {
			if (is_guaranteed_win) {
				best_move = next_move;
			}

			int time_ms = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now() - start).count();
			std::cout << "Iterative deepening time: " << time_ms << "ms; depth: " << ((int)depth_limit - 1) << std::endl;
			break;
		}

		best_move = next_move;
	}

	return best_move;
}

int main() {
	checkers::State state{};
	checkers::Move moves[40], intermediate_moves[60], game_move_history[256], move;
	unsigned int time_limit, num_valid_moves, num_game_moves = 0;
	int player_move;
	bool is_comp[2] = {true, true};
	std::string in;

	// load game settings
	std::cout << "Black is computer? [y]/n:\t";
	std::getline(std::cin, in);
	if (in == "n" || in == "N")
		is_comp[BLK] = false;

	std::cout << "Red is computer? [y]/n:\t";
	std::getline(std::cin, in);
	if (in == "n" || in == "N")
		is_comp[RED] = false;

	std::cout << "Load from file? y/[n]:\t";
	std::getline(std::cin, in);
	if (in == "y" || in == "Y") {
		std::cout << "Enter path to board:\t";
		std::cin >> in;
	} else {
		in = "";
	}
	load_from_file(in, state, time_limit);

	// add initial state to game move history
	game_move_history[num_game_moves++] = checkers::Move{state, 0};

	// game loop
	while (true) {
		print_board(state);

		if (!(num_valid_moves = valid_moves(state, moves, intermediate_moves))) {
			std::cout << "No valid moves left." << std::endl;
			break;
		}

		print_valid_moves(moves, num_valid_moves, num_game_moves);

		if (is_comp[state.turn]) {
			// if only one valid move, then return it
			if (num_valid_moves == 1) {
				std::cout << "Choosing only move: " << std::endl;
				move = moves[0];
			} else {
				move = iterative_deepening_search(state, time_limit);
				moves[0] = move;
			}
			game_move_history[num_game_moves++] = move;
			state = move.state;
		} else {
			std::cout << "Enter your move: ";
			std::cin >> player_move;
			while (player_move < -1 || player_move >= (signed) num_valid_moves || (player_move == -1 && num_game_moves < 3)) {
				std::cout << "Invalid move. Try again: ";
				std::cin >> player_move;
			}

			// undo functionality
			if (player_move == -1) {
				state = game_move_history[num_game_moves - 3].state;
				num_game_moves -= 2;
			}
			// regular move functionality
			else {
				game_move_history[num_game_moves++] = moves[player_move];
				state = moves[player_move].state;
			}
		}
	}

	return 0;
}