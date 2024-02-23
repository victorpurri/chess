// ==UserScript==
// @name        arrow
// @namespace   vmp
// @match       https://www.chess.com/*
// @grant       none
// @version     3.0.1
// @author      vmp
// @description chess
// @run-at      document-start
// ==/UserScript==

function CHESS(Chess) {

Chess.DEPTH = 4;
Chess.RANKS = 8;
Chess.LAST_RANK = Chess.RANKS - 1;
Chess.FILES = 8;
Chess.LAST_FILE = Chess.FILES - 1;
Chess.FILE_CHARACTERS = "abcdefgh";
Chess.RANK_CHARACTERS = "12345678";
Chess.Piece = {
	PAWN: 0,
	KNIGHT: 1,
	BISHOP: 2,
	ROOK: 3,
	QUEEN: 4,
	KING: 5
};
Chess.PieceColor = {
	WHITE: 0,
	BLACK: 1
};
Chess.PIECE_NAMES = [ "pawn", "knight", "bishop", "rook", "queen", "king" ];
Chess.PIECE_ALGEBRAIC_NAMES = " NBRQK";
Chess.PIECE_CHARACTERS = "\u2659\u265F\u2658\u265E\u2657\u265D\u2656\u265C\u2655\u265B\u2654\u265A";
Chess.getRank = function(index) {
	return index >>> 3;
};
Chess.getFile = function(index) {
	return index & 7;
};
Chess.isInsideBoard = function(rank, file) {
	return !((rank | file) & ~7);
};
Chess.getIndex = function(rank, file) {
	return file + rank * Chess.FILES;
};
Chess.isLight = function(rank, file) {
	return !!((rank + file) % 2);
};
Chess.getAlgebraic = function(rank, file) {
	return Chess.FILE_CHARACTERS[file] + Chess.RANK_CHARACTERS[rank];
};
Chess.getIndexFromAlgebraic = function(algebraic) {
	var file = Chess.FILE_CHARACTERS.indexOf(algebraic[0]);
	var rank = Chess.RANK_CHARACTERS.indexOf(algebraic[1]);
	return Chess.getIndex(rank, file);
};
Chess.getAlgebraicFromIndex = function(index) {
	return Chess.getAlgebraic(Chess.getRank(index), Chess.getFile(index));
};
Chess.getPieceCharacter = function(piece, color) {
	return Chess.PIECE_CHARACTERS.charAt(piece * 2 + color);
};
Chess.getOtherPieceColor = function(color) {
	return (color ^ 1);
};
Chess.Bitboard = function(low, high) {
	this.low = low >>> 0;
	this.high = high >>> 0;
};
Chess.Bitboard.popcnt32 = function(v) {
	v >>>= 0;
	v -= (v >>> 1) & 0x55555555;
	v = (v & 0x33333333) + ((v >>> 2) & 0x33333333);
	return ((v + (v >>> 4) & 0xF0F0F0F) * 0x1010101) >>> 24;
};
Chess.Bitboard.popLowestBit32 = function (v) {
	v >>>= 0;
	return (v & (v - 1)) >>> 0;
};
Chess.Bitboard.getLowestBitPosition32 = function(v) {
	v >>>= 0;
	return Chess.Bitboard.popcnt32((v & -v) - 1);
};
Chess.Bitboard.prototype.popcnt = function() {
	return Chess.Bitboard.popcnt32(this.low) + Chess.Bitboard.popcnt32(this.high);
};
Chess.Bitboard.prototype.popLowestBit = function() {
	if (this.low) {
		this.low = Chess.Bitboard.popLowestBit32(this.low);
	} else {
		this.high = Chess.Bitboard.popLowestBit32(this.high);
	}
	return this;
};
Chess.Bitboard.prototype.getLowestBitPosition = function() {
	if (this.low) {
		return Chess.Bitboard.getLowestBitPosition32(this.low);
	}
	return 32 + Chess.Bitboard.getLowestBitPosition32(this.high);
};
Chess.Bitboard.prototype.extractLowestBitPosition = function() {
	var index = this.getLowestBitPosition();
	this.popLowestBit();
	return index;
};
Chess.Bitboard.prototype.isEmpty = function() {
	return !this.low && !this.high;
};
Chess.Bitboard.prototype.isClear = function(index) {
	index >>>= 0;
	if (index < 32) {
		return !(this.low & (1 << index));
	}
	return !(this.high & (1 << (index - 32)));
};
Chess.Bitboard.prototype.isSet = function(index) {
	return !this.isClear(index);
};
Chess.Bitboard.prototype.setBit = function(index) {
	index >>>= 0;
	if (index < 32) {
		this.low = (this.low | (1 << index)) >>> 0;
	} else {
		this.high = (this.high | (1 << (index - 32))) >>> 0;
	}
	return this;
};
Chess.Bitboard.prototype.clearBit = function(index) {
	index >>>= 0;
	if (index < 32) {
		this.low = (this.low & ~(1 << index)) >>> 0;
	} else {
		this.high = (this.high & ~(1 << (index - 32))) >>> 0;
	}
	return this;
};
Chess.Bitboard.prototype.and = function(other) {
	this.low = (this.low & other.low) >>> 0;
	this.high = (this.high & other.high) >>> 0;
	return this;
};
Chess.Bitboard.prototype.and_not = function(other) {
	this.low = (this.low & ~other.low) >>> 0;
	this.high = (this.high & ~other.high) >>> 0;
	return this;
};
Chess.Bitboard.prototype.or = function(other) {
	this.low = (this.low | other.low) >>> 0;
	this.high = (this.high | other.high) >>> 0;
	return this;
};
Chess.Bitboard.prototype.xor = function(other) {
	this.low = (this.low ^ other.low) >>> 0;
	this.high = (this.high ^ other.high) >>> 0;
	return this;
};
Chess.Bitboard.prototype.not = function() {
	this.low = (~this.low) >>> 0;
	this.high = (~this.high) >>> 0;
	return this;
};
Chess.Bitboard.prototype.shl = function(v) {
	v >>>= 0;
	if (v > 31) {
		this.high = (this.low << (v - 32)) >>> 0;
		this.low = 0 >>> 0;
	} else if (v > 0) {
		this.high = ((this.high << v) | (this.low >>> (32 - v))) >>> 0;
		this.low = (this.low << v) >>> 0;
	}
	return this;
};
Chess.Bitboard.prototype.shr = function(v) {
	v >>>= 0;
	if (v > 31) {
		this.low = this.high >>> (v - 32);
		this.high = 0 >>> 0;
	} else if (v > 0) {
		this.low = ((this.low >>> v) | (this.high << (32 - v))) >>> 0;
		this.high >>>= v;
	}
	return this;
};
Chess.Bitboard.prototype.shiftLeft = function(v) {
	if (v > 63 || v < -63) {
		this.low = this.high = 0 >>> 0;
	} else if (v > 0) {
		this.shl(v);
	} else if (v < 0) {
		this.shr(-v);
	}
	return this;
};
Chess.Bitboard.prototype.isEqual = function(other) {
	return this.low === other.low && this.high === other.high;
};
Chess.Bitboard.prototype.dup = function() {
	return Chess.Bitboard.make(this.low, this.high);
};
Chess.Bitboard.make = function(low, high) {
	return new Chess.Bitboard(low, high);
};
Chess.Bitboard.makeZero = function() {
	return Chess.Bitboard.make(0, 0);
};
Chess.Bitboard.makeOne = function() {
	return Chess.Bitboard.make(0xFFFFFFFF, 0xFFFFFFFF);
};
Chess.Bitboard.makeLightSquares = function() {
	return Chess.Bitboard.make(0x55AA55AA, 0x55AA55AA);
};
Chess.Bitboard.makeDarkSquares = function() {
	return Chess.Bitboard.make(0xAA55AA55, 0xAA55AA55);
};
Chess.Bitboard.makeFile = function(file) {
	return Chess.Bitboard.make(0x01010101, 0x01010101).shl(file);
};
Chess.Bitboard.makeFiles = function() {
	var b = [];
	for (var i = 0; i < 8; ++i) {
		b.push(Chess.Bitboard.makeFile(i));
	}
	return b;
};
Chess.Bitboard.makeRank = function(rank) {
	return Chess.Bitboard.make(0xFF, 0).shl(rank * 8);
};
Chess.Bitboard.makeRanks = function() {
	var b = [];
	for (var i = 0; i < 8; ++i) {
		b.push(Chess.Bitboard.makeRank(i));
	}
	return b;
};
Chess.Bitboard.makeIndex = function(index) {
	return Chess.Bitboard.makeZero().setBit(index);
};
Chess.Bitboard.makeIndices = function() {
	var b = [];
	for (var i = 0; i < 64; ++i) {
		b.push(Chess.Bitboard.makeIndex(i));
	}
	return b;
};
Chess.Bitboard.makeDiagonal = function(diagonal) {
	return Chess.Bitboard.make(0x10204080, 0x01020408).and(Chess.Bitboard.makeOne().shiftLeft(diagonal * 8)).shiftLeft(diagonal);
};
Chess.Bitboard.makeDiagonals = function() {
	var b = [];
	for (var i = -7; i < 8; ++i) {
		b.push(Chess.Bitboard.makeDiagonal(i));
	}
	return b;
};
Chess.Bitboard.makeAntidiagonal = function(antidiagonal) {
	return Chess.Bitboard.make(0x08040201, 0x80402010).and(Chess.Bitboard.makeOne().shiftLeft(-antidiagonal * 8)).shiftLeft(antidiagonal);
};
Chess.Bitboard.makeAntidiagonals = function() {
	var b = [];
	for (var i = -7; i < 8; ++i) {
		b.push(Chess.Bitboard.makeAntidiagonal(i));
	}
	return b;
};
Chess.Bitboard.makeKnightMovement = function(index) {
	var b = Chess.Bitboard.makeZero().setBit(index);
	var l1 = b.dup().shr(1).and_not(Chess.Bitboard.FILES[7]);
	var l2 = b.dup().shr(2).and_not(Chess.Bitboard.FILES[7]).and_not(Chess.Bitboard.FILES[6]);
	var r1 = b.dup().shl(1).and_not(Chess.Bitboard.FILES[0]);
	var r2 = b.dup().shl(2).and_not(Chess.Bitboard.FILES[0]).and_not(Chess.Bitboard.FILES[1]);
	var v1 = l2.or(r2);
	var v2 = l1.or(r1);
	return v1.dup().shl(8).or(v1.shr(8)).or(v2.dup().shl(16)).or(v2.shr(16));
};
Chess.Bitboard.makeKnightMovements = function() {
	var b = [];
	for (var i = 0; i < 64; ++i) {
		b.push(Chess.Bitboard.makeKnightMovement(i));
	}
	return b;
};
Chess.Bitboard.makeKingMovement = function(index) {
	var b = Chess.Bitboard.makeZero().setBit(index);
	var c = b.dup().shr(1).and_not(Chess.Bitboard.FILES[7]).or(b.dup().shl(1).and_not(Chess.Bitboard.FILES[0]));
	var u = b.dup().or(c).shr(8);
	var d = b.dup().or(c).shl(8);
	return c.or(u).or(d);
};
Chess.Bitboard.makeKingMovements = function() {
	var b = [];
	for (var i = 0; i < 64; ++i) {
		b.push(Chess.Bitboard.makeKingMovement(i));
	}
	return b;
};
Chess.Bitboard.ZERO = Chess.Bitboard.makeZero();
Chess.Bitboard.ONE = Chess.Bitboard.makeOne();
Chess.Bitboard.LIGHT_SQUARES = Chess.Bitboard.makeLightSquares();
Chess.Bitboard.DARK_SQUARES = Chess.Bitboard.makeDarkSquares();
Chess.Bitboard.FILES = Chess.Bitboard.makeFiles();
Chess.Bitboard.RANKS = Chess.Bitboard.makeRanks();
Chess.Bitboard.DIAGONALS = Chess.Bitboard.makeDiagonals();
Chess.Bitboard.ANTIDIAGONALS = Chess.Bitboard.makeAntidiagonals();
Chess.Bitboard.KNIGHT_MOVEMENTS = Chess.Bitboard.makeKnightMovements();
Chess.Bitboard.KING_MOVEMENTS = Chess.Bitboard.makeKingMovements();
Chess.Bitboard.prototype.str = function() {
    return this.high.toString(16).padStart(8,'0') + ":" + this.low.toString(16).padStart(8,'0')
}
Chess.Move = function(from, to, kind, piece, capturedPiece) {
	this.value = (to & 0x3F) | ((from & 0x3F) << 6) | ((kind & 0xF) << 12) | ((piece & 0x7) << 16) | (((capturedPiece | 0) & 0x7) << 19);
};
Chess.Move.Kind = {
	POSITIONAL: 0,
	DOUBLE_PAWN_PUSH: 1,
	KING_CASTLE: 2,
	QUEEN_CASTLE: 3,
	CAPTURE: 4,
	EN_PASSANT_CAPTURE: 5,
	KNIGHT_PROMOTION: 8,
	BISHOP_PROMOTION: 9,
	ROOK_PROMOTION: 10,
	QUEEN_PROMOTION: 11,
	KNIGHT_PROMOTION_CAPTURE: 12,
	BISHOP_PROMOTION_CAPTURE: 13,
	ROOK_PROMOTION_CAPTURE: 14,
	QUEEN_PROMOTION_CAPTURE: 15
};
Chess.Move.prototype.getTo = function() {
	return this.value & 0x3F;
};
Chess.Move.prototype.getFrom = function() {
	return (this.value >>> 6) & 0x3F;
};
Chess.Move.prototype.getKind = function() {
	return  ((this.value >>> 12) & 0xF);
};
Chess.Move.prototype.getPiece = function() {
	return  ((this.value >>> 16) & 0x7);
};
Chess.Move.prototype.isCapture = function() {
	return !!(this.getKind() & 4);
};
Chess.Move.prototype.getCapturedPiece = function() {
	return  ((this.value >>> 19) & 0x7);
};
Chess.Move.prototype.isPromotion = function() {
	return !!(this.getKind() & 8);
};
Chess.Move.prototype.isCastle = function() {
	return this.getKind() === Chess.Move.Kind.KING_CASTLE || this.getKind() === Chess.Move.Kind.QUEEN_CASTLE;
};
Chess.Move.prototype.getPromotedPiece = function() {
	if (this.isPromotion()) {
		return  (Chess.Piece.KNIGHT + (this.getKind() & 3));
	}
	return Chess.Piece.PAWN;
};
Chess.Move.prototype.getCaptureSquare = function() {
	if (this.getKind() !== Chess.Move.Kind.EN_PASSANT_CAPTURE) {
		return this.getTo();
	}
	return this.getTo() + ((this.getFrom() < this.getTo()) ? -Chess.FILES : Chess.FILES);
};
Chess.Move.prototype.getString = function() {
	if (!this.isCastle()) {
		return Chess.PIECE_ALGEBRAIC_NAMES.charAt(this.getPiece()) +
			Chess.getAlgebraicFromIndex(this.getFrom()) +
			(this.isCapture() ? "x" : "-") +
			Chess.getAlgebraicFromIndex(this.getTo()) +
			((this.getKind() === Chess.Move.Kind.EN_PASSANT_CAPTURE) ? "e.p." : "") +
			(this.isPromotion() ? Chess.PIECE_ALGEBRAIC_NAMES.charAt(this.getPromotedPiece()) : "");
	}
	return "0-0" + ((this.getKind() === Chess.Move.Kind.QUEEN_CASTLE) ? "-0" : "");
};
Chess.Position = function(data=null) {
	this.pieces = [];
	this.halfmoveClock = 0;
	this.madeMoves = [];
    if (data === null) {
        this.bitboards = [
            Chess.Bitboard.RANKS[1].dup().or(Chess.Bitboard.RANKS[6]),
            Chess.Bitboard.makeIndex(1).or(Chess.Bitboard.makeIndex(6)).or(Chess.Bitboard.makeIndex(57)).or(Chess.Bitboard.makeIndex(62)),
            Chess.Bitboard.makeIndex(2).or(Chess.Bitboard.makeIndex(5)).or(Chess.Bitboard.makeIndex(58)).or(Chess.Bitboard.makeIndex(61)),
            Chess.Bitboard.makeIndex(0).or(Chess.Bitboard.makeIndex(7)).or(Chess.Bitboard.makeIndex(56)).or(Chess.Bitboard.makeIndex(63)),
            Chess.Bitboard.makeIndex(3).or(Chess.Bitboard.makeIndex(59)),
            Chess.Bitboard.makeIndex(4).or(Chess.Bitboard.makeIndex(60)),
            Chess.Bitboard.RANKS[0].dup().or(Chess.Bitboard.RANKS[1]),
            Chess.Bitboard.RANKS[6].dup().or(Chess.Bitboard.RANKS[7])
        ];
        this.turn = Chess.PieceColor.WHITE;
        this.castlingRights = 15;
        this.enPassantSquare = -1;
    } else {
        this.bitboards = [
            Chess.Bitboard.make(data[0], data[1]),
            Chess.Bitboard.make(data[2], data[3]),
            Chess.Bitboard.make(data[4], data[5]),
            Chess.Bitboard.make(data[6], data[7]),
            Chess.Bitboard.make(data[8], data[9]),
            Chess.Bitboard.make(data[10], data[11]),
            Chess.Bitboard.make(data[12], data[13]),
            Chess.Bitboard.make(data[14], data[15])
        ]
        this.turn = data[16]
        this.castlingRights = data[17]
        this.enPassantSquare = data[18]
    }
    this.irreversibleHistory = [];
	this.fillPiecesFromBitboards();
};
Chess.Position.ANY_WHITE = Chess.Piece.KING + 1;
Chess.Position.ANY_BLACK = Chess.Position.ANY_WHITE + 1;
Chess.Position.ROOK_INDICES = [7, 63, 0, 56];
Chess.Position.SLIDING_MASKS = [Chess.Bitboard.makeFile(Chess.LAST_FILE).not(), Chess.Bitboard.ONE, Chess.Bitboard.makeFile(0).not()];
Chess.Position.Status = {
	NORMAL: 0,
	CHECKMATE: 1,
	STALEMATE_DRAW: 2,
	FIFTY_MOVE_RULE_DRAW: 3,
	THREEFOLD_REPETITION_RULE_DRAW: 4,
	INSUFFICIENT_MATERIAL_DRAW: 5
};

Chess.Position.prototype.serialize = function() {
    return [
        this.bitboards[0].low,
        this.bitboards[0].high,
        this.bitboards[1].low,
        this.bitboards[1].high,
        this.bitboards[2].low,
        this.bitboards[2].high,
        this.bitboards[3].low,
        this.bitboards[3].high,
        this.bitboards[4].low,
        this.bitboards[4].high,
        this.bitboards[5].low,
        this.bitboards[5].high,
        this.bitboards[6].low,
        this.bitboards[6].high,
        this.bitboards[7].low,
        this.bitboards[7].high,
        this.turn,
        this.castlingRights,
        this.enPassantSquare
    ]
}
Chess.Position.prototype.hash = function() {
    let h = 0;
    for (let x of this.serialize()) {
        x ^= x >> 17;
        x *= 0xed5ad4bb;
        x ^= x >> 11;
        x *= 0xac4c1b51;
        x ^= x >> 14;
        x &= x;
        h ^= x;
    }
    return h;
}

Chess.Position.perft = function(depth, chessPosition) {
	if (!depth) {
		return 1;
	}
	if (!chessPosition) {
		chessPosition = new Chess.Position;
	}
	var nodes = 0;
	chessPosition.getMoves(true).forEach( function(move) {
		if (chessPosition.makeMove(move)) {
			nodes += Chess.Position.perft(depth - 1, chessPosition);
			chessPosition.unmakeMove();
		}
	});
	return nodes;
};
Chess.Position.prototype.getMoves = function(pseudoLegal, onlyCaptures) {
	var moves = this.generateMoves(!!onlyCaptures);
	return pseudoLegal ? moves : moves.filter(Chess.Position.prototype.isMoveLegal, this);
};
Chess.Position.prototype.getMovesBoth = function(pseudoLegal, onlyCaptures) {
    var orig_turn = this.turn
    this.turn = Chess.PieceColor.WHITE
	var moves_white = this.generateMoves(!!onlyCaptures);
	moves_white = pseudoLegal ? moves_white : moves_white.filter(Chess.Position.prototype.isMoveLegal, this);
    this.turn = Chess.PieceColor.BLACK
	var moves_black = this.generateMoves(!!onlyCaptures);
	moves_black = pseudoLegal ? moves_black : moves_black.filter(Chess.Position.prototype.isMoveLegal, this);
    this.turn = orig_turn
    return moves_white.concat(moves_black)
};
Chess.Position.prototype.getColorBitboard = function(color) {
	return this.bitboards[Chess.Position.ANY_WHITE + color];
};
Chess.Position.prototype.getPieceBitboard = function(piece) {
	return this.bitboards[piece];
};
Chess.Position.prototype.getPieceColorBitboard = function(piece, color) {
	return this.bitboards[piece].dup().and(this.getColorBitboard(color));
};
Chess.Position.prototype.getKingPosition = function(color) {
	return this.getPieceColorBitboard(Chess.Piece.KING, color).getLowestBitPosition();
};
Chess.Position.prototype.getOccupiedBitboard = function() {
	return this.bitboards[Chess.Position.ANY_WHITE].dup().or(this.bitboards[Chess.Position.ANY_BLACK]);
};
Chess.Position.prototype.getEmptyBitboard = function() {
	return this.getOccupiedBitboard().not();
};
Chess.Position.prototype.getTurnColor = function() {
	return this.turn;
};
Chess.Position.prototype.findPieceAtOrNull = function(index) {
	for (var piece = Chess.Piece.PAWN; piece <= Chess.Piece.KING; ++piece) {
		if (this.getPieceBitboard(piece).isSet(index)) {
			return piece;
		}
	}
	return null;
};
Chess.Position.prototype.getPieceAtOrNull = function(index) {
	return this.pieces[index];
};
Chess.Position.prototype.fillPiecesFromBitboards = function() {
	this.pieces.length = 0;
	for (var index = 0; index < 64; ++index) {
		this.pieces.push(this.findPieceAtOrNull(index));
	}
};
Chess.Position.prototype.isKingInCheck = function() {
    const p = this.getKingPosition(this.getTurnColor())
    if (p>=64) return false
	return this.isAttacked(Chess.getOtherPieceColor(this.getTurnColor()), p);
};
Chess.Position.makePawnAttackMask = function(color, pawns) {
	var white = (color === Chess.PieceColor.WHITE);
	var attacks1 = pawns.dup().and_not(Chess.Bitboard.FILES[0]).shiftLeft(white ? 7 : -9);
	var attacks2 = pawns.dup().and_not(Chess.Bitboard.FILES[Chess.LAST_FILE]).shiftLeft(white ? 9 : -7);
	return attacks1.or(attacks2);
};
Chess.Position.makeSlidingAttackMask = function(fromBB, occupied, rankDirection, fileDirection) {
	var bb = Chess.Bitboard.makeZero();
	var direction = rankDirection * Chess.FILES + fileDirection;
	var mask = Chess.Position.SLIDING_MASKS[1 + fileDirection];
	for (fromBB.shiftLeft(direction); !fromBB.and(mask).isEmpty(); fromBB.and_not(occupied).shiftLeft(direction)) {
		bb.or(fromBB);
	}
	return bb;
};
Chess.Position.makeBishopAttackMask = function(fromBB, occupied) {
	return Chess.Position.makeSlidingAttackMask(fromBB.dup(), occupied, 1, 1).or(
		Chess.Position.makeSlidingAttackMask(fromBB.dup(), occupied, 1, -1)).or(
		Chess.Position.makeSlidingAttackMask(fromBB.dup(), occupied, -1, 1)).or(
		Chess.Position.makeSlidingAttackMask(fromBB.dup(), occupied, -1, -1));
};
Chess.Position.makeRookAttackMask = function(fromBB, occupied) {
	return Chess.Position.makeSlidingAttackMask(fromBB.dup(), occupied, 0, 1).or(
		Chess.Position.makeSlidingAttackMask(fromBB.dup(), occupied, 0, -1)).or(
		Chess.Position.makeSlidingAttackMask(fromBB.dup(), occupied, 1, 0)).or(
		Chess.Position.makeSlidingAttackMask(fromBB.dup(), occupied, -1, 0));
};
Chess.Position.prototype.isAttacked = function(color, index) {
	var pawns = this.getPieceColorBitboard(Chess.Piece.PAWN, color);
	if (Chess.Position.makePawnAttackMask(color, pawns).isSet(index)) {
		return true;
	}
	var knights = this.getPieceColorBitboard(Chess.Piece.KNIGHT, color);
	if (!Chess.Bitboard.KNIGHT_MOVEMENTS[index].dup().and(knights).isEmpty()) {
		return true;
	}
	var king = this.getPieceColorBitboard(Chess.Piece.KING, color);
	if (!Chess.Bitboard.KING_MOVEMENTS[index].dup().and(king).isEmpty()) {
		return true;
	}
	var occupied = this.getOccupiedBitboard();
	var queens = this.getPieceColorBitboard(Chess.Piece.QUEEN, color);
	var bq = this.getPieceColorBitboard(Chess.Piece.BISHOP, color).dup().or(queens);
	if (Chess.Position.makeBishopAttackMask(bq, occupied).isSet(index)) {
		return true;
	}
	var rq = this.getPieceColorBitboard(Chess.Piece.ROOK, color).dup().or(queens);
	if (Chess.Position.makeRookAttackMask(rq, occupied).isSet(index)) {
		return true;
	}
	return false;
};
Chess.Position.prototype.getAttackers = function(color, index) {
    var retval = 0
    var pawns = this.getPieceColorBitboard(Chess.Piece.PAWN, color);
	if (Chess.Position.makePawnAttackMask(color, pawns).isSet(index)) {
		retval |= (1 << Chess.Piece.PAWN);
	}
	var knights = this.getPieceColorBitboard(Chess.Piece.KNIGHT, color);
	if (!Chess.Bitboard.KNIGHT_MOVEMENTS[index].dup().and(knights).isEmpty()) {
		retval |= (1 << Chess.Piece.KNIGHT);
	}
	var king = this.getPieceColorBitboard(Chess.Piece.KING, color);
	if (!Chess.Bitboard.KING_MOVEMENTS[index].dup().and(king).isEmpty()) {
		retval |= (1 << Chess.Piece.KING);
	}
	var occupied = this.getOccupiedBitboard();
	var queens = this.getPieceColorBitboard(Chess.Piece.QUEEN, color);
	var bq = this.getPieceColorBitboard(Chess.Piece.BISHOP, color);
	if (Chess.Position.makeBishopAttackMask(bq, occupied).isSet(index)) {
		retval |= (1 << Chess.Piece.BISHOP);
	}
	var rq = this.getPieceColorBitboard(Chess.Piece.ROOK, color);
	if (Chess.Position.makeRookAttackMask(rq, occupied).isSet(index)) {
		retval |= (1 << Chess.Piece.ROOK);
	}
  	if (Chess.Position.makeBishopAttackMask(queens, occupied).isSet(index)) {
		retval |= (1 << Chess.Piece.QUEEN);
	}
	if (Chess.Position.makeRookAttackMask(queens, occupied).isSet(index)) {
        retval |= (1 << Chess.Piece.QUEEN)
    }
	return false;
};
Chess.Position.getCastlingIndex = function(color, kingSide) {
	return color + (kingSide ? 0 : 2);
};
Chess.Position.getCastlingRookSquare = function(color, kingSide) {
	return Chess.Position.ROOK_INDICES[Chess.Position.getCastlingIndex(color, kingSide)];
};
Chess.Position.prototype.hasCastlingRight = function(color, kingSide) {
	return 0 !== (this.castlingRights & (1 << Chess.Position.getCastlingIndex(color, kingSide)));
};
Chess.Position.prototype.clearCastlingRight = function(color, kingSide) {
	this.castlingRights &= ~(1 << Chess.Position.getCastlingIndex(color, kingSide));
};
Chess.Position.prototype.canCastle = function(color, kingSide, onlyLegal) {
	if (!this.hasCastlingRight(color, kingSide)) {
		return false;
	}
	var direction = kingSide ? 1 : -1;
	var kingPosition = (color === Chess.PieceColor.WHITE) ? 4 : 60;
	var occupied = this.getOccupiedBitboard();
	if (occupied.isSet(kingPosition + direction) || occupied.isSet(kingPosition + 2 * direction)) {
		return false;
	}
	if (!kingSide && occupied.isSet(kingPosition + 3 * direction)) {
		return false;
	}
	if (onlyLegal && !this.isCastlingLegal(color, kingSide)) {
		return false;
	}
	return true;
};
Chess.Position.prototype.isCastlingLegal = function(color, kingSide) {
	var otherColor = Chess.getOtherPieceColor(color);
	var direction = kingSide ? 1 : -1;
	var kingPosition = (color === Chess.PieceColor.WHITE) ? 4 : 60;
	return !this.isAttacked(otherColor, kingPosition) && !this.isAttacked(otherColor, kingPosition + direction) && !this.isAttacked(otherColor, kingPosition + 2 * direction);
};
Chess.Position.prototype.canEnPassant = function() {
	return this.getEnPassantSquare() >= 0;
};
Chess.Position.prototype.getEnPassantSquare = function() {
	return this.enPassantSquare;
};
Chess.Position.prototype.isInsufficientMaterialDraw = function() {
	if (!this.getPieceBitboard(Chess.Piece.PAWN).isEmpty()) {
		return false;
	}
	if (!this.getPieceBitboard(Chess.Piece.ROOK).isEmpty()) {
		return false;
	}
	if (!this.getPieceBitboard(Chess.Piece.QUEEN).isEmpty()) {
		return false;
	}

	var whiteCount = this.getColorBitboard(Chess.PieceColor.WHITE).popcnt();
	var blackCount = this.getColorBitboard(Chess.PieceColor.BLACK).popcnt();
	if (whiteCount + blackCount < 4) {

		return true;
	}
	if (!this.getPieceBitboard(Chess.Piece.KNIGHT).isEmpty()) {
		return false;
	}

	var bishops = this.getPieceBitboard(Chess.Piece.BISHOP);
	if (bishops.dup().and(Chess.Bitboard.LIGHT_SQUARES).isEqual(bishops) || bishops.dup().and(Chess.Bitboard.DARK_SQUARES).isEqual(bishops)) {
		return true;
	}
	return false;
};
Chess.Position.prototype.isDraw = function() {
	return this.isInsufficientMaterialDraw();
};
Chess.Position.prototype.getStatus = function() {
	if (!this.getMoves().length) {
		return this.isKingInCheck() ? Chess.Position.Status.CHECKMATE : Chess.Position.Status.STALEMATE_DRAW;
	}
	if (this.isFiftyMoveRuleDraw()) {
		return Chess.Position.Status.FIFTY_MOVE_RULE_DRAW;
	}
	if (this.isThreefoldRepetitionRuleDraw()) {
		return Chess.Position.Status.THREEFOLD_REPETITION_RULE_DRAW;
	}
	if (this.isInsufficientMaterialDraw()) {
		return Chess.Position.Status.INSUFFICIENT_MATERIAL_DRAW;
	}
	return Chess.Position.Status.NORMAL;
};
Chess.Position.prototype.generateMoves = function(onlyCaptures) {
	var moves = [];
	var turnColor = this.getTurnColor();
	var opponentBB = this.getColorBitboard(Chess.getOtherPieceColor(turnColor));
	var occupied = this.getOccupiedBitboard();
	var chessPosition = this;

	function addPawnMoves(toMask, movement, kind) {
		while (!toMask.isEmpty()) {
			var index = toMask.extractLowestBitPosition();
			moves.push(new Chess.Move(index - movement, index, kind, Chess.Piece.PAWN, chessPosition.getPieceAtOrNull(index)));
		}
	}
	function addPawnPromotions(toMask, movement, capture) {
		addPawnMoves(toMask.dup(), movement, capture ? Chess.Move.Kind.QUEEN_PROMOTION_CAPTURE : Chess.Move.Kind.QUEEN_PROMOTION);
		addPawnMoves(toMask.dup(), movement, capture ? Chess.Move.Kind.ROOK_PROMOTION_CAPTURE : Chess.Move.Kind.ROOK_PROMOTION);
		addPawnMoves(toMask.dup(), movement, capture ? Chess.Move.Kind.BISHOP_PROMOTION_CAPTURE : Chess.Move.Kind.BISHOP_PROMOTION);
		addPawnMoves(toMask.dup(), movement, capture ? Chess.Move.Kind.KNIGHT_PROMOTION_CAPTURE : Chess.Move.Kind.KNIGHT_PROMOTION);
	}
	var fileDirection = 1 - 2 * turnColor;
	var rankDirection = Chess.FILES * fileDirection;
	var turnPawns = this.getPieceColorBitboard(Chess.Piece.PAWN, turnColor);
	var lastRow = Chess.Bitboard.RANKS[turnColor ? 0 : Chess.LAST_RANK];
	if (!onlyCaptures) {

		var doublePawnPushed = turnPawns.dup().and(Chess.Bitboard.RANKS[turnColor ? 6 : 1]).shiftLeft(2 * rankDirection).and_not(occupied).and_not(occupied.dup().shiftLeft(rankDirection));
		addPawnMoves(doublePawnPushed, 2 * rankDirection, Chess.Move.Kind.DOUBLE_PAWN_PUSH);


		var positionalPawnMoved = turnPawns.dup().shiftLeft(rankDirection).and_not(occupied);
		addPawnMoves(positionalPawnMoved.dup().and_not(lastRow), rankDirection, Chess.Move.Kind.POSITIONAL);
		addPawnPromotions(positionalPawnMoved.dup().and(lastRow), rankDirection, false);
	}


	var leftFile = Chess.Bitboard.FILES[turnColor ? Chess.LAST_FILE : 0];
	var leftCaptureMovement = rankDirection - fileDirection;
	var pawnLeftCaptured = turnPawns.dup().and_not(leftFile).shiftLeft(leftCaptureMovement).and(opponentBB);
	addPawnMoves(pawnLeftCaptured.dup().and_not(lastRow), leftCaptureMovement, Chess.Move.Kind.CAPTURE);
	addPawnPromotions(pawnLeftCaptured.dup().and(lastRow), leftCaptureMovement, true);
	var rightFile = Chess.Bitboard.FILES[turnColor ? 0 : Chess.LAST_FILE];
	var rightCaptureMovement = rankDirection + fileDirection;
	var pawnRightCaptured = turnPawns.dup().and_not(rightFile).shiftLeft(rightCaptureMovement).and(opponentBB);
	addPawnMoves(pawnRightCaptured.dup().and_not(lastRow), rightCaptureMovement, Chess.Move.Kind.CAPTURE);
	addPawnPromotions(pawnRightCaptured.dup().and(lastRow), rightCaptureMovement, true);

	if (this.canEnPassant()) {
		var pawnLeftEnPassant = Chess.Bitboard.makeIndex(this.getEnPassantSquare() + fileDirection).and(turnPawns).and_not(leftFile).shiftLeft(leftCaptureMovement);
		addPawnMoves(pawnLeftEnPassant, leftCaptureMovement, Chess.Move.Kind.EN_PASSANT_CAPTURE);
		var pawnRightEnPassant = Chess.Bitboard.makeIndex(this.getEnPassantSquare() - fileDirection).and(turnPawns).and_not(rightFile).shiftLeft(rightCaptureMovement);
		addPawnMoves(pawnRightEnPassant, rightCaptureMovement, Chess.Move.Kind.EN_PASSANT_CAPTURE);
	}

	function addNormalMoves(from, toMask, piece) {
		while (!toMask.isEmpty()) {
			var to = toMask.extractLowestBitPosition();
			moves.push(new Chess.Move(from, to, opponentBB.isSet(to) ? Chess.Move.Kind.CAPTURE : Chess.Move.Kind.POSITIONAL, piece, chessPosition.getPieceAtOrNull(to)));
		}
	}
	var mask = this.getColorBitboard(turnColor).dup().not();
	if (onlyCaptures) {
		mask.and(opponentBB);
	}
	var turnKnights = this.getPieceColorBitboard(Chess.Piece.KNIGHT, turnColor).dup();
	while (!turnKnights.isEmpty()) {
		var knightPosition = turnKnights.extractLowestBitPosition();
		addNormalMoves(knightPosition, Chess.Bitboard.KNIGHT_MOVEMENTS[knightPosition].dup().and(mask), Chess.Piece.KNIGHT);
	}
	var turnQueens = this.getPieceColorBitboard(Chess.Piece.QUEEN, turnColor).dup();
	while (!turnQueens.isEmpty()) {
		var queenPosition = turnQueens.extractLowestBitPosition();
		addNormalMoves(queenPosition, Chess.Position.makeBishopAttackMask(Chess.Bitboard.makeIndex(queenPosition), occupied).or(
			Chess.Position.makeRookAttackMask(Chess.Bitboard.makeIndex(queenPosition), occupied)).and(mask), Chess.Piece.QUEEN);
	}
	var turnBishops = this.getPieceColorBitboard(Chess.Piece.BISHOP, turnColor).dup();
	while (!turnBishops.isEmpty()) {
		var bishopPosition = turnBishops.extractLowestBitPosition();
		addNormalMoves(bishopPosition, Chess.Position.makeBishopAttackMask(Chess.Bitboard.makeIndex(bishopPosition), occupied).and(mask), Chess.Piece.BISHOP);
	}
	var turnRooks = this.getPieceColorBitboard(Chess.Piece.ROOK, turnColor).dup();
	while (!turnRooks.isEmpty()) {
		var rookPosition = turnRooks.extractLowestBitPosition();
		addNormalMoves(rookPosition, Chess.Position.makeRookAttackMask(Chess.Bitboard.makeIndex(rookPosition), occupied).and(mask), Chess.Piece.ROOK);
	}
	var kingPosition = this.getKingPosition(turnColor);
    if (kingPosition<64) {
        addNormalMoves(kingPosition, Chess.Bitboard.KING_MOVEMENTS[kingPosition].dup().and(mask), Chess.Piece.KING);
    }
	if (!onlyCaptures && kingPosition<64) {

		if (this.canCastle(turnColor, true, true)) {
			moves.push(new Chess.Move(kingPosition, kingPosition + 2, Chess.Move.Kind.KING_CASTLE, Chess.Piece.KING, null));
		}
		if (this.canCastle(turnColor, false, true)) {
			moves.push(new Chess.Move(kingPosition, kingPosition - 2, Chess.Move.Kind.QUEEN_CASTLE, Chess.Piece.KING, null));
		}
	}
	return moves;
};
Chess.Position.prototype.capturePiece = function(piece, color, index) {
	this.getPieceBitboard(piece).clearBit(index);
	this.getColorBitboard(color).clearBit(index);
	this.pieces[index] = null;
};
Chess.Position.prototype.unCapturePiece = function(piece, color, index) {
	this.getPieceBitboard(piece).setBit(index);
	this.getColorBitboard(color).setBit(index);
	this.pieces[index] = (piece);
};
Chess.Position.prototype.movePiece = function(piece, color, from, to) {
	var fromToBB = Chess.Bitboard.makeIndex(from).or(Chess.Bitboard.makeIndex(to));
	this.getPieceBitboard(piece).xor(fromToBB);
	this.getColorBitboard(color).xor(fromToBB);
	this.pieces[from] = null;
	this.pieces[to] = (piece);
};
Chess.Position.prototype.castleRook = function(color, kingSide) {
	var from = Chess.Position.getCastlingRookSquare(color, kingSide);
	var to = from + (kingSide ? -2 : 3);
	this.movePiece(Chess.Piece.ROOK, color, from, to);
};
Chess.Position.prototype.unCastleRook = function(color, kingSide) {
	var to = Chess.Position.getCastlingRookSquare(color, kingSide);
	var from = to + (kingSide ? -2 : 3);
	this.movePiece(Chess.Piece.ROOK, color, from, to);
};
Chess.Position.prototype.promotePiece = function(pieceOld, pieceNew, color, index) {
	this.getPieceBitboard(pieceOld).clearBit(index);
	this.getPieceBitboard(pieceNew).setBit(index);
	this.pieces[index] = (pieceNew);
};
Chess.Position.prototype.updatePieces = function(move) {
	if (move.isCapture()) {
		this.capturePiece(move.getCapturedPiece(), Chess.getOtherPieceColor(this.getTurnColor()), move.getCaptureSquare());
	}
	if (move.isCastle()) {
		this.castleRook(this.getTurnColor(), move.getKind() === Chess.Move.Kind.KING_CASTLE);
	}
	this.movePiece(move.getPiece(), this.getTurnColor(), move.getFrom(), move.getTo());
	if (move.isPromotion()) {
		this.promotePiece(Chess.Piece.PAWN, move.getPromotedPiece(), this.getTurnColor(), move.getTo());
	}
};
Chess.Position.prototype.revertPieces = function(move) {
	if (move.isPromotion()) {
		this.promotePiece(move.getPromotedPiece(), Chess.Piece.PAWN, this.getTurnColor(), move.getTo());
	}
	this.movePiece(move.getPiece(), this.getTurnColor(), move.getTo(), move.getFrom());
	if (move.isCastle()) {
		this.unCastleRook(this.getTurnColor(), move.getKind() === Chess.Move.Kind.KING_CASTLE);
	}
	if (move.isCapture()) {
		this.unCapturePiece(move.getCapturedPiece(), Chess.getOtherPieceColor(this.getTurnColor()), move.getCaptureSquare());
	}
};
Chess.Position.prototype.isMoveLegal = function(move) {
	this.updatePieces(move);
	var kingInCheck = this.isKingInCheck();
	this.revertPieces(move);
	return !kingInCheck;
};
Chess.Position.prototype.makeMove = function(move) {
	this.updatePieces(move);
	if (this.isKingInCheck()) {
		this.revertPieces(move);
		return false;
	}
	this.madeMoves.push(move);
	this.irreversibleHistory.push(this.enPassantSquare);
	this.irreversibleHistory.push(this.castlingRights);
	this.irreversibleHistory.push(this.halfmoveClock);
	if (move.getKind() === Chess.Move.Kind.DOUBLE_PAWN_PUSH) {
		this.enPassantSquare = move.getTo();
	} else {
		this.enPassantSquare = -1;
	}
	var turnColor = this.getTurnColor();
	if (move.getPiece() === Chess.Piece.KING) {
		this.clearCastlingRight(turnColor, true);
		this.clearCastlingRight(turnColor, false);
	} else if (move.getPiece() === Chess.Piece.ROOK) {
		if (move.getFrom() === Chess.Position.getCastlingRookSquare(turnColor, true)) {
			this.clearCastlingRight(turnColor, true);
		} else if (move.getFrom() === Chess.Position.getCastlingRookSquare(turnColor, false)) {
			this.clearCastlingRight(turnColor, false);
		}
	}
	var otherColor = Chess.getOtherPieceColor(turnColor);
	if (move.getCapturedPiece() === Chess.Piece.ROOK) {
		if (move.getCaptureSquare() === Chess.Position.getCastlingRookSquare(otherColor, true)) {
			this.clearCastlingRight(otherColor, true);
		} else if (move.getCaptureSquare() === Chess.Position.getCastlingRookSquare(otherColor, false)) {
			this.clearCastlingRight(otherColor, false);
		}
	}
	if (move.isCapture() || move.getPiece() === Chess.Piece.PAWN) {
		this.halfmoveClock = 0;
	} else {
		++this.halfmoveClock;
	}
	this.turn = otherColor;
	return true;
};
Chess.Position.prototype.getMadeMoveCount = function() {
	return this.madeMoves.length;
};
Chess.Position.prototype.canUndo = function() {
	return !!this.getMadeMoveCount();
};
Chess.Position.prototype.getLastMove = function() {
	if (!this.canUndo()) {
		return null;
	}
	return this.madeMoves[this.madeMoves.length - 1];
};
Chess.Position.prototype.unmakeMove = function() {
	if (!this.canUndo()) {
		return null;
	}
	var move = (this.madeMoves.pop());
	this.turn = Chess.getOtherPieceColor(this.getTurnColor());
	this.revertPieces(move);
	this.halfMoveClock = (this.irreversibleHistory.pop());
	this.castlingRights = (this.irreversibleHistory.pop());
	this.enPassantSquare = (this.irreversibleHistory.pop());
	return move;
};
Chess.Position.prototype.str = function() {
    let rv = []
    for (let i=0; i<8; i++) {
       let s = this.bitboards[i].high.toString(16).padStart(8,'0') + this.bitboards[i].low.toString(16).padStart(8,'0')
       rv.push(s)
    }
    return rv.join("/")
}
Chess.Parser = function() {
};
Chess.Parser.clean = function(text) {
	text = text.replace(/[\r\n\t]/gm, " ");
	text = text.replace(/[\u002D\u05BE\u1806\u2010\u2011\u2012\u2013\u2014\u2015\u207B\u208B\u2212\u2E3A\u2E3B\uFE58\uFE63\uFF0D]/g, "-");
	while (true) {
		var replaced = text.replace(/\([^()]*\)/g, "");
		if (replaced === text) {
			break;
		}
		text = replaced;
	}
	text = text.replace(/[^ a-z0-9.=:\u00BD-]/gi, " ");
	text = text.replace(/  +/g, " ");
	return text;
};
Chess.Parser.parseOneMove = function(chessPosition, text) {
	var legalMoves = chessPosition.getMoves();
	var castling = text.match(/0-0(?:-0)?|O-O(?:-O)?/i);
	if (castling) {
		var kind = (castling[0].length === 3) ? Chess.Move.Kind.KING_CASTLE : Chess.Move.Kind.QUEEN_CASTLE;
		return legalMoves.filter(function(move) { return move.getKind() === kind; });
	}
	var move = text.match(/([NBRQK])?([a-h])?([1-8])?-?([x:])?([a-h])([1-8])?(?:[=(]([NBRQ]))?/);
	if (move) {
		var piece = move[1];
		var fromFile = move[2];
		var fromRank = move[3];
		var capture = move[4];
		var toFile = move[5];
		var toRank = move[6];
		var promotedPiece = move[7];
		return legalMoves.filter(function(move) {
			if (piece !== undefined && Chess.PIECE_ALGEBRAIC_NAMES[move.getPiece()] !== piece) {
				return false;
			}
			if (piece === undefined && move.getPiece() !== Chess.Piece.PAWN) {
				return false;
			}
			if (fromFile !== undefined && Chess.FILE_CHARACTERS[Chess.getFile(move.getFrom())] !== fromFile) {
				return false;
			}
			if (fromRank !== undefined && Chess.RANK_CHARACTERS[Chess.getRank(move.getFrom())] !== fromRank) {
				return false;
			}
			if (capture !== undefined && !move.isCapture()) {
				return false;
			}
			if (toFile !== undefined && Chess.FILE_CHARACTERS[Chess.getFile(move.getTo())] !== toFile) {
				return false;
			}
			if (toRank !== undefined && Chess.RANK_CHARACTERS[Chess.getRank(move.getTo())] !== toRank) {
				return false;
			}
			if (promotedPiece !== undefined && Chess.PIECE_ALGEBRAIC_NAMES[move.getPromotedPiece()] !== promotedPiece) {
				return false;
			}
			return true;
		});
	}
	return null;
};
Chess.Parser.parseMoves = function(text) {
	var chessPosition = new Chess.Position;
	Chess.Parser.clean(text).split(" ").every(function(moveText) {
		var moveNumber = moveText.match(/\d+\./);
		if (moveNumber) {
			return true;
		}
		var gameOver = moveText.match(/1-0|0-1|\u00BD-\u00BD/);
		if (gameOver) {
			return false;
		}
		var moves = Chess.Parser.parseOneMove(chessPosition, moveText);
		if (!moves || moves.length !== 1) {
			throw new Error("Parse error in '" + moveText + "'");
		}
		chessPosition.makeMove(moves[0]);
		return true;
	});
	return chessPosition;
};
Chess.AI = function() {
};
Chess.AI.PIECE_VALUES = [100, 300, 300, 500, 900, 20000];
Chess.AI.PIECE_SQUARE_TABLES = [

	[
		0, 0, 0, 0, 0, 0, 0, 0,
		50, 50, 50, 50, 50, 50, 50, 50,
		10, 10, 20, 30, 30, 20, 10, 10,
		5, 5, 10, 25, 25, 10, 5, 5,
		0, 0, 0, 20, 20, 0, 0, 0,
		5, -5, -10, 0, 0, -10, -5, 5,
		5, 10, 10, -20, -20, 10, 10, 5,
		0, 0, 0, 0, 0, 0, 0, 0
	],

	[
		-50, -40, -30, -30, -30, -30, -40, -50,
		-40, -20, 0, 0, 0, 0, -20, -40,
		-30, 0, 10, 15, 15, 10, 0, -30,
		-30, 5, 15, 20, 20, 15, 5, -30,
		-30, 0, 15, 20, 20, 15, 0, -30,
		-30, 5, 10, 15, 15, 10, 5, -30,
		-40, -20, 0, 5, 5, 0, -20, -40,
		-50, -40, -30, -30, -30, -30, -40, -50
	],

	[
		-20, -10, -10, -10, -10, -10, -10, -20,
		-10, 0, 0, 0, 0, 0, 0, -10,
		-10, 0, 5, 10, 10, 5, 0, -10,
		-10, 5, 5, 10, 10, 5, 5, -10,
		-10, 0, 10, 10, 10, 10, 0, -10,
		-10, 10, 10, 10, 10, 10, 10, -10,
		-10, 5, 0, 0, 0, 0, 5, -10,
		-20, -10, -10, -10, -10, -10, -10, -20
	],

	[
		0, 0, 0, 0, 0, 0, 0, 0,
		5, 10, 10, 10, 10, 10, 10, 5,
		-5, 0, 0, 0, 0, 0, 0, -5,
		-5, 0, 0, 0, 0, 0, 0, -5,
		-5, 0, 0, 0, 0, 0, 0, -5,
		-5, 0, 0, 0, 0, 0, 0, -5,
		-5, 0, 0, 0, 0, 0, 0, -5,
		0, 0, 0, 5, 5, 0, 0, 0
	],

	[
		-20, -10, -10, -5, -5, -10, -10, -20,
		-10, 0, 0, 0, 0, 0, 0, -10,
		-10, 0, 5, 5, 5, 5, 0, -10,
		-5, 0, 5, 5, 5, 5, 0, -5,
		0, 0, 5, 5, 5, 5, 0, -5,
		-10, 5, 5, 5, 5, 5, 0, -10,
		-10, 0, 5, 0, 0, 0, 0, -10,
		-20, -10, -10, -5, -5, -10, -10, -20
	],

	[
		-30, -40, -40, -50, -50, -40, -40, -30,
		-30, -40, -40, -50, -50, -40, -40, -30,
		-30, -40, -40, -50, -50, -40, -40, -30,
		-30, -40, -40, -50, -50, -40, -40, -30,
		-20, -30, -30, -40, -40, -30, -30, -20,
		-10, -20, -20, -20, -20, -20, -20, -10,
		 20, 20, 0, 0, 0, 0, 20, 20,
		 20, 30, 10, 0, 0, 10, 30, 20
	]
];
Chess.AI.BISHOP_PAIR_VALUE = Chess.AI.PIECE_VALUES[Chess.Piece.PAWN] / 2;
Chess.AI.BISHOP_PAIR_VALUE_EN = true;
Chess.AI.getMaterialValue = function(chessPosition, color) {
	var value = 0;
	for (var piece = Chess.Piece.PAWN; piece < Chess.Piece.KING; ++piece) {
		value += chessPosition.getPieceColorBitboard(piece, color).popcnt() * Chess.AI.PIECE_VALUES[piece];
	}
	if (Chess.AI.BISHOP_PAIR_VALUE_EN && chessPosition.getPieceColorBitboard(Chess.Piece.BISHOP, color).popcnt() > 1) {
		value += Chess.AI.BISHOP_PAIR_VALUE;
	}
	return value;
};
Chess.AI.evaluateMaterial = function(chessPosition) {
	return Chess.AI.getMaterialValue(chessPosition, Chess.PieceColor.WHITE) - Chess.AI.getMaterialValue(chessPosition, Chess.PieceColor.BLACK);
};
Chess.AI.getPieceSquareValue = function(chessPosition, color) {
	var value = 0;
	for (var piece = Chess.Piece.PAWN; piece <= Chess.Piece.KING; ++piece) {
		var pieces = chessPosition.getPieceColorBitboard(piece, color).dup();
		while (!pieces.isEmpty()) {
			var index = pieces.extractLowestBitPosition();
			value += Chess.AI.PIECE_SQUARE_TABLES[piece][color ? index : (56 ^ index)];
		}
	}
	return value;
};
Chess.AI.evaluateLocations = function(chessPosition) {
	return Chess.AI.getPieceSquareValue(chessPosition, Chess.PieceColor.WHITE) - Chess.AI.getPieceSquareValue(chessPosition, Chess.PieceColor.BLACK);
};
Chess.AI.makePawnPositionalMask = function(color, pawns, empty) {
	var white = (color === Chess.PieceColor.WHITE);
	var positional = pawns.dup().shiftLeft(white ? 8 : -8).and(empty);
	var doublePush = pawns.dup().and(Chess.Bitboard.RANKS[white ? 1 : 6]).shiftLeft(white ? 16 : -16).and(empty).and(empty.dup().shiftLeft(white ? 8 : -8));
	return positional.or(doublePush);
};
Chess.AI.getMobilityValue = function(chessPosition, color) {
	var us = chessPosition.getColorBitboard(color);
	var opponent = chessPosition.getColorBitboard(Chess.getOtherPieceColor(color));
	var occupied = chessPosition.getOccupiedBitboard();
	var empty = chessPosition.getEmptyBitboard();
	var mobility = 0;
	mobility += Chess.AI.makePawnPositionalMask(color, chessPosition.getPieceColorBitboard(Chess.Piece.PAWN, color), empty).popcnt();
	mobility += Chess.Position.makePawnAttackMask(color, chessPosition.getPieceColorBitboard(Chess.Piece.PAWN, color)).and(opponent).popcnt();
	var knights = chessPosition.getPieceColorBitboard(Chess.Piece.KNIGHT, color).dup();
	while (!knights.isEmpty()) {
		mobility += Chess.Bitboard.KNIGHT_MOVEMENTS[knights.extractLowestBitPosition()].dup().and_not(us).popcnt();
	}
    const kp = chessPosition.getKingPosition(color)
    if (kp<64) mobility += Chess.Bitboard.KING_MOVEMENTS[kp].dup().and_not(us).popcnt();
	var queens = chessPosition.getPieceColorBitboard(Chess.Piece.QUEEN, color);
	var bq = chessPosition.getPieceColorBitboard(Chess.Piece.BISHOP, color).dup().or(queens);
	mobility += Chess.Position.makeBishopAttackMask(bq, occupied).and_not(us).popcnt();
	var rq = chessPosition.getPieceColorBitboard(Chess.Piece.ROOK, color).dup().or(queens);
	mobility += Chess.Position.makeRookAttackMask(rq, occupied).and_not(us).popcnt();
	return mobility * Chess.AI.PIECE_VALUES[Chess.Piece.PAWN] / 100;
};
Chess.AI.evaluate = function(chessPosition) {
	return Chess.AI.evaluateMaterial(chessPosition) + Chess.AI.evaluateLocations(chessPosition);
};
Chess.AI.evals = 0
Chess.AI.table = new Array(16384)
Chess.AI.search = function(chessPosition) {
	function sortMoves(moves) {
		function scoreMove(move) {
			var score = move.isCapture() ? ((1 + move.getCapturedPiece()) / (1 + move.getPiece())) : 0;
			score = 6 * score + move.getPiece();
			score = 16 * score + move.getKind();
			score = 64 * score + move.getTo();
			score = 64 * score + move.getFrom();
			return score;
		}
		function compareMoves(a, b) {
			return scoreMove(b) - scoreMove(a);
		}
		moves.sort(compareMoves);
		return moves;
	}
	function quiescenceSearch(chessPosition, alpha, beta, depth=0) {
		var standPatValue = Chess.AI.evaluate(chessPosition);
        if (depth >= 4) {
            return standPatValue;
        }
		var white = (chessPosition.getTurnColor() === Chess.PieceColor.WHITE);
		if (white) {
			if (standPatValue >= beta) {
				return beta;
			}
			alpha = (standPatValue > alpha) ? standPatValue : alpha;
		} else {
			if (standPatValue <= alpha) {
				return alpha;
			}
			beta = (standPatValue < beta) ? standPatValue : beta;
		}
		var moves = sortMoves(chessPosition.getMoves(true, !chessPosition.isKingInCheck()));
		for (var i = 0; i < moves.length; ++i) {
			if (chessPosition.makeMove(moves[i])) {
				var value = quiescenceSearch(chessPosition, alpha, beta, depth+1);
				chessPosition.unmakeMove();
				if (white) {
					if (value >= beta) {
						return beta;
					}
					alpha = (value > alpha) ? value : alpha;
				} else {
					if (value <= alpha) {
						return alpha;
					}
					beta = (value < beta) ? value : beta;
				}
			}
		}
		return (white ? alpha : beta);
	}
	function alphaBeta(chessPosition, depth, alpha, beta, use_tt=false) {
		if (depth < 1) {
			const rv = quiescenceSearch(chessPosition, alpha, beta);
            return rv
        }
        Chess.AI.evals++

        const alpha_orig = alpha
        const beta_orig = alpha

        const white = (chessPosition.getTurnColor() === Chess.PieceColor.WHITE);

        const serial = chessPosition.serialize()
        const hash = chessPosition.hash()
        const tt_pos = (hash & 0xffff)
        const tt_entry = Chess.AI.table[tt_pos]

        if (use_tt && tt_entry !== undefined) {
            const [tt_hash, tt_depth, tt_flag, tt_rv] = tt_entry
            //if (tt_hash==hash) {
            //    console.log("hit", tt_entry, depth, alpha, beta)
            //}
            if (tt_hash==hash && tt_depth>=depth) {
                if (tt_flag==0) {
                    return tt_rv
                } else if (tt_flag < 0) {
                    alpha = (tt_rv > alpha) ? tt_rv : alpha
                } else if (tt_flag > 0) {
                    beta = (tt_rv < beta) ? tt_rv : beta
                }
                if (alpha > beta) {
                    return tt_rv
                }
            }
        }

        var moves = sortMoves(chessPosition.getMoves(true, false));
        var bestMove = null;
        var bestMoveVal = null;
		var legal = false;
		for (var i = 0; i < moves.length; ++i) {
			if (chessPosition.makeMove(moves[i])) {
				legal = true;
				var value = alphaBeta(chessPosition, depth - 1, alpha, beta, use_tt);
				chessPosition.unmakeMove();
				if (white && value > alpha) {
					alpha = value
				}
                if (!white && value < beta) {
					beta = value
				}
				if (beta <= alpha) {
					break;
				}
			}
		}
		if (!legal) {
			if (!chessPosition.isKingInCheck()) {
				return 0;
			}
			var mate = Chess.AI.PIECE_VALUES[Chess.Piece.KING];
            mate += depth
			return white ? -mate : mate;
		}
        const rv = (white ? alpha : beta)
        const flag = ((rv <= alpha_orig) ? 1 : ((rv >= beta) ? -1 : 0))
        if (use_tt) {
            Chess.AI.table[tt_pos] = [hash, depth, flag, rv]
        }
		return rv;
	}
	var alpha = -Infinity;
	var beta = Infinity;
	var moves = null;
	var bestMove = null;
	var bestMoveVal = null;
    moves = sortMoves(chessPosition.getMoves(true));
    for (let i=0; i<moves.length; i++) {
        if (chessPosition.makeMove(moves[i])) {
            var value = alphaBeta(chessPosition, Chess.DEPTH, alpha, beta, true);
            //Chess.AI.evals = 0
            //var start = performance.now()
            //var value = alphaBeta(chessPosition, Chess.DEPTH, alpha, beta, true);
            //const elapsed = performance.now()-start
            //const evals = Chess.AI.evals
            //Chess.AI.evals = 0
            //start = performance.now()
            //var value_tt = alphaBeta(chessPosition, Chess.DEPTH, alpha, beta, true);
            //const evals_tt = Chess.AI.evals
            //const elapsed_tt = performance.now()-start
            //console.log("evals", evals, Math.round(elapsed), evals_tt, Math.round(elapsed_tt))
            //if (value != value_tt) {
            //    throw new Error(`tt mismatch ${moves[i].getString()} nott=${value} tt=${value_tt}`)
            //}
            chessPosition.unmakeMove();
            if (chessPosition.getTurnColor() === Chess.PieceColor.WHITE) {
                if (value > alpha) {
                    alpha = value;
                    bestMoveVal = value;
                    bestMove = moves[i];
                }
            } else {
                if (value < beta) {
                    beta = value;
                    bestMoveVal = value;
                    bestMove = moves[i];
                }
            }
        }
        postMessage([ID, i, moves.length, [bestMove, bestMoveVal]])
    }
    return [bestMove, bestMoveVal]
};
}

function Chess() {}
CHESS(Chess)

class UniversalBoardDrawer {
    constructor(boardElem, config) {
        this.boardElem = boardElem;

        this.window = config?.window || window;
        this.document = this.window?.document;
        this.parentElem = config?.parentElem || this.document.body;

        this.boardDimensions = {
            'width': config?.boardDimensions?.[0] || 8,
            'height': config?.boardDimensions?.[1] || 8
        };

        this.adjustSizeByDimensions = config?.adjustSizeByDimensions || false;
        this.adjustSizeConfig = config?.adjustSizeConfig;
        this.orientation = config?.orientation || 'w';
        this.zIndex = config?.zIndex || 1000; // container z-index
        this.usePrepend = config?.prepend || false;
        this.debugMode = config?.debugMode || false;

        this.boardContainerElem = null;
        this.singleSquareSize = null;
        this.lastInputPositionStr = null;
        this.lastInputPosition = null;

        this.addedShapes = [];
        this.squareSvgCoordinates = [];
        this.observers = [];
        this.customActivityListeners = [];

        this.defaultFillColor = 'mediumseagreen';
        this.defaultOpacity = 0.8;

        this.updateInterval = 100;

        this.isInputDown = false;
        this.terminated = false;

        if(!this.document) {
            if(this.debugMode) console.error(`Inputted document element doesn't exist!`);

            return;
        }

        if(!this.boardElem) {
            if(this.debugMode) console.error(`Inputted board element doesn't exist!`);

            return;
        }

        if(typeof this.boardDimensions != 'object') {
            if(this.debugMode) console.error(`Invalid board dimensions value, please use array! (e.g. [8, 8])`);

            return;
        }

        this.createOverlaySVG();

        const handleMouseMove = e => {
            if (this.terminated) {
                this.document.removeEventListener('mousemove', handleMouseMove);
                return;
            }
            this.handleMouseEvent.bind(this)(e);
        };

        const handleTouchStart = e => {
            if (this.terminated) {
                this.document.removeEventListener('touchstart', handleTouchStart);
                return;
            }
            this.handleMouseEvent.bind(this)(e);
        };

        const handleMouseDown = () => {
            if (this.terminated) {
                this.document.removeEventListener('mousedown', handleMouseDown);
                return;
            }
            this.isInputDown = true;
        };

        const handleMouseUp = () => {
            if (this.terminated) {
                this.document.removeEventListener('mouseup', handleMouseUp);
                return;
            }

            this.isInputDown = false;
        };

        this.document.addEventListener('mousemove', handleMouseMove);
        this.document.addEventListener('touchstart', handleTouchStart);
        this.document.addEventListener('mousedown', handleMouseDown);
        this.document.addEventListener('mouseup', handleMouseUp);
    }

    setOrientation(orientation) {
        this.orientation = orientation;

        this.updateDimensions();
    }

    setBoardDimensions(dimensionArr) {

        const [width, height] = dimensionArr || [8, 8];
        this.boardDimensions = { width, height };

        this.updateDimensions();
    }

    setAdjustSizeByDimensions(boolean) {
        this.adjustSizeByDimensions = boolean;

        this.updateDimensions();
    }

    createArrowBetweenPositions(from, to, config) {
        const fromCoordinateObj = this.squareSvgCoordinates.find(x => this.coordinateToFen(x.coordinates) == from);
        const toCoordinateObj = this.squareSvgCoordinates.find(x => this.coordinateToFen(x.coordinates) == to);

        if(!fromCoordinateObj || !toCoordinateObj) {
            if(this.debugMode) console.error('Coordinates', from, to, 'do not exist. Possibly out of bounds?');

            return;
        }

        let [fromX, fromY] = fromCoordinateObj?.positions;
        let [toX, toY] = toCoordinateObj?.positions;

        fromX += (config?.fromXdelta || 0)
        fromY += (config?.fromYdelta || 0)
        toX += (config?.toXdelta || 0)
        toY += (config?.toYdelta || 0)

        const distance = Math.sqrt(Math.pow(fromX - toX, 2) + Math.pow(fromY - toY, 2));
        const angle = Math.atan2(fromY - toY, fromX - toX);

        const scale = this.singleSquareSize / 100;

        const lineWidth = (config.size || 15) * scale;
        const arrowheadWidth = Math.round(config.size*2.5) * scale;
        const arrowheadHeight = Math.round(config.size*1.5) * scale;
        const startOffset = 20 * scale;

        const arrowElem = this.document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
            arrowElem.setAttribute('transform', `rotate(${angle * (180 / Math.PI) - 90} ${fromX} ${fromY})`);

        const arrowPoints = [
            { x: fromX - lineWidth / 2, y: fromY - startOffset },
            { x: fromX - lineWidth / 2, y: fromY - distance + arrowheadHeight },
            { x: fromX - arrowheadWidth / 2, y: fromY - distance + arrowheadHeight },
            { x: fromX, y: fromY - distance },
            { x: fromX + arrowheadWidth / 2, y: fromY - distance + arrowheadHeight },
            { x: fromX + lineWidth / 2, y: fromY - distance + arrowheadHeight },
            { x: fromX + lineWidth / 2, y: fromY - startOffset }
        ];

        const pointsString = arrowPoints.map(point => `${point.x},${point.y}`).join(' ');
            arrowElem.setAttribute('points', pointsString);
            arrowElem.style.fill = this.defaultFillColor;
            arrowElem.style.opacity = this.defaultOpacity;

        const style = config?.style;

        if(style) arrowElem.setAttribute('style', style);

        return arrowElem;
    }

    createDotOnSVG(x, y) {
        const dot = this.document.createElementNS('http://www.w3.org/2000/svg', 'circle');
            dot.setAttribute('cx', x);
            dot.setAttribute('cy', y);
            dot.setAttribute('r', '1');
            dot.setAttribute('fill', 'black');

        this.addedShapes.push({ type: 'debugDot', 'element': dot });

        this.boardContainerElem.appendChild(dot);
    }

    removeAllExistingShapes() {
        this.addedShapes
            .forEach(shapeObj => {
                shapeObj.element?.remove();
            });
    }

    removeAllDebugDots() {
        this.addedShapes
            .filter(shapeObj => shapeObj.type == 'debugDot')
            .forEach(debugDotObj => {
                debugDotObj.element?.remove();
            });
    }

    updateShapes() {
        if(this.debugMode) {
            this.removeAllDebugDots();

            this.squareSvgCoordinates.forEach(x => this.createDotOnSVG(...x.positions));
        }

        this.addedShapes
            .filter(shapeObj => shapeObj.type != 'debugDot')
            .forEach(shapeObj => {
                const newShapeElem = this.createArrowBetweenPositions(...shapeObj.positions, shapeObj.config);

                this.transferAttributes(newShapeElem, shapeObj.element);
            });
    }

    coordinateToFen(coordinates) {
        let [x, y] = coordinates;

        x = this.orientation == 'w' ? x : this.boardDimensions.width - x + 1;
        y = this.orientation == 'b' ? y : this.boardDimensions.height - y + 1;

        const getCharacter = num => String.fromCharCode(96 + num);

        const file = getCharacter(x);
        const rank = y;

        return file + rank;
    }

    updateCoords() {
        this.squareSvgCoordinates = []; // reset coordinate array

        // calculate every square center point coordinates relative to the svg
        for(let y = 0; this.boardDimensions.height > y; y++) {
            for(let x = 0; this.boardDimensions.width > x; x++) {
                this.squareSvgCoordinates.push({
                    coordinates: [x + 1, y + 1],
                    positions: [this.squareWidth / 2 + (this.squareWidth * x),
                                this.squareHeight / 2 + (this.squareHeight * y)]
                });
            }
        }
    }

    transferAttributes(fromElem, toElem) {
        if(fromElem && fromElem?.attributes && toElem) {
            [...fromElem.attributes].forEach(attr =>
                toElem.setAttribute(attr.name, attr.value));
        }
    }

    createShape(type, positions, config) {
        if(this.terminated) {
            if(this.debugMode) console.warn('Failed to create shape! Tried to create shape after termination!');

            return false;
        }

        if(!this.boardContainerElem) {
            if(this.debugMode) console.warn(`Failed to create shape! Board SVG doesn't exist yet! (createOverlaySVG() failed?)`);

            return false;
        }

        const element = this.createArrowBetweenPositions(...positions, config);
        if(element) {
            this.addedShapes.push({ type, positions, config, element });

            if(this.usePrepend) {
                this.boardContainerElem.prepend(element);
            } else {
                this.boardContainerElem.appendChild(element);
            }

            return element;
        }

        return null;
    }

    updateDimensions() {
        const boardRect = this.boardElem.getBoundingClientRect(),
              bodyRect = this.document.body.getBoundingClientRect(); // https://stackoverflow.com/a/62106310

        let boardWidth = boardRect.width,
            boardHeight = boardRect.height;

        let boardPositionTop = boardRect.top - bodyRect.top,
            boardPositionLeft = boardRect.left - bodyRect.left;

        if(this.adjustSizeByDimensions) {

            if(this.boardDimensions.width > this.boardDimensions.height) {
                const multiplier = this.boardDimensions.height / this.boardDimensions.width,
                      newHeight = boardWidth * multiplier;

                if(boardHeight !== newHeight) {
                    if(!this.adjustSizeConfig?.noTopAdjustment)
                        boardPositionTop += (boardHeight - newHeight) / 2;

                    boardHeight = newHeight;
                }
            }
            else {
                const multiplier = this.boardDimensions.width / this.boardDimensions.height,
                      newWidth = boardWidth * multiplier;

                if(boardWidth !== newWidth) {
                    if(!this.adjustSizeConfig?.noLeftAdjustment)
                        boardPositionLeft += (boardWidth - newWidth) / 2;

                    boardWidth = newWidth;
                }
            }

        }

        this.boardContainerElem.style.width = boardWidth + 'px';
        this.boardContainerElem.style.height = boardHeight + 'px';
        this.boardContainerElem.style.left = boardPositionLeft + 'px';
        this.boardContainerElem.style.top = boardPositionTop + 'px';

        const squareWidth = boardWidth / this.boardDimensions.width;
        const squareHeight = boardHeight / this.boardDimensions.height;

        this.singleSquareSize = squareWidth;
        this.squareWidth = squareWidth;
        this.squareHeight = squareHeight;

        this.updateCoords();
        this.updateShapes();
    }

    createOverlaySVG() {
        const svg = this.document.createElementNS('http://www.w3.org/2000/svg', 'svg');
            svg.style.position = 'absolute';
            svg.style.pointerEvents = 'none';
            svg.style['z-index'] = this.zIndex;

        this.boardContainerElem = svg;

        this.updateDimensions();

        this.parentElem.appendChild(this.boardContainerElem);

        const rObs = new ResizeObserver(this.updateDimensions.bind(this));
            rObs.observe(this.boardElem);
            rObs.observe(this.document.body);

        this.observers.push(rObs);

        let oldBoardRect = JSON.stringify(this.boardElem.getBoundingClientRect());

        const additionalCheckLoop = setInterval(() => {
            if(this.terminated) {
                clearInterval(additionalCheckLoop);

                return;
            }

            const boardRect = JSON.stringify(this.boardElem.getBoundingClientRect());

            if(boardRect !== oldBoardRect) {
                oldBoardRect = boardRect;

                this.updateDimensions();
            }
        }, this.updateInterval);
    }

    getCoordinatesFromInputPosition(e) {
        const boardRect = this.boardElem.getBoundingClientRect();

        const { clientX, clientY } = e.touches ? e.touches[0] : e;
        const isOutOfBounds = clientX < boardRect.left || clientX > boardRect.right || clientY < boardRect.top || clientY > boardRect.bottom;

        const relativeX = clientX - boardRect.left;
        const relativeY = clientY - boardRect.top;

        return isOutOfBounds
            ? [null, null]
            : [Math.floor(relativeX / this.squareWidth) + 1, Math.floor(relativeY / this.squareHeight) + 1];
    }

    handleMouseEvent(e) {
        if(this.isInputDown) return;

        const position = this.getCoordinatesFromInputPosition(e),
                positionStr = position?.toString();

        if(positionStr != this.lastInputPositionStr) {
            const enteredSquareListeners = this.customActivityListeners.filter(obj => obj.square == this.coordinateToFen(position));

            enteredSquareListeners.forEach(obj => obj.cb('enter'));

            if(this.lastInputPosition && this.lastInputPosition[0] != null) {
                const leftSquareListeners = this.customActivityListeners.filter(obj => obj.square == this.coordinateToFen(this.lastInputPosition));

                leftSquareListeners.forEach(obj => obj.cb('leave'));
            }

            this.lastInputPositionStr = positionStr;
            this.lastInputPosition = position;
        }
    }

    addSquareListener(square, cb) {
        this.customActivityListeners.push({ square, cb });

        return { remove: () => {
            this.customActivityListeners = this.customActivityListeners.filter(obj => obj.square != square && obj.cb != cb);
        }};
    }

    terminate() {
        this.terminated = true;

        this.observers.forEach(observer => observer.disconnect());

        this.boardContainerElem.remove();
    }
}
function mv(p,dx,dy) {
    let x = Math.floor(p/10) + dx;
    let y = p%10 + dy;
    if (x<1 || x>8 || y<1 || y>8) return 0;
    return x*10+y
}
function xy2p(x,y) { return x*10+y }
function p2x(p) { return Math.floor(p/10) }
function p2y(p) { return p%10 }
function p2s(p) {
    let x = Math.floor(p/10);
    let y = p%10;
    return String.fromCharCode(96+x) + String(y)
}

function a2ft(i) {
    return [Math.floor(i/100), i%100]
}
function ft2a(f,t) {
   return f*100+t
}
function b2x(b) {
    return (b & 7) + 1
}
function b2y(b) {
    return (b >> 3) + 1
}
function xy2b(x,y) {
    return ((y-1) << 3) + x - 1
}
function p2b(p) {
    return xy2b(p2x(p),p2y(p))
}
function b2p(b) {
    return xy2p(b2x(b),b2y(b))
}
function b2a(f,t) {
    return ft2a(b2p(f),b2p(t))
}

function update(board, from, to) {
    let rv = structuredClone(board)
    rv[to] = rv[from]
    delete rv[from]
    return rv
}
function get_value(t) {
    if (t=="p") return 1
    else if (t=="n" || t=="b") return 3
    else if (t=="r") return 5
    else if (t=="q") return 9
    else return 16
}
function cmp_value(at, bt) {
    if (at==bt) return 0
    const av = get_value(at)
    const bv = get_value(bt)
    if (av<bv) return -1
    else return 1
}
function chess_type_str_to_enum (s) {
    if (s=="p") return Chess.Piece.PAWN
    if (s=="n") return Chess.Piece.KNIGHT
    if (s=="b") return Chess.Piece.BISHOP
    if (s=="r") return Chess.Piece.ROOK
    if (s=="q") return Chess.Piece.QUEEN
    return Chess.Piece.KING
}
function chess_type_enum_to_str (e) {
    if (e==Chess.Piece.PAWN) return "p"
    if (e==Chess.Piece.KNIGHT) return "n"
    if (e==Chess.Piece.BISHOP) return "b"
    if (e==Chess.Piece.ROOK) return "r"
    if (e==Chess.Piece.QUEEN) return "q"
    return "k"
}
function chess_color_str_to_enum (s) {
    if (s=="w") return Chess.PieceColor.WHITE
    return Chess.PieceColor.BLACK
}
function chess_color_enum_to_str (e) {
    if (e==Chess.PieceColor.WHITE) return "w"
    return "b"
}
function cap_str(m) {
    if (m.isCapture()) {
        return b2p(m.getFrom()).toString() + chess_type_enum_to_str(m.getPiece()) + ">" + b2p(m.getTo()).toString() + chess_type_enum_to_str(m.getCapturedPiece())
    } else {
        return b2p(m.getFrom()).toString() + chess_type_enum_to_str(m.getPiece()) + ">" + b2p(m.getTo()).toString()
    }
}
function chess_captures(chess_pos, b, d, moves) {
    let rv = 0
    if (d==8) {
        rv = Chess.AI.evaluateMaterial(chess_pos)
    } else {
        const c = chess_color_enum_to_str(chess_pos.turn)
        const next_move_list = moves===null ? chess_pos.getMoves(true, true) : moves
        rv = Chess.AI.evaluateMaterial(chess_pos)
        for (let j=0; j<next_move_list.length; j++) {
            const next_move = next_move_list[j]
            if (next_move.getTo()!=b) continue
            if (chess_pos.makeMove(next_move)) {
                const material = Chess.AI.evaluateMaterial(chess_pos)
                const result = chess_captures(chess_pos, b, d+1, null)
                if (c=="w") {
                    if (result>rv) rv = result
                } else {
                    if (result<rv) rv = result
                }
                chess_pos.unmakeMove()
            }
        }
    }
    return rv
}

function chess_eval (chess_pos, is_my, turn) {
    let rv = new Object()

    rv.moves = new Set()
    rv.captures = new Set()
    rv.checks = new Set()
    rv.mates = new Set()
    rv.hangs = new Set()
    rv.stales = new Set()
    rv.double_attack = new Set()
    rv.threats = []
    rv.safe = new Set()
    rv.bad = new Set()
    rv.even = new Set()
    rv.good = new Set()
    //rv.ctrl = []

    const orig_turn = chess_pos.turn
    chess_pos.turn = turn

    /*
    for (let b=0; b<64; b++) {
        const attackers = chess_pos.getAttackers(turn, b)
        rv.ctrl[b] = attackers
    }
    */
    const material = Chess.AI.evaluateMaterial(chess_pos)
    const c = chess_color_enum_to_str(turn)

    let move_list = chess_pos.getMoves(true)
    for (let i=0; i<move_list.length; i++) {
        const move = move_list[i]
        const from = b2p(move.getFrom())
        const to = b2p(move.getTo())
        const arr = ft2a(from,to)
        if (chess_pos.makeMove(move)) {
            const material_after_move = Chess.AI.evaluateMaterial(chess_pos)
            rv.moves.add(arr)
            const next_move_list = chess_pos.getMoves(true)
            const capture_eval = chess_captures(chess_pos, move.getTo(), 0, next_move_list)
            const is_good = (c=="w") ? (capture_eval>material) : (capture_eval<material)
            const is_bad  = (c=="w") ? (capture_eval<material) : (capture_eval>material)
            let attacked = false
            for (let j=0; j<next_move_list.length; j++) {
                const next_move = next_move_list[j]
                if (next_move.isCapture() && next_move.getTo()==move.getTo()) { attacked = true }
                if (is_my && chess_pos.makeMove(next_move)) {
                    if (!chess_pos.getMoves().length) {
                        rv.hangs.add(arr)
                    }
                    chess_pos.unmakeMove(next_move)
                }
            }
            if (move.isCapture()) {
                rv.captures.add(arr)
                const high_value_cap =
                    (move.getCapturedPiece()==Chess.Piece.QUEEN && move.getPiece()!=Chess.Piece.QUEEN) ||
                    (move.getCapturedPiece()==Chess.Piece.ROOK && move.getPiece()==Chess.Piece.PAWN) ||
                    (move.getCapturedPiece()==Chess.Piece.ROOK && move.getPiece()==Chess.Piece.BISHOP) ||
                    (move.getCapturedPiece()==Chess.Piece.ROOK && move.getPiece()==Chess.Piece.KNIGHT) ||
                    (move.getCapturedPiece()==Chess.Piece.BISHOP && move.getPiece()==Chess.Piece.PAWN) ||
                    (move.getCapturedPiece()==Chess.Piece.KNIGHT && move.getPiece()==Chess.Piece.PAWN)
                const free_piece =
                    !attacked
                const good_trade =
                    is_good
                if (high_value_cap || free_piece || good_trade) {
                    let threat = new Object()
                    threat.from = from
                    threat.to = to
                    threat.high_value_cap = high_value_cap
                    threat.free_piece = free_piece
                    threat.good_trade = good_trade
                    rv.threats.push(threat)
                }
            }
            if (attacked) {
                if (is_good) rv.good.add(to)
                else if (is_bad) rv.bad.add(to)
                else rv.even.add(to)
            } else {
                rv.safe.add(to)
            }
            if (move.isCapture()) {
                if (chess_pos.isAttacked(turn, move.getTo())) {
                    rv.double_attack.add(to)
                }
            }
            if (!(move.isCapture() && move.getCapturedPiece()==Chess.Piece.KING)) {
                const king_in_check = chess_pos.isKingInCheck()
                const has_next_move = next_move_list.filter(Chess.Position.prototype.isMoveLegal, chess_pos).length>0
                if (has_next_move) {
                    if (king_in_check) rv.checks.add(arr)
                } else if (king_in_check) {
                    rv.mates.add(arr)
                } else {
                    rv.stales.add(arr)
                }
            }
            chess_pos.unmakeMove()
        }
    }

    chess_pos.turn = orig_turn

    return rv
}

function draw() {

    let white_moves_elems = document.getElementsByClassName('white node')
    let black_moves_elems = document.getElementsByClassName('black node')
    let moves = white_moves_elems.length + black_moves_elems.length
    try {
        let white_selected_num = parseInt(document.getElementsByClassName('white node selected')[0].parentElement.getAttribute('data-whole-move-number'))
        moves = 2*white_selected_num-1
    } catch (err) {
        //console.log(err)
    }
    try {
        let black_selected_num = parseInt(document.getElementsByClassName('black node selected')[0].parentElement.getAttribute('data-whole-move-number'))
        moves = 2*black_selected_num
    } catch (err) {
        //console.log(err)
    }


    let board = new Object()
    for (const c of be.childNodes) {
        if (typeof(c.getAttribute) == "function") {
            let n = c.getAttribute("class");
            if (n && n=="coordinates") {
                for (const x of c.childNodes) {
                    if (x.getAttribute("y")=="3.5") {
                        if (x.innerHTML=="1") bd.orientation = "b"
                        if (x.innerHTML=="8") bd.orientation = "w"
                    }
                }
            }
        }
    }

    try {
        let clock_top = document.getElementsByClassName("clock-top")[0].getElementsByClassName("clock-time-monospace")[0].innerHTML.match("(\\d+):(\\d+)(\\.(\\d+))?")
        let clock_bot = document.getElementsByClassName("clock-bottom")[0].getElementsByClassName("clock-time-monospace")[0].innerHTML.match("(\\d+):(\\d+)(\\.(\\d+))?")
        let top_time = parseInt(clock_top[1])*60+parseInt(clock_top[2])
        let bot_time = parseInt(clock_bot[1])*60+parseInt(clock_bot[2])
        if (clock_top[4] !== undefined) {
            top_time += parseInt(clock_top[4])/10.0
        }
        if (clock_bot[4] !== undefined) {
            bot_time += parseInt(clock_bot[4])/10.0
        }
        var base = 1
        if (bot_time < 10) {
            base = 7
        } else if (bot_time < 60) {
            // 10=5
            // 60=20
            base = 5 + 15*(bot_time-10)/50.0
        } else if (bot_time < 120) {
            // 60=20
            // 120=30
            base = 20 + 10*(bot_time-60)/60.0
        } else {
            base = 30
        }
        if (bot_time < 10) {
            let yt = 0
            let yb = 0
            const ht = Math.round(Math.min(50,top_time*50.0/base))
            const hb = Math.round(Math.min(50,bot_time*50.0/base))
            if (bd.orientation=="w") {
                yb = 50
                yt = 50-ht
            } else {
                yt = 50
                yb = 50-hb
            }
            document.getElementsByClassName("arrows")[0].innerHTML = `<rect width=8 height=${ht} x=46 y=${yt} style='opacity:0.4; fill:red'/><rect width=8 height=${hb} x=46 y=${yb} style='opacity:0.4; fill:green'/>`
        } else {
            let delta = top_time-bot_time
            let f = ""
            let h = 0
            if (delta>base) { h = 50 }
            else if (delta<-base) { h = -50 }
            else { h = Math.round(delta*50.0/base) }
            let x = 0
            let y = 0
            if (bd.orientation=="w") {
                x = 0
                // top/black has more time
                if (h>0) { y = 50-h ; f = "red" }
                // bot/white has more time
                else { y = 50; h = -h ; f = "green" }
            } else {
                x = 98
                // top/white has more time
                if (h>0) { y = 50 ; f = "red" }
                // bot/black has more time
                else { y = 50+h; h = -h ; f = "green" }
            }
            document.getElementsByClassName("arrows")[0].innerHTML = `<rect width=2 height=${h} x=${x} y=${y} style='opacity:0.6; fill:${f}'/>`
        }
    } catch (err) {
        //console.log(err)
    }

    let drag_from_i = 0
    let drag_to_i = 0
    let drag_v = ""
    for (const c of be.childNodes) {
        if (typeof(c.getAttribute) == "function") {
            let n = c.getAttribute("class");
            if (n && n.substr(0,5) == "piece") {
                let i = 0
                let v = ""
                if (n.substr(9,6) == "square") {
                    i = parseInt(n.substr(16,2))
                    v = n.substr(6,2);
                }
                if (n.substr(6,6) == "square") {
                    i = parseInt(n.substr(13,2))
                    v = n.substr(16,2);
                }
                if (n.includes("dragging")) {
                    let m = c.getAttribute("style").match("transform: translate\\((-?\\d+).*%, (-?\\d+).*%")
                    if (m) {
                        /*
                          orientation=w
                            18      88
                            11      81
                          orientation=b
                            81      11
                            88      18
                        */
                        let tc = parseInt(m[1])
                        let tr = parseInt(m[2])
                        if (bd.orientation=="b") {
                            tc = 700-tc
                            tr = 700-tr
                        }
                        let nc = Math.ceil((tc+50)/100.0)
                        let nr = 9-Math.ceil((tr+50)/100.0)
                        nc = Math.max(Math.min(nc,8),1)
                        nr = Math.max(Math.min(nr,8),1)
                        drag_to_i = nc*10+nr
                        drag_from_i = i
                        drag_v = v
                    }
                }
                if (i) {
                    board[i] = v
                }
            }
            if (n && n.startsWith("highlight") && c.style['background-color'].includes("226, 188, 135")) {
                c.style.opacity = 0.1;
            }
        }
    }
    let dragging = false
    if (drag_from_i) {
        dragging = true
        delete board[drag_from_i]
        if (drag_to_i) board[drag_to_i] = drag_v
    }
    for (let [p,v] of Object.entries(board)) {
        delete board[p]
        board[p] = {n:v, c:v.substr(0,1), t:v.substr(1,1)}
    }

    if (old_board) {
        let changed = false
        for (const pos in board) {
            if (!(pos in old_board)) { console.log("changed not in old",pos); changed = true; break }
            if (old_board[pos].n != board[pos].n) { console.log("changed diff",pos,board[pos],old_board[pos]); changed = true; break }
        }
        for (const pos in old_board) {
            if (!(pos in board)) { console.log("changed not in new",pos); changed = true; break }
        }
        if (old_moves != moves) {
            console.log("changed moves", old_moves, moves)
            changed = true
        }
        if (!changed) {
            setTimeout(draw, 100)
            return
        }
    }
    old_moves = moves
    old_board = board
    bd.removeAllExistingShapes();

    let chess_pos = new Chess.Position;
    let chess_bbs = []
    for (let i=0; i<8; i++) {
        let bbz = Chess.Bitboard.makeZero()
        chess_bbs.push(bbz)
    }
    let board_2d = []
    for (let i=0; i<8; i++) {
        let board_row = []
        for (let j=0; j<8; j++) {
            board_row.push(null)
        }
        board_2d.push(board_row)
    }
    for (let [pos,piece] of Object.entries(board)) {
        pos = parseInt(pos)
        board_2d[p2x(pos)-1][p2y(pos)-1] = piece
        const bit = p2x(pos)-1+(p2y(pos)-1)*8
        const piece_ptr =
              (piece.t=="p") ? Chess.Piece.PAWN :
              (piece.t=="n") ? Chess.Piece.KNIGHT :
              (piece.t=="b") ? Chess.Piece.BISHOP :
              (piece.t=="r") ? Chess.Piece.ROOK :
              (piece.t=="q") ? Chess.Piece.QUEEN :
              Chess.Piece.KING
        const color_ptr = (piece.c=="w" ? Chess.Position.ANY_WHITE : Chess.Position.ANY_BLACK)
        chess_bbs[piece_ptr].setBit(bit)
        chess_bbs[color_ptr].setBit(bit)
    }

    chess_pos.bitboards = chess_bbs
    let castle_wk = 1
    let castle_wq = 2
    let castle_bk = 4
    let castle_bq = 8
    if (board_2d[0][0]===null || board_2d[0][0].n!="wr" || board_2d[0][4]===null || board_2d[0][4].n!="wk") castle_wk = 0
    if (board_2d[0][7]===null || board_2d[0][7].n!="wr" || board_2d[0][4]===null || board_2d[0][4].n!="wk") castle_wq = 0
    if (board_2d[7][0]===null || board_2d[7][0].n!="br" || board_2d[7][4]===null || board_2d[7][4].n!="bk") castle_bk = 0
    if (board_2d[7][7]===null || board_2d[7][7].n!="br" || board_2d[7][4]===null || board_2d[7][4].n!="bk") castle_bq = 0
	chess_pos.castlingRights = castle_wk | castle_wq | castle_bk | castle_bq;
	chess_pos.fillPiecesFromBitboards();
    chess_pos.turn = ((moves + dragging) % 2) ? Chess.PieceColor.BLACK : Chess.PieceColor.WHITE

    const in_check = chess_pos.isKingInCheck()
    const my_in_check = chess_pos.turn==chess_color_str_to_enum(bd.orientation) && in_check
    const opp_in_check = chess_pos.turn!=chess_color_str_to_enum(bd.orientation) && in_check

    const my_color_enum  = bd.orientation=="w" ? Chess.PieceColor.WHITE : Chess.PieceColor.BLACK
    const opp_color_enum = bd.orientation=="b" ? Chess.PieceColor.WHITE : Chess.PieceColor.BLACK

    Chess.AI.BISHOP_PAIR_VALUE_EN = false;
    const my_eval  = chess_eval(chess_pos, true, my_color_enum)
    const opp_eval = chess_eval(chess_pos, false, opp_color_enum)
    Chess.AI.BISHOP_PAIR_VALUE_EN = true;

    // tru
    let tru = new Set()
    let tru_moves = new Set()
    if (!in_check) {
        let orig_caps = new Set()
        let xcaps = chess_pos.getMovesBoth(true, true)
        for (let xcap of xcaps) {
            orig_caps.add(b2a(xcap.getFrom(), xcap.getTo()))
        }
        const occupied = chess_pos.getOccupiedBitboard()
        for (let b=0; b<64; b++) {
            if (occupied.isSet(b)) {
                const p = b2p(b)
                const piece = board[p]
                if (piece && piece.t!="k") {
                    chess_pos.capturePiece(chess_type_str_to_enum(piece.t), chess_color_str_to_enum(piece.c), b)
                    let xcaps = chess_pos.getMovesBoth(true, true)
                    for (let xcap of xcaps) {
                        const a = b2a(xcap.getFrom(), xcap.getTo())
                        if (orig_caps.has(a)) continue
                        const xpiece = board[b2p(xcap.getFrom())]
                        const xtarget = board[b2p(xcap.getTo())]
                        const is_tru =
                              "qrb".includes(xpiece.t) &&
                              "qrbnk".includes(xtarget.t) &&
                              xpiece.c!=xtarget.c;
                        if (is_tru) {
                            tru.add(p)
                            tru_moves.add(a)
                        }
                    }
                    chess_pos.unCapturePiece(chess_type_str_to_enum(piece.t), chess_color_str_to_enum(piece.c), b)
                }
            }
        }
    }

    for (const arr of tru_moves) {
        let [from,to] = a2ft(arr)
        bd.createShape('arrow', [p2s(from), p2s(to)], {size:20, style:"fill: purple; opacity: 0.3"});
    }
    for (const e of [my_eval, opp_eval]) {
        const is_my = (e==my_eval)
        for (const t of e.threats) {
            const is_strong = t.free_piece || t.high_value_cap
            const color = is_my ? "blue" : "red"
            const sz = is_strong ? 20 : 10
            const op = 0.7
            bd.createShape('arrow', [p2s(t.from), p2s(t.to)], {size:sz, style:`fill: ${color}; opacity: ${op}`});
        }
        for (const arr of e.checks) {
            let [from,to] = a2ft(arr)
            if (board[from].c==bd.orientation && opp_in_check) continue
            if (board[from].c!=bd.orientation && my_in_check) continue
            bd.createShape('arrow', [p2s(from), p2s(to)], {size:13, style:"fill: pink; opacity: 0.7"});
        }
        for (const arr of e.mates) {
            let [from,to] = a2ft(arr)
            if (board[from].c==bd.orientation && opp_in_check) continue
            if (board[from].c!=bd.orientation && my_in_check) continue
            bd.createShape('arrow', [p2s(from), p2s(to)], {size:20, style:"fill: red; opacity: 0.7"});
        }
        for (const arr of e.stales) {
            let [from,to] = a2ft(arr)
            if (board[from].c==bd.orientation && opp_in_check) continue
            if (board[from].c!=bd.orientation && my_in_check) continue
            bd.createShape('arrow', [p2s(from), p2s(to)], {size:20, style:"fill: gray; opacity: 0.7"});
        }
    }
    const hangs_not = my_eval.moves.size - my_eval.hangs.size
    console.log("hangs", my_eval.moves.size, my_eval.hangs.size, hangs_not)
    if (my_eval.hangs.size > hangs_not) {
        for (const arr of my_eval.moves) {
            if (my_eval.hangs.has(arr)) continue
            let [from,to] = a2ft(arr)
            bd.createShape('arrow', [p2s(from), p2s(to)], {size:20, style:"fill: cyan; opacity: 0.3"});
        }
    } else {
        for (const arr of my_eval.hangs) {
            let [from,to] = a2ft(arr)
            bd.createShape('arrow', [p2s(from), p2s(to)], {size:20, style:"fill: black; opacity: 0.3"});
        }
    }
    for (const obj of hl) {
        obj.remove()
    }

    {
        const backrank = (bd.orientation=="w") ? 1 : 8
        const nextrank = (bd.orientation=="w") ? 2 : 7
        const lst = [
            [1,2,[1,2]],
            [2,3,[1,2,3]],
            [7,6,[6,7,8]],
            [8,7,[7,8]]
        ]
        for (var i of lst) {
            let [x,bx,nxs] = i
            const piece = board_2d[x-1][backrank-1]
            if (piece !== null && piece.c==bd.orientation && piece.t=="k") {
                const backrank_block = board_2d[bx-1][backrank-1]
                if (backrank_block === null) {
                    var found_hole = false
                    for (var nx of nxs) {
                        const nextrank_piece = board_2d[nx-1][nextrank-1]
                        if (nextrank_piece === null) {
                            found_hole = true
                            break
                        }
                    }
                    if (!found_hole) {
                        opp_eval.good.add(xy2p(x,backrank))
                    }
                }
            }
        }
    }
    for (let b=0; b<64; b++) {
        const pos = b2p(b)
        const piece = board[pos]
        const is_tru = tru.has(pos)
        let has_my_piece = false
        let has_opp_piece = false
        if (piece) {
            if (piece.c==bd.orientation) has_my_piece = true
            else has_opp_piece = true
        }
        const is_opp_defended = chess_pos.isAttacked(opp_color_enum, b)
        const is_opp_safe = opp_eval.safe.has(pos)
        const is_opp_bad = opp_eval.bad.has(pos)
        const is_opp_even = opp_eval.even.has(pos)
        const is_opp_good = opp_eval.good.has(pos)
        const is_opp_double = opp_eval.double_attack.has(pos)
        const is_my_safe = my_eval.safe.has(pos)
        const is_my_good = my_eval.good.has(pos)
        const is_my_even = my_eval.even.has(pos)
        const is_my_bad = my_eval.bad.has(pos)
        const is_my_double = my_eval.double_attack.has(pos)
        let color = null
        let color2 = null
        if (has_my_piece && is_opp_safe) {
            color = "red"
        } else if (has_my_piece && is_opp_good) {
            color = "orange"
        } else if (has_my_piece && is_opp_even) {
            color = "greenyellow"
        } else if (has_my_piece && is_opp_bad) {
            color = "green"
        } else if (is_my_bad || is_my_even || is_my_good) {
            if (is_my_bad) {
                color = "yellow"
                if (is_my_good) { color2 = "green" }
                if (is_my_even) { color2 = "greenyellow" }
            }
            else if (is_my_even) {
                color = "greenyellow"
                if (is_my_good) { color2 = "green" }
            } else {
                color = "green"
            }
        } else if (is_my_safe && has_opp_piece) {
            color = "blue"
        } else if (is_my_safe) {
            color = "green"
        } else if (has_opp_piece && !is_opp_defended) {
            color = "pink"
        }
        if (!color) continue
        let d = document.createElement("div")
        let bg = []
        d.setAttribute("class", `highlight square-${pos}`)
        if (is_opp_double) {
            bg.push(`radial-gradient(at 85% 15%, red 13%, transparent 17%)`)
        }
        if (is_my_double) {
            bg.push(`radial-gradient(at 15% 15%, blue 13%, transparent 17%)`)
        }
        if (is_tru) {
            bg.push(`repeating-linear-gradient( -45deg, purple, purple 5%, transparent 5%, transparent 10%)`)
        }
        if (color2) {
            bg.push(`linear-gradient( -135deg, ${color2}, ${color2} 45%, transparent 55%, transparent)`)
        }
        bg.push(color)
        bg = bg.join(", ")
        bg = `background: ${bg}`
        d.setAttribute("style", `opacity: 0.5; ${bg}`)
        d.setAttribute('data-test-element', 'highlight')
        be.insertBefore(d, be.firstChild);
        hl.push(d)
    }

    if (!dragging) {

        try {
            let fen_board = []
            for (let i=0; i<8; i++) {
                let fen_row = []
                for (let j=0; j<8; j++) {
                    fen_row.push(null)
                }
                fen_board.push(fen_row)
            }
            for (let [pos,piece] of Object.entries(board)) {
                pos = parseInt(pos)
                const x = Math.floor(pos/10)
                const y = pos%10
                let fen_piece = piece.t
                if (piece.c == "w") {
                    fen_piece = piece.t.toUpperCase()
                }
                fen_board[8-y][x-1] = fen_piece
            }

            let castle_K = "K"
            let castle_Q = "Q"
            let castle_k = "k"
            let castle_q = "q"
            if (fen_board[0][0] != "r" || fen_board[0][4] != "k") castle_q = ""
            if (fen_board[0][7] != "r" || fen_board[0][4] != "k") castle_k = ""
            if (fen_board[7][0] != "R" || fen_board[7][4] != "K") castle_Q = ""
            if (fen_board[7][7] != "R" || fen_board[7][4] != "K") castle_K = ""
            let castle = castle_K+castle_Q+castle_k+castle_q
            if (castle == "") castle = "-"

            for (let i=0; i<8; i++) {
                let cnt = 0
                let s = []
                for (let j=0; j<8; j++) {
                    let p = fen_board[i][j]
                    if (p == null) {
                        cnt++
                    } else {
                        if (cnt>0) { s += cnt.toString() }
                        s += p
                        cnt = 0
                    }
                }
                if (cnt>0) { s += cnt.toString() }
                fen_board[i] = s
            }
            let fen = fen_board.join("/")

            let color = ""
            if ((moves%2)==1) {
                color = "black"
                fen += " b "
            } else {
                color = "white"
                fen += " w "
            }

            fen += castle
            current_fen = fen
            console.log("lichess",last_fen,moves,fen,fen in fen_cache)
            if (fen in fen_cache) {
                fen_callback(moves, fen, color, fen_cache[fen])
            } else if (last_fen==0 || moves<24 || (moves-last_fen)<3) {
                let r = new XMLHttpRequest()
                r.open("GET", `https://explorer.lichess.ovh/lichess?fen=${encodeURIComponent(fen)}&color=${color}`, true)
                r.onload = (e) => {
                    if (r.readyState == 4 && r.status == 200) fen_callback(moves, fen, color, r.responseText)
                }
                r.send()
            }

            stockfish_clear()
            stockfish_queue.push(current_fen)
            stockfish.postMessage(`position fen ${current_fen}`)
            stockfish.postMessage(`eval`)
            stockfish.postMessage(`go depth 8 movetime 2000`)

        } catch (err) {
            //console.log(err)
        }

        /*
        if (moves > 10) {
            try {
                let s = ""
                for (let move=0; move<moves; move++) {
                    let e = null
                    if ((move%2)==0) {
                        s += ` ${move/2+1}.`
                        e = white_moves_elems[move/2]
                    } else {
                        e = black_moves_elems[(move-1)/2]
                    }
                    let h = e.innerHTML
                    let mv = `${h}`
                    let m
                    m = h.match("data-figurine=.(.).*>(.*)")
                    if (m) {
                        mv = `${m[1]}${m[2]}`
                    }
                    m = h.match("(.*=)<.*data-figurine=.(.)")
                    if (m) {
                        mv = `${m[1]}${m[2]}`
                    }
                    m = mv.match("(.*)\\+")
                    if (m) {
                        mv = `${m[1]}`
                    }
                    m = mv.match("(.*)#")
                    if (m) {
                        mv = `${m[1]} 1-0`
                    }
                    s += ` ${mv}`
                }
                s = s.substr(1)

                engine_clear()
                engine_id++
                engine_white_to_move = ((moves % 2)==0)
                engine_start = performance.now()
                engine_pos = Chess.Parser.parseMoves(s)
                engine_curr = Chess.AI.evaluate(engine_pos)
                engine_show = (my_eval.hangs.size>0 || in_check)

                const code = `
                        ${CHESS.toString()}
                        function Chess() {} CHESS(Chess)
                        const ID = ${engine_id}
                        const STR = "${s}"
                        const POS = new Chess.Position([${engine_pos.serialize()}])
                        const R = Chess.AI.search(POS)
                        `

                var blob = new Blob([code], {type: 'application/javascript'});
                var worker = new Worker(URL.createObjectURL(blob));
                worker.postMessage([i,s]);
                worker.onmessage = worker_callback
                engine_workers.push(worker)

            } catch (err) {
                console.log(err)
            }
        }
        */
    }

    setTimeout(draw, 100)
}
function stockfish_clear() {
    stockfish_arrow?.remove()
    stockfish_arrow = null
}
function stockfish_callback(e) {
    //     STOCFISH Total Evaluation: -7.45 (white side)
    console.log("STOCKFISH", e.data)
    if (e.data.startsWith("Total Evaluation: ")) {
        stockfish_fen = stockfish_queue.shift()
        stockfish_eval_static = parseFloat(e.data.split(" ")[2])
        document.getElementsByClassName("coordinate-light")[1].innerHTML = stockfish_eval_static
        document.getElementsByClassName("coordinate-dark")[1].innerHTML = stockfish_eval_static
        return
    }
    if (stockfish_fen != current_fen) {
        return
    }
    const wtm = stockfish_fen.includes(" w ")
    const spl = e.data.split(" ")
    const cmd = spl.shift()
    var depth = null
    var score = null
    var move = null
    if (cmd == "info") {
        while (spl.length) {
            const subcmd = spl.shift()
            if (subcmd == "depth") {
                depth = parseInt(spl.shift())
            } else if (subcmd == "score") {
                spl.shift()
                score = parseInt(spl.shift())/100.0
                if (!wtm) score *= -1
            } else if (subcmd == "pv") {
                move = spl.shift()
                break
            }
        }
        if (score !== null) {
            document.getElementsByClassName("coordinate-light")[2].innerHTML = `${score}`
            document.getElementsByClassName("coordinate-dark")[2].innerHTML = `${score}`
            stockfish_eval_search = score
        }
    }
    if (cmd == "bestmove") {
        const delta = (stockfish_eval_search - stockfish_eval_static)
        const show =
              (wtm ? (delta > 1) : (delta < -1)) ||
              (bd.orientation == "w" && stockfish_eval_static < -1.5) ||
              (bd.orientation == "b" && stockfish_eval_static >  1.5)
        if (show) {
            const move = spl.shift()
            let from = move.substr(0,2)
            let to = move.substr(2,2)
            stockfish_arrow = bd.createShape('arrow', [from, to], {size:30, style:`fill: white; opacity: 0.8`});
        }
    }
}
/*
function worker_callback(e) {
    const [id,mv_index,mv_total,result] = e.data
    //console.log("worker_cb", `${id} ${mv_index}/${mv_total}`, result[1])
    if (id != engine_id) {
        //console.log("  stale")
        return
    }
    const [r_mv,r_eval] = result
    try {
        document.getElementsByClassName("coordinate-light")[1].innerHTML = `${mv_index+1}/${mv_total}`
        document.getElementsByClassName("coordinate-dark")[1].innerHTML = `${mv_index+1}/${mv_total}`
    } catch (err) {}
    if (r_mv !== null) {
        const mv = new Chess.Move(0,0,0,0,0)
        mv.value = r_mv.value
        console.log(`    move=${mv.getString()} -> ${r_eval}`)
        if (engine_best_move===null) {
            engine_best_move = mv
            engine_best_eval = r_eval
        }
        if (engine_white_to_move && r_eval>engine_best_eval) {
            engine_best_move = mv
            engine_best_eval = r_eval
        }
        if (!engine_white_to_move && r_eval<engine_best_eval) {
            engine_best_move = mv
            engine_best_eval = r_eval
        }
    }

    const elapsed = Math.round(performance.now() - engine_start)
    var val_str = (engine_best_eval / 100.0).toFixed(1)
    if (val_str > 0) val_str = "+" + val_str
    try {
        document.getElementsByClassName("coordinate-light")[2].innerHTML = val_str
        document.getElementsByClassName("coordinate-dark")[2].innerHTML = val_str
        document.getElementsByClassName("coordinate-light")[3].innerHTML = `${elapsed}ms`
        document.getElementsByClassName("coordinate-dark")[3].innerHTML = `${elapsed}ms`
    } catch (err) {}

    engine_arrow?.remove()
    engine_arrow = null
    if (engine_best_move!==null) {
        const delta = (engine_best_eval - engine_curr)
        const show =
              (engine_white_to_move ? (delta > 100) : (delta < -100)) ||
              (bd.orientation == "w" && engine_curr < -150) ||
              (bd.orientation == "b" && engine_curr > 150)
        if (engine_show || show) {
            let from = Chess.getAlgebraicFromIndex(engine_best_move.getFrom())
            let to = Chess.getAlgebraicFromIndex(engine_best_move.getTo())
            engine_arrow = bd.createShape('arrow', [from, to], {size:30, style:`fill: white; opacity: 0.8`});
        }
    }
}
function engine_clear() {
    engine_arrow?.remove()
    for (const worker of engine_workers) {
        worker.terminate()
    }
    engine_workers = []
    engine_white_to_move = false
    engine_best_move = null
    engine_best_eval = 0
    engine_start = null
    engine_arrow = null
    engine_pos = null
    engine_curr = 0
    engine_show = false
    try {
        document.getElementsByClassName("coordinate-light")[1].innerHTML = "-"
        document.getElementsByClassName("coordinate-dark")[1].innerHTML = "-"
    } catch (err) {}
}
*/

var fen_cache = {}
function fen_callback(moves, fen, color, txt) {
    fen_cache[fen] = txt
    if (current_fen != fen) return
    let fen_json = JSON.parse(txt)
    let draw = []
    let tot = 0
    for (let m=0; m<fen_json.moves.length; m++) {
        let c = fen_json.moves[m].black + fen_json.moves[m].draws + fen_json.moves[m].white
        if (c<10) continue
        let c_win = 1
        let c_lose = 1
        let c_draw = fen_json.moves[m].draw
        if (color == "black") {
            c_win = fen_json.moves[m].black
            c_lose = fen_json.moves[m].white
        } else {
            c_win = fen_json.moves[m].white
            c_lose = fen_json.moves[m].black
        }
        console.log(`   ${fen_json.moves[m].uci}  ${c_win} ${c_lose} ${1.0*c_win/c_lose}`)
        const from = fen_json.moves[m].uci.substr(0,2)
        const to = fen_json.moves[m].uci.substr(2,2)
        const win = 1.0*(c_win+50)/(c_win+c_lose+100)
        const i = [win, c, from, to]
        if (win>0.6 || draw.length<6) {
            draw.push(i)
            tot += c
        }
    }
    //if (draw.length>0) {
    //    engine_clear()
    //}
    for (let i of draw) {
        last_fen = moves
        let [win, c, from, to] = i
        let fill = "yellow"
        if (win > 0.43) fill = "olive"
        if (win > 0.47) fill = "green"
        if (win > 0.6) fill = "lime"
        if (win > 0.7) fill = "aqua"
        if (win > 0.8) fill = "blue"
        let sz = 10+20*c/tot
        //console.log(`  draw ${from},${to} c=${c} sz=${sz}`)
        bd.createShape('arrow', [from, to], {size:sz, style:`fill: ${fill}; opacity: 0.7`});
    }
}

const observer = new MutationObserver((mutationsList, observer) => {
    be = document.querySelector('wc-chess-board');
    if (be) {
        console.log("found board");
        observer.disconnect()
        bd = new UniversalBoardDrawer(be, {
            'window': window,
            'boardDimensions': [8, 8],
            'playerColor': 'w', // assuming you're playing as white
            'zIndex': 99999,
            'debugMode': true
        });
        draw()
        //be.addEventListener("mouseup", draw);
        //const board_observer = new MutationObserver((mutationList, observer) => {draw();})
        //board_observer.observe(be, { childList: true, subtree: true });
        document.getElementsByClassName("coordinate-light")[0].innerHTML = stockfish
        document.getElementsByClassName("coordinate-dark")[0].innerHTML = code.length


    }
});

/*
alert('start')
const code = (await (await fetch("https://nmrugg.github.io/kingdom/js/stockfish6.js")).text())
alert(code.length)
var blob = new Blob([code], {type: 'application/javascript'});
var stockfish = new Worker(URL.createObjectURL(blob));
alert(stockfish)
stockfish.onmessage = stockfish_callback
*/

var stockfish_queue = []
var stockfish_fen = null
var stockfish_eval_static = null
var stockfish_eval_search = null
var stockfish_arrow = null

var DEBUG = false

var bd = null;
var be = null;
var hl = []
var old_board = null
var old_moves = 0
var current_fen = null
var last_fen = 0
//var engine_id = 0
//var engine_workers = []
//var engine_white_to_move = false
//var engine_best_move = null
//var engine_best_eval = 0
//var engine_start = null
//var engine_arrow = null
//var engine_pos = null
//var engine_curr = 0
//var engine_show = false

observer.observe(document, { childList: true, subtree: true });


