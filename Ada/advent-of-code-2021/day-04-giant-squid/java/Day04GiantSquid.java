import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class BingoNum {
	boolean marked;
	int num;
}

class Board {
	int id;
	int total;
	ArrayList<ArrayList<BingoNum>> board;
}

class Winner {
	int boardId;
	int restnum;
}

public class Day04GiantSquid {
	static final int size = 5;
	static Board board1, board2, board3;

	public static void main(String []args) {
		board1 = initializeBoard(1);
		board2 = initializeBoard(2);
		board3 = initializeBoard(3);
		var bingo = new ArrayList<Integer>();
		var bufread = new BufferedReader(new InputStreamReader(System.in));
		try {
			var s = bufread.readLine();
			System.out.println("read line: " + s);
			for (var snum : s.split(",")) {
				var num = Integer.parseInt(snum);
				bingo.add(num);
			}
			s = bufread.readLine();
			System.out.println("read empty: '" + s + "' ok");
			populateBoard(bufread, board1);
			populateBoard(bufread, board2);
			populateBoard(bufread, board3);
		} catch(Exception e) {
			e.printStackTrace();
		}

		for (var board : List.of(board1, board2, board3).toArray()) {
			checkboard((Board) board);
		}

		//System.out.println("ok to check boards");
		System.out.println("ok check bingo");
		for (var nobj : bingo.toArray()) {
			var num = (int) nobj;
			var w = turnBingo(num);
			if (w.boardId > 0) {
				System.out.println("winning board " + w.boardId + " bingo: " + num +
						" with rest unmarked total: " + w.restnum);
				System.out.println("Total winning: " + w.restnum * num);
				break;
			}
		}
		for (var board : List.of(board1, board2, board3).toArray()) {
			checkboard((Board) board);
		}
	}

	static Board initializeBoard(int id) {
		var res = new Board();
		res.board = new ArrayList<ArrayList<BingoNum>>();
		res.id = id;
		res.total = 0;
		for (var i = 0; i < size; i++) {
			var aa = new ArrayList<BingoNum>();
			for (var j = 0; j < size; j++) {
				aa.add(new BingoNum());
			}
			res.board.add(aa);
		}
		return res;
	}
	
	static void populateBoard(BufferedReader buf, Board board)
			throws Exception {
		for (var i = 0; i < size; i++) {
			var s = buf.readLine();
			var theStr = Arrays.stream(s.split(" ")).filter(str -> str != null && !str.isEmpty()).toArray();
			for (var j = 0; j < size; j++) {
				var bn = board.board.get(i).get(j);
				var ss = (String)theStr[j];
				System.out.print(ss + " ");
				bn.num = Integer.parseInt(ss.toString().trim());
				board.total += bn.num;
				board.board.get(i).set(j, bn);
			}
			System.out.println();
		}
		buf.readLine();
		System.out.println();
	}

	static Winner checkWin() {
		var win = new Winner();
		var list = List.of(board1, board2, board3);
		for (var z = 0; z < list.size(); z++) {
			var board = list.get(z);
			for (var i = 0; i < size; i++) {
				var bothfail = false;
				for (var j = 0; j < size; j++) {
					if (!board.board.get(i).get(j).marked) {
						bothfail = true;
						break;
					}
				}
				if (bothfail) {
					bothfail = false;
					for (var j = 0; j < size; j++) {
						if (!board.board.get(j).get(i).marked) {
							bothfail = true;
							break;
						}
					}
				}
				if (bothfail) {
					continue;
				}
				win.boardId = board.id;
				win.restnum = board.total;
				return win;
			}
		}
		return win;
	}

	static void checkbingo(ArrayList<Integer> al) {
		System.out.print("bingo: ");
		for (var i : al) {
			System.out.print(i + " ");
		}
		System.out.println();
	}

	static void checkboard(Board board) {
		System.out.printf("board: %d, with total: %d\n", board.id, board.total);
		for (var line : board.board.toArray()) {
			var theline = (ArrayList<BingoNum>) line;
			for (var bn : theline.toArray()) {
				var b = (BingoNum)bn;
				System.out.printf("%2d%c ", b.num, b.marked ? '^' : ' ');
			}
			System.out.println();
		}
		System.out.println();
	}

	static void checkPerBoard(Board board, int num) {
		for (var i = 0; i < size; i++) {
			for (var j = 0; j < size; j++) {
				var bn = board.board.get(i).get(j);
				if (bn.num == num) {
					board.total -= num;
					bn.marked = true;
					bn.num = num;
					board.board.get(i).set(j, bn);
					return;
				}
			}
		}
	}

	static Winner turnBingo(int num) {
		var w = new Winner();
		checkPerBoard(board1, num);
		w = checkWin();
		if (w.boardId > 0) {
			return w;
		}

		checkPerBoard(board2, num);
		w = checkWin();
		if (w.boardId > 0) {
			return w;
		}

		checkPerBoard(board3, num);
		w = checkWin();
		if (w.boardId > 0) {
			return w;
		}
		return w;
	}
}
