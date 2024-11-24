import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Scanner;

class Player {
	int id, score, pos;

	public Player(int id) {
		this.id = id;
	}

	public String toString() {
		return String.format("Player{%d}{Score: %d, Pos: %d}", id, score, pos);
	}
}

public class Day21DiracDice {
	public static void main(String[] args) {
		var buf = new BufferedReader(new InputStreamReader(System.in));
		Player[] players = {new Player(1), new Player(2)};
		try {
			while (true) {
				var s = buf.readLine();
				if (s == null || s.isEmpty()) break;
				var sc = new Scanner(s);
				sc.next();
				var id = sc.nextInt() - 1;
				sc.next();
				sc.next();
				players[id].pos = sc.nextInt();
				sc.close();
			}
		} catch(Exception e) {
			e.printStackTrace();
		}

		for (var player : players) {
			System.out.println(player);
		}
		var totalpoints = play(players);
		System.out.println("The total points of lose player multiplied with die rolls is " +
				totalpoints);
	}

	static int play(Player[] players) {
		final int winningScore = 1000;
		var turn = 0;
		var rolls = 0;
		while(players[0].score < winningScore && players[1].score < winningScore) {
			var player = players[rolls%players.length];
			var moves = 0;
			for (var i = 1; i <= 3; i++) {
				rolls += 1;
				moves += rolls;
			}
			turn += 3;
			var currpos = (player.pos + moves) % 10;
			if (currpos == 0) currpos = 10;
			//var prevscore = player.score;
			//var prevpos = player.pos;
			player.pos = currpos;
			player.score += currpos;
		}
		/*
		for (var player : players) {
			System.out.println(player);
		}
		System.out.printf("turn: %d, rolls: %d, player.score: %d\n",
				turn, rolls, players[turn % players.length].score);
				*/
		return rolls * players[turn % players.length].score;
	}
}
