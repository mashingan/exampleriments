import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class Octopus {
	int energy;
	boolean flashed;

	Octopus(int energy) {
		this.flashed = false;
		this.energy = energy;
	}

	Octopus(boolean flashed) {
		this.flashed = flashed;
	}

	Octopus(int energy, boolean flashed) {
		this.flashed = flashed;
		this.energy = energy;
	}

	public String toString() {
		return String.format("%d%c", energy, flashed? '*' : ' ');
	}
}

public class Day11DumboOctopus {
	public static void main(String []args) {
		var buf = new BufferedReader(new InputStreamReader(System.in));
		ArrayList<ArrayList<Octopus>> arrlist = new ArrayList<>();
		try {
			while(true) {
				var s = buf.readLine();
				if (s == null || s.isEmpty()) { break; }
				var sc = s.toCharArray();
				var arrel = IntStream
					.range(0, sc.length).map(i -> sc[i] - '0')
					.boxed()
					.map(energy -> new Octopus(energy))
					.collect(Collectors.toCollection(ArrayList::new));
				arrlist.add(arrel);
			}
		} catch(Exception e) {
			e.printStackTrace();
		}
		var target = 100;
		var arrocto = arrlist.stream().map(
				(arrlist1) -> arrlist1.stream().toArray(Octopus[]::new)).
			toArray(Octopus[][]::new);
		System.out.println("Initial Octopus: ");
		checkOctopus(arrocto);
		var total = flashSteps(arrocto, target);
		System.out.println("Total flashes for 100 steps are " + total);
		checkOctopus(arrocto);
	}

	static void energyPropagation(Octopus[][] arrocto, int i, int j) {
		if (i >= arrocto.length || i < 0 || j >= arrocto[i].length || j < 0) return;
		if (arrocto[i][j].flashed) return;
		if (arrocto[i][j].energy + 1 > 9) {
			arrocto[i][j].energy = 0;
			arrocto[i][j].flashed = true;
			for (var ii = -1; ii <= 1; ii++) {
				for (var jj = -1; jj <= 1; jj++) {
					energyPropagation(arrocto, i+ii, j+jj);
				}
			}
		} else {
			arrocto[i][j].energy += 1;
		}
	}

	static void checkOctopus(Octopus[][] arrocto) {
		for (var i = 0; i < arrocto.length; i++) {
			for (var j = 0; j < arrocto[i].length; j++) {
				System.out.print(arrocto[i][j].toString() + ' ');
			}
			System.out.println();
		}
		System.out.println();
	}

	static int flashStep(Octopus[][] arrocto) {
		for (var i = 0; i < arrocto.length; i++) {
			for (var j = 0; j < arrocto[i].length; j++) {
				energyPropagation(arrocto, i, j);
			}
		}
		//checkOctopus(arrocto);
		return cleanFlashes(arrocto);
	}

	static int cleanFlashes(Octopus[][] arrocto) {
		var flashed = 0;
		for (var i = 0; i < arrocto.length; i++) {
			for (var j = 0; j < arrocto[i].length; j++) {
				if (arrocto[i][j].flashed) {
					flashed++;
					arrocto[i][j].flashed = false;
				}
			}
		}
		return flashed;
	}

	static int flashSteps(Octopus[][] arrocto, int target) {
		var flashes = 0;
		for (var i = 0; i < target; i++) {
			//System.out.println("Octopus day " + (i+1));
			flashes += flashStep(arrocto);
		}
		return flashes;
	}
}
