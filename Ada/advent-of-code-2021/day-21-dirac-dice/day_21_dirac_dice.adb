-- compile it: (small)
-- gnatmake -O2 day_21_dirac_dice.adb -bargs -largs -s
-- (normal)
-- gnatmake day_21_dirac_dice.adb
-- run in cmd (not powershell)
-- type input.txt | day_21_dirac_dice.exe
-- or simply run:
-- build-run.bat
-- to clean folder: adaclean
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure day_21_dirac_dice is
	package tio renames Ada.Text_IO;
	package nio renames Ada.Integer_Text_IO;
	package fix renames Ada.Strings.Fixed;

	subtype Dice is Integer range 1..100;
	subtype Space is Integer range 1..10;

	type Player is record
		id: Natural;
		pos : Space;
		score: Natural;
	end record;

	player1 : aliased Player := (1, 1, 0);
	player2 : aliased Player := (2, 1, 0);

	procedure fetch_id_pos(s: in string) is
		chr_space : String := " ";
		colon : String := ":";
		index : Integer := 0;
		last : Positive := 1;
		num : Integer := 0;
		id : Integer;
		player_acc : access Player;
	begin
		index := fix.Index (s, chr_space);
		if index < s'first then return; end if;
		nio.Get(s(index+1..s'last), id, last);
		if id = 1 then
			player_acc := player1'access;
		else
			player_acc := player2'access;
		end if;
		index := fix.Index(s, colon, last);
		if index < s'first then return; end if;
		nio.Get(s(index+2..s'last), num, last);
		player_acc.pos := num;
	end fetch_id_pos;

	procedure Read_pos is
		line : string(1..40);
		length : Natural;
	begin
		tio.Get_Line(line, length);
		fetch_id_pos (line(1..length));
		tio.Put_Line("read line: " & line(1..length) & " with length:" & length'image);
		tio.Get_Line(line, length);
		fetch_id_pos (line(1..length));
		tio.Put_Line("read line: " & line(1..length) & " with length:" & length'image);
	end Read_pos;

	winning_score : constant := 1000;
	players : array (1 .. 2) of access Player := (player1'access, player2'access);
	function play return Integer is
		rolls : Natural := 0;
		playeridx : positive := 1;
		playerslen : positive := players'length;
		dielen : Positive := dice'last;
		spacelen : Positive := space'last;
		the_player : access Player;
		next_move : Integer := 0;
		curr_pos : Integer := 0;
		prev_score : Integer := 0;
		prev_pos : Integer := 0;
		turns : Natural := 0;
	begin
		while player1.score < winning_score and player2.score < winning_score loop
			next_move := 0;
			playeridx := (rolls rem playerslen) + 1;
			the_player := players(playeridx);
			--tio.New_Line;
			for I in 0 .. 2 loop
				rolls := (rolls+1) rem dielen;
				if rolls = 0 then rolls := dice'last; end if;
				--tio.Put(" rolls:" & rolls'image & ' ');
				next_move := next_move + rolls;
			end loop;
			turns := turns + 3;
			--tio.Put_Line ("next_move:" & next_move'image & ", the_player.pos:" & the_player.pos'image);
			curr_pos := ((the_player.pos + next_move) rem spacelen);
			if curr_pos = 0 then curr_pos := space'last; end if;
			prev_score := the_player.score;
			prev_pos := the_player.pos;
			the_player.pos := curr_pos;
			the_player.score := the_player.score + the_player.pos;
			--tio.Put_Line("   prev_pos:" & prev_pos'image & ", next_move:" & next_move'image &
			--", curr_pos:" & curr_pos'image);
			--tio.Put_Line ("Player:" & the_player.id'image & "," &  prev_score'image & " +" &
			--curr_pos'image & " =" & the_player.score'image);
			exit when the_player.score >= winning_score;
			--exit when turns > 15;
		end loop;
		declare
			lost_points : Integer;
		begin
			lost_points := players((playeridx rem playerslen) + 1).score;
			tio.Put_Line ("turns:" & turns'image & ", lost_points:" & lost_points'image);
			tio.Put_Line ("players win score:" & Integer'Image(players(((playeridx+1) rem playerslen) +1).score));
			return turns * lost_points;
		end;
	end play;

begin
	Read_pos;
	tio.Put_Line (player1'image);
	tio.Put_Line (player2'image);
	tio.Put_Line ("The multiplier of rolls and lose score is" & play'image);
end day_21_dirac_dice;
