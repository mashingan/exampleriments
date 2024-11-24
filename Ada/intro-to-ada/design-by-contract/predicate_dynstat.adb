-- compile: gnatmake -I.. predicate_dynstat
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Vectors;
with myutils; use myutils;

procedure predicate_dynstat is
    pragma Assertion_Policy (Static_Predicate => Check,
                             Dynamic_Predicate => Check);

    package Courses is
        type Course_Container is private;
        
        type Course is record
            Name        : Unbounded_String;
            Start_Date  : Time;
            End_Date    : Time;
        end record
        with Dynamic_Predicate => Course.Start_Date <= Course.End_Date;

        procedure Add (CC : in out Course_Container; C : Course);

    private
        package Course_Vectors is new Ada.Containers.Vectors
        (Index_Type => Natural,
        Element_Type => Course);

        type Course_Container is record
            V : Course_Vectors.Vector;
        end record;
    end Courses;

    package body Courses is
        procedure Add (CC: in out Course_Container; C : Course) is
        begin
            CC.V.Append (C);
        end Add;
    end Courses;

    use Courses;
    CC : Course_Container;

    procedure failed_dynamic_predicate (CC: in out Course_Container) is
    begin
        Add (CC,
            Course'(
                Name        => To_Unbounded_String("Intro to Video Recording"),
                Start_Date  => Time_Of(2019, 5, 1),
                End_Date    => Time_Of(2018, 5, 10)));
        --                              ^ will trigger error for dynamic check
        --                                because  Start_Date >= End_Date
        
    end failed_dynamic_predicate;

    subtype Work_Days is Days range Monday .. Friday;

    subtype Test_Days is Work_Days
    with Static_Predicate => Test_Days in Monday | Wednesday | Friday;

    type Tests_Week is array (Days) of Natural
        with Dynamic_Predicate =>
        (for all I in Tests_Week'range =>
            (case I is
                when Test_Days => Tests_Week (I) > 0,
                when Others => Tests_Week(I) = 0));

    Num_Tests : Tests_Week := (
        Monday => 3, Tuesday => 0,
        Wednesday => 4, Thursday => 0,
        Friday => 2, Saturday => 0, Sunday => 0);

    procedure Display_Tests (N : Tests_Week) is
    begin
        for I in Test_Days loop
            Put_Line(
                "# tests on " & CapFirst(Test_days'image(I)) &
                " => " & Integer'image(N(I)));
        end loop;
    end Display_Tests;

begin
    Add (CC,
        Course'(
            Name        => To_Unbounded_String("Intro to Photography"),
            Start_Date  => Time_Of(2018, 5, 1),
            End_Date    => Time_Of(2018, 5, 10)));

    --failed_dynamic_predicate(CC);

    Display_Tests(Num_Tests);
    Num_Tests (Tuesday) := 2;
    Display_Tests(Num_Tests);
end predicate_dynstat;
