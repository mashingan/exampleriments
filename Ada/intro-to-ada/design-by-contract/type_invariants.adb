with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Vectors;

procedure type_invariants is
    pragma Assertion_Policy (Type_Invariant => check);

    package Courses is
        type Course is private
            with Type_invariant => check (Course);

        type Course_Container is private;

        procedure Add (CC : in out Course_Container; C : Course);
        
        function Init (name : string; Start_Date, End_Date: TIme) return Course;

        function Check (C : Course) return Boolean;

    private
        type Course is record
            Name        : Unbounded_String;
            Start_Date  : Time;
            End_Date    : Time;
        end record;

        function Check (C : Course) return Boolean is
            (C.Start_Date <= C.End_Date);

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

        function Init (Name: String; Start_Date, End_Date: Time) return Course is
            (Course'(
                Name        => To_Unbounded_String(Name),
                Start_Date  => Start_Date,
                End_Date    => End_Date));
                
    end Courses;

    use Courses;
    CC : Course_Container;

    procedure failed_dynamic_predicate (CC: in out Course_Container) is
    begin
        Add (CC,
            Init(
                Name        => "Intro to Video Recording",
                Start_Date  => Time_Of(2019, 5, 1),
                End_Date    => Time_Of(2018, 5, 10)));
        --                              ^ will trigger error for dynamic check
        --                                because  Start_Date >= End_Date
        
    end failed_dynamic_predicate;

begin
    Add (CC,
        Init(
            Name        => "Intro to Photography",
            Start_Date  => Time_Of(2018, 5, 1),
            End_Date    => Time_Of(2018, 5, 10)));

    failed_dynamic_predicate(CC);
end type_invariants;
