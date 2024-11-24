with Ada.Text_IO; use Ada.Text_IO;

procedure Runway_Airplane is
    type Airplane_ID is range 1..10;
    task type Airplane(ID: Airplane_ID);
    type Airplane_Access is access Airplane;

    protected type Runway is
        entry Assign_Aircraft(ID: Airplane_ID);
        entry Cleared_Runway(ID: Airplane_ID);
        entry Wait_for_clear;
    private
    Clear : Boolean := true;
    end Runway;

    type Runway_Access is access all Runway;

    protected body Runway is
    entry Cleared_Runway(ID: Airplane_ID) when not clear is
    begin
        clear := true;
        put_line(Airplane_ID'image(ID) & " cleared runway");
    end;
    entry Assign_Aircraft(ID: Airplane_ID) when clear is
    begin
        clear := false;
        put_line(Airplane_ID'image(ID) & " on runway");
    end;
    entry Wait_for_clear when clear is
    begin
        null;
    end;
    end Runway;

    task type Controller (My_Runway: Runway_Access) is
        entry Request_Takeoff(ID: in Airplane_ID; Takeoff: out Runway_Access);
        entry Request_Approach(ID: in Airplane_ID; Approach: out Runway_Access);
    end Controller;

    Runway1: aliased Runway;
    Controller1: Controller(Runway1'Access);

    task body Controller is
    begin
        loop
            My_Runway.Wait_for_clear;
            select
            when Request_Approach'count = 0 =>
                accept Request_Takeoff(ID: in Airplane_ID; Takeoff: out Runway_Access) do
                    My_Runway.Assign_Aircraft(ID);
                    Takeoff := My_Runway;
                end Request_Takeoff;
            or accept Request_Approach(ID: in Airplane_ID; Approach: out Runway_Access) do
                My_Runway.Assign_Aircraft(ID);
                Approach := My_Runway;
            end Request_Approach;
            or terminate;
            end select;
        end loop;
    end;

    task body Airplane is
        Rwy: Runway_Access;
    begin
        Controller1.Request_Takeoff(ID, Rwy);
        Put_line(Airplane_ID'image(ID) & " taking off...");
        delay 0.2;
        Rwy.Cleared_Runway(ID);
        delay 0.2;
        Put_Line(Airplane_ID'image(ID) & " is flying.");
        loop
            select
            Controller1.Request_Approach(ID, Rwy);
            exit;
            or
            delay 0.5;
            put_line(Airplane_ID'image(ID) & " in holding pattern");
            end select;
        end loop;
        delay 0.2;
        Put_Line(Airplane_ID'image(ID) & " touched down!");
        Rwy.Cleared_Runway(ID);
    end;

    New_Airplane : Airplane_Access;

begin
    for I in Airplane_ID'range loop
        New_Airplane := new Airplane(I);
    end loop;
end Runway_Airplane;
