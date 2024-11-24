-- example from
--https://en.wikipedia.org/wiki/Ada_(programming_language)
with ada.text_io; use ada.text_io;

procedure Traffic is
    type Airplane_ID is range 1..10;

    task type 飛行機 (ID: Airplane_ID);
    type Airplane_Access is access Airplane;

    protected type Runway is
    entry Assign_Aircraft (ID: Airplane_ID);
    entry Cleared_Runway (ID: Airplane_ID);
    entry Wait_for_clear;
    private
    Clear : Boolean := true;
    end Runway;

    type Runway_Access is access all Runway;

    task type Controller (My_Runway: Runway_Access) is
        entry Request_Takeoff(ID: in Airplane_ID; Takeoff: out Runway_Access);
        entry Request_Approach(ID: in Airplane_ID; Approach: out Runway_Access);
        end Controller;

    Runway1 : aliased Runway;
    Controller1: Controller(Runway1'Access);

    protected body Runway is
        entry Assign_Aircraft(ID: Airplane_ID)

    when Clear is
    begin
        Clear := False;
        Put_line(Airplane_ID'Image(ID) & " on runway ");
    end;

    entry Cleared_Runway(ID: Airplane_ID)
        when not clear is
            begin
                clear := true;
                put_line(Airplane_ID'Image(ID) & " cleared runway ");
            end;

    entry Wait_for_clear
        when clear is
            begin
                null;
            end;
    end Runway;

    task body Controller is
    begin
        loop
            My_Runway.Wait_For_Clear;
            select
                when Request_Approach'count = 0 =>
                    accept Request_Takeoff(ID: in Airplane_ID; Takeoff: out Runway_Access)
                        do
                            My_Runway.Assign_Aircraft(ID);
                            Takeoff := My_Runway;
                    end Request_Takeoff;
            or
            accept Request_Approach(ID: in Airplane_ID; Approach: out Runway_Access) do
                My_Runway.Assign_Aircraft(ID);
                Approach := My_Runway;
            end Request_Approach;
            or
            terminate;
        end select;
    end loop;
    end;

    task body 飛行機 is
        Rwy: Runway_Access;
    begin
        Controller1.Request_Takeoff(ID, Rwy);
        Put_line(Airplane_ID'image(ID) & " taking off...");
        delay 1.0;
        Rwy.Cleared_Runway(ID);
        delay 1.5;
        loop
            select
            Controller1.Request_Approach(ID, Rwy);
            exit;
            or
            delay 2.0;
            put_line(Airplane_ID'Image(ID) & " in holding pattern");
        end select;
    end loop;
    delay 3.0;
    Put_line(Airplane_ID'Image(ID) & "   touched down!");
    Rwy.Cleared_Runway(ID);
    end;

    New_Airplane: Airplane_Access;

begin
    for I in Airplane_ID'Range loop
        New_Airplane := new 飛行機(I);
        delay 1.0;
    end loop;
end Traffic;
