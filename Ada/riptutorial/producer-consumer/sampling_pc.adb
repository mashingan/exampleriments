with Ada.Text_IO; use Ada.Text_IO;

procedure Sampling_PC is
    type TenRanger is range 1..10;
    protected Buf is
        procedure Write(Item : in TenRanger);
        function Read return TenRanger;
        procedure Set_Done;
        function Get_Done return Boolean;
    private
        Value : TenRanger := TenRanger'First;
        Is_done : Boolean := false;
    end Buf;

    protected body Buf is
        procedure Write(Item : in TenRanger) is
        begin
            Value := Item;
        end Write;
        function Read return TenRanger is
        begin
            return Value;
        end Read;
        procedure Set_Done is begin Is_done := true; end Set_Done;
        function Get_Done return Boolean is begin return Is_Done; end Get_done;
    end Buf;

    task Consumer;
    task body Consumer is
    begin
        while not Buf.Get_Done loop
            Put_Line("Consumer read " & TenRanger'Image(Buf.Read));
        end loop;
    end Consumer;

begin
    for I in TenRanger loop
        Put_Line("Producer writing " & TenRanger'Image(I));
        Buf.Write(I);
    end loop;
    Buf.Set_Done;
end Sampling_PC;
