with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Text_IO;

procedure synchro_buffer is
    type Work_Item is range 1 .. 100;

    package Work_Item_Queue_Interface is
        new Ada.Containers.Synchronized_Queue_Interfaces (
            Element_Type => Work_Item);

    package Work_Item_Queue is
        new Ada.Containers.Unbounded_Synchronized_Queues (
            Queue_Interfaces => Work_Item_Queue_Interface);

    Queue : Work_Item_Queue.Queue;

    task type Producer;
    task type Consumer;

    theproducer: Producer;
    Consumers : array (1 .. 10) of Consumer;

    task body Producer is
    begin
        for Item in Work_Item loop
            Queue.Enqueue (New_Item => Item);
        end loop;
    end Producer;

    task body Consumer is
        Item : Work_Item;
    begin
        loop
            select
            Queue.Dequeue (Element => Item);
            Ada.Text_IO.Put_Line(Work_Item'Image(Item));
        else exit;
    end select; end loop; Ada.Text_IO.Put_Line("end consumer");
    end Consumer;

begin
    null;
end synchro_buffer;
