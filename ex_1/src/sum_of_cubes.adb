with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Integer_Text_IO;

procedure Sum_Of_Cubes is
    package IO renames Ada.Text_IO;
    package Int_IO renames Ada.Integer_Text_IO;
    package CL renames Ada.Command_Line;

    max_n   : Integer :=
       Integer'Value (CL.Argument (1));    -- Represents r in 0 < n <= r
    n_tasks : Integer := Integer'Value (CL.Argument (2));    -- Number of tasks

    type Bool_Array is array (0 .. max_n - 1) of Boolean;

    protected Protected_Bool_Array is
        procedure Mark_Index (index : Positive);
    private
        n_tuples_left : Integer := max_n;
        check_array   : Bool_Array;
    end Protected_Bool_Array;

    protected body Protected_Bool_Array is
        procedure Mark_Index (index : Positive) is
        begin
            if (index < check_array'Length) then
                check_array (index) := True;
            end if;
            --return True;
        end Mark_Index;

    end Protected_Bool_Array;

    task type Generate_Tuples is
        entry Start (task_index : in Positive);
    end Generate_Tuples;

    task body Generate_Tuples is
    begin
        accept Start (task_index : in Positive);

        --  for I in Character range 'A' .. 'Z' loop
        --      IO.Put (I);
        --      IO.New_Line;
        --  end loop;
    end Generate_Tuples;

    procedure Usage is
    begin
        IO.Put_Line ("Usage: sum_of_cubes <max. n> <tasks>");
    end Usage;

    tasks :
       array (0 .. Integer'Value (CL.Argument (2)) - 1) of Generate_Tuples;

begin
    -- Parse arguments
    if CL.Argument_Count /= 2 then
        Usage;
        return;
    end if;

    -- max_n   := Integer'Value (CL.Argument (1));
    -- n_tasks := Integer'Value (CL.Argument (2));

    IO.Put_Line ("Real Time & Parallel Programming Exercise 1");
    IO.Put_Line ("-------------------------------------------");
    IO.Put ("Max. n: ");
    Int_IO.Put (max_n, Width => CL.Argument (1)'Length);
    IO.New_Line;
    IO.Put ("Tasks:  ");
    Int_IO.Put (n_tasks, Width => CL.Argument (1)'Length);
    IO.New_Line;
    IO.Put_Line ("-------------------------------------------");

    -- Tab := [(0 .. n_tasks) of Generate_Tuples(0)];
    -- Tab(1).Start;
    --  for i in 0 .. n_tasks loop
    --      tasks(i).task_index := i;
    --  end loop;

end Sum_Of_Cubes;
