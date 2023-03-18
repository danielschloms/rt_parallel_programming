with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Integer_Text_IO;

procedure Sum_Of_Cubes is
    package IO renames Ada.Text_IO;
    package Int_IO renames Ada.Integer_Text_IO;
    package CL renames Ada.Command_Line;

    max_n : Integer;    -- Represents r in 0 < n <= r
    n_tasks : Integer;  -- Number of tasks

    procedure Usage is
    begin
        IO.Put_Line ("Usage: sum_of_cubes <max. n> <tasks>");
    end Usage;

    --  function Check_Tuple (

    --  )

begin
    -- Parse arguments
    if CL.Argument_Count /= 2 then
        Usage;
        return;
    end if;

    max_n := Integer'Value (CL.Argument (1));
    n_tasks := Integer'Value (CL.Argument (2));

    IO.Put_Line("Real Time & Parallel Programming Exercise 1");
    IO.Put_Line("-------------------------------------------");
    IO.Put("Max. n: ");
    Int_IO.Put(max_n, Width => CL.Argument (1)'Length);
    IO.New_Line;
    IO.Put("Tasks:  ");
    Int_IO.Put(n_tasks, Width => CL.Argument (1)'Length);
    IO.New_Line;
    IO.Put_Line("-------------------------------------------");
    

end Sum_Of_Cubes;
