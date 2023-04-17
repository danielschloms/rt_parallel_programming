with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Integer_Text_IO;

procedure Main is
    package IO renames Ada.Text_IO;
    package Int_IO renames Ada.Integer_Text_IO;
    package CL renames Ada.Command_Line;

    max_n   : Positive;    -- Represents r in 0 < n <= r
    n_tasks : Positive;    -- Number of tasks

    procedure Usage is
    begin
        IO.Put_Line ("Usage: sum_of_cubes <max. n> <tasks>");
    end Usage;

    procedure Find_Solutions
       (max_n : Positive; n_tasks : Positive; n_solutions : Positive)
    is
        type Bool_Array is array (0 .. n_solutions - 1) of Boolean;

        protected Protected_Bool_Array is
            procedure Mark_Index (index : in Positive);
            function Get_N_Missing return Positive;
            function Check_Index (index : in Positive) return Boolean;
        private
            n_solutions_left : Positive   := n_solutions;
            check_array      : Bool_Array := (others => False);
        end Protected_Bool_Array;

        protected body Protected_Bool_Array is
            procedure Mark_Index (index : in Positive) is
            begin
                if (check_array (index) = False) then
                    check_array (index) := True;
                    n_solutions_left    := n_solutions_left - 1;
                end if;
            end Mark_Index;

            function Check_Index (index : in Positive) return Boolean is
            begin
                return check_array (index);
            end Check_Index;

            function Get_N_Missing return Positive is
            begin
                return n_solutions_left;
            end Get_N_Missing;
        end Protected_Bool_Array;

    begin
        null;
    end Find_Solutions;

    --  task type Generate_Tuples is
    --      entry Start (task_index : in Positive);
    --  end Generate_Tuples;

    --  task body Generate_Tuples is
    --  begin
    --      accept Start (task_index : in Positive);

    --      --  for I in Character range 'A' .. 'Z' loop
    --      --      IO.Put (I);
    --      --      IO.New_Line;
    --      --  end loop;
    --  end Generate_Tuples;

    --  tasks :
    --     array (0 .. Integer'Value (CL.Argument (2)) - 1) of Generate_Tuples;

begin
    IO.Put_Line("Deprecated!");
    return;
    -- Parse arguments
    if CL.Argument_Count /= 2 then
        Usage;
        return;
    end if;

    begin
        max_n   := Positive'Value (CL.Argument (1));
        n_tasks := Positive'Value (CL.Argument (2));
    exception
        when e : Constraint_Error =>
            Usage;
            raise Constraint_Error with "Invalid input argument(s)";
    end;

    IO.Put_Line ("Real Time & Parallel Programming Exercise 1");
    IO.Put_Line ("-------------------------------------------");
    IO.Put ("Max. n: ");
    Int_IO.Put (max_n, Width => CL.Argument (1)'Length);
    IO.New_Line;
    IO.Put ("Tasks:  ");
    Int_IO.Put (n_tasks, Width => CL.Argument (2)'Length);
    IO.New_Line;
    IO.Put_Line ("-------------------------------------------");

    -- Tab := [(0 .. n_tasks) of Generate_Tuples(0)];
    -- Tab(1).Start;
    --  for i in 0 .. n_tasks loop
    --      tasks(i).task_index := i;
    --  end loop;

end Main;
