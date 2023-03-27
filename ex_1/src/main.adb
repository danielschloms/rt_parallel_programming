with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Integer_Text_IO;

procedure Main is
    package IO renames Ada.Text_IO;
    package Int_IO renames Ada.Integer_Text_IO;
    package CL renames Ada.Command_Line;

    subtype LLInteger is Long_Long_Integer;

    -- Check a, b against (2^64)^(1/3)
    -- Could also use Long_Long_Long_Integer?
    MAX_THIRD_ROOT : constant LLInteger := 2_642_245;

    max_n       : LLInteger;    -- Represents r in 0 < n <= r
    n_tasks     : LLInteger;    -- Number of tasks
    n_solutions : LLInteger := 0;

    procedure Usage is
    begin
        IO.Put_Line ("Usage: sum_of_cubes <max. n> <tasks>");
    end Usage;

    procedure Find_Solutions
       (max_n : LLInteger; n_tasks : LLInteger; n_solutions : LLInteger)
    is
        type Bool_Array is array (0 .. n_solutions - 1) of Boolean;

        protected Protected_Bool_Array is
            procedure Mark_Index (index : in LLInteger);
            procedure Init (n_solutions : in LLInteger);
            function Get_N_Missing return LLInteger;
            function Check_Index (index : in LLInteger) return Boolean;
        private
            n_solutions_left : LLInteger  := n_solutions;
            check_array      : Bool_Array := (others => False);
        end Protected_Bool_Array;

        protected body Protected_Bool_Array is
            procedure Init (n_solutions : in LLInteger) is
            begin
                n_solutions_left := n_solutions;
                IO.Put_Line
                   ("Initialize with" & LLInteger'Image (n_solutions_left) &
                    " solutions");
            end Init;

            procedure Mark_Index (index : in LLInteger) is
            begin
                if (check_array (index) = False) then
                    check_array (index) := True;
                    n_solutions_left    := n_solutions_left - 1;
                end if;
            end Mark_Index;

            function Check_Index (index : in LLInteger) return Boolean is
            begin
                return check_array (index);
            end Check_Index;

            function Get_N_Missing return LLInteger is
            begin
                return n_solutions_left;
            end Get_N_Missing;
        end Protected_Bool_Array;

        task type Generate_Tuples is
            entry Start (index : in LLInteger);
        end Generate_Tuples;

        task body Generate_Tuples is
            task_index : LLInteger;
            a          : LLInteger;
            c_cubed    : LLInteger;

        begin
            accept Start (index : in LLInteger) do
                task_index := index;
                IO.Put_Line ("Start task" & LLInteger'Image (task_index));
            end Start;

            -- Init
            a := task_index;
            while True loop
                if Protected_Bool_Array.Get_N_Missing = 0 then
                    IO.Put_Line
                       ("Task" & LLInteger'Image (task_index) & ": All done");
                    exit;
                end if;
                if a >= MAX_THIRD_ROOT then
                    IO.Put_Line
                       ("Task" & LLInteger'Image (task_index) &
                        ": Reached max. third root");
                    exit;
                end if;

                for n in 1 .. max_n loop
                    if (i mod 9 = 4) or (i mod 9 = 5) then
                        continue;
                    end if;

                    for b in -a .. a loop
                        if b = 0 then
                            continue;
                        end if;

                        c_cubed := n - a**3 - b**3;
                    end loop;

                end loop;
            end loop;
        end Generate_Tuples;

        task_array : array (1 .. n_tasks) of Generate_Tuples;

    begin
        Protected_Bool_Array.Init (n_solutions);
        for i in 1 .. n_tasks loop
            task_array (i).Start (i);
        end loop;
    end Find_Solutions;

begin
    -- Parse arguments
    if CL.Argument_Count /= 2 then
        Usage;
        return;
    end if;

    begin
        max_n   := LLInteger'Value (CL.Argument (1));
        n_tasks := LLInteger'Value (CL.Argument (2));
    exception
        when e : Constraint_Error =>
            Usage;
            raise Constraint_Error with "Invalid input argument(s)";
    end;

    for i in 1 .. max_n loop
        if (i mod 9 /= 4) and (i mod 9 /= 5) then
            n_solutions := n_solutions + 1;
        end if;
    end loop;

    IO.Put_Line ("Real Time & Parallel Programming Exercise 1");
    IO.Put_Line ("-------------------------------------------");
    IO.Put_Line ("Max. n:        " & LLInteger'Image (max_n));
    IO.Put_Line ("Tasks:         " & LLInteger'Image (n_tasks));
    IO.Put_Line ("# of solutions:" & LLInteger'Image (n_solutions));
    IO.Put_Line ("-------------------------------------------");

    Find_Solutions (max_n, n_tasks, n_solutions);

    -- Tab := [(0 .. n_tasks) of Generate_Tuples(0)];
    -- Tab(1).Start;
    --  for i in 0 .. n_tasks loop
    --      tasks(i).task_index := i;
    --  end loop;

end Main;
