with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

-- https://en.wikibooks.org/wiki/Ada_Programming/Mathematical_calculations#Root
-- needed for floating point exponentiation
with Ada.Numerics.Generic_Elementary_Functions;

procedure Main is
    package IO renames Ada.Text_IO;
    package CL renames Ada.Command_Line;

    subtype Int16 is Short_Integer;
    subtype Int32 is Long_Integer;
    subtype Int64 is Long_Long_Integer;
    subtype LLFloat is Long_Long_Float;

    -- See root calculation link above
    package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions
       (LLFloat);
    use Value_Functions;

    -- Check a, b against max_Int64^(1/3) (precalculated)
    -- Could also use Long_Long_Long_Integer?
    ONE_THIRD      : constant LLFloat := 1.0 / 3.0;
    MAX_THIRD_ROOT : constant Int64   := Int64 (LLFloat (Int64'Last)**ONE_THIRD);

    max_n   : Int64;    -- Represents r in 0 < n <= r
    n_tasks : Int64;    -- Number of tasks

    procedure Usage is
    begin
        IO.Put_Line ("Usage: sum_of_cubes <max. n> <tasks>");
    end Usage;

    procedure Find_Solution (current_n : Int64; n_tasks : Int64) is

        protected Shared_Check is

            procedure Set (value : in Boolean);
            function Check return Boolean;

        private

            found : Boolean := False;

        end Shared_Check;

        protected body Shared_Check is

            procedure Set (value : in Boolean) is
            begin
                found := value;
            end Set;

            function Check return Boolean is
            begin
                return found;
            end Check;

        end Shared_Check;

        task type Generate_Tuples is
            entry Start (index : in Int64; n : in Int64);
        end Generate_Tuples;

        task body Generate_Tuples is

            --  package Value_Functions is new Ada.Numerics
            --     .Generic_Elementary_Functions
            --     (LLFloat);
            --  use Value_Functions;

            task_index : Int64;
            current_n  : Int64;
            a          : Int64;
            c_cubed    : Int64;
            cc_root    : LLFloat;
            c_cubed_bn : Int64;
            cc_root_bn : LLFloat;

        begin

            accept Start (index : in Int64; n : in Int64) do
                task_index := index;
                current_n  := n;
                IO.Put_Line
                   ("Start task" & Int64'Image (task_index) & " for n =" &
                    Int64'Image (n));
            end Start;

            a := task_index;

            -- IO.Put_Line ("t" & Int64'Image (task_index) & " 1");

            while a < MAX_THIRD_ROOT loop

                if (Shared_Check.Check = True) then
                    IO.Put_Line
                       ("Task" & Int64'Image (task_index) &
                        ": Solution check exit");
                    exit;
                end if;

                -- IO.Put_Line ("t" & Int64'Image (task_index) & " 2");

                for b in Int64 range -a .. a loop
                    --  IO.Put_Line
                    --     ("t" & Int64'Image (task_index) & " a" &
                    --      Int64'Image (a) & " b" & Int64'Image (b));
                    if b /= 0 then
                        c_cubed := current_n - (a**3) - (b**3);

                        if c_cubed /= 0 then
                            --  IO.Put_Line
                            --     ("t" & Int64'Image (task_index) & " a" &
                            --      Int64'Image (a) & " b" & Int64'Image (b) &
                            --      "ccubed " & Int64'Image (c_cubed));

                            if (c_cubed > 0) then
                                cc_root := LLFloat (c_cubed)**(1.0 / 3.0);
                            else
                                cc_root := -LLFloat (-c_cubed)**(1.0 / 3.0);
                            end if;

                            --  IO.Put_Line
                            --     ("casted" &
                            --      LLFloat'Image (LLFloat (Int64 (cc_root))));
                            --  IO.Put_Line
                            --     ("not casted" & LLFloat'Image (cc_root));
                            if LLFloat (Int64 (cc_root)) = cc_root then
                                Shared_Check.Set (True);
                                IO.Put_Line
                                   ("Task" & Int64'Image (task_index) &
                                    ": Found solution for n =" &
                                    Int64'Image (current_n));
                                exit;
                            end if;
                        end if;
                        --  IO.Put_Line
                        --     ("t" & Int64'Image (task_index) & " try negative");

                        c_cubed_bn := current_n - (a**3) - (-b**3);

                        if c_cubed_bn /= 0 then

                            if (c_cubed_bn > 0) then
                                cc_root_bn :=
                                   LLFloat (c_cubed_bn)**(1.0 / 3.0);
                            else
                                cc_root_bn :=
                                   -LLFloat (-c_cubed_bn)**(1.0 / 3.0);
                            end if;

                            if LLFloat (Int64 (cc_root_bn)) = cc_root_bn then
                                Shared_Check.Set (True);
                                IO.Put_Line
                                   ("Task" & Int64'Image (task_index) &
                                    ": Found solution for n =" &
                                    Int64'Image (current_n));
                                exit;
                            end if;
                        end if;

                        --  IO.Put_Line
                        --     ("t" & Int64'Image (task_index) & " increase b");
                    end if;
                end loop;

                a := a + n_tasks;
            end loop;

            if a >= MAX_THIRD_ROOT then
                IO.Put_Line
                   ("Task" & Int64'Image (task_index) &
                    ": Reached max. third root");
            end if;
            IO.Put_Line ("Unreachable!");
        exception
            when e : Constraint_Error =>
                IO.Put_Line ("Constr");
                IO.Put_Line
                   ("t" & Int64'Image (task_index) & Exception_Message (e));
            when e : others           =>
                IO.Put_Line
                   ("t" & Int64'Image (task_index) & Exception_Message (e));
        end Generate_Tuples;

        task_array : array (1 .. n_tasks) of Generate_Tuples;

    begin
        Shared_Check.Set (False);
        for index in 1 .. n_tasks loop
            task_array (index).Start (index, current_n);
        end loop;
    exception
        when others =>
            IO.Put_Line ("exception");
    end Find_Solution;

begin
    -- Parse arguments
    if CL.Argument_Count /= 2 then
        Usage;
        return;
    end if;

    begin
        max_n   := Int64'Value (CL.Argument (1));
        n_tasks := Int64'Value (CL.Argument (2));
    exception
        when e : Constraint_Error =>
            Usage;
            raise Constraint_Error with "Invalid input argument(s)";
    end;

    IO.Put_Line ("Real Time & Parallel Programming Exercise 1");
    IO.Put_Line ("-------------------------------------------");
    IO.Put_Line ("Max. n:        " & Int64'Image (max_n));
    IO.Put_Line ("Tasks:         " & Int64'Image (n_tasks));
    IO.Put_Line ("-------------------------------------------");

    -- Test & Debug stuff
    IO.Put_Line ("Const one third:" & LLFloat'Image (ONE_THIRD));
    IO.Put_Line ("Const max root:" & Int64'Image (MAX_THIRD_ROOT));
    --

    Find_Solution (9, 10);
    return;

    for n in 1 .. max_n loop
        if (n mod 9 /= 4) and (n mod 9 /= 5) then
            Find_Solution (n, n_tasks);
        end if;
    end loop;

end Main;
