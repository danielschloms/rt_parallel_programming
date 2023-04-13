with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;  use Ada.Real_Time;

-- https://en.wikibooks.org/wiki/Ada_Programming/Mathematical_calculations#Root
-- needed for floating point exponentiation
with Ada.Numerics.Generic_Elementary_Functions;

-- Project packages
with Typenames; use Typenames;

procedure Main is
    package IO renames Ada.Text_IO;
    package CL renames Ada.Command_Line;

    --  subtype Int16 is Short_Integer;
    --  subtype Int32 is Long_Integer;
    --  subtype Int64 is Long_Long_Integer;
    --  -- Int128 too large
    --  subtype LLFloat is Long_Long_Float;

    -- See root calculation link above
    package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions
       (LLFloat);
    use Value_Functions;

    ONE_THIRD      : constant LLFloat := 1.0 / 3.0;
    -- Check a, b against max_Int64^(1/3) (precalculated) ~ 2.09 * 10^6
    MAX_THIRD_ROOT : constant Int64 := Int64 (LLFloat (Int64'Last)**ONE_THIRD);
    BM_LIMIT       : constant Int64   := 10_000;

    max_n   : Int64;    -- Represents r in 0 < n <= r
    n_tasks : Int64;    -- Number of tasks
    timeout : Duration; -- Max. time to calculate a solution

    procedure Usage is
    begin
        IO.Put_Line ("Usage: main <max. n> <tasks> <timeout> [-b]");
    end Usage;

    procedure Find_Solution (current_n : Int64; n_tasks : Int64; limit : Int64)
    is

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
            entry Start (index : in Int64; n : in Int64; limit : in Int64);
        end Generate_Tuples;

        task body Generate_Tuples is

            task_index : Int64;
            current_n  : Int64;
            current_a  : Int64;
            max_a      : Int64;

            function Check_Tuple (a : in Int64; b : in Int64) return Boolean is
                cc_root   : LLFloat;
                c         : Int64;
                check_sum : Int64;
                c_cubed   : Int64;
            begin

                c_cubed := current_n - (a**3) - (b**3);

                if c_cubed /= 0 then

                    if (c_cubed > 0) then
                        cc_root := LLFloat (c_cubed)**ONE_THIRD;
                    else
                        cc_root := -LLFloat (-c_cubed)**ONE_THIRD;
                    end if;

                    -- Previous check was with Float(Int(c)) == c
                    -- but this resulted in inaccuracies due to the nature of Float.
                    -- Therefore, check if a^3 + b^3 + Int(c)^3 == n -> works
                    c         := Int64 (cc_root);
                    check_sum := (a**3) + (b**3) + (c**3);

                    if check_sum = current_n then
                        Shared_Check.Set (True);
                        IO.Put_Line
                           ("Task" & Int64'Image (task_index) & ": " &
                            Int64'Image (current_n) & " = (" &
                            Int64'Image (a) & "**3) + (" & Int64'Image (b) &
                            "**3) + (" & Int64'Image (c) & "**3)");
                        return True;
                    end if;

                end if;

                return False;
            end Check_Tuple;

        begin

            accept Start (index : in Int64; n : in Int64; limit : in Int64) do
                task_index := index;
                current_n  := n;
                max_a      := limit;
                --  IO.Put_Line
                --     ("Start task" & Int64'Image (task_index) & " for n =" &
                --      Int64'Image (n));
            end Start;

            current_a := task_index;

            while current_a < max_a loop

                if (Shared_Check.Check = True) then
                    --  IO.Put_Line
                    --     ("Task" & Int64'Image (task_index) &
                    --      ": Solution check exit");
                    exit;
                end if;

                for current_b in Int64 range -current_a .. current_a loop
                    if current_b /= 0 then
                        if Check_Tuple (current_a, current_b) = True then
                            exit;
                        elsif Check_Tuple (-current_a, current_b) = True then
                            exit;
                        end if;
                    end if;
                end loop;

                current_a := current_a + n_tasks;
            end loop;

            if current_a >= MAX_THIRD_ROOT then
                IO.Put_Line
                   ("Task" & Int64'Image (task_index) &
                    ": Reached max. third root");
            end if;

        exception
            when e : others =>
                IO.Put_Line
                   ("Exception Task" & Int64'Image (task_index) &
                    Exception_Message (e));
        end Generate_Tuples;

        task_array : array (1 .. n_tasks) of Generate_Tuples;

    begin
        Shared_Check.Set (False);
        for index in 1 .. n_tasks loop
            task_array (index).Start (index, current_n, limit);
        end loop;
    exception
        when e : others =>
            IO.Put_Line ("Find_Solution exception: " & Exception_Message (e));
    end Find_Solution;

    start_time : Time;
    stop_time  : Time;
    elapsed    : Time_Span;

begin
    -- Parse arguments
    if CL.Argument_Count = 3 or CL.Argument_Count = 4 then
        begin
            max_n   := Int64'Value (CL.Argument (1));
            n_tasks := Int64'Value (CL.Argument (2));
            timeout := Duration'Value (CL.Argument (3));

            if (max_n <= 0 or n_tasks <= 0 or timeout <= 0.0) then
                IO.Put_Line ("Only positive non-zero values allowed");
                return;
            end if;
        exception
            when e : Constraint_Error =>
                Usage;
                raise Constraint_Error with "Invalid input argument(s)";
        end;
    end if;

    if CL.Argument_Count = 3 then

        IO.Put_Line ("Real Time & Parallel Programming Exercise 2");
        IO.Put_Line ("-------------------------------------------");
        IO.Put_Line ("Max. n:        " & Int64'Image (max_n));
        IO.Put_Line ("Tasks:         " & Int64'Image (n_tasks));
        IO.Put_Line
           ("Max. timeout:  " & Duration'Image (timeout) & " seconds");
        IO.Put_Line ("-------------------------------------------");

        start_time := Clock;
        for n in 1 .. max_n loop
            if (n mod 9 /= 4) and (n mod 9 /= 5) then
                case n is
                    -- https://math.stackexchange.com/questions/1386034/integer-solutions-to-the-equation-a3b3c3-30
                    -- This problem seems to not be solved for n = 114, 165, 390, 579, 627, 633, 732, 795, 906, 921, 975
                    -- and would take a very long time for some values (e.g. n = 39)
                    --  when 30 =>
                    --      IO.Put_Line
                    --         ("n = 30 solution too large: 30 = (2_220_422_932**3) - (283_059_965**3) - (2_218_888_517**3)");
                    --  when 33 =>
                    --      IO.Put_Line
                    --         ("n = 33 solution too large: 30 = (8_866_128_975_287_528**3) - (8_778_405_442_862_239**3) - (2_736_111_468_807_040**3)");
                    when others =>
                        Find_Solution (n, n_tasks, MAX_THIRD_ROOT);
                end case;
            end if;
        end loop;
        stop_time := Clock;
        elapsed   := stop_time - start_time;
        IO.Put_Line
           ("Max. n =" & Int64'Image (max_n) & ", elapsed time:" &
            Duration'Image (To_Duration (elapsed)) & " seconds," &
            Int64'Image (n_tasks) & " tasks");

    elsif CL.Argument_Count = 4 then
        if CL.Argument (3) = "-b" then
            start_time := Clock;
            Find_Solution (max_n, n_tasks, BM_LIMIT);
            stop_time := Clock;
            elapsed   := stop_time - start_time;
            IO.Put_Line
               ("BM elapsed time: " & Duration'Image (To_Duration (elapsed)) &
                " seconds, limit" & Int64'Image (BM_LIMIT) & "," &
                Int64'Image (n_tasks) & " tasks");
        else
            Usage;
            return;
        end if;
    else
        Usage;
        return;
    end if;

end Main;
