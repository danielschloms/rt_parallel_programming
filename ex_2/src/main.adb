with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;  use Ada.Real_Time;
with Sum_Of_Cubes; use Sum_Of_Cubes;

-- Project packages
with Typenames; use Typenames;
with Constants; use Constants;

procedure Main is
    package IO renames Ada.Text_IO;
    package CL renames Ada.Command_Line;

    max_n   : Int64;    -- Represents r in 0 < n <= r
    n_tasks : Int64;    -- Number of tasks
    timeout : Duration; -- Max. time to calculate a solution

    procedure Usage is
    begin
        IO.Put_Line ("Usage: main <max. n> <tasks> <timeout>");
    end Usage;

    start_time : Time;
    stop_time  : Time;
    elapsed    : Time_Span;

begin
    -- Parse arguments
    if CL.Argument_Count /= 3 then
        Usage;
        return;
    end if;

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

    IO.Put_Line ("Real Time & Parallel Programming Exercise 2");
    IO.Put_Line ("-------------------------------------------");
    IO.Put_Line ("Max. n:        " & Int64'Image (max_n));
    IO.Put_Line ("Tasks:         " & Int64'Image (n_tasks));
    IO.Put_Line ("Max. timeout:  " & Duration'Image (timeout) & " seconds");
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
                    Find_Solution (n, n_tasks, MAX_THIRD_ROOT, timeout);
            end case;
        end if;
    end loop;
    stop_time := Clock;
    elapsed   := stop_time - start_time;
    IO.Put_Line
       ("Max. n =" & Int64'Image (max_n) & ", elapsed time:" &
        Duration'Image (To_Duration (elapsed)) & " seconds," &
        Int64'Image (n_tasks) & " tasks");

end Main;
