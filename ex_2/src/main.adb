with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;  use Ada.Real_Time;
with Sum_Of_Cubes;   use Sum_Of_Cubes;

-- Project packages
with Typenames; use Typenames;
with Constants; use Constants;

-- @brief The main procedure of exercise 2
-- Usage: main <max. n> <tasks> <timeout>
-- Finds solutions for the Diophantine equation n = a**3 + b**3 + c**3 with the specified max. n,
-- number of tasks and max. calculation time per solution
procedure Main is
    package IO renames Ada.Text_IO;
    package CL renames Ada.Command_Line;

    max_n   : Int64;    -- Represents r in 0 < n <= r
    n_tasks : Int64;    -- Number of tasks
    timeout : Duration; -- Max. time to calculate a solution

    -- @brief Prints usage hints
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
    IO.Put_Line ("Timeout:       " & Duration'Image (timeout) & " seconds");
    IO.Put_Line ("-------------------------------------------");

    start_time := Clock;
    for n in 1 .. max_n loop
        if (n mod 9 /= 4) and (n mod 9 /= 5) then
            -- https://math.stackexchange.com/questions/1386034/integer-solutions-to-the-equation-a3b3c3-30
            -- Some n take very long to calculate -> exercise 2 deals with this via timeout
            Find_Solution (n, n_tasks, MAX_THIRD_ROOT, timeout);
        end if;
    end loop;
    stop_time := Clock;
    elapsed   := stop_time - start_time;
    IO.Put_Line
       ("Max. n =" & Int64'Image (max_n) & ", elapsed time:" &
        Duration'Image (To_Duration (elapsed)) & " seconds," &
        Int64'Image (n_tasks) & " tasks");

end Main;
