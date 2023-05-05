with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Real_Time;    use Ada.Real_Time;

with Train_Types;      use Train_Types;
with Train_Simulation; use Train_Simulation;

-- @brief The main procedure of exercise 2
-- Usage: main <max. n> <tasks> <timeout>
-- Finds solutions for the Diophantine equation n = a**3 + b**3 + c**3 with the specified max. n,
-- number of tasks and max. calculation time per solution
procedure Main is

    section_delays : Duration_Array := (0.05, 0.1, 0.15);

    -- @brief Prints usage hints
    procedure Usage is
    begin
        Put_Line ("Usage: main");
    end Usage;

    procedure Order_Permutation
       (entry_delays : in Duration_Array; leave_delays : in Duration_Array)
    is
    begin
        for first in 1 .. n_trains loop
            for second in 1 .. n_trains loop
                for third in 1 .. n_trains loop
                    if (first /= second) and (first /= third) and (second /= third) then
                        Simulate((first, second, third), entry_delays, leave_delays);
                    end if;
                end loop;
            end loop;
        end loop;
    end Order_Permutation;

    procedure Entry_Delay_Permutation(leave_delays : in Duration_Array) is
    begin
        for first in 1 .. n_trains loop
            for second in 1 .. n_trains loop
                for third in 1 .. n_trains loop
                    Order_Permutation((section_delays(first), section_delays(second), section_delays(third)), leave_delays);
                end loop;
            end loop;
        end loop;
    end Entry_Delay_Permutation;

    procedure Leave_Delay_Permutation is
    begin
        for first in 1 .. n_trains loop
            for second in 1 .. n_trains loop
                for third in 1 .. n_trains loop
                    Entry_Delay_Permutation((section_delays(first), section_delays(second), section_delays(third)));
                end loop;
            end loop;
        end loop;
    end Leave_Delay_Permutation;

begin
    -- Parse arguments
    if Argument_Count /= 0 then
        Usage;
        return;
    end if;

    Put_Line ("Real Time & Parallel Programming Exercise 3");
    Put_Line ("-------------------------------------------");
    Leave_Delay_Permutation;

end Main;
