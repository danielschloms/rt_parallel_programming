-- David Reitgruber
-- 11777716
-- The results of the empirical testing are in the results.txt file

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_line; use Ada.Command_Line;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

procedure ex1 is

    --needed to calculate third root correctly
    package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions(Long_Long_Float);
    use Value_Functions;

    n : Long_Long_Integer; --number to find
    tasks : Long_Long_Integer; --number of tasks

    MAX_VALUE : constant Long_Long_Integer := 2642245; --overflow check

    --check validity of input arguments
    --two arguments of type Long_Long_Integer
    procedure check_args(n, tasks : out Long_Long_Integer) is
        Argument_Count_Error : exception;
        Invalid_N_Error : exception;
    begin

        if Argument_Count /= 2 then
            raise Argument_Count_Error with "Usage: ex1 [n] [# of tasks]";
        end if;

        n := Long_Long_Integer'Value(Argument(1));
        tasks := Long_Long_Integer'Value(Argument(2));

        exception
        when E : Constraint_Error =>
            raise Constraint_Error with "Invalid argument!";
    end check_args;


    --found solution object for tasks to write
    --inticates if a solution was found
    protected found_solution is
        procedure Set;
        procedure Reset;
        function Get return Boolean;
    private
        found : Boolean := False;
    end found_solution;

    protected body found_solution is
        procedure Set is
        begin
            found := True;
        end Set;

        procedure Reset is
        begin
            found := False;
        end Reset;

        function Get return Boolean is
        begin
            return found;
        end Get;
    end found_solution;


    --searches for sum of cubes
    --tasks in extra procedure ensures that all tasks are done, before the next number is calculated
    --otherwise reset of the found variable does not work
    procedure sum_of_cubes(tasks, n : Long_Long_Integer) is

        task type sum_of_cubes_finder(tasks, n, task_number :Long_Long_Integer) is
        end sum_of_cubes_finder;

        --define array of tasks
        type sum_of_cubes_finder_array is array(Long_Long_Integer range <>) of access sum_of_cubes_finder;
        finders : access sum_of_cubes_finder_array;


        --Task body
        task body sum_of_cubes_finder is
            a : Long_Long_Integer := 0;
            c : Long_Long_Float;
            c_3 : Long_Long_Integer; --c^3

            procedure check_interval(a, n, task_number : Long_Long_Integer) is
            begin
                for b in -a..a loop
                    if b /= 0 then
                        --break if solution found
                        if found_solution.get=true then
                            return;
                        end if;
                        c_3 := n - a**3 - b**3; --calculate c^3
                        if c_3 /= 0 then
                            if c_3 < 0 then
                                c := -Long_Long_Float(-c_3)**(1.0/3.0); -- get c as Float
                            else
                                c := Long_Long_Float(c_3)**(1.0/3.0);
                            end if;
                            
                            if found_solution.get = false and ((a**3 + b**3 + Long_Long_Integer(c)**3) = n) then --check if c as integer results in n
                                found_solution.set;
                                Put_Line(Long_Long_Integer'Image(n)&"=("&Long_Long_Integer'Image(a)&")^3+(" & Long_Long_Integer'Image(b) & ")^3+(" & Long_Long_Integer'Image(Long_Long_Integer(c)) & ")^3"); 
                            end if;
                        end if;
                    end if;
                end loop;
            end check_interval;

        --begin task sum_of_cubes_finder
        begin
            --reject numbers mod 9 which equal 4 or 5
            if (n mod 9 /= 4) and (n mod 9 /= 5) then
                a := task_number; --start with task number
                --overflow check or solution not found
                while abs(a) < MAX_VALUE and found_solution.get = false loop
                    if a /=0 then
                        check_interval(a, n, task_number);
                        exit when found_solution.get;
                        check_interval(-a, n, task_number);
                    end if;
                    a := a + tasks; --add amount of tasks to skip numbers used by other tasks
                end loop;

                if abs(a) >= MAX_VALUE then
                    Put_Line("Overflow for a= " & Long_Long_Integer'Image(a));
                end if;
            else
                if task_number=1 then
                    Put_Line("No Solution for " & Long_Long_Integer'Image(n));
                end if;
            end if;
        end sum_of_cubes_finder;

    --begin sum_of_cubes
    begin
        --reset solution found everytime we want to find values for a new number
        found_solution.reset;  

        finders := new sum_of_cubes_finder_array(1..tasks); 

        --create tasks
        for t in 1..tasks loop
            finders(t) := new sum_of_cubes_finder(tasks, n, t-1);
        end loop;
    end sum_of_cubes;


--begin ex1
begin
    check_args(n, tasks);

    --iterate over numbers
    for i in 1..n loop 
        sum_of_cubes(tasks, i);
    end loop;
end ex1;