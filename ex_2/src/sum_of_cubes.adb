with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;

with Typenames; use Typenames;
with Constants; use Constants;

package body Sum_Of_Cubes is

  package IO renames Ada.Text_IO;

  package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions
   (LLFloat);
  use Value_Functions;

  -- @brief Procedure to find an integer solution for a**3 + b**3 + c**3 = n
  --
  -- @param current_n the n for with a solution must be found
  -- @param n_tasks # of tasks that search for a single solution in parallel
  -- @param limit the max search range (MAX_THIRD_ROOT per default)
  -- @param timeout the time limit to search for a solution
  procedure Find_Solution
   (current_n : Int64; n_tasks : Int64; limit : Int64; timeout : Duration)
  is

    -- @brief Protected object to share solution state
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

    -- @brief Task to generate tuples a, b, c in the form a**3 + b**3 + c**3 = n
    --
    -- Entry Start params:
    -- @param index the task index
    -- @param n the current n
    -- @param limit the search range limit
    -- @param timeout the max search time for a task
    task type Generate_Tuples is
      entry Start
       (index   : in Int64; n : in Int64; limit : in Int64;
        timeout : in Duration);
    end Generate_Tuples;

    task body Generate_Tuples is

      task_index : Int64;
      current_n  : Int64;
      current_a  : Int64;
      max_a      : Int64;
      max_time   : Duration;

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

          -- c is integer third root -> solution found, set in protected shared object
          if check_sum = current_n then
            Shared_Check.Set (True);
            IO.Put_Line
             ("Task" & Int64'Image (task_index) & ": " &
              Int64'Image (current_n) & " = (" & Int64'Image (a) & "**3) + (" &
              Int64'Image (b) & "**3) + (" & Int64'Image (c) & "**3)");
            return True;
          end if;

        end if;

        return False;
      end Check_Tuple;

    begin

      accept Start
       (index   : in Int64; n : in Int64; limit : in Int64;
        timeout : in Duration)
      do
        task_index := index;
        current_n  := n;
        max_a      := limit;
        max_time   := timeout;
      end Start;

      -- For setting task timeouts, see Asynchronous Transfer of Control
      -- https://www.adaic.org/resources/add_content/standards/12aarm/html/AA-9-7-4.html
      select
        delay max_time;
        IO.Put_Line
         ("Task" & Int64'Image (task_index) & ", n =" &
          Int64'Image (current_n) & ": timeout");
      then abort

        current_a := task_index;

        -- Get tuples by iterating b from -a to a and calculating the corresponding c in Check_Tuple()
        -- Perform the check for a, b and -a, b
        -- Different tasks don't perform the same calculations since a is incremented by the # of tasks
        while current_a < max_a loop

          if (Shared_Check.Check = True) then
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

        -- Max. third root is defined by the third root of a max. Int64
        if (current_a >= MAX_THIRD_ROOT) then
          IO.Put_Line
           ("Task" & Int64'Image (task_index) & ": Reached max. third root");
        end if;
      end select;

    exception
      when e : others =>
        IO.Put_Line
         ("Exception Task" & Int64'Image (task_index) & Exception_Message (e));
    end Generate_Tuples;

    task_array : array (1 .. n_tasks) of Generate_Tuples;

  begin
    Shared_Check.Set (False);
    for index in 1 .. n_tasks loop
      task_array (index).Start (index, current_n, limit, timeout);
    end loop;
  exception
    when e : others =>
      IO.Put_Line ("Find_Solution exception: " & Exception_Message (e));
  end Find_Solution;

end Sum_Of_Cubes;
