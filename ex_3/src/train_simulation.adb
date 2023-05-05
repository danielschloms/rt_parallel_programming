with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Train_Types; use Train_Types;

package body Train_Simulation is

    -- Use protected **type** for builtin atomic entry
    -- https://www.adaic.org/resources/add_content/docs/95style/html/sec_6/6-1-1.html

    -- @brief Model of a track section
    protected type Track_Section is

        -- @brief Provides entry to a track section if occupied == false and sets it to true
        entry Enter;

        -- @brief Frees up the track section (occupied set to false)
        procedure Leave;

    private

        occupied : Boolean := False;

    end Track_Section;

    protected body Track_Section is

        entry Enter when occupied = False is
        begin
            occupied := True;
        end Enter;

        procedure Leave is
        begin
            occupied := False;
        end Leave;

    end Track_Section;

    track_sections : array (Track_Section_Index'Range) of Track_Section;

    procedure Reset_Track is
    begin
        for i in track_sections'Range loop
            track_sections (i).Leave;
        end loop;

        for i in initial_occupied'Range loop
            track_sections (initial_occupied (i)).Enter;
        end loop;
    end Reset_Track;

    procedure Simulate
       (order        : in Train_Order; entry_delays : in Duration_Array;
        leave_delays : in Duration_Array)
    is

        task type Train is
            entry Start
               (path        : in Train_Path; entry_delay : in Duration;
                leave_delay : in Duration);
        end Train;

        task body Train is

            current_path_index : Path_Index;
            local_leave_delay  : Duration;
            local_entry_delay  : Duration;
            local_path         : Train_Path;

            procedure Goto_Track_Section
               (section_index : in Track_Section_Index)
            is
            begin
                delay local_entry_delay;
                track_sections (section_index).Enter;
                delay local_leave_delay;
                track_sections (local_path (current_path_index)).Leave;
            end Goto_Track_Section;
        begin
            accept Start
               (path        : in Train_Path; entry_delay : in Duration;
                leave_delay : in Duration)
            do
                local_path         := path;
                local_entry_delay  := entry_delay;
                local_leave_delay  := leave_delay;
                current_path_index := 1;
            end Start;

            select
                delay 2.0;
                Put_Line ("Deadlock");
            then abort

                while current_path_index < Path_Index'Last loop
                    Goto_Track_Section (local_path (current_path_index + 1));
                    current_path_index := current_path_index + 1;
                end loop;
                -- Put_Line ("Done");
            end select;

        end Train;

        train_tasks : array (1 .. n_trains) of Train;

    begin

        Put_Line
           ("Order:" & Integer'Image (order (1)) & "," &
            Integer'Image (order (2)) & "," & Integer'Image (order (3)));
        Put_Line
           ("Entry delays:" & Duration'Image (entry_delays (1)) & "," &
            Duration'Image (entry_delays (2)) & "," &
            Duration'Image (entry_delays (3)));
        Put_Line
           ("Leave delays:" & Duration'Image (leave_delays (1)) & "," &
            Duration'Image (leave_delays (2)) & "," &
            Duration'Image (leave_delays (3)));

        Reset_Track;
        train_tasks (1).Start
           (paths (order (1)), entry_delays (1), leave_delays (1));
        delay 0.0001;
        train_tasks (2).Start
           (paths (order (2)), entry_delays (2), leave_delays (2));
        delay 0.0001;
        train_tasks (3).Start
           (paths (order (3)), entry_delays (3), leave_delays (3));

    end Simulate;

end Train_Simulation;
