with Train_Types; use Train_Types;

package Train_Simulation is
    procedure Simulate
       (order        : in Train_Order; entry_delays : in Duration_Array;
        leave_delays : in Duration_Array);
end Train_Simulation;
