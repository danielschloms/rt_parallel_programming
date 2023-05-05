package Train_Types is
    n_trains         : Integer := 3;
    path_length      : Integer := 3;
    n_track_sections : Integer := 5;

    type Train_Order is array (1 .. n_trains) of Integer;
    type Duration_Array is array (1 .. n_trains) of Duration;
    type Track_Section_Index is range 1 .. 5;
    type Path_Index is range 1 .. 3;
    type Train_Path is array (Path_Index'Range) of Track_Section_Index;

    initial_occupied : array (1 .. n_trains) of Track_Section_Index :=
       (1, 2, 5);

    path_train1 : Train_Path := (1, 3, 4);
    path_train2 : Train_Path := (2, 3, 5);
    path_train3 : Train_Path := (5, 3, 1);

    paths : array (1 .. n_trains) of Train_Path :=
       (path_train1, path_train2, path_train3);
end Train_Types;
