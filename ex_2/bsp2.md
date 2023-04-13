## Exercise 2:

Augment your solution of erxercise 1 by limiting the runtime.

If the calculation of the solution for a certain n takes very long, the calculation of exactly that n shall be terminated. Calculation shall then proceed with the next candidate number. Thus, your program shall be equipped with an additional parameter which tells how much time (in seconds) we are willing to wait for an answer (per n). Note that Ada has language features to setup deadlines very easily. These have to be used, otherwise your solution will be considered wrong.

Remarks:

Depending on your computer and operating system, changes in scheduling will be become effictive only at certain points in time (and not immediately if a condition becomes true). If e.g. your code of a 'then abort' statement does not abort immediately, it may help to insert a 'delay 0.0' statements at proper places of the abort-able code which enforce re-calculation of the scheduling. (Delay  statements among many others are so-called dispatching points in Ada.)

For exercise 2 it is not tolerated any more to solve it with only one program file. Please separate your program along interfaces into independent modules which can be compiled separately.

Also, your program should be documented. Comments are your friends on the long run, even if it is painful and time-consuming to write them at the first place. Use comments to organize your code and to separate it into distinguishable blocks.

Useful Ada packages:
Ada.Real_Time
Ada.Execution_Time
Ada.Calendar