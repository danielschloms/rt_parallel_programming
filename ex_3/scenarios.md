# Order permutation

The most basic scenarios are where only the start order of the tasks (trains) is switched.
In this case there are $n!$ different possibilities, so 6 possibilities with 3 trains.
Only the scenario T1-T3-T2 works, otherwise a deadlock occurs.

# Leave delay permutation

This does not affect this small example, as the critical parts are that T1 enters section 3 first, and T3 enters section 3 second.
In larger examples this could cause/prevent deadlocks if, for example, a train that is held up longer lets a different train pass instead of causing a deadlock.

# Entry delay permutation

I also added different delays for entering a new section (e.g. trains need longer to start/ramp up).
In this case, the any start order that still permits the usage of section 3 in order T1-T3-T2 works,
e.g. start order T1-T2-T3, but T2 having a longer entry delay than T3.
This essentially overwrites the start order permutation effect.

# Number of scenarios

The total number of scenarios depends on the various factors/delays that are modelled.
If the start order, entry, and leave delay are considered, there are $n! * n^2 * n^2$ scenarios.
This could be expanded even more if, for example, trains are longer than one section, such that they leave the first section only after entering the third section and so on.