import os
import subprocess

# Call from ex1 folder!
BIN = os.getcwd() + "/bin/main"
N_TASKS_TEST_VECTOR = [1, 2, 3, 4, 5, 6, 7, 8, 9,
                       10, 15, 20, 30, 50, 75, 100, 200, 500, 1000]


def fast_case_bm():
    max_n = 29

    for n_tasks in N_TASKS_TEST_VECTOR:
        p = subprocess.run([BIN, str(max_n), str(n_tasks)])


def long_bm():
    # n = 30 has a solution too large to compute in reasonable time, use as BM
    n = 30
    large_tw = N_TASKS_TEST_VECTOR + [2500, 5000, 7500, 10000]
    for n_tasks in large_tw:
        p = subprocess.run([BIN, str(n), str(n_tasks), "-b"])


def main():
    #fast_case_bm()
    long_bm()


if __name__ == '__main__':
    main()
