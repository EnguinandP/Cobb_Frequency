import matplotlib.pyplot as plt
import numpy as np
import csv
import argparse

# parser = argparse.ArgumentParser(description="args for scatterplot.")
# parser.add_argument("parameters", help="Specify table [1|2].")

# args = parser.parse_args()
# parameters = args.parameters

# Sample data
x = []
y = []
colors = []

sol_x = []
sol_y = []

data = []
length = 0

harvest = np.array([[0.8, 2.4, 2.5, 3.9, 0.0, 4.0, 0.0],
                    [2.4, 0.0, 4.0, 1.0, 2.7, 0.0, 0.0],
                    [1.1, 2.4, 0.8, 4.3, 1.9, 4.4, 0.0],
                    [0.6, 0.0, 0.3, 0.0, 3.1, 0.0, 0.0],
                    [0.7, 1.7, 0.6, 2.6, 2.2, 6.2, 0.0],
                    [1.3, 1.2, 0.0, 0.0, 0.0, 3.2, 5.1],
                    [0.1, 2.0, 0.0, 1.4, 0.0, 1.9, 6.3]])


iterations = [
    10000,
    20000,
    30000,
    40000,
    50000,
]

restarts = [
    1,
    5,
    10,
    15,
    20,
    25,
    30,
    35,
    40,
    45,
    50,   
]

files = [
    "rq3_p2_sized_list",
    "rq3_ur_depth_tree"
]

for f in files:
    path = f"bin/tables/{f}.csv"

    with open(path, "r") as csv_file:
        csv_reader = csv.DictReader(csv_file)

        for line in csv_reader:
            data.append([int(line['iterations']), int(line['restarts']), float(line['score']) ])
            length += 1

    data_sorted = sorted(data, key=lambda x: (x[0], x[1]))

    for d in data_sorted:
        print(d)

    iter_intervals = [line[0] for line in data_sorted]
    n_iter = len(set(iter_intervals))

    r_intervals = [line[1] for line in data_sorted]
    n_res = len(set(r_intervals))

    print(n_iter)
    print(n_res)

    data_array = [[0 for i in range(n_res)] for j in range(n_iter)]

    n = 0
    for i in range(n_res):
        for j in range(n_iter):
            data_array[j][i] = data_sorted[n][2]
            n += 1

    for row in data_array:
        print(row)

    fig, ax = plt.subplots()
    im = ax.imshow(data_array)

    ax.set_xticks(range(len(restarts)), labels=restarts,
                rotation=45, ha="right", rotation_mode="anchor")
    ax.set_yticks(range(len(iterations)), labels=iterations)

    for i in range(n_res):
        for j in range(n_iter):
            text = ax.text(i, j, data_array[j][i],
                        ha="center", va="center", color="w", fontsize=5)

    # plt.figure(figsize=(10, 6))

    # color guide on the side
    plt.colorbar(im, label='score')

    plt.xlabel('# restarts')
    plt.ylabel('# iterations')
    plt.title('iterations and restarts on score')
    fig.tight_layout()

    fig_path = "bin/graphs/" + f + ".png"
    plt.savefig(fig_path) 
    # plt.show()
    data = []

