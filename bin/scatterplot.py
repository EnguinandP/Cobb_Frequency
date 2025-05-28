import matplotlib.pyplot as plt
import numpy as np
import csv
import argparse

parser = argparse.ArgumentParser(description="args for scatterplot.")
parser.add_argument("parameters", help="Specify table [1|2].")

args = parser.parse_args()
parameters = args.parameters

# Sample data
x = []
y = []
z = []
colors = []

sol_x = []
sol_y = []
sol_z = []

data = []
length = 0

def randrange(n, vmin, vmax):
    return (vmax - vmin)*np.random.rand(n) + vmin

if parameters == '1':

    with open("bin/results.result", "r") as csv_file:
        csv_reader = csv.DictReader(csv_file)

        for line in csv_reader:
            data.append(line)
            length += 1

    for row in data:
        try:
            colors.append(int(row['iteration']))
            x.append(int(row['cand weights']))
            y.append(float(row['score']))
        except ValueError:
            sol_x = int(row['cand weights'])
            sol_y = float(row['score'])

    plt.figure(figsize=(10, 6))

    sc = plt.scatter(x, y, s=10, c=colors, cmap='viridis', alpha=0.1)
    # mark solution
    plt.scatter(sol_x, sol_y, s=20, c='red')
    plt.text(sol_x, sol_y, f'solution ({sol_x}, {sol_y})', fontsize=9)

    # mark start
    plt.scatter(x[0], y[0], s=20, c='purple')
    plt.text(x[0]+ 0.1, y[0], 'start', fontsize=9)

    # color guide on the side
    plt.colorbar(sc, label='iteration #')

    plt.xlabel('weight')
    plt.ylabel('score')
    plt.title('score vs weight over iterations')

    plt.show()

else:
    with open("bin/results.result", "r") as csv_file:
        csv_reader = csv.DictReader(csv_file)

        for line in csv_reader:
            data.append(line)
            length += 1

    for row in data:
        try:
            colors.append(int(row['iteration']))
            x.append(int(row['cand weights x']))
            y.append(int(row['cand weights y']))
            z.append(float(row['score']))
        except ValueError:
            sol_x = int(row['cand weights x'])
            sol_x = int(row['cand weights y'])
            sol_y = float(row['score'])

    plt.figure(figsize=(10, 6))


    fig = plt.figure()
    ax = fig.add_subplot(projection='3d')

    # For each set of style and range settings, plot n random points in the box
    # defined by x in [23, 32], y in [0, 100], z in [zlow, zhigh].

    ax.scatter(x, y, z, c=colors, cmap='viridis', alpha=0.1)

    # mark solution
    # plt.scatter(sol_x, sol_y, sol_z)
    # plt.text(sol_x + 0.1, sol_y, sol_z, f'solution ({sol_x}, {sol_y})', fontsize=9)

    # # mark start
    # plt.scatter(x[0], y[0], z[0], s=20, c='purple')
    # plt.text(x[0]+ 0.1, y[0], z[0], 'start', fontsize=9)

    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('score')

    plt.show()