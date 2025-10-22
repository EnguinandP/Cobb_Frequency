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

def randrange(n, vmin, vmax):
    return (vmax - vmin)*np.random.rand(n) + vmin


# with open("bin/results.result", "r") as csv_file:
#     csv_reader = csv.DictReader(csv_file)

#     for line in csv_reader:
#         data.append(line)
#         length += 1

# for row in data:
#     colors.append(int(row['score']))
#     x.append(int(row['# iteration']))
#     y.append(float(row['# restarts']))
    

fig, ax = plt.subplots()
im = ax.imshow(harvest)

# plt.figure(figsize=(10, 6))

# color guide on the side
plt.colorbar(im, label='score')

plt.xlabel('# iterations')
plt.ylabel('# restarts')
plt.title('iterations and restarts on score')

plt.show()
