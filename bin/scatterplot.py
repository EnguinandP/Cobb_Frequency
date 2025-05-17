import matplotlib.pyplot as plt
import numpy as np
import csv

# Sample data
x = []
y = []
colors = []

sol_x = []
sol_y = []

data = []
length = 0

with open("bin/results.result", "r") as csv_file:
    csv_reader = csv.DictReader(csv_file)

    for line in csv_reader:
        # print(row)
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
plt.scatter(sol_x, sol_y, s=20, c='red')
plt.text(sol_x + 0.1, sol_y, 'solution', fontsize=9)

# color guide on the side
plt.colorbar(sc, label='iteration #')

plt.xlabel('weight')
plt.ylabel('score')
plt.title('score vs weight over iterations')

plt.show()