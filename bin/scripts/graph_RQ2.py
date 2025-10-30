import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import csv

freq_path = f"bin/tables/rq2/frequency.csv"
para_path = f"bin/tables/rq2/parametrized.csv"
unroll_path = f"bin/tables/rq2/unrolled.csv"

start = []
freq = []
para = []
unroll = []

names = []
explicit_names = []

names_simple = [    
    "bst", 
    "tree", 
    "elist", 
    "rbt", 
    "list", 
    "", 
    ]

tests = [
    ("sized_list","uni_len",10.0),
    ("depth_bst","stick",0.8),
    ("depth_bst","h_bal",2.0),
    ("depth_tree","h_bal",1.5),
    ("depth_tree","stick",0.8),
    ("depth_tree","uni_height",5.0),
    ("even_list","uni_len",11.0),
    ("rb_tree","uni_height",5.0),
]

name_dict = {
    "depth_bst":"bst", 
    "depth_tree":"tree", 
    "even_list":"elist", 
    "rb_tree":"rbt", 
    "sized_list":"list", 
    "LoadedDice":"LDt",
    "Dragen":"Dt", 
    }

gen_group = []
gen_alpha = []

with open(freq_path, "r") as csv_file:
    csv_reader = csv.DictReader(csv_file)

    t = "sized_list"
    ti = 0
    gen_group.append(1)

    for i, line in enumerate(csv_reader):
        if not (t == line['data type']):
            gen_group.append(i)
            ti = i
            t = line['data type']
        
        if line['feature vector'] == "h_bal":
            fv_name = "balance"
        else:
            fv_name = line['feature vector']

        names.append(f"{name_dict[line['data type']]} {i - ti}")
        explicit_names.append(f"{name_dict[line['data type']]} x {line['feature vector']}")
        start_score = float(line['start score']) 
        score = float(line['end score']) 

        # print(start_score, score)
        if score < 0:
            freq.append(0)
        else:
            freq.append(score)

        if start_score < 0:
            start.append(0)
        else:
            start.append(start_score)

with open(para_path, "r") as csv_file:
    csv_reader = csv.DictReader(csv_file)

    for line in csv_reader:
        t = (line['data type'], line['feature vector'], float(line['target']))
        # if line['feature vector'] == 'stick':
        #     print(t)
        # print(t)
        # print("now")
        if t in tests:
            print("got")

            print(t)

            score = float(line['end score']) 

            # print(start_score, end_score)
            if score < 0:
                para.append(0)
            else:
                para.append(score)

with open(unroll_path, "r") as csv_file:
    csv_reader = csv.DictReader(csv_file)

    for line in csv_reader:
        t = (line['data type'], line['feature vector'], float(line['target']))
        if t in tests:

            score = float(line['end score']) 

            # print(start_score, end_score)
            if score < 0:
                unroll.append(0)
            else:
                unroll.append(score)


gen_group.append(len(start) - 1)
# print(names)
# print(len(names))
print(gen_alpha)
print(gen_group)

for i in range(len(gen_group) - 1):
    gen_alpha.append((gen_group[i], gen_group[i + 1], (i + 3) * (1.0 / 8)))


print(len(start), len(freq), len(unroll), len(para))

barWidth = .2

bar1 = np.arange(len(start))
bar2 = [x + barWidth for x in bar1]
bar3 = [x + barWidth for x in bar2]
bar4 = [x + barWidth for x in bar3]

fig, ax = plt.subplots(figsize =(8.5, 2))

# plt.xticks([(r + barWidth) for r in gen_group], names_simple)
plt.xticks([(r) for r in range(len(names))], explicit_names, rotation = 45, fontsize=5)

# b = True
# for s, e, a in gen_alpha:
#     color1 = 'lightcoral'
#     color2 = 'midnightblue'
#     if b:
#         a = 1
#         color1 = 'lightcoral'
#         color2 = 'midnightblue'
#     else:
#         a = .5
#         color1 = 'mistyrose'
#         color2 = 'royalblue'
#         # color1 = adjust_saturation1('lightcoral', .06)
#         # color2 = adjust_saturation2('darkslateblue', .0)
#     b = not(b)
#     ax.bar(bar1[s:e], start[s:e], color=color1, width = barWidth, label ='start', edgecolor='lightcoral') 
#     ax.bar(bar2[s:e], freq[s:e], color=color2, width = barWidth, label ='end', edgecolor='midnightblue') 
#     ax.bar(bar3[s:e], para[s:e], color=color2, width = barWidth, label ='end', edgecolor='midnightblue') 
#     # ax.bar(bar4[s:e], unroll[s:e], color=color2, width = barWidth, label ='end', edgecolor='midnightblue') 
#     # print(color)

color1 = 'lightcoral'
color2 = 'midnightblue'
ax.bar(bar1, start, color=color1, width = barWidth, label ='start', edgecolor='lightcoral') 
ax.bar(bar2, freq, color=color2, width = barWidth, label ='no repair', edgecolor='midnightblue') 
ax.bar(bar3, para, color='cornflowerblue', width = barWidth, label ='parametrized', edgecolor='midnightblue') 
ax.bar(bar4, unroll, color='lightsteelblue', width = barWidth, label ='unrolled', edgecolor='midnightblue') 


plt.xlabel('generator + obj_fun')
plt.ylabel('score')

ax.set_yscale('log', base=10)
plt.ylim(bottom=1e-1)


plt.tight_layout()
plt.legend()
fig_path = "bin/graphs/rq2.pdf"
plt.savefig(fig_path, format="pdf") 

plt.show() 
