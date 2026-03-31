import sys
import json
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
from functools import partial

# input: ratio list
# output: function

# n = 10

# uniform tree height example
y = np.array([ 0.111111, 0.353553, 0.475963, 0.546982, 0.598410, 0.646164, 0.703333, 0.792807, 1.000000])

# uniform size example
# y = 1 / (x + 1)
# y = np.array([0.5000, 0.7000, 0.7000, 0.8500, 0.8500, 0.8500, 0.8500, 0.8500, 0.8500, 0.8500 ])
# average size example
# y = np.array([0.5000, 0.5000, 0.5000, 0.5000, 0.5000, 0.5000, 1.0000, 1.0000, 1.0000, 1.0000 ])
# nil len example
# y = np.array([0.5000, 0.5000, 0.5000, 0.5000, 0.5000, 0.5000, 0.0909, 1.0000, 1.0000, 0.8500 ])
# y = np.array([2.3, 3.1, 3.8, 4.4, 5.0])
n = len(y)
x = np.arange(1, n + 1)

print(y)
print(x)


# use linear regression
slope, intercept, r_value, p_value, std_err = stats.linregress(x, y)

def line_model(size, a):
    return 1 / ( size + a)

def reciprocal_model(size, a, b):
    return 1 / ( a * size + b)

def step_model(x, change_index, a, b):
    return np.where(x < change_index+1, a, b)

# params is a, b
# covariance is covariance matrix estimating the uncertainty of parameters
# use curve_fit
params, covariance = curve_fit(line_model, x, y)

rparams, rcovariance = curve_fit(reciprocal_model, x, y)
ra, rb = rparams

# use linear regression with linearized ratios
lin_y = 1 / y
rslope, rintercept, rr_value, rp_value, rstd_err = stats.linregress(x, lin_y)

# step model with change index
change_index = np.where(np.diff(y) != 0)[0][0] + 1
# print(change_index)
low_value = y[:change_index].mean()
high_value = y[change_index:].mean()
def found_step_model(x):
    return np.where(x < change_index+1, low_value, high_value)

a, = params


plt.scatter(x, y, label='Data points')
plt.plot(x, 1 / (x + a), color='red', label='line model fit')
plt.plot(x, 1 / (ra * x + rb), color='red', label='reciprocal model fit')
# plt.plot(x, rslope * x + rintercept, color='green', label='linearized reciprical model fit')
plt.plot(x, slope * x + intercept, color='red', label='linear fit')
plt.plot(x, found_step_model(x), color='red', label='step fit')


def sse(y, yhat):
    return np.sum((y - yhat)**2)

def aic(n, sse, k):
    return n * np.log(sse/n) + 2*k

def select_model(x, y):
    n = len(y)

    results = {}

    # Linear model
    slope, intercept, *_ = stats.linregress(x, y)
    y_linear = slope*x + intercept

    err = sse(y, y_linear)
    results["linear"] = {
        "params": (slope, intercept),
        "sse": err,
        "aic": aic(n, err, 2),
        "predict": lambda x: slope*x + intercept
    }

    # Reciprocal model
    # y = 1/(ax + b)
    lin_y = 1/y
    a, b, *_ = stats.linregress(x, lin_y)
    y_recip = 1/(a*x + b)

    err = sse(y, y_recip)
    results["reciprocal"] = {
        "params": (a, b),
        "sse": err,
        "aic": aic(n, err, 2),
        "predict": lambda x: 1/(a*x + b)
    }

    # Step model
    change_index = np.where(np.diff(y) != 0)[0][0] + 1
    low = y[:change_index].mean()
    high = y[change_index:].mean()

    y_step = np.where(x < change_index+1, low, high)

    err = sse(y, y_step)
    results["step"] = {
        "params": (change_index, low, high),
        "sse": err,
        "aic": aic(n, err, 3),
        "predict": lambda x: np.where(x < change_index+1, low, high)
    }

    # Zero step model
    zero_indices = np.where(y == 0)[0]

    if len(zero_indices) > 0:

        first_zero = zero_indices[0]
        if first_zero > 0:

            c = y[:first_zero].mean()

            y_before = y[:first_zero]
            pred_before = np.full_like(y_before, c)

            err = sse(y_before, pred_before)

            results["zero_step"] = {
                "params": (first_zero, c),
                "sse": err,
                "aic": aic(first_zero, err, 2),
                "predict": lambda x: np.where(x < first_zero+1, c, 0)
            }

    # best = min(results, key=lambda m: results[m]["aic"])
    # return best, results

    best = min(results, key=lambda m: results[m]["aic"])
    return best, results

# best_model, results = select_model(x, y)
# pred = results[best_model]["predict"]

# data = json.loads(sys.stdin.read())

# y = np.array(data)
# x = np.arange(1, len(y)+1)

# model, results_dict = select_model(x, y)
# params = results_dict[model]["params"]

# print(json.dumps({
#     "model": model,
#     "params": list(params)
# }), flush=True)

plt.scatter(x, y)
# plt.plot(x, pred(x), color='red')

plt.xlabel('Independent Variable X')
plt.ylabel('Dependent Variable Y')
plt.legend()
plt.title('Simple Linear Regression')
plt.show()

