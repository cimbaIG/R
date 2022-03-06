import numpy as np
from sympy import re

#Load R and Python results from respective txt files.
R_results = np.loadtxt('R_results.txt')
python_results = np.loadtxt('python_results.txt')

#Calculate residuals
residuals = R_results - python_results 

#Print max, min and mean residuals
print("Max residual: ", np.amax(residuals), ", Min residual: ", np.amin(residuals),
    ", Mean residual: ", np.mean(residuals))