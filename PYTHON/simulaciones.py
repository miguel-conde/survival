import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import expon, geom

np.random.seed(2023)

N = 100
p = 0.1

data_surv = pd.DataFrame({'tt': [0], 'd': [np.nan], 'exposed': [N]})
data_geom = geom.rvs(p=p, size=N)
data_geom = pd.DataFrame({'tt': data_geom}).sort_values('tt')
data_summarised = data_geom.groupby('tt').size().reset_index(name='d')
data_summarised['exposed'] = N - data_summarised['d'].cumsum()
data_surv = pd.concat([data_surv, data_summarised], ignore_index=True)
data_surv['S'] = data_surv['exposed'] / N

data_th = pd.DataFrame({'tt': np.arange(1, N+1), 
                        'S_th': 1 - geom.cdf(np.arange(N)+1, p=p)})
data_th_exp = pd.DataFrame({'tt': np.arange(1, N+1),
                            'S_th_exp': 1 - expon.cdf(np.arange(N)+1, scale=1/p)})

data_surv = pd.merge(pd.merge(data_surv, data_th, how = "outer", on = 'tt'), 
                              data_th_exp, how = "outer", on = 'tt') \
                              .sort_values('tt') \
                              .iloc[:50] \
                              .dropna(subset = ['exposed'])

plt.step(data_surv['tt'], data_surv['S'], '-', label='Survival');
plt.step(data_surv['tt'], data_surv['S_th'], '--', label='Theoretical (Geom)');
plt.step(data_surv['tt'], data_surv['S_th_exp'], '--', label='Theoretical (Exp)');
plt.xlabel('Time (tt)');
plt.ylabel('Survival Probability (S)');
plt.legend();
plt.show()

