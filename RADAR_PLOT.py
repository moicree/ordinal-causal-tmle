#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# 📌 1. 데이터 입력
data = {
    'Scenario': ['S1']*9 + ['S2']*9 + ['S3']*9 + ['S4']*9,
    'Method': ['adj','gcomp_param','gcomp_SL','ipw_param','ipw_SL','tmle_bin_param','tmle_bin_SL','tmle_cont_param','tmle_cont_SL']*4,
    'Bias': [
        0.0698,0.0698,-0.0133,0.0565,0.0817,-0.0261,-0.0316,-0.0271,-0.005,
        0.0721,0.0582,0.0243,0.062,0.0855,-0.0157,-0.0178,-0.0159,-0.0048,
        0.0709,0.0709,-0.0114,0.0506,0.0306,0.0339,0.0236,0.0585,0.0207,
        0.069,0.0571,0.0236,0.0495,0.0292,0.0424,0.047,0.0426,0.0423
    ],
    'SD': [
        0.0382,0.0382,0.0297,0.0707,0.0714,0.044,0.0426,0.0476,0.0478,
        0.0379,0.0306,0.0328,0.0755,0.0764,0.0481,0.0532,0.0487,0.0509,
        0.0381,0.0381,0.0302,0.0331,0.0747,0.0387,0.0366,0.0389,0.033,
        0.0397,0.0312,0.0354,0.0334,0.0816,0.0314,0.058,0.0315,0.0317
    ],
    'RMSE': [
        0.0795,0.0795,0.0325,0.0905,0.108,0.0511,0.053,0.0547,0.048,
        0.0814,0.0657,0.0408,0.0976,0.115,0.0506,0.056,0.0512,0.0511,
        0.0805,0.0805,0.0323,0.0605,0.0807,0.0514,0.0435,0.0702,0.0389,
        0.0796,0.0651,0.0425,0.0597,0.0866,0.0527,0.0746,0.053,0.0529
    ]
}

df = pd.DataFrame(data)

# 📌 2. adj + param만 필터
df = df[df['Method'].isin([
    'adj','gcomp_param','ipw_param','tmle_bin_param','tmle_cont_param'
])]

# 📌 3. Bias 절대값 (논문용 추천)
df['Bias'] = df['Bias'].abs()

# 📌 4. 정규화 (🔥 핵심 수정)
df_norm = df.copy()
df_norm[['Bias','SD','RMSE']] = (
    df.groupby('Scenario')[['Bias','SD','RMSE']]
    .transform(lambda x: (x - x.min()) / (x.max() - x.min()))
)

# 📌 5. Radar 설정
metrics = ['Bias','SD','RMSE']
angles = np.linspace(0, 2*np.pi, len(metrics), endpoint=False).tolist()
angles += angles[:1]

fig, axs = plt.subplots(2,2, figsize=(12,10), subplot_kw=dict(polar=True))
axs = axs.flatten()

scenarios = df_norm['Scenario'].unique()
methods = df_norm['Method'].unique()

for i, scenario in enumerate(scenarios):
    ax = axs[i]
    ax.set_theta_offset(np.pi/2)
    ax.set_theta_direction(-1)
    ax.set_title(f'{scenario}', size=14)

    ax.set_xticks(angles[:-1])
    ax.set_xticklabels(metrics)
    ax.set_ylim(0,1)

    scenario_data = df_norm[df_norm['Scenario']==scenario]

    for method in methods:
        values = scenario_data[scenario_data['Method']==method][metrics].values.flatten().tolist()
        values += values[:1]

        ax.plot(angles, values, label=method)
        ax.fill(angles, values, alpha=0.1)

# 📌 6. 범례
plt.legend(loc='upper center', bbox_to_anchor=(0.5,-0.1), ncol=3)

plt.suptitle('Radar Chart (adj + param methods)', size=16)
plt.tight_layout()
plt.show()

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# 📌 데이터
data = {
    'Scenario': ['S1']*9 + ['S2']*9 + ['S3']*9 + ['S4']*9,
    'Method': ['adj','gcomp_param','gcomp_SL','ipw_param','ipw_SL','tmle_bin_param','tmle_bin_SL','tmle_cont_param','tmle_cont_SL']*4,
    'Bias': [
        0.0698,0.0698,-0.0133,0.0565,0.0817,-0.0261,-0.0316,-0.0271,-0.005,
        0.0721,0.0582,0.0243,0.062,0.0855,-0.0157,-0.0178,-0.0159,-0.0048,
        0.0709,0.0709,-0.0114,0.0506,0.0306,0.0339,0.0236,0.0585,0.0207,
        0.069,0.0571,0.0236,0.0495,0.0292,0.0424,0.047,0.0426,0.0423
    ],
    'SD': [
        0.0382,0.0382,0.0297,0.0707,0.0714,0.044,0.0426,0.0476,0.0478,
        0.0379,0.0306,0.0328,0.0755,0.0764,0.0481,0.0532,0.0487,0.0509,
        0.0381,0.0381,0.0302,0.0331,0.0747,0.0387,0.0366,0.0389,0.033,
        0.0397,0.0312,0.0354,0.0334,0.0816,0.0314,0.058,0.0315,0.0317
    ],
    'RMSE': [
        0.0795,0.0795,0.0325,0.0905,0.108,0.0511,0.053,0.0547,0.048,
        0.0814,0.0657,0.0408,0.0976,0.115,0.0506,0.056,0.0512,0.0511,
        0.0805,0.0805,0.0323,0.0605,0.0807,0.0514,0.0435,0.0702,0.0389,
        0.0796,0.0651,0.0425,0.0597,0.0866,0.0527,0.0746,0.053,0.0529
    ]
}

df = pd.DataFrame(data)

# 📌 adj + SL만 선택
df = df[df['Method'].isin([
    'adj','gcomp_SL','ipw_SL','tmle_bin_SL','tmle_cont_SL'
])]

# 📌 Bias 절대값
df['Bias'] = df['Bias'].abs()

# 📌 정규화 (🔥 중요)
df_norm = df.copy()
df_norm[['Bias','SD','RMSE']] = (
    df.groupby('Scenario')[['Bias','SD','RMSE']]
    .transform(lambda x: (x - x.min()) / (x.max() - x.min()))
)

# 📌 Radar 설정
metrics = ['Bias','SD','RMSE']
angles = np.linspace(0, 2*np.pi, len(metrics), endpoint=False).tolist()
angles += angles[:1]

fig, axs = plt.subplots(2,2, figsize=(12,10), subplot_kw=dict(polar=True))
axs = axs.flatten()

scenarios = df_norm['Scenario'].unique()
methods = df_norm['Method'].unique()

for i, scenario in enumerate(scenarios):
    ax = axs[i]
    ax.set_theta_offset(np.pi/2)
    ax.set_theta_direction(-1)
    ax.set_title(f'{scenario}', size=14)

    ax.set_xticks(angles[:-1])
    ax.set_xticklabels(metrics)
    ax.set_ylim(0,1)

    scenario_data = df_norm[df_norm['Scenario']==scenario]

    for method in methods:
        values = scenario_data[scenario_data['Method']==method][metrics].values.flatten().tolist()
        values += values[:1]

        ax.plot(angles, values, label=method)
        ax.fill(angles, values, alpha=0.1)

# 📌 범례
plt.legend(loc='upper center', bbox_to_anchor=(0.5,-0.1), ncol=3)

plt.suptitle('Radar Chart (Adj vs Super Learner Methods)', size=16)
plt.tight_layout()
plt.show()


# In[ ]:




