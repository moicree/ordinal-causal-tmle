#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os

# ==================================================
# Radar plot visualization for simulation study
# ==================================================

# =========================
# 1. Save path
# =========================
save_path = "figures"
os.makedirs(save_path, exist_ok=True)

# =========================
# 2. Data
# =========================
data = [

# S1
['S1','adj',0.1491,0.0274,0.1516],
['S1','gcomp_param',0.1470,0.0274,0.1496],
['S1','gcomp_SL',0.0484,0.0247,0.0544],
['S1','ipw_param',0.0823,0.0486,0.0955],
['S1','ipw_SL',0.1107,0.0539,0.1231],
['S1','tmle_bin_param',0.0105,0.0273,0.0293],
['S1','tmle_bin_SL',0.0231,0.0328,0.0401],
['S1','tmle_cont_param',0.0016,0.0324,0.0324],
['S1','tmle_cont_SL',0.0298,0.0330,0.0444],

# S2
['S2','adj',0.1491,0.0274,0.1516],
['S2','gcomp_param',0.1604,0.0215,0.1619],
['S2','gcomp_SL',0.1094,0.0188,0.1110],
['S2','ipw_param',0.0810,0.0468,0.0936],
['S2','ipw_SL',0.1083,0.0542,0.1211],
['S2','tmle_bin_param',0.0047,0.0335,0.0338],
['S2','tmle_bin_SL',0.0065,0.0372,0.0378],
['S2','tmle_cont_param',0.0062,0.0338,0.0343],
['S2','tmle_cont_SL',0.0341,0.0450,0.0564],

# S3
['S3','adj',0.1492,0.0269,0.1516],
['S3','gcomp_param',0.1472,0.0268,0.1496],
['S3','gcomp_SL',0.0488,0.0236,0.0542],
['S3','ipw_param',0.1794,0.0263,0.1813],
['S3','ipw_SL',0.1722,0.0324,0.1752],
['S3','tmle_bin_param',0.0141,0.0206,0.0250],
['S3','tmle_bin_SL',0.0150,0.0206,0.0255],
['S3','tmle_cont_param',0.1343,0.0252,0.1366],
['S3','tmle_cont_SL',0.0671,0.0238,0.0712],

# S4
['S4','adj',0.1492,0.0269,0.1516],
['S4','gcomp_param',0.1606,0.0215,0.1620],
['S4','gcomp_SL',0.1102,0.0187,0.1118],
['S4','ipw_param',0.1793,0.0252,0.1811],
['S4','ipw_SL',0.1722,0.0309,0.1750],
['S4','tmle_bin_param',0.1411,0.0203,0.1425],
['S4','tmle_bin_SL',0.1402,0.0202,0.1416],
['S4','tmle_cont_param',0.1443,0.0213,0.1459],
['S4','tmle_cont_SL',0.1428,0.0212,0.1443],

]

df = pd.DataFrame(
    data,
    columns=['Scenario','Method','Bias','SD','RMSE']
)

# =========================
# 3. Plot style
# =========================
plt.rcParams.update({
    'font.family': 'serif',
    'font.serif': ['Times New Roman'],
    'font.size': 11
})

# =========================
# 4. Method labels
# =========================
method_labels = {

    'adj': 'Adjusted estimator',

    'gcomp_param': 'G-computation',
    'gcomp_SL': 'G-computation',

    'ipw_param': 'IPW',
    'ipw_SL': 'IPW',

    'tmle_bin_param': 'Threshold-based TMLE',
    'tmle_bin_SL': 'Threshold-based TMLE',

    'tmle_cont_param': 'Continuous-score TMLE',
    'tmle_cont_SL': 'Continuous-score TMLE'
}

# =========================
# 5. Colors and styles
# =========================
color_map = {
    'adj': '#1f77b4',
    'gcomp': '#d62728',
    'ipw': '#ff7f0e',
    'tmle_bin': '#bcbd22',
    'tmle_cont': '#2ca02c'
}

style_map = {
    'adj': ('-', 'o'),
    'gcomp': ('--', 's'),
    'ipw': ('-.', '^'),
    'tmle_bin': (':', 'D'),
    'tmle_cont': ('-', 'x')
}

# =========================
# 6. Radar plot function
# =========================
def plot_radar(df, methods, save_name):

    df_plot = df[df['Method'].isin(methods)].copy()

    # Normalize within each scenario
    df_plot[['Bias','SD','RMSE']] = (
        df_plot.groupby('Scenario')[['Bias','SD','RMSE']]
        .transform(
            lambda x: (x - x.min()) / (x.max() - x.min() + 1e-8)
        )
    )

    metrics = ['Bias','SD','RMSE']

    angles = np.linspace(
        0,
        2*np.pi,
        len(metrics),
        endpoint=False
    ).tolist()

    angles += angles[:1]

    fig, axs = plt.subplots(
        2, 2,
        figsize=(10,8),
        subplot_kw=dict(polar=True)
    )

    axs = axs.flatten()

    scenarios = ['S1','S2','S3','S4']

    for i, scenario in enumerate(scenarios):

        ax = axs[i]

        ax.set_theta_offset(np.pi/2)
        ax.set_theta_direction(-1)

        ax.set_title(
            scenario,
            fontsize=12,
            weight='bold'
        )

        ax.set_xticks(angles[:-1])
        ax.set_xticklabels(metrics)

        ax.set_ylim(0,1)

        ax.grid(
            True,
            linestyle='--',
            linewidth=0.5,
            alpha=0.6
        )

        scenario_data = df_plot[
            df_plot['Scenario'] == scenario
        ]

        for method in methods:

            values = scenario_data[
                scenario_data['Method'] == method
            ][metrics].values.flatten().tolist()

            values += values[:1]

            base = (
                method
                .replace('_param','')
                .replace('_SL','')
            )

            linestyle, marker = style_map[base]
            color = color_map[base]

            ax.plot(
                angles,
                values,
                label=method_labels[method],
                color=color,
                linestyle=linestyle,
                linewidth=2,
                marker=marker,
                markersize=4
            )

    fig.legend(
        [method_labels[m] for m in methods],
        loc='lower center',
        ncol=2,
        frameon=False
    )

    plt.tight_layout()

    plt.subplots_adjust(bottom=0.18)

    # Save PDF
    plt.savefig(
        f'{save_path}/{save_name}.pdf',
        format='pdf',
        dpi=600,
        bbox_inches='tight',
        facecolor='white'
    )

    # Save PNG
    plt.savefig(
        f'{save_path}/{save_name}.png',
        format='png',
        dpi=600,
        bbox_inches='tight',
        facecolor='white'
    )

    plt.show()

# =========================
# 7. Generate plots
# =========================

# Parametric methods
plot_radar(
    df,
    methods=[
        'adj',
        'gcomp_param',
        'ipw_param',
        'tmle_bin_param',
        'tmle_cont_param'
    ],
    save_name='radar_parametric'
)

# Super Learner methods
plot_radar(
    df,
    methods=[
        'adj',
        'gcomp_SL',
        'ipw_SL',
        'tmle_bin_SL',
        'tmle_cont_SL'
    ],
    save_name='radar_superlearner'
)


# In[ ]:




