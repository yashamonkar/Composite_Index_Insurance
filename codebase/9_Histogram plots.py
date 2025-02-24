# -*- coding: utf-8 -*-
"""
Created on Mon Jan 15 18:28:52 2024

@author: Yash Amonkar

Code to create the histogram of payouts for the portfolio of indices & composite
index insurance with an objective to show the overpayments
"""

#Import Libraries
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import random


#Set the working directory. 
directory_path = 'C:/Users/amonkar/Documents/GitHub/Composite_Index_Insurance'
os.chdir(directory_path)


#-----------------------------------------------------------------------------#
#Input the revenue and pauout slides
revenues = pd.read_csv('sims/All_revenues_no_tax.csv')
payouts = pd.read_csv('sims/All_payouts_no_tax.csv')


#%%
#Histograms of Payouts

d = .015

fig, axs = plt.subplots(2, 2, sharex=True, figsize = (12,8))
fig.text(0.5, -0.02, 'Payouts ($ billion)', ha='center', fontsize=20)
fig.text(-0.01, 0.4, 'Frequency (Years)',rotation=90,fontsize=20)


n, bins, _ = axs[0,0].hist(payouts['Unmanaged'], bins=20, color='tab:blue', label='Unmanaged Losses')
bin_width = bins[1] - bins[0]
width = bin_width

axs[0,0].hist(payouts['Composite_index'], alpha=1.0, bins=np.arange(min(payouts['Composite_index']), max(payouts['Composite_index']) + width, width),color='darkorange',label='Composite Payout',linestyle='dashed', linewidth=2, edgecolor='k', hatch='///',histtype='step')
axs[0,0].tick_params(labelsize=16)
axs[0,0].set_ylim(300, 500)
axs[0,0].spines['bottom'].set_visible(False)
axs[0,0].xaxis.tick_top()
axs[0,0].tick_params(labeltop=False)  # don't put tick labels at the top
kwargs = dict(transform=axs[0,0].transAxes, color='k', clip_on=False)
axs[0,0].plot((-d, +d), (-d, +d), **kwargs)  # top-left diagonal
axs[0,0].plot((1 - d, 1 + d), (-d, +d), **kwargs)  # top-right diagonal
# axs[0,0].set_ylim(800,825)


axs[1,0].hist(payouts['Unmanaged'], bins=20, color='tab:blue')
axs[1,0].hist(payouts['Composite_index'], alpha=1.0, bins=np.arange(min(payouts['Composite_index']), max(payouts['Composite_index']) + width, width),color='darkorange',linestyle='dashed', linewidth=2, edgecolor='k', hatch='///',histtype='step')
# axs[1,0].set_ylim(0, 625)
axs[1,0].set_ylim(0, 100)
axs[1,0].tick_params(labelsize=16)

axs[1,0].spines['top'].set_visible(False)

axs[1,0].xaxis.tick_bottom()
  # how big to make the diagonal lines in axes coordinates
# arguments to pass to plot, just so we don't keep repeating them
kwargs.update(transform=axs[1,0].transAxes)  # switch to the bottom axes
axs[1,0].plot((-d, +d), (1 - d, 1 + d), **kwargs)  # bottom-left diagonal
axs[1,0].plot((1 - d, 1 + d), (1 - d, 1 + d), **kwargs)  # bottom-right diagonal
# ax.set_ylim(.78, 1.)  # outliers only
# ax2.set_ylim(0, .22)  # most of the data

n, bins, _ = axs[0, 1].hist(payouts['Unmanaged'], bins=20)
bin_width = bins[1] - bins[0]
width = bin_width
axs[0,1].tick_params(labelsize=16)
axs[0,1].hist(payouts['Portfolio'], alpha=1.0, bins=np.arange(min(payouts['Portfolio']), max(payouts['Portfolio']) + width, width),color='brown',label='Portfolio Payout', linestyle='dashed', linewidth=2, edgecolor='k',hatch='...',histtype='step')
axs[1,1].hist(payouts['Unmanaged'], bins=20)
axs[1,1].hist(payouts['Portfolio'], alpha=1.0,bins=np.arange(min(payouts['Portfolio']),max(payouts['Portfolio']) + width, width), color='brown', linestyle='dashed', linewidth=2, edgecolor='k',hatch='...',histtype='step')
# axs[1,1].set_ylim(0, 625)
axs[1, 1].set_ylim(0, 100)
axs[1,1].tick_params(labelsize=16)
# axs[0,1].set_ylim(800,825)
axs[0,1].set_ylim(300, 500)
# hide the spines between ax and ax2
axs[0,1].spines['bottom'].set_visible(False)
axs[1,1].spines['top'].set_visible(False)
axs[0,1].xaxis.tick_top()
axs[0,1].tick_params(labeltop=False)  # don't put tick labels at the top
axs[1,1].xaxis.tick_bottom()
d = .015  # how big to make the diagonal lines in axes coordinates
# arguments to pass to plot, just so we don't keep repeating them
kwargs = dict(transform=axs[0,1].transAxes, color='k', clip_on=False)
axs[0,1].plot((-d, +d), (-d, +d), **kwargs)  # top-left diagonal
axs[0,1].plot((1 - d, 1 + d), (-d, +d), **kwargs)  # top-right diagonal

kwargs.update(transform=axs[1,1].transAxes)  # switch to the bottom axes
axs[1,1].plot((-d, +d), (1 - d, 1 + d), **kwargs)  # bottom-left diagonal
axs[1,1].plot((1 - d, 1 + d), (1 - d, 1 + d), **kwargs)  # bottom-right diagonal

lines_labels = [ax.get_legend_handles_labels() for ax in fig.axes]
lines, labels = [sum(lol, []) for lol in zip(*lines_labels)]
axs[0,1].legend(lines, labels,prop={'size': 18},loc='upper right')

#Add Labels
axs[0,0].text(0.05, 0.9, 'A', transform=axs[0,0].transAxes, size=24, weight='bold')
axs[0,1].text(0.05, 0.9, 'B', transform=axs[0,1].transAxes, size=24, weight='bold')


axs[1,1].annotate('Overpayments', 
                  xy=(0.1, 20), xycoords='data',
                  xytext=(0.2, 100), textcoords='data',
                  arrowprops=dict(arrowstyle="->", linewidth=2,
                                  connectionstyle="arc3"),
                  fontsize=18, 
                  bbox=dict(boxstyle="round,pad=0.3", edgecolor='black', facecolor='white'))

# Ensure that the added annotation does not cut off
#plt.tight_layout()

plt.tight_layout()
plt.savefig('figures/Historgram_Payouts.png' , bbox_inches='tight',dpi=250)
plt.show()


