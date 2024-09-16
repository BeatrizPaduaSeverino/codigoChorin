import matplotlib.pyplot as plt
import numpy as np
import matplotlib.ticker
from matplotlib.colors import TwoSlopeNorm
from numpy import meshgrid
from scipy.interpolate import RectBivariateSpline
from spf import *
from variables import * #import variables to characteristic

rad = 1.0


x = np.loadtxt('data/x.dat', skiprows=0, unpack=True)
y = np.loadtxt('data/y.dat', skiprows=0, unpack=True)

Z   = np.loadtxt('data/Z.dat', skiprows=0, unpack=True)

resP   = np.loadtxt('resP.dat', skiprows=0, unpack=True)
resU   = np.loadtxt('resU.dat', skiprows=0, unpack=True)
resV   = np.loadtxt('resV.dat', skiprows=0, unpack=True)
resZ   = np.loadtxt('resZ.dat', skiprows=0, unpack=True)
resH   = np.loadtxt('resH.dat', skiprows=0, unpack=True)


X,Y = np.meshgrid(x,y)


plt.figure(figsize=(10, 6))

plt.subplot(231)
CS = plt.contourf(x,y,resP,50,cmap='seismic',norm=TwoSlopeNorm(0))
plt.contour(x,y,Z,colors=('k',),linestyles=('--',),linewidths=(1,), levels=[1])
cbar = plt.colorbar(CS)
cbar.set_label(r'$p$', x=-0.12, labelpad = 5)
plt.axis('scaled')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(232)
CS = plt.contourf(x,y,resZ,50,cmap='seismic',norm=TwoSlopeNorm(0))
plt.contour(x,y,Z,colors=('k',),linestyles=('--',),linewidths=(2,), levels=[1])
cbar = plt.colorbar(CS)
cbar.set_label(r'Z', x=-0.12, labelpad = 5)
plt.axis('scaled')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(233)
CS = plt.contourf(x,y,resH,50,cmap='seismic',norm=TwoSlopeNorm(0))
plt.contour(x,y,Z,colors=('k',),linestyles=('--',),linewidths=(2,), levels=[1])
cbar = plt.colorbar(CS)
cbar.set_label(r'H', x=-0.12, labelpad = 5)
plt.axis('scaled')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(235)
CS = plt.contourf(x,y,resU,50,cmap='seismic',norm=TwoSlopeNorm(0))
plt.contour(x,y,Z,colors=('k',),linestyles=('--',),linewidths=(2,), levels=[1])
cbar = plt.colorbar(CS)
cbar.set_label(r'u', x=-0.12, labelpad = 5)
plt.axis('scaled')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(236)
CS = plt.contourf(x,y,resV,50,cmap='seismic',norm=TwoSlopeNorm(0))
plt.contour(x,y,Z,colors=('k',),linestyles=('--',),linewidths=(2,), levels=[1])
cbar = plt.colorbar(CS)
cbar.set_label(r'v', x=-0.12, labelpad = 5)
plt.axis('scaled')
plt.xlabel('x')
plt.ylabel('y')

SPF()
F = plt.gcf()
F.set_size_inches(12.95,5.95)
plt.tight_layout(pad=0.35)
#plt.savefig('output/subplot.png', bbox_inches='tight',dpi=200)

plt.show()
