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

xm = np.loadtxt('data/xm.dat', skiprows=0, unpack=True)
ym = np.loadtxt('data/ym.dat', skiprows=0, unpack=True)

xp = np.loadtxt('data/xp.dat', skiprows=0, unpack=True)
yp = np.loadtxt('data/yp.dat', skiprows=0, unpack=True)

u = np.loadtxt('data/um.dat', skiprows=0, unpack=True)
v = np.loadtxt('data/vm.dat', skiprows=0, unpack=True)

P   = np.loadtxt('data/P.dat', skiprows=0, unpack=True)
rho = np.loadtxt('data/rho.dat', skiprows=0, unpack=True)
T   = np.loadtxt('data/T.dat', skiprows=0, unpack=True)
Z   = np.loadtxt('data/Z.dat', skiprows=0, unpack=True)
H   = np.loadtxt('data/H.dat', skiprows=0, unpack=True)


y  = y  - Hjet
ym = ym - Hjet
yp = yp - Hjet

#x_grid,y_grid= np.loadtxt('data/grid_m.dat', skiprows=0, unpack=True)


Nx_u = int(len(xm))
Ny_u = int(len(y))
Nx_v = int(len(x))
Ny_v = int(len(ym))
u = np.array(u)
v = np.array(v)

x_int = np.linspace(min(xm), max(xm), num=Nx_v-1,endpoint='True')
y_int = np.linspace(min(ym), max(ym), num=Ny_u-1,endpoint='True')


X,Y = np.meshgrid(x,ym)
X2,Y2 = np.meshgrid(x_int,y_int)
interp_spline = RectBivariateSpline(ym,x,v)
v_int = interp_spline(y_int,x_int)

X,Y = np.meshgrid(xm,y)
X2,Y2 = np.meshgrid(x_int,y_int)
interp_spline = RectBivariateSpline(y,xm,u)
u_int = interp_spline(y_int,x_int)

##INTERPOLATE SCALARS
Nx_scalar = int(len(x))
Ny_scalar = int(len(y))
P = np.array(P)
rho = np.array(rho)
T = np.array(T)
Z = np.array(Z)
H = np.array(H)

X,Y = np.meshgrid(x,y)
X2,Y2 = np.meshgrid(x_int,y_int)
interp_spline = RectBivariateSpline(y,x,rho)
rho_int = interp_spline(y_int,x_int)
interp_spline = RectBivariateSpline(y,x,T)
T_int = interp_spline(y_int,x_int)
interp_spline = RectBivariateSpline(y,x,Z)
Z_int = interp_spline(y_int,x_int)
interp_spline = RectBivariateSpline(y,x,H)
H_int = interp_spline(y_int,x_int)

Xp,Yp = np.meshgrid(xp,yp)
X2,Y2 = np.meshgrid(x_int,y_int)
interp_spline = RectBivariateSpline(yp,xp,P)
P_int = interp_spline(y_int,x_int)
##### DIMENSIONAL
#x_int = x_int*Lc
#y_int = y_int*Lc
#P_int = P_int*pc#rhoc*Vc*10e-2*Vc*10e-2
#rho_int = rho_int*rhoc
#T_int = T_int*Tc
#u_int = u_int*Vc
#v_int = v_int*Vc


plt.figure(figsize=(16, 8))
CS = plt.contourf(x,y,T,50,cmap='afmhot')
plt.contour(x,y,Z,colors=('k',),linestyles=('--',),linewidths=(1,), levels=[1])
cbar = plt.colorbar(CS)
cbar.set_label(r'$T$', x=-0.12, labelpad = 5)
plt.axis('scaled')
plt.xlabel('x')
plt.ylabel('y')


plt.figure(figsize=(10, 6))

plt.subplot(231)
CS = plt.contourf(x,y,rho,50,cmap='seismic',norm=TwoSlopeNorm(0))
plt.contour(x,y,Z,colors=('k',),linestyles=('--',),linewidths=(1,), levels=[1])
cbar = plt.colorbar(CS)
cbar.set_label(r'$\rho$', x=-0.12, labelpad = 5)
plt.axis('scaled')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(232)
CS = plt.contourf(x,y,Z,50,cmap='seismic',norm=TwoSlopeNorm(0))
plt.contour(x,y,Z,colors=('k',),linestyles=('--',),linewidths=(2,), levels=[1])
cbar = plt.colorbar(CS)
cbar.set_label(r'Z', x=-0.12, labelpad = 5)
plt.axis('scaled')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(233)
CS = plt.contourf(x,y,H,50,cmap='seismic',norm=TwoSlopeNorm(0))
plt.contour(x,y,Z,colors=('k',),linestyles=('--',),linewidths=(2,), levels=[1])
cbar = plt.colorbar(CS)
cbar.set_label(r'H', x=-0.12, labelpad = 5)
plt.axis('scaled')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(234)
CS = plt.contourf(xp,yp,P,50,cmap='seismic',norm=TwoSlopeNorm(0))
plt.contour(x,y,Z,colors=('k',),linestyles=('--',),linewidths=(2,), levels=[1])
cbar = plt.colorbar(CS)
cbar.set_label(r'P', x=-0.12, labelpad = 5)
plt.axis('scaled')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(235)
CS = plt.contourf(xm,y,u,50,cmap='seismic',norm=TwoSlopeNorm(0))
plt.contour(x,y,Z,colors=('k',),linestyles=('--',),linewidths=(2,), levels=[1])
cbar = plt.colorbar(CS)
cbar.set_label(r'u', x=-0.12, labelpad = 5)
plt.axis('scaled')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(236)
CS = plt.contourf(x,ym,v,50,cmap='seismic',norm=TwoSlopeNorm(0))
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
