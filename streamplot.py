import matplotlib.pyplot as plt
import numpy as np
import matplotlib.ticker
from numpy import meshgrid
from scipy.interpolate import interp1d
from scipy.interpolate import RectBivariateSpline
from scipy.interpolate import CloughTocher2DInterpolator
from scipy.spatial import Delaunay
from spf import *
from variables import * #import variables to characteristic

rad = 1.0
or_x = 10.0
or_y = 0.0


x = np.loadtxt('data/x.dat', skiprows=0, unpack=True)
y = np.loadtxt('data/y.dat', skiprows=0, unpack=True)

xm = np.loadtxt('data/xm.dat', skiprows=0, unpack=True)
ym = np.loadtxt('data/ym.dat', skiprows=0, unpack=True)


u = np.loadtxt('data/um.dat', skiprows=0, unpack=True)
v = np.loadtxt('data/vm.dat', skiprows=0, unpack=True)

Z = np.loadtxt('data/Z.dat', skiprows=0, unpack=True)
P   = np.loadtxt('data/P.dat', skiprows=0, unpack=True)

y  = y  - Hjet
ym = ym - Hjet


#x_grid,y_grid= np.loadtxt('data/grid_m.dat', skiprows=0, unpack=True)

Nx_u = int(len(xm))
Ny_u = int(len(y))
Nx_v = int(len(x))
Ny_v = int(len(ym))
u = np.array(u)
v = np.array(v)
P = np.array(P)

x_int = np.linspace(min(xm), max(xm), num=Nx_v-1,endpoint='True')
y_int = np.linspace(min(ym), max(ym), num=Ny_u-1,endpoint='True')

X,Y = np.meshgrid(x,y)
X2,Y2 = np.meshgrid(x_int,y_int)
interp_spline = RectBivariateSpline(y,x,Z)
Z_int = interp_spline(y_int,x_int)

X,Y = np.meshgrid(x,ym)
X2,Y2 = np.meshgrid(x_int,y_int)
interp_spline = RectBivariateSpline(ym,x,v)
v_int = interp_spline(y_int,x_int)

X,Y = np.meshgrid(xm,y)
X2,Y2 = np.meshgrid(x_int,y_int)
interp_spline = RectBivariateSpline(y,xm,u)
u_int = interp_spline(y_int,x_int)

for i in range(len(x_int)):
    for j in range(len(y_int)):
        if x_int[i]**2 + y_int[j]**2 < 1:
            u_int[j,i] = 0
            v_int[j,i] = 0

#### DIMENSIONAL
#x_int = x_int*Lc
#y_int = y_int*Lc
#u_int = u_int*Vc
#v_int = v_int*Vc

#X,Y = np.meshgrid(x,y)

plt.figure(figsize=(10, 4))

#plt.subplot(121)
speed = (u_int*u_int + v_int*v_int)**(0.5)
plt.streamplot(x_int, y_int, u_int, v_int,density=2, linewidth=0.5, color='k',arrowstyle='->')
plt.contourf( x_int,y_int,speed,60, cmap=plt.cm.get_cmap('cool'))
cbar = plt.colorbar()
#cbar.set_label(r'$\sqrt{\hat{u}^2 + \hat{v}^2}\ [cm/s]$', x=-0.12, labelpad = 5)
cbar.set_label(r'$\vec{u}}$', x=-0.12, labelpad = 5)
plt.contour(x_int,y_int,Z_int,colors=('k',),linestyles=('--',),linewidths=(2,), levels=[1])
plt.axis('scaled')
#plt.xlabel('$\hat{x}\ [cm]$')
#plt.ylabel('$\hat{y}\ [cm]$')
plt.xlabel('${x}$')
plt.ylabel('${y}$')

SPF()
F = plt.gcf()
F.set_size_inches(4.95,4.95)
#plt.savefig('output/streamlines.png', bbox_inches='tight',dpi=200)
plt.show()
