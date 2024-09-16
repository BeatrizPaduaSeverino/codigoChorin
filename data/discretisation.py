import matplotlib.pyplot as plt
#from mpl_toolkits import mplot3d
from numpy import loadtxt, meshgrid, matmul, abs, amax, amin, empty_like, zeros, transpose, diff, sin, cos

x = loadtxt('gridx.dat', skiprows=0, unpack=True)
y = loadtxt('gridy.dat', skiprows=0, unpack=True)

dx = loadtxt('stencilx.dat')
dy = loadtxt('stencily.dat')

dxx = loadtxt('stencilxx.dat')
dyy = loadtxt('stencilyy.dat')

nx = len(x)
ny = len(y)

#print( linalg.cond(df1x), linalg.cond(df1y) )
#print( linalg.cond(stencilx), linalg.cond(stencily) )

f      = zeros( [ny, nx] )
fx    = empty_like(f)
fy    = empty_like(f)
fxx  = empty_like(f)
fyy  = empty_like(f)

for i in range(nx):
    for j in range(ny):
        xp        = x[i]
        yp        = y[j]
        f[j,i]      =   -sin(xp)*cos(yp)
        fx[j,i]    =   -cos(xp)*cos(yp)
        fxx[j,i]  =    sin(xp)*cos(yp)
        fy[j,i]    =    sin(xp)*sin(yp)
        fyy[j,i]  =    sin(xp)*cos(yp)

#ddxN  = kron( stencilx, identity(len(stencily)) )
#ddyN  = kron( identity(len(stencilx)), stencily  )

fxN = transpose(matmul( dx, transpose(f) ))
fyN = matmul( dy, f )

fxxN = transpose(matmul( dxx, transpose(f) ))
fyyN = matmul( dyy, f )

errx = abs(fx - fxN)
erry = abs(fy - fyN)

errxx = abs(fxx - fxxN)
erryy = abs(fyy - fyyN)

print('Grid spacing - x (min, max): ', amin(diff(x)), ', ', amax(diff(x)))
print('Grid spacing - y (min, max): ', amin(diff(y)), ', ', amax(diff(y)))

print('Error - fx (min, max): ', amin(errx), ', ', amax(errx))
print('Error - fy (min, max): ', amin(erry), ', ', amax(erry))

print('Error - fxx (min, max): ', amin(errxx), ', ', amax(errxx))
print('Error - fyy (min, max): ', amin(erryy), ', ', amax(erryy))

X, Y = meshgrid(x, y)

############# GRID
#plt.figure()
plt.scatter(X, Y, 3, color='black')
#plt.spy(dx, marker='x', markersize='3', color='black')
#plt.spy(dy, marker='x', markersize='3', color='black')
#plt.spy(dx2, marker='x', markersize='3', color='black')
#plt.spy(dy2, marker='x', markersize='3', color='black')
#plt.contourf(X, Y, f)
#fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
#ax.plot_surface(X, Y, f)
plt.xlabel('x')
plt.ylabel('y')
#plt.xlim(1.2,2.8)
#plt.ylim(1.2,2.8)
#plt.legend( loc= 'upper right')
#plt.savefig('grid.png', bbox_inches='tight',dpi=200)
plt.show()

