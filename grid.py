import matplotlib.pyplot as plt
from numpy import loadtxt, meshgrid

x = loadtxt('data/x.dat', skiprows=0, unpack=True)
y = loadtxt('data/y.dat', skiprows=0, unpack=True)

X, Y = meshgrid(x, y)

############# GRID
plt.figure()
plt.scatter(X, Y, 3, color='black')
plt.xlabel('x')
plt.ylabel('y')
plt.show()

