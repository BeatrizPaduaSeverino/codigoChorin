# STANDARD PLOTTING FORMAT
#
# Format a plot as the adopted standard

import matplotlib.pyplot as plt

from matplotlib import rc
#rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})

rc('font',**{'family':'serif','serif':['Times'], 'size':10})
#rc('text', usetex=True)

def SPF():
	F = plt.gcf()
	A = plt.gca()

	F.set_size_inches(2.95,2.95)

	L = plt.legend(loc='best')
	L.get_frame().set_alpha(0.0)
	L.get_frame().set_edgecolor('w')

	A.tick_params(axis='y', which='minor',direction='in')
	A.tick_params(axis='x', which='minor',direction='in')
	A.tick_params(axis='both',direction='in')
	plt.tight_layout(pad=0.15)
