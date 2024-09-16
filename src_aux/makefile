OBJ_DIR = ../libs
#---------------------------------------------------------------------------------------------
FFLAG   = -O2 -march=native --check=all -fstack-protector -fimplicit-none #-bounds-check
#---------------------------------------------------------------------------------------------
FCOMPL  = gfortran #-J$(MOD_DIR)
#---------------------------------------------------------------------------------------------
#LIBS01  =
#---------------------------------------------------------------------------------------------
# List of other directories for source files
#.PREFIXES: .

#.SUFFIXES:
#.SUFFIXES: .f90 .o
#
#%.o :	%.f90
#	@echo "Compiling file:" $@
#	@${FCOMPL} ${FFLAG} -c $< -o $@

#---------------------------------------------------------------------------------------------

SRCS=Lagrange_Interpolation_Method.f90 Piecewise_Polynomial_Interpolation.f90 Linear_Algebra.f90 Nonlinear_Algebra.f90 Optimum_Grid_Methods.f90

OBJS =  $(SRCS:.f90=.o)

libFDq.a:$(OBJS)
	ar rcs $@ $(OBJS)
	cp $@ $(OBJ_DIR)
	@rm -f *.o
	@rm -f *.a

%.o : %.f90
	$(FCOMPL) $(FFLAG) -c $<

clean:
	@rm -f ${MOD_DIR}*.mod
	@rm -f *.o
	@rm -f *.a
	@rm -f  *.out
	@rm -rf *CORE*
	@rm -rf *core*
	@rm -rf fort*   
	@rm -rf *~*
	@rm -rf prg*
	@rm -rf *.obj
	@rm -rf *.pdb
	@rm -rf *.phy

