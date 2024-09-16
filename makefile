SRC     = ./src
OBJ_DIR = ./libs
MOD_DIR = ./bin
obj     = prg

#---------------------------------------------------------------------------------------------
FFLAG    = -O2 -march=native --check=all -fstack-protector -fimplicit-none
#FFLAG   = -O3 --check=all --tracer --warn-all -fstack-protector -fimplicit-none
#FFLAG   = -O3 -check all -traceback -warn all -fstack-protector -assume protect_parens -implicitnone 
#---------------------------------------------------------------------------------------------
FCOMPL  = gfortran -J$(MOD_DIR)
#---------------------------------------------------------------------------------------------
#LIBS01  = $(OBJ_DIR)/libFDq.a -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lpthread /opt/libs/libNAG/libNAG.a 
LIBS01  = $(OBJ_DIR)/libFDq.a
OPTIONS = -DDOUBLE_PREC -DOVERWRITE
#OPTIONS =

#---------------------------------------------------------------------------------------------
# List of other directories for source files


.PREFIXES: .

.SUFFIXES:
.SUFFIXES: .f90 .o

$(OBJ_DIR)/%.o :	$(SRC)/%.f90
	@echo "Compiling file:" $@
	@${FCOMPL} $(OPTF) $(OPTIONS) ${FFLAG}  -fopenmp -c $< -o $@

#---------------------------------------------------------------------------------------------

MODULES01 =                                   \
$(OBJ_DIR)/setPrecision.o                     \
$(OBJ_DIR)/parameters.o	                      \
$(OBJ_DIR)/variables.o	                      \
$(OBJ_DIR)/auxiliary.o                        \
$(OBJ_DIR)/discretisation.o                   \
$(OBJ_DIR)/initialCondition.o	              \
$(OBJ_DIR)/boundaryConditions.o	              \
$(OBJ_DIR)/coefficients.o                     \
$(OBJ_DIR)/rhs.o                              \
$(OBJ_DIR)/residue.o                          \
$(OBJ_DIR)/solver.o                           \
$(OBJ_DIR)/dataWrite.o

FILES01   = $(OBJ_DIR)/main.o

OBJECTS01 = ${MODULES01} ${FILES01}

runs:
	make clean
	@echo ""
	cp src_aux/*.mod ${MOD_DIR}
	@echo ""
	make EVP;
	./${obj}
	make clean

clean:
	@rm -f ${MOD_DIR}/*.mod
	@rm -f ./libs/*.o
	@rm -f ./libs/*.mod
	@rm -f  *.out
	@rm -rf *CORE*
	@rm -rf *core*
	@rm -rf fort*   
	@rm -rf *~*
	@rm -rf prg*
	@rm -rf *.obj
	@rm -rf *.pdb
	@rm -rf *.phy

cleanfile:
	@rm -rf data/*.dat
	@rm -rf data/*.plt
	@rm -rf data/*.m
	@rm -rf data/*.bin
	@rm -rf data/*~*
	@rm -rf data/octave-core

#------------------------------------------------------------------------------------------------------------------------------
EVP:	${OBJECTS01}
	${FCOMPL} ${OBJECTS01} ${FFLAG} ${LIBS01}  $(LIBS)  -fopenmp -o ${obj}
	chmod 710 ${obj}
#--------------------------------------------------------------------------------------------



