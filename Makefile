FC= gfortran
FFLAGS=-O3
FLIB=-L /home/pablo/SRC/libpab
FMODULES=-I /home/pablo/SRC/libpab

PESdir=
PES=
OBJECTS=$(PES:%.f90=%.o)

all: approach_probability get_probabilities mean_ener get_mean_ener

approach_probability: approach_probability.f90
	$(FC) $(FFLAGS) -o approach_probability.x $^ $(FLIB) -lpab $(FMODULES)

get_probabilities: get_probabilities.o
	$(FC) $(FFLAGS) -o get_probabilities.x $^

mean_ener: $(OBJECTS) mean_energy_distance.f90
	$(FC) $(FFLAGS) -o mean_ener.x $^

get_mean_ener: get_mean_ener.o
	$(FC) $(FFLAGS) -o get_mean_ener.x $^

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@
