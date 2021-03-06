FC= gfortran
FFLAGS=-O3
FLIB=-L /home/pablo/SRC/libpab
FMODULES=-I /home/pablo/SRC/libpab

BIN_DIR=/home/pablo/SRC/QCT_analisis

PESdir=
PES=
OBJECTS=$(PES:%.f90=%.o)

all: approach_probability get_probabilities mean_ener get_mean_ener make_approach_line line_from_rebound.f90

approach_probability: $(BIN_DIR)/approach_probability.f90
	$(FC) $(FFLAGS) -o approach_probability.x $^ $(FLIB) -lpab $(FMODULES)

get_probabilities: $(BIN_DIR)/get_probabilities.o
	$(FC) $(FFLAGS) -o get_probabilities.x $^

mean_ener: $(OBJECTS) $(BIN_DIR)/mean_energy_distance.f90
	$(FC) $(FFLAGS) -o mean_ener.x $^

get_mean_ener: $(BIN_DIR)/get_mean_ener.o
	$(FC) $(FFLAGS) -o get_mean_ener.x $^

make_approach_line: $(BIN_DIR)/io.f90 $(BIN_DIR)/make_approach_line.f90
	$(FC) $(FFLAGS) -o make_approach_line.x $^ $(FLIB) -lpab $(FMODULES)

line_from_rebound: $(BIN_DIR)/line_from_rebound.f90
	$(FC) $(FFLAGS) -o line_from_rebound.x $^ $(FLIB) -lpab $(FMODULES)

remove_vibration: $(BIN_DIR)/remove_vibration.f90
	$(FC) $(FFLAGS) -o remove_vibration.x $^ $(FLIB) -lpab $(FMODULES)

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f $(BIN_DIR)/*.o $(BIN_DIR)/*.mod $(BIN_DIR)/*.x
