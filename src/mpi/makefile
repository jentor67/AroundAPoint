FC = gfortran
 FFLAGS = -std=f2008 -O2 -Wall
# recommended
TARGET = my_program
SRCS = constantsmodule.f90 readconfigmodule.f90 startparametersmodule.f90 vectormodule.f90 gravitymodule.f90 mpi_loop.f90 main.f90  
OBJS = $(SRCS:.f90=.o)

all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGET) *.mod

.PHONY: all clean
