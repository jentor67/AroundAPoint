# Compiler and flags
FC = gfortran
FFLAGS = -std=f2008 -Wall -MMD -MP

# Target
TARGET = program

# Sources and objects
#SRCS = $(wildcard *.f95)
SRCS = main.f95 constantsModule.f95 gravityModule.f95 startParametersModule.f95 vectorModule.f95 
OBJS = $(SRCS:.f95=.o)
DEPS = $(OBJS:.o=.d)

# Target to print the variable
#print-fc:
#	@echo "The Fortran compiler is: $(SRCS)"


# Default rule
all: $(TARGET)

# Link
$(TARGET): $(OBJS)
	$(FC) $(OBJS) -o $(TARGET)

# Compile with dependency generation
%.o: %.f95
	$(FC) $(FFLAGS) -cpp $< -o $@

# Include dependency files if they exist
-include $(DEPS)

# Clean
clean:
	rm -f $(OBJS) $(TARGET) *.mod $(DEPS)
