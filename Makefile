PROG = $(wildcard prog_*.f90)
MODS = $(wildcard mod_*.f90)
OBJS = $(patsubst %.f90,%.o,$(MODS))

FC      = gfortran
FCFLAGS = -Wall -Wextra -fbacktrace -fcheck=all
LPFLAGS = -llapack -L/usr/lib/x86_64-linux-gnu/lapack/

PROGRAM = tensors.x

default: $(PROGRAM)

$(PROGRAM) : $(OBJS)
	$(FC) $(FCFLAGS) -o $@ $(PROG) $^ $(LPFLAGS)

$(OBJS) : %.o : %.f90
	$(FC) $(FCFLAGS) -c $< -o $@ $(LPFLAGS)

mod_procedures.o : mod_diag.o

debug:
	@echo $(PROG)
	@echo $(MODS)
	@echo $(OBJS)

clean:
	rm $(PROGRAM) $(OBJS) $(patsubst %.o,%.mod,$(OBJS)) $(wildcard *.dat)

.PHONY = default debug clean
