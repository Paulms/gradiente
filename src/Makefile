# Makefile created by mkmf $Id: mkmf,v 18.0 2010/03/02 23:26:08 fms Exp $ 



include template_mkmf


.DEFAULT:
	-echo $@ does not exist.
all: gradiente
datos.o: ./datos.f90 decimal.o morse.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./datos.f90
decimal.o: ./decimal.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./decimal.f90
gradiente.o: ./gradiente.f90 morse.o decimal.o iterativos.o datos.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./gradiente.f90
iterativos.o: ./iterativos.f90 decimal.o morse.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./iterativos.f90
morse.o: ./morse.f90 decimal.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./morse.f90
SRC = ./datos.f90 ./iterativos.f90 ./gradiente.f90 ./morse.f90 ./decimal.f90
OBJ = datos.o iterativos.o gradiente.o morse.o decimal.o
clean: neat
	-rm -f .gradiente.cppdefs $(OBJ) gradiente
neat:
	-rm -f $(TMPFILES)
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
gradiente: $(OBJ) 
	$(LD) $(OBJ) -o gradiente  $(LDFLAGS)
