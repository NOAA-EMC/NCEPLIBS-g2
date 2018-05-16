include ../version.inc

F90=ifort
FFLAGS_4=-O0 -check all -debug all -traceback
FFLAGS_d=-O0 -check all -debug all -traceback -r8 -i4

LDFLAGS_4= -L.. -lg2_$(VERSION_STRING)_4 $(PNG_LIB) $(JASPER_LIB) $(W3NCO_LIB4) $(Z_LIB)
LDFLAGS_d= -L.. -lg2_$(VERSION_STRING)_d $(PNG_LIB) $(JASPER_LIB) $(W3NCO_LIB4) $(Z_LIB)
#-L.. -lg2_d -lpng -ljasper -L/nwprod2/lib/w3nco/v2.0.6/ -lw3nco_v2.0.6_d

TEST_4=../exec/g2_unit_test_4
TEST_d=../exec/g2_unit_test_d

DVAL_4=../exec/g2_dump_values_4
DVAL_d=../exec/g2_dump_values_d

all: $(TEST_4) $(TEST_d) $(DVAL_4) $(DVAL_d)

clean: 
	rm -f *.o *.a *~ a.out

bare: clean
	rm -f test_g2_4 test_g2_d

$(TEST_4): test_g2.f90 ../libg2_$(VERSION_STRING)_4.a
	$(F90) -o $(TEST_4) $(FFLAGS_4) test_g2.f90 $(LDFLAGS_4)

$(TEST_d): test_g2.f90 ../libg2_$(VERSION_STRING)_d.a
	$(F90) -o $(TEST_d) $(FFLAGS_d) test_g2.f90 $(LDFLAGS_d)

$(DVAL_4): dump_values.f90 ../libg2_$(VERSION_STRING)_4.a
	$(F90) -o $(DVAL_4) $(FFLAGS_4) dump_values.f90 $(LDFLAGS_4)

$(DVAL_d): dump_values.f90 ../libg2_$(VERSION_STRING)_d.a
	$(F90) -o $(DVAL_d) $(FFLAGS_d) dump_values.f90 $(LDFLAGS_d)
