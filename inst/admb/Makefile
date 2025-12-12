#Makefile for building and running the jjm model on a Linux or Mac OSX
EXEC=jjms
# TPL=$(EXEC1).tpl

all: $(EXEC)

$(EXEC): $(EXEC).tpl
	admb -f $(EXEC)

clean:
	rm -f $(EXEC) *.eva *.log *.rpt *.htp *.cor *.par *.r* *.b* *.p* *.obj
	rm -f *.rep *.bar *.psv *.std $(EXEC).cpp admodel.* variance *.o *.ecm *.mc2 *.mcm
