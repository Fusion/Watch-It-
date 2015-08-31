EXECUTABLE=watchit.app

all: $(EXECUTABLE)

$(EXECUTABLE):
	raco exe --gui watchit.rkt

clean:
	rm -rf $(EXECUTABLE)
