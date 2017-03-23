FSC = fsc.exe
FSCFLAGS = --platform:x64 --nologo --warn:5

all: parser.fsx
	$(FSC) parser.fsx $(FSCFLAGS)
