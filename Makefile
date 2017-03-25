FSC = fsc.exe
FSCFLAGS = --platform:x64 --nologo --warn:5

all: *.fsx
	$(FSC) utils.fsx header.fsx sideinfo.fsx maindata.fsx frame.fsx parser.fsx $(FSCFLAGS)
