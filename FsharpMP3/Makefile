FSC = fsc.exe
FSCFLAGS = --platform:x64 --nologo --warn:5

all: *.fs
	fsc Utils.fs Header.fs Sideinfo.fs Maindata.fs Frame.fs Program.fs $(FSCFLAGS)

debug: *.fsx
	fsc Utils.fs Header.fs Sideinfo.fs Maindata.fs Frame.fs Program.fs -g $(FSCFLAGS)

clean:
	rm *.exe *.pdb *.dll
