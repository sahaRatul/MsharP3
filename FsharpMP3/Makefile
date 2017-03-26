FSC = fsc.exe
FSCFLAGS = --warn:5

all: *.fs
	fsc AssemblyInfo.fs Utils.fs Tables.fs Header.fs Sideinfo.fs Frame.fs Maindata.fs Program.fs $(FSCFLAGS)

debug: *.fsx
	fsc AssemblyInfo.fs Utils.fs Tables.fs Header.fs Sideinfo.fs Frame.fs Maindata.fs Program.fs -g $(FSCFLAGS)

msbuild:*.fs
	MSBuild FsharpMP3.fsproj

clean:
	MSBuild FsharpMP3.fsproj /t:Clean
