FSC = fsc.exe
FSCFLAGS = --warn:5

all: *.fs
	fsc AssemblyInfo.fs Utils.fs Tables.fs Header.fs Sideinfo.fs Frame.fs Maindata.fs MathUtils.fs Program.fs --optimize $(FSCFLAGS)

debug: *.fs
	fsc AssemblyInfo.fs Utils.fs Tables.fs Header.fs Sideinfo.fs Frame.fs Maindata.fs Program.fs MathUtils.fs -g $(FSCFLAGS)

msbuild:*.fs
	MSBuild MsharP3.fsproj

clean:
	MSBuild MsharP3.fsproj /t:Clean
