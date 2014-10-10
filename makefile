.PHONY: build

TARGET = main
HASKELL_OBJS = Main.hs

C_OBJS =
CPP_OBJS =
#test.cpp

# -fexcess-precision -optc-O2 -funbox-strict-fields -optc-mfpmath=sse -optc-msse2 -optc-msse3

GHC_CC_OPT = -threaded -eventlog -rtsopts -fexcess-precision -package GLFW  -L/usr/local/lib -L/usr/lib -lstdc++ --make -odir `uname -m` -hidir `uname -m`
GHC_CC_OPTIMIZE = -O3 -optc-O2 -funbox-strict-fields -optc-msse2 -optc-msse3
# -fllvm
GHC_LD_OPT =
GHC_LD_PACKAGES =
GHC_CC_PROF_OPT = -prof -auto-all $(GHC_CC_OPT)
GHC_CC_COVERAGE_OPT = -fhpc $(GHC_CC_OPT)

CLANG_CC_OPT =  -g -O3 -o `uname -m`/%.o
CLANG_CPP_OPT =  -g -O3

#%.o: %.hs
#	ghc $(GHC_CC_OPT) -c $< -o $@

# %.o: %.c %.h
# 	gcc $(GCC_CC_OPT) -c $< -o $@

# %.o: %.cpp
# 	g++ $(GCC_CPP_OPT) -c $< -o $@

# all: $(C_OBJS) $(CPP_OBJS) $(HASKELL_OBJS)
# 	ghc $(GHC_LD_OPT) $(GHC_LD_PACKAGES) $(HASKELL_OBJS) $(C_OBJS) $(CPP_OBJS) -o $(TARGET)

build:
	ghc $(GHC_CC_OPT) $(GHC_CC_OPTIMIZE) $(HASKELL_OBJS) $(C_OBJS) $(CPP_OBJS) $(GHC_LD_OPT) $(GHC_LD_PACKAGES) -o $(TARGET)

profile:
	ghc $(GHC_CC_PROF_OPT) $(GHC_CC_OPTIMIZE) $(HASKELL_OBJS) $(C_OBJS) $(CPP_OBJS) $(GHC_LD_OPT) $(GHC_LD_PACKAGES) -o $(TARGET)

coverage:
	ghc $(GHC_CC_PROF_OPT) $(GHC_CC_COVERAGE_OPT) $(GHC_CC_OPTIMIZE) $(HASKELL_OBJS) $(C_OBJS) $(CPP_OBJS) $(GHC_LD_OPT) $(GHC_LD_PACKAGES) -o $(TARGET)

clean:
	@rm -rf `uname -m` $(TARGET)

clean_coverage:
	@rm -rf .hpc *.html $(TARGET).tix

clean_maino:
	@rm $(TARGET).o

app:
	@mkbndl -f majv 1 -minv 0 -rev 0 -longv "pre alpha 1" -build 0000 -id org.nohopeforjoy.cine -r Data/* -ic Data/Icons/cine.icns cine

all: build app

rebuild: clean all

relink: clean_maino all
