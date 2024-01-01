GHC = ghc
GHC_FLAGS = --make -o MyExecutable

# Source files
SOURCE_FILES = main.hs Extra.hs Interpreter.hs Compiler.hs Parser.hs

# Output files
OUTPUT_FILES = *.o *.hi MyExecutable

# Default target
all: build

# Build target
build:
	$(GHC) $(GHC_FLAGS) $(SOURCE_FILES)

# Clean target
clean:
	rm -f $(OUTPUT_FILES)

# Run target
run:
	./MyExecutable $(FILE)

.PHONY: all build clean run
