#ASM = java -jar /Users/mario/Development/msx/Compilers/glass.jar
ASM = /Users/mario/Development/msx/Compilers/sjasmplus/build/release/sjasmplus
#ASM = ~/Development/msx/cpm/cpm M80 =
ASMFLAGS = --raw=$@ --sym=$(basename $@).sym
OUTPUT_DIR = dist

all: $(OUTPUT_DIR)/driver.bin
.PHONY: all clean copy

$(OUTPUT_DIR)/driver.bin: driver.asm ch376s.asm print_bios.asm
	$(ASM) $(ASMFLAGS) $< 
	
clean:
	-rm $(OUTPUT_DIR)/*.rom
	-rm $(OUTPUT_DIR)/*.com
	-rm $(OUTPUT_DIR)/*.bin
	-rm $(OUTPUT_DIR)/*.sym

copy:
	cp $(OUTPUT_DIR)/main.bin /Volumes/Untitled
