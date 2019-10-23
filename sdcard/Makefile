VASM_FLAGS	:= -m68020 -Fhunkexe -kick1hunks -nosym -showcrit
RL_TARGET	:= 192.168.2.66

.PHONY: all clean install

all: replaysd.device testsd.exe sd.device
	@echo ""

%.device: %.s kprintf.i Makefile
	$(VASM) $< -o $@ -L $(@:%.device=%.txt) $(VASM_FLAGS) -I $(INC_ASM)

%.exe: testsd.s kprintf.i Makefile
	$(VASM) $< -o $@ -L $(@:%.exe=%.txt) $(VASM_FLAGS) -I $(INC_ASM)

clean:
	rm -f *.device *.txt

install: all
	$(RL) $(RL_TARGET) c:avail flush
	$(RL) $(RL_TARGET) c:protect ram:*.device rwed
	$(RL) $(RL_TARGET) c:protect ram:SD0#? rwed
	$(RL) $(RL_TARGET) c:copy *.device ram:
	$(RL) $(RL_TARGET) c:copy SD0#? ram:
#	$(RL) $(RL_TARGET) c:mount ram:SD0

flush:
	$(RL) $(RL_TARGET) c:avail flush

test: all
	$(RL) $(RL_TARGET) sd.device

sd: all flush
	$(RL) $(RL_TARGET) c:protect ram:*.device rwed
	$(RL) $(RL_TARGET) c:copy sd.device ram:
	$(RL) $(RL_TARGET) c:execute giggledisk

hdtoolbox: all flush
	$(RL) $(RL_TARGET) c:protect ram:*.device rwed
	$(RL) $(RL_TARGET) c:copy sd.device ram:
	$(RL) $(RL_TARGET) SYS:Tools/HDToolBox ram:sd.device

include ../Makefile.build
