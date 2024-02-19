package cpu

import nbus "../bus"
import nrom "../rom"
import "core:fmt"
import "core:testing"

// https://github.com/christopherpow/nes-test-roms/blob/master/other/nestest.txt
@(test)
nestest_compliance :: proc(t: ^testing.T) {
	bus := nbus.new_bus()
	cpu := new_cpu(&bus)
	reset_interupt(&cpu)

	nestest := #load("./test_files/nestest.nes")

	rom, ok := nrom.parse(nestest)
	testing.expect_value(t, ok, true)

	copy(bus.prg_rom, rom.prg_rom)
	copy(bus.prg_rom[0x4000:], rom.prg_rom)

	cpu.registers.program_counter = 0x0C000

	cpu.cycles = 7
	for (cpu.cycles < 26554) {
		handle_instruction(&cpu)
	}

	testing.expect_value(t, read_byte(&cpu, 0x02), 0)
	testing.expect_value(t, read_byte(&cpu, 0x03), 0)
	testing.log(t, "All 256 instructions should work !")
}
