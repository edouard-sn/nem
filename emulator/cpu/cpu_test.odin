package cpu

import nrom "../rom"
import "core:fmt"
import "core:strings"
import "core:testing"

@(private)
sb := &strings.Builder{}

@(private)
string_builder_dump :: proc(format: string, args: ..any) {
	fmt.sbprintf(sb, format, ..args)
}

// https://github.com/christopherpow/nes-test-roms/blob/master/other/nestest.txt
// Skips PPU for now
@(test)
nestest_compliance :: proc(t: ^testing.T) {
	bus := new_bus()
	cpu := new_cpu(&bus)
	reset_interupt(&cpu)

	nestest := #load("../test_roms/other/nestest.nes")
	golden_log := #load("../test_roms/other/nestest.log", string)
	gl_lines := strings.split(golden_log, "\n")

	rom, ok := nrom.parse(nestest)
	testing.expect_value(t, ok, true)

	copy(bus.prg_rom, rom.prg_rom)
	copy(bus.prg_rom[0x4000:], rom.prg_rom)

	cpu.registers.program_counter = 0x0C000

	cpu.cycles = 7

	ppu_index := 0
	ppu_offset :: len("PPU:   0,  0")
	for line_index := 0; cpu.cycles < 26554; line_index += 1 {

		handle_instruction(&cpu, string_builder_dump)
		defer strings.builder_reset(sb)

		dump := strings.trim(strings.to_string(sb^), "\n")
		line := gl_lines[line_index]

		// Skip the only 4 I/O accesses
		if line_index == 8980 || line_index == 8982 || line_index == 8984 || line_index == 8986 {
			continue
		}

		if ppu_index == 0 {
			ppu_index = strings.index(dump, "PPU:")
		}

		before_ppu := dump[:ppu_index] == line[:ppu_index]
		testing.expectf(t, before_ppu, "State problem - Line %d:\n%s\n%s\n", line_index, dump[:ppu_index], line[:ppu_index])

		ppu_end := ppu_index + ppu_offset
		after_ppu := dump[ppu_end:] == line[ppu_end:]
		testing.expectf(t, after_ppu, "Cycle problem - Line %d:\n%s\n%s\n", line_index, dump[ppu_end:], line[ppu_end:])

		testing.expectf(t, read_byte(&cpu, 0x02) == 0, "Error: %02X", read_byte(&cpu, 0x02))
		testing.expectf(t, read_byte(&cpu, 0x03) == 0, "Error: %02X", read_byte(&cpu, 0x03))
	}
}
