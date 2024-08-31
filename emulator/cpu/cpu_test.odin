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
	defer strings.builder_destroy(sb)

	bus := Bus{}
	cpu := CPU{}

	init_bus(&bus)
	defer destroy_bus(&bus)
	init_cpu(&cpu, &bus)

	nestest := #load("../test_roms/other/nestest.nes")
	golden_log := #load("../test_roms/other/nestest.log", string)

	gl_lines := strings.split(golden_log, "\n")
	defer delete(gl_lines)

	rom, ok := nrom.parse(nestest)
	testing.expect_value(t, ok, true)

	copy(bus.prg_rom, rom.prg_rom)
	copy(bus.prg_rom[0x4000:], rom.prg_rom)

	cpu.registers.program_counter = 0x0C000

	ppu_index := 0
	ppu_offset :: len("PPU:   0,  0")
	last_line := "no previous line"
	for line_index := 0; cpu.cycles < 26554; line_index += 1 {
		handle_instruction(&cpu, string_builder_dump)
		defer strings.builder_reset(sb)

		// Skip the only 5 I/O accesses
		if line_index == 8980 || line_index == 8982 || line_index == 8984 || line_index == 8986 || line_index == 8988 {
			continue
		}

		err1 := unsafe_read(&cpu, 0x02)
		err2 := unsafe_read(&cpu, 0x03)
		testing.expectf(t, err1 == 0, "Error: %02X", err1)
		testing.expectf(t, err2 == 0, "Error: %02X", err2)

		dump := strings.trim(strings.to_string(sb^), "\n")
		line := gl_lines[line_index]
		defer last_line = line

		if ppu_index == 0 {
			ppu_index = strings.index(dump, "PPU:")
		}

		before_ppu := dump[:ppu_index] == line[:ppu_index]
		testing.expectf(
			t,
			before_ppu,
			"State problem - Line %d:\n%s\n%s\nprevious: %s\n",
			line_index,
			dump[:ppu_index],
			line[:ppu_index],
			last_line,
		)

		ppu_end := ppu_index + ppu_offset
		after_ppu := dump[ppu_end:] == line[ppu_end:]
		testing.expectf(t, after_ppu, "Cycle problem - Line %d:\n%s\n%s\n", line_index, dump[ppu_end:], line[ppu_end:])
	}
}
