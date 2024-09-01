package emulator

import "core:fmt"
import "core:strings"
import "core:testing"


@(private = "file")
sb := &strings.Builder{}

@(private = "file")
string_builder_dump :: proc(format: string, args: ..any) {
	fmt.sbprintf(sb, format, ..args)
}

// https://github.com/christopherpow/nes-test-roms/blob/master/other/nestest.txt
// Skips PPU for now
@(test)
nestest_compliance :: proc(t: ^testing.T) {
	defer strings.builder_destroy(sb)

	console := new_console()
	defer destroy_console(console)
	bus := console.bus
	cpu := console.cpu
	ppu := console.ppu

	nestest :: #load("test_roms/other/nestest.nes")
	golden_log :: #load("test_roms/other/nestest.log", string)

	gl_lines := strings.split(golden_log, "\n")
	defer delete(gl_lines)

	rom, ok := rom_parse(nestest)
	testing.expect_value(t, ok, true)

	bus_load_rom(bus, &rom)

	cpu.registers.program_counter = 0x0C000

	ppu_index := strings.index(gl_lines[0], "PPU:")
	ppu_offset :: len("PPU:   0,  0")
	last_line := "no previous line"
	for line_index := 0; cpu.cycles < 26554; line_index += 1 {
		cpu_handle_instruction(cpu, string_builder_dump)
		defer strings.builder_reset(sb)

		// Skip the only 5 I/O accesses
		if line_index == 8980 || line_index == 8982 || line_index == 8984 || line_index == 8986 || line_index == 8988 {
			continue
		}

		err1 := cpu.memory.raw[0x02]
		err2 := cpu.memory.raw[0x03]
		testing.expectf(t, err1 == 0, "Error: %02X", err1)
		testing.expectf(t, err2 == 0, "Error: %02X", err2)

		dump := strings.trim(strings.to_string(sb^), "\n")
		line := gl_lines[line_index]
		defer last_line = line

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
