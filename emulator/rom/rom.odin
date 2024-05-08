package ines

import "core:fmt"
import "core:os"

INES_HEADER_MAGIC :: "NES\x1a"
PRG_ROM_PAGE_SIZE :: 0x4000
CHR_ROM_PAGE_SIZE :: 0x2000

ScreenStatus :: enum {
	Vertical,
	Horizontal,
	FourScreen,
}

ROM :: struct {
	prg_rom:            []byte,
	chr_rom:            []byte,
	mapper:             int,
	screen:             ScreenStatus,
	battery_backed_ram: bool,
}

load_rom :: proc(name: string) -> (rom: ROM, ok: bool) {
	data := os.read_entire_file(name) or_return

	return parse(data)
}

parse :: proc(data: []byte) -> (rom: ROM, ok: bool) {
	buffer := data

	for letter, idx in INES_HEADER_MAGIC {
		if (byte(letter) != buffer[idx]) {
			return rom, false
		}
	}

	prg_page_count := int(buffer[4])
	chr_page_count := int(buffer[5])
	control_byte_1 := buffer[6]
	control_byte_2 := buffer[7]

	if control_byte_1 & 0b1000 != 0 {
		rom.screen = .FourScreen
	}
	 else {
		rom.screen = control_byte_1 & 0b1 != 0 ? .Vertical : .Horizontal
	}

	rom.battery_backed_ram = (control_byte_1 & 0b10 != 0)

	has_trainer := (control_byte_1 & 0b100 != 0)

	prg_rom_start := 16
	if (has_trainer) {
		prg_rom_start += 512
	}
	prg_rom_end := prg_rom_start + prg_page_count * PRG_ROM_PAGE_SIZE

	chr_rom_start := prg_rom_end
	chr_rom_end := chr_rom_start + chr_page_count * CHR_ROM_PAGE_SIZE

	rom.prg_rom = data[prg_rom_start:prg_rom_end]
	rom.chr_rom = data[chr_rom_start:chr_rom_end]

	if ((control_byte_2 & 0x8) != 0) {
		fmt.println("Invalid iNES (1.0) ROM header.")
		return rom, false
	}

	hi := control_byte_1 & 0xF0
	lo := control_byte_2 & 0xF0
	rom.mapper = int(hi | (lo >> 4))

	return rom, true
}
