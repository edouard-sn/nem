package ines

import "core:os"

INES_HEADER_MAGIC :: "NES\x1a"
PRG_ROM_PAGE_SIZE :: 0x4000
CHR_ROM_PAGE_SIZE :: 0x2000

ROM :: struct {
	prg_rom:            []byte,
	chr_rom:            []byte,
	mapper:             int,
	screen:             enum {
		Vertical,
		Horizontal,
		FourScreen,
	},
	battery_backed_ram: bool,
}

load_rom :: proc(name: string) -> ROM {
	data, ok := os.read_entire_file(name)
	if !ok {
		// could not read file
		return ROM{}
	}
	defer delete(data, context.allocator)
	return parse(data)
}

parse :: proc(data: []byte) -> ROM {
	buffer := data
	rom := ROM{}

	for letter, idx in INES_HEADER_MAGIC {
		assert(byte(letter) == buffer[idx], "Invalid iNES ROM header")
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

	prg_rom_start := 16 + (has_trainer ? 512 : 0)
	prg_rom_end := prg_rom_start + prg_page_count * PRG_ROM_PAGE_SIZE

	chr_rom_start := prg_rom_end
	chr_rom_end := chr_rom_start + chr_page_count * CHR_ROM_PAGE_SIZE

	rom.prg_rom = data[prg_rom_start:prg_rom_end]
	rom.chr_rom = data[chr_rom_start:chr_rom_end]

	assert(control_byte_2 & 0xF == 0, "Invalid iNES (1.0) ROM header.")

	hi := control_byte_1 & 0xF0
	lo := control_byte_2 & 0xF0
	rom.mapper = int(hi | (lo >> 4))

	return rom
}
