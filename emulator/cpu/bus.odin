package cpu

import nrom "../rom"
import nppu "../ppu"
// import "core:fmt"

CPU_RAM_MAP_BEGIN :: CPU_ZERO_PAGE_BEGIN
CPU_RAM_MAP_END :: CPU_RAM_END
CPU_ZERO_PAGE_BEGIN :: 0x0000
CPU_ZERO_PAGE_END :: 0x0100
CPU_STACK_BEGIN :: 0x0100
CPU_STACK_END :: 0x0200
CPU_RAM_BEGIN :: 0x0200
CPU_RAM_END :: 0x0800
RAM_MIRRORS_BEGIN :: 0x0800
CPU_RAM_MIRRORS_END :: 0x2000

IO_MAP_BEGIN :: IO_FIRST_BEGIN
IO_MAP_END :: IO_SECOND_END
IO_FIRST_BEGIN :: 0x2000
IO_FIRST_END :: 0x2008
IO_SECOND_BEGIN :: 0x4000
IO_SECOND_END :: 0x4020

IO_MIRRORS_BEGIN :: 0x2008
IO_MIRRORS_END :: 0x4000

EXPANSION_ROM_BEGIN :: 0x4020
EXPANSION_ROM_END :: 0x6000

SRAM_BEGIN :: 0x6000
SRAM_END :: 0x8000

PRG_ROM_LOWER_BEGIN :: 0x8000
PRG_ROM_LOWER_END :: 0xC000
PRG_ROM_UPPER_BEGIN :: 0xC000
PRG_ROM_UPPER_END :: 0x10000

Bus :: struct {
	raw:      ^[0x10000]byte,
	cpu_vram: []byte,

	//  bus slices
	ram:      []byte,
	ram_map:  struct {
		zero_page: []byte,
		stack:     []byte,
		ram:       []byte,
	},
	prg_rom:  []byte,

	ppu: ^nppu.PPU
}

init_bus :: proc(bus: ^Bus) {
	bus.raw = new([0x10000]byte)

	bus.cpu_vram = bus.raw[CPU_RAM_BEGIN:CPU_RAM_END]
	// CPU RAM map init 
	bus.ram = bus.raw[CPU_RAM_MAP_BEGIN:CPU_RAM_MAP_END]

	bus.ram_map.zero_page = bus.raw[CPU_ZERO_PAGE_BEGIN:CPU_ZERO_PAGE_END]
	bus.ram_map.stack = bus.raw[CPU_STACK_BEGIN:CPU_STACK_END]
	bus.ram_map.ram = bus.raw[CPU_RAM_BEGIN:CPU_RAM_END]

	bus.prg_rom = bus.raw[PRG_ROM_LOWER_BEGIN:PRG_ROM_UPPER_END]
}

load_rom :: proc(bus: ^Bus, rom: ^nrom.ROM) {
	assert(len(rom.prg_rom) <= (PRG_ROM_UPPER_END - PRG_ROM_LOWER_BEGIN), "ROM is too big")
	copy(bus.prg_rom, rom.prg_rom)
}

// Returns ~(u16)0 in case of unvalid address 
safe_address :: proc(address: u16) -> u16 {
	switch address {
		case 0x0000 ..< CPU_RAM_MIRRORS_END:
			// The NES BUS has 8KB of RAM, but only 2KB of address space.
			// The address bus is only 11 bits wide, so the 2 most significant bits are ignored.
			// 0x7FF is the highest address in the address space.
			return address & (0b111_1111_1111)
		case PRG_ROM_LOWER_BEGIN ..= (PRG_ROM_UPPER_END - 1):
			return address
		case:
			return ~(u16)(0)
	}
}

// Returns pointer to memory and real address after mirroring
safe_pointer :: #force_inline proc(bus: ^Bus, address: u16) -> ^byte {
	effective_address := safe_address(address)
	return &bus.raw[effective_address]
}
