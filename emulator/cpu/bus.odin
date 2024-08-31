package cpu

import nppu "../ppu"
import nrom "../rom"
import "core:log"
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
	ppu:      ^nppu.PPU,
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

destroy_bus :: proc(bus: ^Bus) {
	free(bus.raw)
	bus^ = Bus{}
}

load_rom :: proc(bus: ^Bus, rom: ^nrom.ROM) {
	assert(len(rom.prg_rom) <= (PRG_ROM_UPPER_END - PRG_ROM_LOWER_BEGIN), "ROM is too big")
	copy(bus.prg_rom, rom.prg_rom)
}

@(private)
unsafe_read :: #force_inline proc(cpu: ^CPU, address: u16) -> u8 {
	return cpu.memory.raw[address]
}

@(private)
unsafe_read_u16 :: #force_inline proc(cpu: ^CPU, address: u16) -> u16 {
	return u16(unsafe_read(cpu, address)) | (u16(unsafe_read(cpu, address + 1)) << 8)
}

read_byte :: proc(cpu: ^CPU, address: u16) -> byte {
	switch address {
	case 0x0000 ..< CPU_RAM_MIRRORS_END:
		// The NES BUS has 8KB of RAM, but only 2KB of address space.
		// The address bus is only 11 bits wide, so the 2 most significant bits are ignored.
		// 0x7FF is the highest address in the address space.
		return cpu.memory.raw[address & (0b111_1111_1111)]
	case 0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014:
		log.panicf("Trying to access write-only PPU registers [%04X]", address)
	case 0x2002:
		result := cpu.memory.ppu.registers.status^
		cpu.memory.ppu.registers.status^ &= ~{.VblankStatus}
		cpu.memory.ppu.registers.w = false
		return transmute(u8)result
	case 0x2004:
		return cpu.memory.ppu.registers.oam_data^
	case 0x2007:
		return nppu.read_data(cpu.memory.ppu)
	case 0x2008 ..= 0x3FFF:
		return read_byte(cpu, address & 0b00100000_00000111)
	case 0x8000 ..= 0xFFFF:
		addr := address - 0x8000
		if addr >= 0x4000 {
			addr = addr % 0x4000
		}
		return cpu.memory.prg_rom[addr]
	case:
		log.warnf("Weird memory access [%04X]", address)
		return cpu.memory.raw[address]

	}
	return 0
}

write_byte :: proc(cpu: ^CPU, address: u16, value: byte) {
	switch address {
	case 0x0000 ..= 0x1FFF:
		cpu.memory.raw[address & (0b111_1111_1111)] = value
	case 0x2000:
		cpu.memory.ppu.registers.controller^ = transmute(nppu.ControlRegister)value
	case 0x2003:
		cpu.memory.ppu.registers.oam_address^ = value
	case 0x2004:
		cpu.memory.ppu.registers.oam_data^ = value
		cpu.memory.ppu.registers.oam_address^ += 1
	case 0x2005:
		nppu.write_scroll(cpu.memory.ppu, value)
	case 0x2006:
		nppu.update_ppu_address_reg(cpu.memory.ppu, value)
	case 0x2007:
		nppu.write_data(cpu.memory.ppu, value)
	case 0x2008 ..= 0x3FFF:
		write_byte(cpu, address & 0b00100000_00000111, value)
	case 0x8000 ..= 0xFFFF:
		log.panic("Attempt to write to Cartridge ROM space: %04X", address)
	case:
		log.infof("Ignoring mem write-access at %04X", address)
	}
}

read_u16 :: #force_inline proc(cpu: ^CPU, address: u16) -> u16 {
	return u16(read_byte(cpu, address)) | (u16(read_byte(cpu, address + 1)) << 8)
}

write_u16 :: #force_inline proc(cpu: ^CPU, address: u16, value: u16) {
	write_byte(cpu, address, u8(value & 0xFF))
	write_byte(cpu, address + 1, u8(value >> 8))
}
