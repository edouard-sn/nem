package emulator

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
	raw:         ^[0x10000]byte,

	//  bus slices
	ram:         []byte,
	ram_map:     struct {
		zero_page: []byte,
		stack:     []byte,
		ram:       []byte,
	},
	prg_rom:     []byte,
	callback:    proc(_: ^PPU), // FIXME: that's bollocks
	ppu:         ^PPU,
	dma_address: u16,
}

bus_init :: proc(bus: ^Bus) {
	bus.raw = new([0x10000]byte)

	// CPU RAM map init 
	bus.ram = bus.raw[CPU_RAM_MAP_BEGIN:CPU_RAM_MAP_END]

	bus.ram_map.zero_page = bus.raw[CPU_ZERO_PAGE_BEGIN:CPU_ZERO_PAGE_END]
	bus.ram_map.stack = bus.raw[CPU_STACK_BEGIN:CPU_STACK_END]
	bus.ram_map.ram = bus.raw[CPU_RAM_BEGIN:CPU_RAM_END]

	bus.prg_rom = bus.raw[PRG_ROM_LOWER_BEGIN:PRG_ROM_UPPER_END]
}

bus_destroy :: proc(bus: ^Bus) {
	free(bus.raw)
	bus^ = Bus{}
}

bus_load_rom :: proc(bus: ^Bus, rom: ^ROM) {
	assert(len(rom.prg_rom) <= (PRG_ROM_UPPER_END - PRG_ROM_LOWER_BEGIN), "ROM is too big")
	// Allowed sizes: 16KB or 32KB.
	if len(rom.prg_rom) == 0x4000 {
		copy(bus.prg_rom, rom.prg_rom)
		copy(bus.prg_rom[0x4000:], rom.prg_rom)
	} else if len(rom.prg_rom) == 0x8000 {
		copy(bus.prg_rom, rom.prg_rom)
	} else {
		log.panicf("Unexpected ROM size: %d bytes", len(rom.prg_rom))
	}
	bus.ppu.chr_rom = rom.chr_rom
	bus.ppu.screen = rom.screen
}

bus_read_byte :: proc(bus: ^Bus, address: u16, unsafe := false) -> byte {
	switch address 
	{
	case 0x0000 ..< CPU_RAM_MIRRORS_END:
		// The NES BUS has 8KB of RAM, but only 2KB of address space.
		// The address bus is only 11 bits wide, so the 2 most significant bits are ignored.
		// 0x7FF is the highest address in the address space.
		return bus.raw[address & (0b111_1111_1111)]
	case 0x2000, 0x2001, 0x2003, 0x2005, 0x2006, 0x4014:
		if unsafe {
			return bus.raw[address]
		} else {
			log.panicf("Trying to access write-only PPU registers [%04X]", address)
		}
	case 0x2002:
		result := bus.ppu.registers.status^
		bus.ppu.registers.status^ &= ~{.VblankStatus}
		bus.ppu.registers.w = false
		return transmute(u8)result
	case 0x2004:
		return bus.ppu.oam_data[bus.ppu.registers.oam_address]
	case 0x2007:
		return ppu_read_data(bus.ppu)
	case 0x2008 ..= 0x3FFF:
		return bus_read_byte(bus, address & 0b00100000_00000111)
	case 0x8000 ..= 0xFFFF:
		addr := address - 0x8000
		if addr >= 0x4000 {
			addr = addr % 0x4000
		}
		return bus.prg_rom[addr]
	case:
		log.warnf("Weird memory access [%04X]", address)
		return bus.raw[address]

	}
	return 0
}
bus_write_byte :: proc(bus: ^Bus, address: u16, value: byte) {
	switch address 
	{
	case 0x0000 ..= 0x1FFF:
		bus.raw[address & (0b111_1111_1111)] = value
	case 0x2000:
		ppu_write_controller(bus.ppu, value)
	case 0x2001:
		bus.ppu.registers.mask^ = value
	case 0x2002:
		log.panic("Attempt to write on read-only status register (0x2002)")
	// bus.ppu.registers.status^ = transmute(StatusRegister)value
	case 0x2003:
		bus.ppu.registers.oam_address = value
	case 0x2004:
		bus.ppu.oam_data[bus.ppu.registers.oam_address] = value
		bus.ppu.registers.oam_address += 1
	case 0x2005:
		ppu_write_scroll(bus.ppu, value)
	case 0x2006:
		ppu_update_address(bus.ppu, value)
	case 0x2007:
		ppu_write_data(bus.ppu, value)
	case 0x2008 ..= 0x3FFF:
		bus_write_byte(bus, address & 0b0010_0000_0000_0111, value)
	case 0x8000 ..= 0xFFFF:
		log.panic("Attempt to write to Cartridge ROM space: %04X", address)
	case 0x4014:
		bus.dma_address = u16(value) << 8
	case:
		log.infof("Ignoring mem write-access at %04X", address)
	}
}

bus_unsafe_read_u16 :: #force_inline proc(bus: ^Bus, address: u16) -> u16 {
	return u16(bus_read_byte(bus, address, true)) | (u16(bus_read_byte(bus, address + 1, true)) << 8)
}

bus_read_u16 :: #force_inline proc(bus: ^Bus, address: u16, unsafe := false) -> u16 {
	return u16(bus_read_byte(bus, address, unsafe)) | (u16(bus_read_byte(bus, address + 1, unsafe)) << 8)
}

bus_write_u16 :: #force_inline proc(bus: ^Bus, address: u16, value: u16) {
	bus_write_byte(bus, address, u8(value & 0xFF))
	bus_write_byte(bus, address + 1, u8(value >> 8))
}

bus_tick :: proc(bus: ^Bus, cycles: uint) {
	nmi_before := bus.ppu.nmi_raised
	ppu_tick(bus.ppu, cycles * 3)
	nmi_after := bus.ppu.nmi_raised
	if !nmi_before && nmi_after {
		bus.callback(bus.ppu)
	}
}
