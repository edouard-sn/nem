package ppu

import nrom "../rom"

PPU :: struct {
	registers:      Registers,
	raw:            ^[0x10000]byte,
	palettes:       []byte,
	vram:           []byte,
	pattern_tables: []byte,
	screen:         nrom.ScreenStatus,
}

Registers :: struct {
	controller:  ^byte,
	mask:        ^byte,
	status:      ^byte,
	oam_address: ^byte,
	oam_data:    ^byte,
	scroll:      ^byte,
	address:     AddressRegister,
	data:        ^byte,
	oam_dma:     ^byte,
}

AddressRegister :: struct {
	// name of the high byte / low byte toggle
	w:   bool,
	lo:  byte,
	hi:  byte,
	ptr: ^byte,
}

init_ppu :: proc(ppu: ^PPU, bus_memory: ^[0x10000]byte) {
	ppu^ = PPU {
		registers = {
			controller = &bus_memory[0x2000],
			mask = &bus_memory[0x2001],
			status = &bus_memory[0x2002],
			oam_address = &bus_memory[0x2003],
			oam_data = &bus_memory[0x2004],
			scroll = &bus_memory[0x2005],
			address = {w = true, ptr = &bus_memory[0x2006]},
			data = &bus_memory[0x2007],
			oam_dma = &bus_memory[0x4014],
		},
	}
}

// Returns ~(u16)0 in case of unvalid address 
safe_address :: proc(ppu: ^PPU, address: u16) -> u16 {
	switch address {
	case 0x0000 ..< 0x2000:
		// chr_rom
		return address
	case 0x2000 ..< 0x3000:
		mirrored := address & 0b10111111111111 // mirror 0x3000-0x3eff to 0x2000-0x2eff
		vram_index := mirrored - 0x2000
		name_table := vram_index / 0x400

		if (ppu.screen == .Vertical && name_table > 1) {
			return mirrored - 0x800
		} else if (ppu.screen == .Horizontal && (1 <= name_table && name_table <= 2)) {
			return mirrored - 0x400
		}
		return mirrored
	case 0x3000 ..< 0x3f00:
		panic("Not supposed to access 0x3000 ..< 0x3eff PPU address space")
	case 0x3f00 ..< 0x4000:
		// palette table
		return address
	case:
		return 0
	}
}

get_ppu_address_reg :: proc(ppu: ^PPU) -> u16 {
	reg := &ppu.registers.address

	return u16(reg.hi << 8) | u16(reg.lo & 0xFF)
}

set_ppu_address_reg :: proc(ppu: ^PPU, value: u16) {
	ppu.registers.address.hi = byte(value >> 8) & 0xFF
	ppu.registers.address.lo = byte(value & 0xFF)
}

incr_ppu_address_reg :: proc(ppu: ^PPU, incr: u8) {
	reg := &ppu.registers.address
	old_lo := reg.lo

	reg.lo += 1
	if old_lo > reg.lo {
		reg.hi += 1
	}
	if get_ppu_address_reg(ppu) > 0x3FFF {
		set_ppu_address_reg(ppu, get_ppu_address_reg(ppu) & 0x3FFF)
	}
}

update_ppu_address_reg :: proc(ppu: ^PPU, value: u8) {
	reg := &ppu.registers.address

	if reg.w {
		reg.hi = value
	} else {
		reg.lo = value
	}
	if get_ppu_address_reg(ppu) > 0x3FFF {
		set_ppu_address_reg(ppu, get_ppu_address_reg(ppu) & 0x3FFF)
	}
	reg.w = !reg.w
}
