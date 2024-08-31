package ppu

import nrom "../rom"
import "core:log"

PPU :: struct {
	registers:      Registers,
	raw:            ^[0x10000]byte,
	chr_rom:        []byte,
	palettes:       []byte,
	vram:           []byte,
	pattern_tables: []byte,
	screen:         nrom.ScreenStatus,
	data_buffer:    byte,
}

Registers :: struct {
	controller:  ^ControlRegister,
	mask:        ^byte,
	status:      ^StatusRegister,
	oam_address: ^byte,
	oam_data:    ^byte,
	scroll:      struct {
		x: u8,
		y: u8,
	},
	address:     AddressRegister,
	data:        ^byte,
	oam_dma:     ^byte,
	w:           bool,
	v:           byte,
	t:           byte,
	x:           byte,
}

StatusRegister :: bit_set[enum {
	_,
	_,
	_,
	_,
	_,
	SpriteOverflow,
	SpriteZeroHit,
	VblankStatus,
};u8]

ControlRegister :: bit_set[enum {
	Nametable1,
	Nametable2,
	VramAddIncrement,
	SpritePatternAddr,
	BackroundPatternAddr,
	SpriteSize,
	MasterSlaveSelect,
	GenerateNMI,
};byte]

AddressRegister :: struct {
	lo: byte,
	hi: byte,
}

init_ppu :: proc(ppu: ^PPU, bus_memory: ^[0x10000]byte) {
	ppu^ = PPU {
		registers = {
			controller = cast(^ControlRegister)&bus_memory[0x2000],
			mask = &bus_memory[0x2001],
			status = cast(^StatusRegister)&bus_memory[0x2002],
			oam_address = &bus_memory[0x2003],
			oam_data = &bus_memory[0x2004],
			data = &bus_memory[0x2007],
			oam_dma = &bus_memory[0x4014],
			w = true,
		},
	}
}

write_scroll :: proc(ppu: ^PPU, value: u8) {
	if ppu.registers.w {
		ppu.registers.scroll.x = value
	} else {
		ppu.registers.scroll.y = value
	}
	ppu.registers.w = !ppu.registers.w
}


get_ppu_address_reg :: proc(ppu: ^PPU) -> u16 {
	reg := &ppu.registers.address

	return u16(reg.hi << 8) | u16(reg.lo & 0xFF)
}

set_ppu_address_reg :: proc(ppu: ^PPU, value: u16) {
	ppu.registers.address.hi = byte(value >> 8) & 0xFF
	ppu.registers.address.lo = byte(value & 0xFF)
}

incr_ppu_address_reg :: proc(ppu: ^PPU) {
	reg := &ppu.registers.address
	old_lo := reg.lo

	if .VramAddIncrement in ppu.registers.controller {
		reg.lo += 32
	} else {
		reg.lo += 1
	}
	if old_lo > reg.lo {
		reg.hi += 1
	}
	if get_ppu_address_reg(ppu) > 0x3FFF {
		set_ppu_address_reg(ppu, get_ppu_address_reg(ppu) & 0x3FFF)
	}
}

update_ppu_address_reg :: proc(ppu: ^PPU, value: u8) {
	reg := &ppu.registers.address

	if ppu.registers.w {
		reg.hi = value
	} else {
		reg.lo = value
	}
	if get_ppu_address_reg(ppu) > 0x3FFF {
		set_ppu_address_reg(ppu, get_ppu_address_reg(ppu) & 0x3FFF)
	}
	ppu.registers.w = !ppu.registers.w
}

@(private)
mirror_vram :: proc(ppu: ^PPU, addr: u16) -> u16 {
	mirrored_vram: u16 = addr & 0b1011_1111_1111_1111 // mirror down 0x3000-0x3eff to 0x2000-0x2eff
	vram_index: u16 = mirrored_vram - 0x2000 // to vram vector
	name_table: u16 = vram_index / 0x400 // to the name table index

	switch {
	case ppu.screen == .Vertical && name_table == 2, ppu.screen == .Vertical && name_table == 3:
		return vram_index - 0x800

	case ppu.screen == .Horizontal && name_table == 2, ppu.screen == .Horizontal && name_table == 1:
		return vram_index - 0x400

	case ppu.screen == .Horizontal && name_table == 3:
		return vram_index - 0x800
	case:
		return vram_index
	}
}

read_data :: proc(ppu: ^PPU) -> u8 {
	address := get_ppu_address_reg(ppu)
	incr_ppu_address_reg(ppu)

	switch address {
	case 0 ..= 0x1fff:
		result := ppu.data_buffer
		ppu.data_buffer = ppu.chr_rom[address]
		return result
	case 0x2000 ..= 0x2fff:
		result := ppu.data_buffer
		ppu.data_buffer = ppu.vram[mirror_vram(ppu, address)]
		return result
	case 0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c:
		address -= 0x10
	case 0x3f00 ..= 0x3fff:
		return ppu.palettes[address - 0x3f00]
	case:
		log.panicf("Trying to access unexpected address: %04X", address)
	}
	return 0
}

write_data :: proc(ppu: ^PPU, value: u8) {
	address := get_ppu_address_reg(ppu)

	switch address {
	case 0 ..= 0x1fff:
		log.warnf("Attempt to write on chr rom [%04X]", address)
	case 0x2000 ..= 0x2fff:
		ppu.vram[mirror_vram(ppu, address)] = value
	case 0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c:
		address -= 0x10
		fallthrough
	case 0x3f00 ..= 0x3fff:
		ppu.palettes[address - 0x3f00] = value
	case:
		log.panicf("Trying to access unexpected address: %04X", address)
	}
	incr_ppu_address_reg(ppu)
}
