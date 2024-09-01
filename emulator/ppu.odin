package emulator


import "core:log"

PPU :: struct {
	registers:      PPURegisters,
	raw:            ^[0x10000]byte,
	chr_rom:        []byte,
	palettes:       []byte,
	vram:           []byte,
	pattern_tables: []byte,
	screen:         ScreenStatus,
	data_buffer:    byte,
	cycles:         uint,
	scan_line:      uint,
}

PPURegisters :: struct {
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

ppu_init :: proc(ppu: ^PPU, bus_memory: ^[0x10000]byte) {
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

ppu_write_scroll :: proc(ppu: ^PPU, value: u8) {
	if ppu.registers.w {
		ppu.registers.scroll.x = value
	} else {
		ppu.registers.scroll.y = value
	}
	ppu.registers.w = !ppu.registers.w
}


ppu_get_address :: proc(ppu: ^PPU) -> u16 {
	reg := &ppu.registers.address

	return u16(reg.hi << 8) | u16(reg.lo & 0xFF)
}

@(private = "file")
ppu_set_address :: proc(ppu: ^PPU, value: u16) {
	ppu.registers.address.hi = byte(value >> 8) & 0xFF
	ppu.registers.address.lo = byte(value & 0xFF)
}

@(private = "file")
ppu_increment_address :: proc(ppu: ^PPU) {
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
	if ppu_get_address(ppu) > 0x3FFF {
		ppu_set_address(ppu, ppu_get_address(ppu) & 0x3FFF)
	}
}

ppu_update_address :: proc(ppu: ^PPU, value: u8) {
	reg := &ppu.registers.address

	if ppu.registers.w {
		reg.hi = value
	} else {
		reg.lo = value
	}
	if ppu_get_address(ppu) > 0x3FFF {
		ppu_set_address(ppu, ppu_get_address(ppu) & 0x3FFF)
	}
	ppu.registers.w = !ppu.registers.w
}

@(private = "file")
ppu_mirror_vram :: proc(ppu: ^PPU, addr: u16) -> u16 {
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

ppu_read_data :: proc(ppu: ^PPU) -> u8 {
	address := ppu_get_address(ppu)
	ppu_increment_address(ppu)

	switch address {
	case 0 ..= 0x1fff:
		result := ppu.data_buffer
		ppu.data_buffer = ppu.chr_rom[address]
		return result
	case 0x2000 ..= 0x2fff:
		result := ppu.data_buffer
		ppu.data_buffer = ppu.vram[ppu_mirror_vram(ppu, address)]
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

ppu_write_data :: proc(ppu: ^PPU, value: u8) {
	address := ppu_get_address(ppu)

	switch address {
	case 0 ..= 0x1fff:
		log.warnf("Attempt to write on chr rom [%04X]", address)
	case 0x2000 ..= 0x2fff:
		ppu.vram[ppu_mirror_vram(ppu, address)] = value
	case 0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c:
		address -= 0x10
		fallthrough
	case 0x3f00 ..= 0x3fff:
		ppu.palettes[address - 0x3f00] = value
	case:
		log.panicf("Trying to access unexpected address: %04X", address)
	}
	ppu_increment_address(ppu)
}

tick :: proc(ppu: ^PPU, cycles: uint) -> bool {
	ppu.cycles += cycles
	if ppu.cycles >= 341 {
		ppu.cycles -= 341
		ppu.scan_line += 1

		if ppu.scan_line == 241 {
			if .GenerateNMI in ppu.registers.controller {
				ppu.registers.status^ |= {.VblankStatus}
				// TODO trigger nmi
			}
		} else if ppu.scan_line >= 262 {
			ppu.scan_line = 0
			ppu.registers.status^ &= ~{.VblankStatus}
			return true
		}
	}
	return false
}
