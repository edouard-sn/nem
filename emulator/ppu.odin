package emulator


import "core:log"

PPU :: struct {
	registers:   PPURegisters,
	raw:         ^[0x10000]byte, // TODO: Make it so that raw represents the real PPU memory
	chr_rom:     []byte,
	palettes:    [32]byte,
	vram:        [2048]byte,
	oam_data:    [256]byte,
	screen:      ScreenStatus,
	data_buffer: byte,
	cycles:      uint,
	scan_line:   uint,
	nmi_raised:  bool,
}

PPURegisters :: struct {
	controller:  ^ControlRegister,
	mask:        ^byte,
	status:      ^StatusRegister,
	oam_address: byte,
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
};byte]

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
			// FIXME: don't map to bus memory lol
			controller = cast(^ControlRegister)&bus_memory^[0x2000],
			mask       = &bus_memory[0x2001],
			status     = cast(^StatusRegister)&bus_memory^[0x2002],
			data       = &bus_memory[0x2007],
			oam_dma    = &bus_memory[0x4014],
			w          = false,
		},
	}
}

ppu_write_scroll :: proc(ppu: ^PPU, value: u8) {
	if !ppu.registers.w {
		ppu.registers.scroll.x = value
	} else {
		ppu.registers.scroll.y = value
	}
	ppu.registers.w = !ppu.registers.w
}


ppu_get_address :: proc(ppu: ^PPU) -> u16 {
	reg := &ppu.registers.address

	return u16(reg.hi) << 8 | u16(reg.lo & 0xFF)
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

	if .VramAddIncrement in ppu.registers.controller^ {
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

	if !ppu.registers.w {
		reg.hi = value
	} else {
		reg.lo = value
	}
	if ppu_get_address(ppu) > 0x3FFF {
		ppu_set_address(ppu, ppu_get_address(ppu) & 0x3FFF)
	}
	ppu.registers.w = !ppu.registers.w
}

ppu_mirror_vram :: proc(ppu: ^PPU, addr: u16) -> u16 {
	mirrored_vram := u16(0)
	if addr >= 0x3000 && addr < 0x3F00 {
		mirrored_vram = addr - 0x1000
	} else {
		mirrored_vram = addr
	}
	vram_index: u16 = mirrored_vram - 0x2000 // to vram vector
	name_table: u16 = vram_index / 0x400 // to the name table index

	parameters := struct {
		_: ScreenStatus,
		_: u16,
	}{ppu.screen, name_table}

	switch parameters {
	case {.Vertical, 2}:
		fallthrough
	case {.Vertical, 3}:
		fallthrough
	case {.Horizontal, 3}:
		return vram_index - 0x800
	case {.Horizontal, 2}:
		fallthrough
	case {.Horizontal, 1}:
		return vram_index - 0x400
	case:
		return vram_index
	}
}

ppu_read_data :: proc(ppu: ^PPU) -> u8 {
	address := ppu_get_address(ppu)
	ppu_increment_address(ppu)

	result := ppu.registers.data^
	switch address 
	{
	case 0 ..= 0x1fff:
		ppu.registers.data^ = ppu.chr_rom[address]
	case 0x2000 ..= 0x2fff:
		ppu.registers.data^ = ppu.vram[ppu_mirror_vram(ppu, address)]
	case 0x3f10, 0x3f14, 0x3f18, 0x3f1c:
		address -= 0x10
		fallthrough
	case 0x3f00 ..= 0x3fff:
		return ppu.palettes[address - 0x3f00]
	case:
		log.panicf("Trying to access unexpected address: %04X", address)
	}
	return result
}

import "core:fmt"

ppu_write_data :: proc(ppu: ^PPU, value: u8) {
	address := ppu_get_address(ppu)

	switch address 
	{
	case 0 ..= 0x1fff:
		log.warnf("Attempt to write on chr rom [%04X], testing dangerous shit now", address)
	case 0x2000 ..= 0x2fff:
		ppu.vram[ppu_mirror_vram(ppu, address)] = value
	case 0x3f10, 0x3f14, 0x3f18, 0x3f1c:
		address -= 0x10
		fallthrough
	case 0x3f00 ..= 0x3fff:
		fmt.printf("addr = %02X, value = %02X\n", address, value)
		ppu.palettes[address - 0x3f00] = value
	case:
		log.panicf("Trying to access unexpected address: %04X", address)
	}
	ppu_increment_address(ppu)
}

ppu_is_nmi_raised :: proc(ppu: ^PPU) -> bool {
	result := ppu.nmi_raised
	if ppu.nmi_raised {
		ppu.nmi_raised = false
	}
	return result
}

ppu_write_controller :: proc(ppu: ^PPU, value: u8) {
	nmi_status_before := (.GenerateNMI in ppu.registers.controller^)
	ppu.registers.controller^ = transmute(ControlRegister)value
	if (!nmi_status_before && .GenerateNMI in ppu.registers.controller^ && .VblankStatus in ppu.registers.status) {
		ppu.nmi_raised = true
	}
}

ppu_tick :: proc(ppu: ^PPU, cycles: uint) -> bool {
	ppu.cycles += cycles
	if ppu.cycles >= 341 {
		ppu.cycles -= 341
		ppu.scan_line += 1

		if ppu.scan_line == 241 {
			ppu.registers.status^ |= {.VblankStatus}
			if .GenerateNMI in ppu.registers.controller^ {
				ppu.nmi_raised = true
			}
		} else if ppu.scan_line >= 262 {
			ppu.scan_line = 0
			ppu.registers.status^ &= ~{.VblankStatus}
			return true
		}
	}
	return false
}
