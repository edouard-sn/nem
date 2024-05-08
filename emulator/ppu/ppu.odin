package ppu

import nrom "../rom"

PPU :: struct {
	registers: struct {
		controller:  ^byte,
		mask:        ^byte,
		status:      ^byte,
		oam_address: ^byte,
		oam_data:    ^byte,
		scroll:      ^byte,
		address:     ^byte,
		data:        ^byte,
		oam_dma:     ^byte,
	},
    raw:      ^[0x10000]byte,
    palettes: []byte,
    vram: []byte,
    pattern_tables: []byte,

    screen: nrom.ScreenStatus,
}

init_ppu :: proc(ppu: ^PPU, bus_memory: ^[0x10000]byte) {
    ppu^ = PPU {
        registers =  {
            controller = &bus_memory[0x2000],
            mask = &bus_memory[0x2001],
            status = &bus_memory[0x2002],
            oam_address = &bus_memory[0x2003],
            oam_data = &bus_memory[0x2004],
            scroll = &bus_memory[0x2005],
            address = &bus_memory[0x2006],
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
            effective := 0

            if ( ppu.screen == .Vertical && name_table > 1 ) {
                return mirrored - 0x800
            } else if ( ppu.screen == .Horizontal && (1 <= name_table && name_table <= 2)) {
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