package main

import "core:fmt"
import "core:log"
import "core:strings"
import "emulator/pal"
import "vendor:sdl2"

// This main has no purpose other than to make `odin test` work (by importing the package)

init_sdl_renderer :: proc() -> ^sdl2.Renderer {
	assert(sdl2.Init(sdl2.INIT_EVERYTHING) == 0)
	window := sdl2.CreateWindow(
		"Nem - CHR Rom viewer",
		sdl2.WINDOWPOS_CENTERED,
		sdl2.WINDOWPOS_CENTERED,
		1024,
		720,
		sdl2.WINDOW_SHOWN | sdl2.WINDOW_RESIZABLE,
	)
	assert(window != nil)

	renderer := sdl2.CreateRenderer(window, -1, sdl2.RENDERER_ACCELERATED | sdl2.RENDERER_PRESENTVSYNC)
	assert(renderer != nil)
	assert(sdl2.RenderSetScale(renderer, 100, 100) == 0)
	return renderer
}

get_bg_palette_idxs :: proc(ppu: ^emulator.PPU, tile_col: u8, tile_row: u8) -> [4]byte {
	// fbf = four_by_four block (4x4 blocks share one attribute byte)
	fbf_row := tile_row / 4
	fbf_col := tile_col / 4

	attr_table_idx := fbf_row * 8 + fbf_col
	attr_byte := ppu.vram[0x3C0 + u16(attr_table_idx)]

	// tbt = two_by_two subblock (each 4x4 block contains 4 of these)
	tbt_row := (tile_row % 4) / 2
	tbt_col := (tile_col % 4) / 2
	subtile_index := (tbt_row << 1) | tbt_col // Index within 4x4 block, left to right, top to bottom

	palette_group_index := (attr_byte >> (subtile_index * 2)) & 0b11

	palette_entry_offset := palette_group_index * 4
	return {
		ppu.palettes[0], // Default background color
		ppu.palettes[palette_entry_offset + 1],
		ppu.palettes[palette_entry_offset + 2],
		ppu.palettes[palette_entry_offset + 3],
	}
}

import "core:mem"
import "core:slice"
show_tile :: proc(ppu: ^emulator.PPU, full_palette: pal.PalFile) -> [256 * 256 * 3]byte {
	LINE_LENGTH :: 256 * 3
	frame := [256 * LINE_LENGTH]byte{}

	for tile_y in 0 ..< 30 {
		for tile_x in 0 ..< 32 {
			bg_palette_idxs := get_bg_palette_idxs(ppu, u8(tile_x), u8(tile_y))

			// gets sprite index from nametable
			bank := u16(0)
			tile_address := u16(ppu.vram[tile_y * 32 + tile_x])
			if .BackroundPatternAddr in ppu.registers.controller {
				bank = 0x1000
			}

			// get sprite shape/color data from patterntable (16 bytes)
			tile_data := ppu.chr_rom[bank + tile_address * 16:bank + (tile_address + 1) * 16]

			for pixel_y in 0 ..< 8 {
				bitplane1 := tile_data[pixel_y]
				bitplane2 := tile_data[pixel_y + 8]

				tile_pos_y := tile_y * 8
				target_pixel_y := (tile_pos_y + pixel_y)
				for pixel_x in 0 ..< 8 {
					bit1 := (bitplane1 >> u8(7 - pixel_x)) & 1
					bit2 := (bitplane2 >> u8(7 - pixel_x)) & 1
					pal_index := (bit2 << 1) | bit1

					color_index := bg_palette_idxs[pal_index]

					rgb := full_palette[color_index]

					tile_pos_x := tile_x * 8
					target_pixel_x := (tile_pos_x + pixel_x) * 3

					target_pixel_index := target_pixel_y * LINE_LENGTH + target_pixel_x
					copy(frame[target_pixel_index:], mem.ptr_to_bytes(&rgb)) // just for funsies
				}
			}
		}
	}

	// Auto-convert oam data
	SpriteInfo :: struct {
		y, index:   byte,
		attributes: bit_set[enum {
			PaletteLow,
			PaletteHigh,
			_,
			_,
			_,
			Priority,
			FlipHorizontal,
			FlipVertical,
		};byte],
		x:          byte,
	}
	#assert(size_of(SpriteInfo) == 4)

	sprites := slice.reinterpret([]SpriteInfo, ppu.oam_data[:])

	for sprite in sprites {
		if sprite.y >= 240 {
			continue
		}
		palette_index := transmute(u8)(sprite.attributes) & 0b11
		flip_vertical := .FlipVertical in sprite.attributes
		flip_horizontal := .FlipHorizontal in sprite.attributes

		palette_start := 0x10 + (palette_index * 4)
		ppu_palette_idxs := [4]u8 {
			0,
			ppu.palettes[palette_start + 1],
			ppu.palettes[palette_start + 2],
			ppu.palettes[palette_start + 3],
		}

		bank := u16(0)
		if .SpritePatternAddr in ppu.registers.controller {
			bank = 0x1000
		}
		tile_idx := u16(sprite.index)
		tile_data := ppu.chr_rom[bank + (tile_idx * 16):(bank + (tile_idx + 1) * 16)]

		for y in 0 ..< 8 {
			bitplane1 := tile_data[y]
			bitplane2 := tile_data[y + 8]
			for x in 0 ..< 8 {
				bit1 := (bitplane1 >> u8(7 - x)) & 1
				bit2 := (bitplane2 >> u8(7 - x)) & 1
				inner_palette_index := (bit2 << 1) | bit1

				if inner_palette_index == 0 do continue

				color_index := ppu_palette_idxs[inner_palette_index]
				rgb := full_palette[color_index]
				pixel_x := 0
				pixel_y := 0
				if flip_horizontal {
					pixel_x = int(sprite.x) + 7 - x
				} else {
					pixel_x = int(sprite.x) + x
				}
				if flip_vertical {
					pixel_y = int(sprite.y) + 7 - y
				} else {
					pixel_y = int(sprite.y) + y
				}
				target_pixel_index := pixel_y * LINE_LENGTH + pixel_x * 3
				copy(frame[target_pixel_index:], mem.ptr_to_bytes(&rgb)) // just for funsies
			}
		}

	}


	return frame
}
import "core:os"
import "emulator"
renderer: ^sdl2.Renderer
tile: [256 * 256 * 3]byte
p: pal.PalFile
@(private = "file")
sb := &strings.Builder{}

@(private = "file")
string_builder_dump :: proc(format: string, args: ..any) {
	fmt.sbprintf(sb, format, ..args)
}

main :: proc() {
	when ODIN_DEBUG {
		context.logger = log.create_console_logger()
	}

	console := emulator.new_console()

	renderer = init_sdl_renderer()

	//_ = emulator.new_console()
	r, rok := emulator.rom_read("./pacman.nes")
	if !rok {
		return
	}
	emulator.cpu_init(console.cpu, console.bus)
	emulator.bus_load_rom(console.bus, &r)
	console.ppu.scan_line = 0
	console.ppu.cycles = 0
	emulator.cpu_reset_interupt(console.cpu)

	_p, pok := pal.load("./palettes/2C02G_wiki.pal")
	if !pok {
		return
	}
	p = _p
	log.info("palette successfuly loaded")
	console.bus.callback = proc(ppu: ^emulator.PPU) {
		tile = show_tile(ppu, p)
		texture := sdl2.CreateTexture(renderer, sdl2.PixelFormatEnum.RGB24, sdl2.TextureAccess.TARGET, 256, 256)
		defer sdl2.DestroyTexture(texture)
		assert(texture != nil)
		sdl2.UpdateTexture(texture, nil, raw_data(tile[:]), 256 * 3)
		sdl2.RenderCopy(renderer, texture, nil, nil)
		sdl2.RenderPresent(renderer)

		event: sdl2.Event
		for (sdl2.PollEvent(&event)) {
			#partial switch event.type {
			case .QUIT:
				os.exit(0)
			}
		}
	}
	for {
		emulator.cpu_handle_instruction(console.cpu)
	}
}
