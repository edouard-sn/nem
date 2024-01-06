package main

import "vendor:sdl2"
import "core:os"
import nem_cpu "emulator/cpu"

handle_user_input :: proc(cpu: ^nem_cpu.CPU) {
    event: sdl2.Event

    for (sdl2.PollEvent(&event)) {
        #partial switch event.type {
            case .QUIT:
                os.exit(0)
            // case sdl
            case .KEYDOWN:
                key_memory := &cpu.memory.raw[0xFF]
                #partial switch event.key.keysym.sym {
                    case .z:
                        key_memory^ = 0x77
                    case .q:
                        key_memory^ = 0x61
                    case .s:
                        key_memory^ = 0x73
                    case .d:
                        key_memory^ = 0x64
                }
        }
    }

}

cpu_color_to_sdl_map := [16]sdl2.Color{
    0 = sdl2.Color{0, 0, 0, 255},
    1 = sdl2.Color{255, 255, 255, 255},
    2 = sdl2.Color{128, 128, 128, 255},
    3 = sdl2.Color{255, 0, 0, 255},
    4 = sdl2.Color{0, 255, 0, 255},
    5 = sdl2.Color{0, 0, 255, 255},
    6 = sdl2.Color{255, 0, 255, 255},
    7 = sdl2.Color{255, 255, 0, 255},
    8 = sdl2.Color{0, 255, 255, 255},
    9 = sdl2.Color{128, 128, 128, 255},
    10 = sdl2.Color{255, 0, 0, 255},
    11 = sdl2.Color{0, 255, 0, 255},
    12 = sdl2.Color{0, 0, 255, 255},
    13 = sdl2.Color{255, 0, 255, 255},
    14 = sdl2.Color{255, 255, 0, 255},
    15 = sdl2.Color{0, 255, 255, 255},
}

read_screen_state :: proc(cpu: ^nem_cpu.CPU, array: ^[32 * 3 * 32]u8) -> bool {
    frame_idx := 0
    update := false

    for adr in 0x0200 ..< 0x600 {
        color_index := cpu.memory.raw[adr]
        c := cpu_color_to_sdl_map[color_index % 16]
        if array[frame_idx] != c.r || array[frame_idx + 1] != c.g || array[frame_idx + 2] != c.b {
            update = true
            array[frame_idx] = c.r
            array[frame_idx + 1] = c.g
            array[frame_idx + 2] = c.b
        }
        frame_idx += 3
    }
    return update
}

graphical_callback :: proc(
    cpu: ^nem_cpu.CPU,
    frame: ^[32 * 3 * 32]u8,
    texture: ^sdl2.Texture,
    renderer: ^sdl2.Renderer,
) {
    if (int(cpu.registers.program_counter) > 0x600 + len(snake_program)) {
        os.exit(0)
    }
    handle_user_input(cpu)

    if (read_screen_state(cpu, frame)) {
        sdl2.UpdateTexture(texture, nil, frame, 32 * 3)
        sdl2.RenderCopy(renderer, texture, nil, nil)
        sdl2.RenderPresent(renderer)
    }
}

init_sdl_renderer :: proc() -> ^sdl2.Renderer {
    assert(sdl2.Init(sdl2.INIT_EVERYTHING) == 0)
    window := sdl2.CreateWindow(
        "NEM",
        sdl2.WINDOWPOS_CENTERED,
        sdl2.WINDOWPOS_CENTERED,
        (32 * 10),
        (32 * 10),
        sdl2.WINDOW_SHOWN,
    )
    assert(window != nil)

    renderer := sdl2.CreateRenderer(window, -1, sdl2.RENDERER_ACCELERATED | sdl2.RENDERER_PRESENTVSYNC)
    assert(renderer != nil)
    assert(sdl2.SetHint(sdl2.HINT_RENDER_SCALE_QUALITY, "liznear") == true)
    assert(sdl2.RenderSetScale(renderer, 10, 10) == 0)
    return renderer
}
    