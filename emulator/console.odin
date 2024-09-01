package emulator


Console :: struct {
	bus: ^Bus,
	cpu: ^CPU,
	ppu: ^PPU,
}

bus_new :: proc() -> ^Bus {
	bus := new(Bus)
	bus_init(bus)
	return bus
}

new_ppu :: proc(bus: ^Bus) -> ^PPU {
	ppu := new(PPU)
	ppu_init(ppu, bus.raw)
	bus.ppu = ppu
	return ppu
}

new_cpu :: proc(bus: ^Bus) -> ^CPU {
	cpu := new(CPU)
	cpu_init(cpu, bus)
	return cpu
}

new_console :: proc() -> ^Console {
	console := new(Console)

	bus := bus_new()
	console^ = Console {
		bus = bus,
		cpu = new_cpu(bus),
		ppu = new_ppu(bus),
	}
	return console
}

destroy_console :: proc(console: ^Console) {
	free(console.ppu)
	free(console.cpu)
	bus_destroy(console.bus)
	free(console.bus)
	free(console)

}
