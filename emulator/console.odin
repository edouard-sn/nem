package emulator

import ncpu "cpu"
import nppu "ppu"

Console :: struct {
	bus: ^ncpu.Bus,
	cpu: ^ncpu.CPU,
	ppu: ^nppu.PPU,
}

new_bus :: proc() -> ^ncpu.Bus
{
	bus := new(ncpu.Bus)
	ncpu.init_bus(bus)
	return bus
}

new_ppu :: proc(bus: ^ncpu.Bus) -> ^nppu.PPU
{
	ppu := new(nppu.PPU)
	nppu.init_ppu(ppu, bus.raw)
	bus.ppu = ppu
	return ppu
}

new_cpu :: proc(bus: ^ncpu.Bus) -> ^ncpu.CPU
{
	cpu := new(ncpu.CPU)
	ncpu.init_cpu(cpu, bus)
	return cpu
}

new_console :: proc() -> ^Console {
	console := new(Console)

	bus := new_bus()
	console^ = Console{
		bus = bus,
		cpu = new_cpu(bus),
		ppu = new_ppu(bus),
	}
	return console
}
