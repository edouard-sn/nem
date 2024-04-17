package emulator

import "cpu"

Console :: struct {
	bus: ^cpu.Bus,
	cpu: ^cpu.CPU,
}

new_console :: proc(bus: ^bus.Bus, cpu: ^cpu.CPU) -> Console {
	console := Console {
		bus = bus,
		cpu = cpu,
	}
	return &console
}
