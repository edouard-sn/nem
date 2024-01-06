package emulator

RAM_MAP_BEGIN :: ZERO_PAGE_BEGIN
RAM_MAP_END :: RAM_MIRRORS_END
ZERO_PAGE_BEGIN :: 0x0000
ZERO_PAGE_END :: 0x0100
STACK_BEGIN :: 0x0100
STACK_END :: 0x0200
RAM_BEGIN :: 0x0200
RAM_END :: 0x0800
RAM_MIRRORS_BEGIN :: 0x0800
RAM_MIRRORS_END :: 0x2000

IO_MAP_BEGIN :: IO_FIRST_BEGIN
IO_MAP_END :: IO_SECOND_END
IO_FIRST_BEGIN :: 0x2000
IO_FIRST_END :: 0x2008
IO_SECOND_BEGIN :: 0x4000
IO_SECOND_END :: 0x4020

IO_MIRRORS_BEGIN :: 0x2008
IO_MIRRORS_END :: 0x4000

EXPANSION_ROM_BEGIN :: 0x4020
EXPANSION_ROM_END :: 0x6000

SRAM_BEGIN :: 0x6000
SRAM_END :: 0x8000

PRG_ROM_LOWER_BEGIN :: 0x8000
PRG_ROM_LOWER_END :: 0xC000
PRG_ROM_UPPER_BEGIN :: 0xC000
PRG_ROM_UPPER_END :: 0x10000

@(private)
CPU_MEMORY_SIZE :: 0x10000
Bus :: struct {
	raw:              ^[CPU_MEMORY_SIZE]byte,

	//  bus slices
	ram:              []byte,
	ram_map:          struct {
		zero_page: []byte,
		stack:     []byte,
		ram:       []byte,
		mirrors:   []byte,
	},
	io_registers:     []byte,
	io_registers_map: struct {
		first:   []byte,
		mirrors: []byte,
		second:  []byte,
	},
	expansion_rom:    []byte,
	sram:             []byte,
	prg_rom:          struct {
		lower: []byte,
		upper: []byte,
	},
}

new_bus :: proc() -> Bus {
	bus: Bus

	bus.raw = new([CPU_MEMORY_SIZE]byte)

	// CPU RAM map init
	bus.ram = bus.raw[RAM_MAP_BEGIN:RAM_MAP_END]

	bus.ram_map.zero_page = bus.raw[ZERO_PAGE_BEGIN:ZERO_PAGE_END]
	bus.ram_map.stack = bus.raw[STACK_BEGIN:STACK_END]
	bus.ram_map.ram = bus.raw[RAM_BEGIN:RAM_END]
	bus.ram_map.mirrors = bus.raw[RAM_MIRRORS_BEGIN:RAM_MIRRORS_END]


	// CPU IO Registers map init
	bus.io_registers = bus.raw[IO_MAP_BEGIN:IO_MAP_END]
	bus.io_registers_map.first = bus.raw[IO_FIRST_BEGIN:IO_FIRST_END]
	bus.io_registers_map.mirrors = bus.raw[IO_MIRRORS_BEGIN:IO_MIRRORS_END]
	bus.io_registers_map.second = bus.raw[IO_SECOND_BEGIN:IO_SECOND_END]

	// CPU Expansion ROM map init
	bus.expansion_rom = bus.raw[EXPANSION_ROM_BEGIN:EXPANSION_ROM_END]

	// CPU Expansion SRAM map init
	bus.sram = bus.raw[SRAM_BEGIN:SRAM_END]

	// CPU Expansion PRG ROM map init
	bus.prg_rom.lower = bus.raw[PRG_ROM_LOWER_BEGIN:PRG_ROM_LOWER_END]
	bus.prg_rom.upper = bus.raw[PRG_ROM_UPPER_BEGIN:PRG_ROM_UPPER_END]
	return bus
}

mirror_safe_address :: proc(address: u16) -> u16 {
	switch address {
		case 0x0000 ..= 0x1FFF:
			// The NES BUS has 8KB of RAM, but only 2KB of address space.
			// The address bus is only 11 bits wide, so the 2 most significant bits are ignored.
			// 0x7FF is the highest address in the address space.
			// The modulo permits mirroring.
			return address % 0x0800 
		case:
			return address
		}
	return 0
}

// Mirroring-safe data access
read_byte :: proc(bus: ^Bus, address: u16) -> byte {
	return bus.raw[mirror_safe_address(address)]
}

// Mirroring-safe data modification
write_byte :: proc(bus: ^Bus, address: u16, value: byte) {
	bus.raw[mirror_safe_address(address)] = value
}
