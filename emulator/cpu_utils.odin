package emulator

import "core:fmt"
import "core:strings"

FormatProc :: #type proc(fmt: string, args: ..any)
DumpProc :: #type proc(cpu: ^CPU, instruction: ^Instruction, addressing: ^AddressingHelper, address: u16)
DEFAULT_FORMATTER: FormatProc : proc(format: string, args: ..any) {fmt.printf(format, ..args)}


_bus_read_byte :: proc(bus: ^Bus, address: u16, unsafe := false) -> byte {
	switch address 
	{
	case 0x0000 ..< CPU_RAM_MIRRORS_END:
		// The NES BUS has 8KB of RAM, but only 2KB of address space.
		// The address bus is only 11 bits wide, so the 2 most significant bits are ignored.
		// 0x7FF is the highest address in the address space.
		return bus.raw[address & (0b111_1111_1111)]
	case 0x2000, 0x2001, 0x2003, 0x2005, 0x2006, 0x4014:
		if unsafe {
			return bus.raw[address]
		}
	case 0x2002:
		result := bus.ppu.registers.status^
		return transmute(u8)result
	case 0x2004:
		return bus.ppu.oam_data[bus.ppu.registers.oam_address]
	case 0x2007:
		addr := ppu_get_address(bus.ppu)

		switch addr 
		{
		case 0 ..= 0x2fff:
			result := bus.ppu.registers.data^
			return result
		case 0x3f10, 0x3f14, 0x3f18, 0x3f1c:
			addr -= 0x10
			fallthrough
		case 0x3f00 ..= 0x3fff:
			return bus.ppu.palettes[addr - 0x3f00]
		case:
		//log.panicf("Trying to access unexpected address: %04X", address)
		}
	case 0x2008 ..= 0x3FFF:
		return _bus_read_byte(bus, address & 0b00100000_00000111)
	case 0x8000 ..= 0xFFFF:
		addr := address - 0x8000
		if addr >= 0x4000 {
			addr = addr % 0x4000
		}
		return bus.prg_rom[addr]
	case:
		return bus.raw[address]
	}
	return 0
}


// Dump current state in Nintendulator-style format (nestest.log) into any formatter 
dump_instruction :: proc(
	cpu: ^CPU,
	instruction: ^Instruction,
	addressing: ^AddressingHelper,
	formatter: FormatProc = DEFAULT_FORMATTER,
) {
	address, _ := addressing.handle(cpu)

	formatter("%04X  ", cpu.registers.program_counter)

	// Show opcode and operands
	for i in 0 ..= addressing.bytes {
		formatter("%02X ", _bus_read_byte(cpu.bus, cpu.registers.program_counter + u16(i)))
	}

	// Magic math to align the instruction name
	magic_offset := strings.repeat(" ", (-3 * int(addressing.bytes)) + 6, context.temp_allocator)
	formatter("%s%c%s ", magic_offset, ' ' if instruction.official else '*', instruction.name)

	// Show addressing mode
	operand_addr := cpu.registers.program_counter + 1
	switch instruction.mode 
	{
	case .Implied:
		formatter("                            ")
	case .Accumulator:
		formatter("A                           ")
	case .Immediate:
		formatter("#$%02X                        ", address)
	case .ZeroPage:
		formatter("$%02X = %02X                    ", address, _bus_read_byte(cpu.bus, address))
	case .ZeroPageX:
		formatter(
			"$%02X,X @ %02X = %02X             ",
			_bus_read_byte(cpu.bus, cpu.registers.program_counter + 1),
			address,
			_bus_read_byte(cpu.bus, address),
		)
	case .ZeroPageY:
		formatter(
			"$%02X,Y @ %02X = %02X             ",
			_bus_read_byte(cpu.bus, cpu.registers.program_counter + 1),
			address,
			_bus_read_byte(cpu.bus, address),
		)
	case .Absolute:
		if instruction.changes_pc {
			formatter("$%04X                       ", address)
		} else {
			formatter("$%04X = %02X                  ", address, _bus_read_byte(cpu.bus, address))
		}
	case .Relative:
		formatter("$%04X                       ", address)
	case .AbsoluteX:
		formatter(
			"$%04X,X @ %04X = %02X         ",
			bus_unsafe_read_u16(cpu.bus, operand_addr),
			address,
			_bus_read_byte(cpu.bus, address),
		)
	case .AbsoluteY:
		formatter(
			"$%04X,Y @ %04X = %02X         ",
			bus_unsafe_read_u16(cpu.bus, operand_addr),
			address,
			_bus_read_byte(cpu.bus, address),
		)
	case .Indirect:
		formatter("($%04X) = %04X              ", bus_unsafe_read_u16(cpu.bus, operand_addr), address)
	case .XIndirect:
		operand := _bus_read_byte(cpu.bus, operand_addr)
		offset := operand + cpu.registers.x
		formatter("($%02X,X) @ %02X = %04X = %02X    ", operand, offset, address, _bus_read_byte(cpu.bus, address))
	case .IndirectY:
		no_offset := address - u16(cpu.registers.y)

		formatter(
			"($%02X),Y = %04X @ %04X = %02X  ",
			_bus_read_byte(cpu.bus, operand_addr),
			no_offset,
			address,
			_bus_read_byte(cpu.bus, address),
		)
	}

	// Show registers
	formatter(
		"A:%02X X:%02X Y:%02X P:%02X SP:%02X PPU:% 3d,% 3d CYC:%d\n",
		cpu.registers.accumulator,
		cpu.registers.x,
		cpu.registers.y,
		cpu.registers.flags,
		cpu.registers.stack_pointer,
		cpu.bus.ppu.scan_line,
		cpu.bus.ppu.cycles,
		cpu.cycles,
	)
}
