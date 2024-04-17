package cpu

import "core:fmt"

FormatProc :: #type proc(fmt: string, args: ..any)
DumpProc :: #type proc(cpu: ^CPU, instruction: ^Instruction, addressing: ^AddressingHelper, address: u16)
DEFAULT_FORMATTER: FormatProc : proc(format: string, args: ..any) {fmt.printf(format, ..args)}

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
		formatter("%02X ", read_byte(cpu, cpu.registers.program_counter + u16(i)))
	}

	// Magic math to align the instruction name
	formatter("%*s", (-3 * int(addressing.bytes)) + 6, "")
	formatter(" %s " if instruction.official else "*%s ", instruction.name)

	// Show addressing mode
	operand_addr := cpu.registers.program_counter + 1
	switch instruction.mode {
		case .Implied:
			formatter("                            ")
		case .Accumulator:
			formatter("A                           ")
		case .Immediate:
			formatter("#$%02X                        ", address)
		case .ZeroPage:
			formatter("$%02X = %02X                    ", address, read_byte(cpu, address))
		case .ZeroPageX:
			formatter(
				"$%02X,X @ %02X = %02X             ",
				read_byte(cpu, cpu.registers.program_counter + 1),
				address,
				read_byte(cpu, address),
			)
		case .ZeroPageY:
			formatter(
				"$%02X,Y @ %02X = %02X             ",
				read_byte(cpu, cpu.registers.program_counter + 1),
				address,
				read_byte(cpu, address),
			)
		case .Absolute:
			if instruction.changes_pc {
				formatter("$%04X                       ", address)
			}
			 else {
				formatter("$%04X = %02X                  ", address, read_byte(cpu, address))
			}
		case .Relative:
			formatter("$%04X                       ", address)
		case .AbsoluteX:
			formatter("$%04X,X @ %04X = %02X         ", read_u16(cpu, operand_addr), address, read_byte(cpu, address))
		case .AbsoluteY:
			formatter("$%04X,Y @ %04X = %02X         ", read_u16(cpu, operand_addr), address, read_byte(cpu, address))
		case .Indirect:
			formatter("($%04X) = %04X              ", read_u16(cpu, operand_addr), address)
		case .XIndirect:
			operand := read_byte(cpu, operand_addr)
			offset := operand  + cpu.registers.x
			formatter("($%02X,X) @ %02X = %04X = %02X    ", operand, offset, address, read_byte(cpu, address))
		case .IndirectY:
			no_offset := address - u16(cpu.registers.y)

			formatter(
				"($%02X),Y = %04X @ %04X = %02X  ",
				read_byte(cpu, operand_addr),
				no_offset,
				address,
				read_byte(cpu, address),
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
		0,
		0,
		cpu.cycles,
	)
}
