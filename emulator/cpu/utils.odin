package cpu

import "core:fmt"

// Quick ugly good-enough instruction dump for nestest.log
dump_instruction :: proc(cpu: ^CPU, instruction: ^Instruction, addressing: ^AddressingHelper, address: u16) {
	fmt.printf("%04X  ", cpu.registers.program_counter)
	// Show operands
	for i in 0 ..= addressing.bytes {
		fmt.printf("%02X ", read_byte(cpu, cpu.registers.program_counter + u16(i)))
	}
	fmt.printf("%*s", (3 - addressing.bytes) * 2 + 1 - addressing.bytes, "")
	fmt.printf("%s", instruction.name)

	// Show addressing mode
	switch instruction.mode {
		case .Implied:
			fmt.printf("                             ")
		case .Accumulator:
			fmt.printf(" A                           ")
		case .Immediate:
			fmt.printf(" #$%02X                        ", u8(address))
		case .ZeroPage:
			fmt.printf(" $%02X                         ", u8(address))
		case .ZeroPageX:
			fmt.printf(" $%02X,X                       ", u8(address))
		case .ZeroPageY:
			fmt.printf(" $%02X,Y                       ", u8(address))
		case .Relative:
			fmt.printf(" $%04X                       ", address)
		case .Absolute:
			fmt.printf(" $%04X                       ", address)
		case .AbsoluteX:
			fmt.printf(" $%04X,X                     ", address)
		case .AbsoluteY:
			operand_addr := cpu.registers.program_counter + 1
			lo := read_byte(cpu, operand_addr)
			hi := read_byte(cpu, operand_addr + 1)
			no_offset := (u16(hi) << 8) | u16(lo)

			fmt.printf(" $%04X,Y @ %04X = %02X         ", no_offset, address, read_byte(cpu, address))
		case .Indirect:
			op := cpu.registers.program_counter + 1
			lo := read_byte(cpu, u16(op))
			hi := read_byte(cpu, u16(op + 1))
			mid_address := (u16(hi) << 8 | u16(lo))
			fmt.printf(" ($%04X) = %04X              ", mid_address, address)
		case .XIndirect:
			op := read_byte(cpu, cpu.registers.program_counter + 1)
			offset := op + cpu.registers.x
			fmt.printf(" ($%02X,X) @ %02X = %04X = %02X    ", op, offset, address, read_byte(cpu, address))
		case .IndirectY:
			operand_addr := cpu.registers.program_counter + 1
			temp_address := read_byte(cpu, operand_addr)

			lo := read_byte(cpu, u16(temp_address))
			hi := read_byte(cpu, u16(temp_address + 1))
			target_no_offset := (u16(hi) << 8 | u16(lo))
			target := target_no_offset + u16(cpu.registers.y)

			fmt.printf(
				" ($%02X),Y = %04X @ %04X = %02X  ",
				read_byte(cpu, operand_addr),
				target_no_offset,
				target,
				read_byte(cpu, target),
			)
	}

	// Show registers
	fmt.printf(
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