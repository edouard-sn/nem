// Emulates a 6502 NES cpu
package cpu
CPU6502 :: struct {
	registers: Registers,
	memory:    ^Bus,
	cycles:    uint,
	write:     proc(cpu: ^CPU, address: u16, data: byte),
	read:      proc(cpu: ^CPU, address: u16) -> u8,
}
CPU :: #type CPU6502

Registers :: struct {
	program_counter: u16,
	stack_pointer:   byte,
	accumulator:     byte,
	x:               byte,
	y:               byte,
	flags:           ProcStatus,
}

ProcStatus :: bit_set[enum {
	Carry,
	Zero,
	Interupt,
	Decimal,
	Break,
	Bit5,
	Overflow,
	Negative,
};byte]


init_cpu :: proc(cpu: ^CPU, bus: ^Bus) {
	cpu^ = CPU {
		memory = bus,
		read   = read_byte,
		write  = write_byte,
	}
	reset_interupt(cpu)
}

execute_with_address_resolution :: proc(cpu: ^CPU, instruction: ^Instruction, target: u16) {
	switch ins in instruction.handle {
	case proc(_: ^CPU):
		ins(cpu)
	case proc(_: ^CPU, _: byte):
		if (instruction.mode == .Immediate) {
			ins(cpu, u8(target))
		} else {
			ins(cpu, cpu->read(target))
		}
	case proc(_: ^CPU, _: u16):
		if instruction.mode == .Accumulator {
			cpu.read = proc(cpu: ^CPU, _: u16) -> u8 {return cpu.registers.accumulator}
			cpu.write = proc(cpu: ^CPU, _: u16, data: byte) {cpu.registers.accumulator = data}
		}
		defer if instruction.mode == .Accumulator {
			cpu.read = read_byte
			cpu.write = write_byte
		}
		ins(cpu, target)
	}
}


handle_instruction :: proc(cpu: ^CPU, formatter: FormatProc = nil) {
	op_code := unsafe_read(cpu, cpu.registers.program_counter)
	instruction := instruction_handles[op_code]

	addressing := addressing_helpers[instruction.mode]
	target, page_crossed := addressing.handle(cpu)

	if formatter != nil {
		dump_instruction(cpu, &instruction, &addressing, formatter)
	}

	// Pages on the 6502 are 256 bytes long. If the address crosses a page boundary, we add an extra cycle when needed.
	if (instruction.cycle_page_crossed && page_crossed) {
		cpu.cycles += 1
	}

	execute_with_address_resolution(cpu, &instruction, target)

	cpu.cycles += instruction.cycles

	if (instruction.changes_pc == false) {
		cpu.registers.program_counter += 1 + u16(addressing.bytes) // +1 for the op-code
	}
}

reset_interupt :: proc(cpu: ^CPU) {
	cpu.registers.flags = {.Interupt, .Bit5}
	cpu.registers.x = 0
	cpu.registers.accumulator = 0
	cpu.registers.program_counter = unsafe_read_u16(cpu, 0xFFFC)

	// Shortcut for the 3 bytes pull
	// https://www.pagetable.com/?p=410
	cpu.registers.stack_pointer = 0xFD
	cpu.cycles = 7
}
