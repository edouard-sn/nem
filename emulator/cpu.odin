// Emulates a 6502 NES cpu
package emulator
CPU :: struct {
	registers: Registers,
	bus:       ^Bus,
	cycles:    uint,
	write:     proc(cpu: ^CPU, address: u16, data: byte),
	read:      proc(cpu: ^CPU, address: u16) -> u8,
}

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
	DisableInterupt,
	Decimal,
	Break,
	Bit5,
	Overflow,
	Negative,
};byte]


cpu_init :: proc(cpu: ^CPU, bus: ^Bus) {
	cpu^ = CPU {
		bus   = bus,
		read  = cpu_read_byte,
		write = cpu_write_byte,
	}
}

@(private = "file")
execute_with_address_resolution :: proc(cpu: ^CPU, instruction: ^Instruction, target: u16) {
	switch ins in instruction.handle 
	{
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
			cpu.read = cpu_read_byte
			cpu.write = cpu_write_byte
		}
		ins(cpu, target)
	}
}
import "core:log"
cpu_write_byte :: proc(cpu: ^CPU, address: u16, data: byte) {
	bus_write_byte(cpu.bus, address, data)

	if 0x4014 == address {
		log.infof("DMA on %04X", cpu.bus.dma_address)
		dest := cpu.bus.ppu.oam_data[:]
		for &elem, i in dest {
			elem = bus_read_byte(cpu.bus, cpu.bus.dma_address + u16(i))
		}
		cpu_tick(cpu, 513 + (cpu.cycles & 1))
	}
}

cpu_read_u16 :: proc(cpu: ^CPU, address: u16) -> u16 {
	return bus_read_u16(cpu.bus, address)
}

cpu_read_byte :: proc(cpu: ^CPU, address: u16) -> u8 {
	return bus_read_byte(cpu.bus, address)
}

cpu_base_interupt :: #force_inline proc(
	cpu: ^CPU,
	address: u16,
	push_only_flags: ProcStatus = {},
	pc_offset: u16 = 0,
) {
	stack_pc := cpu.registers.program_counter + pc_offset

	cpu_stack_push(cpu, byte(stack_pc & 0xFF00 >> 8))
	cpu_stack_push(cpu, byte(stack_pc & 0x00FF))

	cpu_stack_push(cpu, transmute(u8)(cpu.registers.flags | push_only_flags))

	cpu.registers.program_counter = cpu_read_u16(cpu, address)
}

cpu_nmi_interupt :: proc(cpu: ^CPU) {
	cpu_base_interupt(cpu, 0xFFFA)
	cpu.registers.flags |= {.DisableInterupt}
	cpu_tick(cpu, 2)
}

cpu_irq_interupt :: proc(cpu: ^CPU) {
	cpu_base_interupt(cpu, 0xFFFE)
	cpu.registers.flags |= {.DisableInterupt}
	cpu_tick(cpu, 2)
}

cpu_handle_instruction :: proc(cpu: ^CPU, formatter: FormatProc = nil) {
	if ppu_is_nmi_raised(cpu.bus.ppu) {
		cpu_nmi_interupt(cpu)
	}

	op_code := bus_read_byte(cpu.bus, cpu.registers.program_counter)
	instruction := instruction_handles[op_code]

	addressing := addressing_helpers[instruction.mode]
	target, page_crossed := addressing.handle(cpu)

	if formatter != nil {
		dump_instruction(cpu, &instruction, &addressing, formatter)
	}

	// Pages on the 6502 are 256 bytes long. If the address crosses a page boundary, we add an extra cycle when needed.
	if (instruction.cycle_page_crossed && page_crossed) {
		cpu_tick(cpu, 1)
	}

	execute_with_address_resolution(cpu, &instruction, target)

	cpu_tick(cpu, instruction.cycles)

	if (instruction.changes_pc == false) {
		cpu.registers.program_counter += 1 + u16(addressing.bytes) // +1 for the op-code
	}
}

cpu_reset_interupt :: proc(cpu: ^CPU) {
	cpu.registers.flags = {.DisableInterupt, .Bit5}
	cpu.registers.x = 0
	cpu.registers.accumulator = 0
	cpu.registers.program_counter = bus_read_u16(cpu.bus, 0xFFFC)

	// Shortcut for the 3 bytes pull
	// https://www.pagetable.com/?p=410
	cpu.registers.stack_pointer = 0xFD
	cpu_tick(cpu, 7)
}

cpu_tick :: proc(cpu: ^CPU, cycles: uint) {
	cpu.cycles += cycles
	bus_tick(cpu.bus, cycles)
}
