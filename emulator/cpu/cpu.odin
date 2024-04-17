// Emulates a 6502 NES cpu
package cpu

CPU6502 :: struct {
	registers: Registers,
	memory:    ^Bus,
	cycles:    uint,
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


new_cpu :: proc(bus: ^Bus) -> CPU {
	cpu: CPU

	cpu.memory = bus

	return cpu
}

//
// Memory wrapper
//

read_byte :: #force_inline proc(cpu: ^CPU, address: u16) -> byte {
	return safe_pointer(cpu.memory, address)^
}

write_byte :: #force_inline proc(cpu: ^CPU, address: u16, value: byte) {
	safe_pointer(cpu.memory, address)^ = value
}

read_u16 :: #force_inline proc(cpu: ^CPU, address: u16) -> u16 {
	return u16(read_byte(cpu, address)) | (u16(read_byte(cpu, address + 1)) << 8)
}

write_u16 :: #force_inline proc(cpu: ^CPU, address: u16, value: u16) {
	write_byte(cpu, address, u8(value & 0xFF))
	write_byte(cpu, address + 1, u8(value >> 8))
}

//
// Interupts (no instruction)
//

reset_interupt :: proc(cpu: ^CPU) {
	cpu.registers.flags = {.Interupt, .Bit5}
	cpu.registers.x = 0
	cpu.registers.accumulator = 0
	cpu.registers.program_counter = read_u16(cpu, 0xFFFC)

	// Shortcut for the 3 bytes pull
	// https://www.pagetable.com/?p=410
	cpu.registers.stack_pointer = 0xFD
}
