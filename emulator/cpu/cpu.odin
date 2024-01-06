// Emulates a 6502 NES cpu
package cpu

import "../bus"
import "core:fmt"

CPU :: struct {
	registers: CPURegisters,
	memory:    ^bus.Bus,
	cycles:    uint,
}

@(private)
CPURegisters :: struct {
	program_counter: u16,
	stack_pointer:   byte,
	accumulator:     byte,
	x:               byte,
	y:               byte,
	flags:           CPUFlagRegistry,
}

@(private)
CPUFlagRegistry :: bit_set[enum {
	Carry,
	Zero,
	Interupt, // IRQ disable
	Decimal, // (BCD for arithmetics)
	Break,
	Bit5,
	Overflow,
	Negative,
};byte]


dump_registers :: proc(cpu: ^CPU) {
	fmt.printf("PC: %x\n", cpu.registers.program_counter)
	fmt.printf("SP: %x\n", cpu.registers.stack_pointer)
	fmt.printf("A: %x\n", cpu.registers.accumulator)
	fmt.printf("X: %x\n", cpu.registers.x)
	fmt.printf("Y: %x\n", cpu.registers.y)
	fmt.printf("Flags: %v\n", cpu.registers.flags)
}

new_cpu :: proc(bus: ^bus.Bus) -> CPU {
	cpu: CPU

	cpu.memory = bus
	cpu.registers.stack_pointer = 0xFF // 0x01FF - 0x0100

	return cpu
}

read_byte :: #force_inline proc(cpu: ^CPU, address: u16) -> byte {
	return bus.read_byte(cpu.memory, address)
}

read_u16 :: #force_inline proc(cpu: ^CPU, address: u16) -> u16 {
	return u16(read_byte(cpu, address)) | (u16(read_byte(cpu, address + 1)) << 8)
}

write_u16 :: #force_inline proc(cpu: ^CPU, address: u16, value: u16) {
	bus.write_byte(cpu.memory, address, u8(value & 0xFF))
	bus.write_byte(cpu.memory, address + 1, u8(value >> 8))
}

push_byte_on_stack :: proc(cpu: ^CPU, value: byte) -> bool {
	fmt.assertf(
		cpu.registers.stack_pointer >= size_of(byte),
		"Stack overflow! %x won't be pushed to the stack. size_of(byte) = %d. stack_pointer = %x\n",
		value,
		size_of(byte),
		cpu.registers.stack_pointer,
	)

	stack := cpu.memory.ram_map.stack
	stack[cpu.registers.stack_pointer] = value
	cpu.registers.stack_pointer -= size_of(byte)

	return true
}

pull_byte_from_stack :: proc(cpu: ^CPU) -> byte {
	fmt.assertf(
		(cpu.registers.stack_pointer < 0xFF),
		"Stack underflow! No value will be pulled from the stack, value will be 0. stack_pointer = %x\n",
		cpu.registers.stack_pointer,
	)

	cpu.registers.stack_pointer += size_of(byte)
	value := cpu.memory.ram_map.stack[cpu.registers.stack_pointer]
	return value
}

pull_u16_from_stack :: #force_inline proc(cpu: ^CPU) -> u16 {
	return u16(pull_byte_from_stack(cpu)) | (u16(pull_byte_from_stack(cpu)) << 8)
}

x_zp_indirect :: proc(cpu: ^CPU) -> u16 {
	operand_addr := cpu.registers.program_counter + 1
	temp_address := u16(read_byte(cpu, operand_addr)) + u16(cpu.registers.x)

	lo := read_byte(cpu, temp_address)
	hi := read_byte(cpu, temp_address + 1)

	return u16(hi) << 8 | u16(lo)
}

zp_indirect_y :: proc(cpu: ^CPU) -> u16 {
	operand_addr := cpu.registers.program_counter + 1
	temp_address := read_byte(cpu, operand_addr)

	lo := read_byte(cpu, u16(temp_address))
	hi := read_byte(cpu, u16(temp_address + 1))

	return (u16(hi) << 8 | u16(lo)) + u16(cpu.registers.y)
}

zeropage :: #force_inline proc(cpu: ^CPU, indexed_value: byte = 0) -> u16 {
	operand_addr := cpu.registers.program_counter + 1
	return u16(read_byte(cpu, operand_addr)) + u16(indexed_value)
}

immediate :: #force_inline proc(cpu: ^CPU) -> u16 {
	operand_addr := cpu.registers.program_counter + 1
	return u16(read_byte(cpu, operand_addr))
}

absolute :: #force_inline proc(cpu: ^CPU, indexed_value: byte = 0) -> u16 {
	operand_addr := cpu.registers.program_counter + 1
	lo := read_byte(cpu, operand_addr)
	hi := read_byte(cpu, operand_addr + 1)

	return u16(hi) << 8 | u16(lo) + u16(indexed_value)
}

relative :: #force_inline proc(cpu: ^CPU) -> u16 {
	operand_addr := cpu.registers.program_counter + 1
	offset := read_byte(cpu, operand_addr)
	return u16(i16(cpu.registers.program_counter) + i16(i8(offset)))
}

reset_interupt :: proc(cpu: ^CPU) {
	cpu.registers.flags = {}
	cpu.registers.x = 0
	cpu.registers.accumulator = 0
	cpu.registers.program_counter = read_u16(cpu, 0xFFFC)
}
