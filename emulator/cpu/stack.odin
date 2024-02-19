package cpu

import "core:fmt"

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