package emulator

import "core:log"

cpu_stack_push :: proc(cpu: ^CPU, value: byte) -> bool {
	log.assertf(
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

cpu_stack_pull :: proc(cpu: ^CPU) -> byte {
	log.assertf(
		(cpu.registers.stack_pointer < 0xFF),
		"Stack underflow! No value will be pulled from the stack, value will be 0. stack_pointer = %x\n",
		cpu.registers.stack_pointer,
	)

	cpu.registers.stack_pointer += size_of(byte)
	value := cpu.memory.ram_map.stack[cpu.registers.stack_pointer]
	return value
}

cpu_stack_pull_u16 :: #force_inline proc(cpu: ^CPU) -> u16 {
	return u16(cpu_stack_pull(cpu)) | (u16(cpu_stack_pull(cpu)) << 8)
}
