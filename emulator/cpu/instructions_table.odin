package cpu

instruction_handles := [256]Instruction {
	0x00 = Instruction {
		name = "BRK",
		handle = brk_instruction,
		cycles = 7,
		mode = .Implied,
		changes_pc = true,
		official = true,
	},
	0x01 = Instruction{name = "ORA", handle = ora_instruction, cycles = 6, mode = .XIndirect, official = true},
	0x02 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0x03 = Instruction{name = "SLO", handle = slo_instruction, cycles = 8, mode = .XIndirect},
	0x04 = Instruction{name = "NOP", handle = nop_instruction, cycles = 3, mode = .ZeroPage},
	0x05 = Instruction{name = "ORA", handle = ora_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0x06 = Instruction{name = "ASL", handle = asl_instruction, cycles = 5, mode = .ZeroPage, official = true},
	0x07 = Instruction{name = "SLO", handle = slo_instruction, cycles = 5, mode = .ZeroPage},
	0x08 = Instruction{name = "PHP", handle = php_instruction, cycles = 3, mode = .Implied, official = true},
	0x09 = Instruction{name = "ORA", handle = ora_instruction, cycles = 2, mode = .Immediate, official = true},
	0x0A = Instruction{name = "ASL", handle = asl_instruction, cycles = 2, mode = .Accumulator, official = true},
	0x0B = Instruction{name = "ANC", handle = anc_instruction, cycles = 2, mode = .Immediate},
	0x0C = Instruction{name = "NOP", handle = nop_instruction, cycles = 4, mode = .Absolute},
	0x0D = Instruction{name = "ORA", handle = ora_instruction, cycles = 4, mode = .Absolute, official = true},
	0x0E = Instruction{name = "ASL", handle = asl_instruction, cycles = 6, mode = .Absolute, official = true},
	0x0F = Instruction{name = "SLO", handle = slo_instruction, cycles = 6, mode = .Absolute},
	0x10 = Instruction {
		name = "BPL",
		handle = bpl_instruction,
		cycles = 2,
		mode = .Relative,
		changes_pc = true,
		official = true,
	},
	0x11 = Instruction {
		name = "ORA",
		handle = ora_instruction,
		cycles = 5,
		mode = .IndirectY,
		cycle_page_crossed = true,
		official = true,
	},
	0x12 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0x13 = Instruction{name = "SLO", handle = slo_instruction, cycles = 8, mode = .IndirectY},
	0x14 = Instruction{name = "NOP", handle = nop_instruction, cycles = 4, mode = .ZeroPageX},
	0x15 = Instruction{name = "ORA", handle = ora_instruction, cycles = 4, mode = .ZeroPageX, official = true},
	0x16 = Instruction{name = "ASL", handle = asl_instruction, cycles = 6, mode = .ZeroPageX, official = true},
	0x17 = Instruction{name = "SLO", handle = slo_instruction, cycles = 6, mode = .ZeroPageX},
	0x18 = Instruction{name = "CLC", handle = clc_instruction, cycles = 2, mode = .Implied, official = true},
	0x19 = Instruction {
		name = "ORA",
		handle = ora_instruction,
		cycles = 4,
		mode = .AbsoluteY,
		cycle_page_crossed = true,
		official = true,
	},
	0x1A = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Implied},
	0x1B = Instruction{name = "SLO", handle = slo_instruction, cycles = 7, mode = .AbsoluteY},
	0x1C = Instruction {
		name = "NOP",
		handle = nop_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
	},
	0x1D = Instruction {
		name = "ORA",
		handle = ora_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
		official = true,
	},
	0x1E = Instruction{name = "ASL", handle = asl_instruction, cycles = 7, mode = .AbsoluteX, official = true},
	0x1F = Instruction{name = "SLO", handle = slo_instruction, cycles = 7, mode = .AbsoluteX},
	0x20 = Instruction {
		name = "JSR",
		handle = jsr_instruction,
		cycles = 6,
		mode = .Absolute,
		changes_pc = true,
		official = true,
	},
	0x21 = Instruction{name = "AND", handle = and_instruction, cycles = 6, mode = .XIndirect, official = true},
	0x22 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0x23 = Instruction{name = "RLA", handle = rla_instruction, cycles = 8, mode = .XIndirect},
	0x24 = Instruction{name = "BIT", handle = bit_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0x25 = Instruction{name = "AND", handle = and_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0x26 = Instruction{name = "ROL", handle = rol_instruction, cycles = 5, mode = .ZeroPage, official = true},
	0x27 = Instruction{name = "RLA", handle = rla_instruction, cycles = 5, mode = .ZeroPage},
	0x28 = Instruction{name = "PLP", handle = plp_instruction, cycles = 4, mode = .Implied, official = true},
	0x29 = Instruction{name = "AND", handle = and_instruction, cycles = 2, mode = .Immediate, official = true},
	0x2A = Instruction{name = "ROL", handle = rol_instruction, cycles = 2, mode = .Accumulator, official = true},
	0x2B = Instruction{name = "ANC", handle = anc_instruction, cycles = 2, mode = .Immediate},
	0x2C = Instruction{name = "BIT", handle = bit_instruction, cycles = 4, mode = .Absolute, official = true},
	0x2D = Instruction{name = "AND", handle = and_instruction, cycles = 4, mode = .Absolute, official = true},
	0x2E = Instruction{name = "ROL", handle = rol_instruction, cycles = 6, mode = .Absolute, official = true},
	0x2F = Instruction{name = "RLA", handle = rla_instruction, cycles = 6, mode = .Absolute},
	0x30 = Instruction {
		name = "BMI",
		handle = bmi_instruction,
		cycles = 2,
		mode = .Relative,
		changes_pc = true,
		official = true,
	},
	0x31 = Instruction {
		name = "AND",
		handle = and_instruction,
		cycles = 5,
		mode = .IndirectY,
		cycle_page_crossed = true,
		official = true,
	},
	0x32 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0x33 = Instruction{name = "RLA", handle = rla_instruction, cycles = 8, mode = .IndirectY},
	0x34 = Instruction{name = "NOP", handle = nop_instruction, cycles = 4, mode = .ZeroPageX},
	0x35 = Instruction{name = "AND", handle = and_instruction, cycles = 4, mode = .ZeroPageX, official = true},
	0x36 = Instruction{name = "ROL", handle = rol_instruction, cycles = 6, mode = .ZeroPageX, official = true},
	0x37 = Instruction{name = "RLA", handle = rla_instruction, cycles = 6, mode = .ZeroPageX},
	0x38 = Instruction{name = "SEC", handle = sec_instruction, cycles = 2, mode = .Implied, official = true},
	0x39 = Instruction {
		name = "AND",
		handle = and_instruction,
		cycles = 4,
		mode = .AbsoluteY,
		cycle_page_crossed = true,
		official = true,
	},
	0x3A = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Implied},
	0x3B = Instruction{name = "RLA", handle = rla_instruction, cycles = 7, mode = .AbsoluteY},
	0x3C = Instruction {
		name = "NOP",
		handle = nop_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
	},
	0x3D = Instruction {
		name = "AND",
		handle = and_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
		official = true,
	},
	0x3E = Instruction{name = "ROL", handle = rol_instruction, cycles = 7, mode = .AbsoluteX, official = true},
	0x3F = Instruction{name = "RLA", handle = rla_instruction, cycles = 7, mode = .AbsoluteX},
	0x40 = Instruction {
		name = "RTI",
		handle = rti_instruction,
		cycles = 6,
		mode = .Implied,
		changes_pc = true,
		official = true,
	},
	0x41 = Instruction{name = "EOR", handle = eor_instruction, cycles = 6, mode = .XIndirect, official = true},
	0x42 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0x43 = Instruction{name = "SRE", handle = sre_instruction, cycles = 8, mode = .XIndirect},
	0x44 = Instruction{name = "NOP", handle = nop_instruction, cycles = 3, mode = .ZeroPage},
	0x45 = Instruction{name = "EOR", handle = eor_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0x46 = Instruction{name = "LSR", handle = lsr_instruction, cycles = 5, mode = .ZeroPage, official = true},
	0x47 = Instruction{name = "SRE", handle = sre_instruction, cycles = 5, mode = .ZeroPage},
	0x48 = Instruction{name = "PHA", handle = pha_instruction, cycles = 3, mode = .Implied, official = true},
	0x49 = Instruction{name = "EOR", handle = eor_instruction, cycles = 2, mode = .Immediate, official = true},
	0x4A = Instruction{name = "LSR", handle = lsr_instruction, cycles = 2, mode = .Accumulator, official = true},
	0x4B = Instruction{name = "ALR", handle = alr_instruction, cycles = 2, mode = .Immediate},
	0x4C = Instruction {
		name = "JMP",
		handle = jmp_instruction,
		cycles = 3,
		mode = .Absolute,
		changes_pc = true,
		official = true,
	},
	0x4D = Instruction{name = "EOR", handle = eor_instruction, cycles = 4, mode = .Absolute, official = true},
	0x4E = Instruction{name = "LSR", handle = lsr_instruction, cycles = 6, mode = .Absolute, official = true},
	0x4F = Instruction{name = "SRE", handle = sre_instruction, cycles = 6, mode = .Absolute},
	0x50 = Instruction {
		name = "BVC",
		handle = bvc_instruction,
		cycles = 2,
		mode = .Relative,
		changes_pc = true,
		official = true,
	},
	0x51 = Instruction {
		name = "EOR",
		handle = eor_instruction,
		cycles = 5,
		mode = .IndirectY,
		cycle_page_crossed = true,
		official = true,
	},
	0x52 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0x53 = Instruction{name = "SRE", handle = sre_instruction, cycles = 8, mode = .IndirectY},
	0x54 = Instruction{name = "NOP", handle = nop_instruction, cycles = 4, mode = .ZeroPageX},
	0x55 = Instruction{name = "EOR", handle = eor_instruction, cycles = 4, mode = .ZeroPageX, official = true},
	0x56 = Instruction{name = "LSR", handle = lsr_instruction, cycles = 6, mode = .ZeroPageX, official = true},
	0x57 = Instruction{name = "SRE", handle = sre_instruction, cycles = 6, mode = .ZeroPageX},
	0x58 = Instruction{name = "CLI", handle = cli_instruction, cycles = 2, mode = .Implied, official = true},
	0x59 = Instruction {
		name = "EOR",
		handle = eor_instruction,
		cycles = 4,
		mode = .AbsoluteY,
		cycle_page_crossed = true,
		official = true,
	},
	0x5A = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Implied},
	0x5B = Instruction{name = "SRE", handle = sre_instruction, cycles = 7, mode = .AbsoluteY},
	0x5C = Instruction {
		name = "NOP",
		handle = nop_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
	},
	0x5D = Instruction {
		name = "EOR",
		handle = eor_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
		official = true,
	},
	0x5E = Instruction{name = "LSR", handle = lsr_instruction, cycles = 7, mode = .AbsoluteX, official = true},
	0x5F = Instruction{name = "SRE", handle = sre_instruction, cycles = 7, mode = .AbsoluteX},
	0x60 = Instruction {
		name = "RTS",
		handle = rts_instruction,
		cycles = 6,
		mode = .Implied,
		changes_pc = true,
		official = true,
	},
	0x61 = Instruction{name = "ADC", handle = adc_instruction, cycles = 6, mode = .XIndirect, official = true},
	0x62 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0x63 = Instruction{name = "RRA", handle = rra_instruction, cycles = 8, mode = .XIndirect},
	0x64 = Instruction{name = "NOP", handle = nop_instruction, cycles = 3, mode = .ZeroPage},
	0x65 = Instruction{name = "ADC", handle = adc_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0x66 = Instruction{name = "ROR", handle = ror_instruction, cycles = 5, mode = .ZeroPage, official = true},
	0x67 = Instruction{name = "RRA", handle = rra_instruction, cycles = 5, mode = .ZeroPage},
	0x68 = Instruction{name = "PLA", handle = pla_instruction, cycles = 4, mode = .Implied, official = true},
	0x69 = Instruction{name = "ADC", handle = adc_instruction, cycles = 2, mode = .Immediate, official = true},
	0x6A = Instruction{name = "ROR", handle = ror_instruction, cycles = 2, mode = .Accumulator, official = true},
	0x6B = Instruction{name = "ARR", handle = arr_instruction, cycles = 2, mode = .Immediate},
	0x6C = Instruction {
		name = "JMP",
		handle = jmp_instruction,
		cycles = 5,
		mode = .Indirect,
		changes_pc = true,
		official = true,
	},
	0x6D = Instruction{name = "ADC", handle = adc_instruction, cycles = 4, mode = .Absolute, official = true},
	0x6E = Instruction{name = "ROR", handle = ror_instruction, cycles = 6, mode = .Absolute, official = true},
	0x6F = Instruction{name = "RRA", handle = rra_instruction, cycles = 6, mode = .Absolute},
	0x70 = Instruction {
		name = "BVS",
		handle = bvs_instruction,
		cycles = 2,
		mode = .Relative,
		changes_pc = true,
		official = true,
	},
	0x71 = Instruction {
		name = "ADC",
		handle = adc_instruction,
		cycles = 5,
		mode = .IndirectY,
		cycle_page_crossed = true,
		official = true,
	},
	0x72 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0x73 = Instruction{name = "RRA", handle = rra_instruction, cycles = 8, mode = .IndirectY},
	0x74 = Instruction{name = "NOP", handle = nop_instruction, cycles = 4, mode = .ZeroPageX},
	0x75 = Instruction{name = "ADC", handle = adc_instruction, cycles = 4, mode = .ZeroPageX, official = true},
	0x76 = Instruction{name = "ROR", handle = ror_instruction, cycles = 6, mode = .ZeroPageX, official = true},
	0x77 = Instruction{name = "RRA", handle = rra_instruction, cycles = 6, mode = .ZeroPageX},
	0x78 = Instruction{name = "SEI", handle = sei_instruction, cycles = 2, mode = .Implied, official = true},
	0x79 = Instruction {
		name = "ADC",
		handle = adc_instruction,
		cycles = 4,
		mode = .AbsoluteY,
		cycle_page_crossed = true,
		official = true,
	},
	0x7A = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Implied},
	0x7B = Instruction{name = "RRA", handle = rra_instruction, cycles = 7, mode = .AbsoluteY},
	0x7C = Instruction {
		name = "NOP",
		handle = nop_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
	},
	0x7D = Instruction {
		name = "ADC",
		handle = adc_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
		official = true,
	},
	0x7E = Instruction{name = "ROR", handle = ror_instruction, cycles = 7, mode = .AbsoluteX, official = true},
	0x7F = Instruction{name = "RRA", handle = rra_instruction, cycles = 7, mode = .AbsoluteX},
	0x80 = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Immediate},
	0x81 = Instruction{name = "STA", handle = sta_instruction, cycles = 6, mode = .XIndirect, official = true},
	0x82 = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Immediate},
	0x83 = Instruction{name = "SAX", handle = sax_instruction, cycles = 6, mode = .XIndirect},
	0x84 = Instruction{name = "STY", handle = sty_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0x85 = Instruction{name = "STA", handle = sta_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0x86 = Instruction{name = "STX", handle = stx_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0x87 = Instruction{name = "SAX", handle = sax_instruction, cycles = 3, mode = .ZeroPage},
	0x88 = Instruction{name = "DEY", handle = dey_instruction, cycles = 2, mode = .Implied, official = true},
	0x89 = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Immediate},
	0x8A = Instruction{name = "TXA", handle = txa_instruction, cycles = 2, mode = .Implied, official = true},
	0x8B = Instruction{name = "XAA", handle = xaa_instruction, cycles = 2, mode = .Immediate},
	0x8C = Instruction{name = "STY", handle = sty_instruction, cycles = 4, mode = .Absolute, official = true},
	0x8D = Instruction{name = "STA", handle = sta_instruction, cycles = 4, mode = .Absolute, official = true},
	0x8E = Instruction{name = "STX", handle = stx_instruction, cycles = 4, mode = .Absolute, official = true},
	0x8F = Instruction{name = "SAX", handle = sax_instruction, cycles = 4, mode = .Absolute},
	0x90 = Instruction {
		name = "BCC",
		handle = bcc_instruction,
		cycles = 2,
		mode = .Relative,
		changes_pc = true,
		official = true,
	},
	0x91 = Instruction{name = "STA", handle = sta_instruction, cycles = 6, mode = .IndirectY, official = true},
	0x92 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0x93 = Instruction{name = "AHX", handle = ahx_instruction, cycles = 6, mode = .IndirectY},
	0x94 = Instruction{name = "STY", handle = sty_instruction, cycles = 4, mode = .ZeroPageX, official = true},
	0x95 = Instruction{name = "STA", handle = sta_instruction, cycles = 4, mode = .ZeroPageX, official = true},
	0x96 = Instruction{name = "STX", handle = stx_instruction, cycles = 4, mode = .ZeroPageY, official = true},
	0x97 = Instruction{name = "SAX", handle = sax_instruction, cycles = 4, mode = .ZeroPageY},
	0x98 = Instruction{name = "TYA", handle = tya_instruction, cycles = 2, mode = .Implied, official = true},
	0x99 = Instruction{name = "STA", handle = sta_instruction, cycles = 5, mode = .AbsoluteY, official = true},
	0x9A = Instruction{name = "TXS", handle = txs_instruction, cycles = 2, mode = .Implied, official = true},
	0x9B = Instruction{name = "TAS", handle = tas_instruction, cycles = 5, mode = .AbsoluteY},
	0x9C = Instruction{name = "SHY", handle = shy_instruction, cycles = 5, mode = .AbsoluteX},
	0x9D = Instruction{name = "STA", handle = sta_instruction, cycles = 5, mode = .AbsoluteX, official = true},
	0x9E = Instruction{name = "SHX", handle = shx_instruction, cycles = 5, mode = .AbsoluteY},
	0x9F = Instruction{name = "AHX", handle = ahx_instruction, cycles = 5, mode = .AbsoluteY},
	0xA0 = Instruction{name = "LDY", handle = ldy_instruction, cycles = 2, mode = .Immediate, official = true},
	0xA1 = Instruction{name = "LDA", handle = lda_instruction, cycles = 6, mode = .XIndirect, official = true},
	0xA2 = Instruction{name = "LDX", handle = ldx_instruction, cycles = 2, mode = .Immediate, official = true},
	0xA3 = Instruction{name = "LAX", handle = lax_instruction, cycles = 6, mode = .XIndirect},
	0xA4 = Instruction{name = "LDY", handle = ldy_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0xA5 = Instruction{name = "LDA", handle = lda_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0xA6 = Instruction{name = "LDX", handle = ldx_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0xA7 = Instruction{name = "LAX", handle = lax_instruction, cycles = 3, mode = .ZeroPage},
	0xA8 = Instruction{name = "TAY", handle = tay_instruction, cycles = 2, mode = .Implied, official = true},
	0xA9 = Instruction{name = "LDA", handle = lda_instruction, cycles = 2, mode = .Immediate, official = true},
	0xAA = Instruction{name = "TAX", handle = tax_instruction, cycles = 2, mode = .Implied, official = true},
	0xAB = Instruction{name = "LAX", handle = lax_instruction, cycles = 2, mode = .Immediate},
	0xAC = Instruction{name = "LDY", handle = ldy_instruction, cycles = 4, mode = .Absolute, official = true},
	0xAD = Instruction{name = "LDA", handle = lda_instruction, cycles = 4, mode = .Absolute, official = true},
	0xAE = Instruction{name = "LDX", handle = ldx_instruction, cycles = 4, mode = .Absolute, official = true},
	0xAF = Instruction{name = "LAX", handle = lax_instruction, cycles = 4, mode = .Absolute},
	0xB0 = Instruction {
		name = "BCS",
		handle = bcs_instruction,
		cycles = 2,
		mode = .Relative,
		changes_pc = true,
		official = true,
	},
	0xB1 = Instruction {
		name = "LDA",
		handle = lda_instruction,
		cycles = 5,
		mode = .IndirectY,
		cycle_page_crossed = true,
		official = true,
	},
	0xB2 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0xB3 = Instruction {
		name = "LAX",
		handle = lax_instruction,
		cycles = 5,
		mode = .IndirectY,
		cycle_page_crossed = true,
	},
	0xB4 = Instruction{name = "LDY", handle = ldy_instruction, cycles = 4, mode = .ZeroPageX, official = true},
	0xB5 = Instruction{name = "LDA", handle = lda_instruction, cycles = 4, mode = .ZeroPageX, official = true},
	0xB6 = Instruction{name = "LDX", handle = ldx_instruction, cycles = 4, mode = .ZeroPageY, official = true},
	0xB7 = Instruction{name = "LAX", handle = lax_instruction, cycles = 4, mode = .ZeroPageY},
	0xB8 = Instruction{name = "CLV", handle = clv_instruction, cycles = 2, mode = .Implied, official = true},
	0xB9 = Instruction {
		name = "LDA",
		handle = lda_instruction,
		cycles = 4,
		mode = .AbsoluteY,
		cycle_page_crossed = true,
		official = true,
	},
	0xBA = Instruction{name = "TSX", handle = tsx_instruction, cycles = 2, mode = .Implied, official = true},
	0xBB = Instruction {
		name = "LAS",
		handle = las_instruction,
		cycles = 4,
		mode = .AbsoluteY,
		cycle_page_crossed = true,
	},
	0xBC = Instruction {
		name = "LDY",
		handle = ldy_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
		official = true,
	},
	0xBD = Instruction {
		name = "LDA",
		handle = lda_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
		official = true,
	},
	0xBE = Instruction {
		name = "LDX",
		handle = ldx_instruction,
		cycles = 4,
		mode = .AbsoluteY,
		cycle_page_crossed = true,
		official = true,
	},
	0xBF = Instruction {
		name = "LAX",
		handle = lax_instruction,
		cycles = 4,
		mode = .AbsoluteY,
		cycle_page_crossed = true,
	},
	0xC0 = Instruction{name = "CPY", handle = cpy_instruction, cycles = 2, mode = .Immediate, official = true},
	0xC1 = Instruction{name = "CMP", handle = cmp_instruction, cycles = 6, mode = .XIndirect, official = true},
	0xC2 = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Immediate},
	0xC3 = Instruction{name = "DCP", handle = dcp_instruction, cycles = 8, mode = .XIndirect},
	0xC4 = Instruction{name = "CPY", handle = cpy_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0xC5 = Instruction{name = "CMP", handle = cmp_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0xC6 = Instruction{name = "DEC", handle = dec_instruction, cycles = 5, mode = .ZeroPage, official = true},
	0xC7 = Instruction{name = "DCP", handle = dcp_instruction, cycles = 5, mode = .ZeroPage},
	0xC8 = Instruction{name = "INY", handle = iny_instruction, cycles = 2, mode = .Implied, official = true},
	0xC9 = Instruction{name = "CMP", handle = cmp_instruction, cycles = 2, mode = .Immediate, official = true},
	0xCA = Instruction{name = "DEX", handle = dex_instruction, cycles = 2, mode = .Implied, official = true},
	0xCB = Instruction{name = "SBX", handle = sbx_instruction, cycles = 2, mode = .Immediate},
	0xCC = Instruction{name = "CPY", handle = cpy_instruction, cycles = 4, mode = .Absolute, official = true},
	0xCD = Instruction{name = "CMP", handle = cmp_instruction, cycles = 4, mode = .Absolute, official = true},
	0xCE = Instruction{name = "DEC", handle = dec_instruction, cycles = 6, mode = .Absolute, official = true},
	0xCF = Instruction{name = "DCP", handle = dcp_instruction, cycles = 6, mode = .Absolute},
	0xD0 = Instruction {
		name = "BNE",
		handle = bne_instruction,
		cycles = 2,
		mode = .Relative,
		changes_pc = true,
		official = true,
	},
	0xD1 = Instruction {
		name = "CMP",
		handle = cmp_instruction,
		cycles = 5,
		mode = .IndirectY,
		cycle_page_crossed = true,
		official = true,
	},
	0xD2 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0xD3 = Instruction{name = "DCP", handle = dcp_instruction, cycles = 8, mode = .IndirectY},
	0xD4 = Instruction{name = "NOP", handle = nop_instruction, cycles = 4, mode = .ZeroPageX},
	0xD5 = Instruction{name = "CMP", handle = cmp_instruction, cycles = 4, mode = .ZeroPageX, official = true},
	0xD6 = Instruction{name = "DEC", handle = dec_instruction, cycles = 6, mode = .ZeroPageX, official = true},
	0xD7 = Instruction{name = "DCP", handle = dcp_instruction, cycles = 6, mode = .ZeroPageX},
	0xD8 = Instruction{name = "CLD", handle = cld_instruction, cycles = 2, mode = .Implied, official = true},
	0xD9 = Instruction {
		name = "CMP",
		handle = cmp_instruction,
		cycles = 4,
		mode = .AbsoluteY,
		cycle_page_crossed = true,
		official = true,
	},
	0xDA = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Implied},
	0xDB = Instruction{name = "DCP", handle = dcp_instruction, cycles = 7, mode = .AbsoluteY},
	0xDC = Instruction {
		name = "NOP",
		handle = nop_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
	},
	0xDD = Instruction {
		name = "CMP",
		handle = cmp_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
		official = true,
	},
	0xDE = Instruction{name = "DEC", handle = dec_instruction, cycles = 7, mode = .AbsoluteX, official = true},
	0xDF = Instruction{name = "DCP", handle = dcp_instruction, cycles = 7, mode = .AbsoluteX},
	0xE0 = Instruction{name = "CPX", handle = cpx_instruction, cycles = 2, mode = .Immediate, official = true},
	0xE1 = Instruction{name = "SBC", handle = sbc_instruction, cycles = 6, mode = .XIndirect, official = true},
	0xE2 = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Immediate},
	0xE3 = Instruction{name = "ISB", handle = isb_instruction, cycles = 8, mode = .XIndirect},
	0xE4 = Instruction{name = "CPX", handle = cpx_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0xE5 = Instruction{name = "SBC", handle = sbc_instruction, cycles = 3, mode = .ZeroPage, official = true},
	0xE6 = Instruction{name = "INC", handle = inc_instruction, cycles = 5, mode = .ZeroPage, official = true},
	0xE7 = Instruction{name = "ISB", handle = isb_instruction, cycles = 5, mode = .ZeroPage},
	0xE8 = Instruction{name = "INX", handle = inx_instruction, cycles = 2, mode = .Implied, official = true},
	0xE9 = Instruction{name = "SBC", handle = sbc_instruction, cycles = 2, mode = .Immediate, official = true},
	0xEA = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Implied, official = true},
	0xEB = Instruction{name = "SBC", handle = sbc_instruction, cycles = 2, mode = .Immediate},
	0xEC = Instruction{name = "CPX", handle = cpx_instruction, cycles = 4, mode = .Absolute, official = true},
	0xED = Instruction{name = "SBC", handle = sbc_instruction, cycles = 4, mode = .Absolute, official = true},
	0xEE = Instruction{name = "INC", handle = inc_instruction, cycles = 6, mode = .Absolute, official = true},
	0xEF = Instruction{name = "ISB", handle = isb_instruction, cycles = 6, mode = .Absolute},
	0xF0 = Instruction {
		name = "BEQ",
		handle = beq_instruction,
		cycles = 2,
		mode = .Relative,
		changes_pc = true,
		official = true,
	},
	0xF1 = Instruction {
		name = "SBC",
		handle = sbc_instruction,
		cycles = 5,
		mode = .IndirectY,
		cycle_page_crossed = true,
		official = true,
	},
	0xF2 = Instruction{name = "JAM", handle = jam_instruction, cycles = 0, mode = .Implied},
	0xF3 = Instruction{name = "ISB", handle = isb_instruction, cycles = 8, mode = .IndirectY},
	0xF4 = Instruction{name = "NOP", handle = nop_instruction, cycles = 4, mode = .ZeroPageX},
	0xF5 = Instruction{name = "SBC", handle = sbc_instruction, cycles = 4, mode = .ZeroPageX, official = true},
	0xF6 = Instruction{name = "INC", handle = inc_instruction, cycles = 6, mode = .ZeroPageX, official = true},
	0xF7 = Instruction{name = "ISB", handle = isb_instruction, cycles = 6, mode = .ZeroPageX},
	0xF8 = Instruction{name = "SED", handle = sed_instruction, cycles = 2, mode = .Implied, official = true},
	0xF9 = Instruction {
		name = "SBC",
		handle = sbc_instruction,
		cycles = 4,
		mode = .AbsoluteY,
		cycle_page_crossed = true,
		official = true,
	},
	0xFA = Instruction{name = "NOP", handle = nop_instruction, cycles = 2, mode = .Implied},
	0xFB = Instruction{name = "ISB", handle = isb_instruction, cycles = 7, mode = .AbsoluteY},
	0xFC = Instruction {
		name = "NOP",
		handle = nop_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
	},
	0xFD = Instruction {
		name = "SBC",
		handle = sbc_instruction,
		cycles = 4,
		mode = .AbsoluteX,
		cycle_page_crossed = true,
		official = true,
	},
	0xFE = Instruction{name = "INC", handle = inc_instruction, cycles = 7, mode = .AbsoluteX, official = true},
	0xFF = Instruction{name = "ISB", handle = isb_instruction, cycles = 7, mode = .AbsoluteX},
}
