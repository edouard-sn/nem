package pal
import "core:log"
import "core:os"

PalFile :: []struct {
	r, g, b: byte,
}

PalSizes :: enum {
	Normal   = 64 * 3,
	Extended = Normal * 8,
}

load :: proc(filepath: string) -> (pal: PalFile, ok: bool) {
	data := os.read_entire_file(filepath) or_return
	data_len := len(data)

	switch PalSizes(data_len) {
	case .Normal, .Extended:
		log.debugf("pal file has correct size (%v)", PalSizes(data_len))
	case:
		log.errorf("palette size is incorrect (%v bytes)", data_len)
		return
	}
	pal = make(PalFile, data_len / 3)
	for i := 0; i < len(pal); i += 1 {
		pal[i] = {
			r = data[i * 3],
			g = data[i * 3 + 1],
			b = data[i * 3 + 2],
		}
	}
	return pal, true
}

destroy :: proc(pal: PalFile) {
	delete(pal)
}
