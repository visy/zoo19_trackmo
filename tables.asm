_lz_moff_length
		;Long (>2 byte matches)
		.byte %00011111		;4 bits
		.byte %00000011		;7 bits
		.byte %01011111		;10 bits
		.byte %00001011		;13 bits
		;Short (2 byte matches)
		.byte %01011111		;10 bits
		.byte %00000000		;8 bits
		.byte %00000111		;6 bits
		.byte %00111111		;3 bits
_lz_moff_adjust_lo
		;Long (>2 byte matches)
		.byte %11111110		;1-16
		.byte %11101110		;17-144
		.byte %01101110		;145-1168
		.byte %01101110		;1169-9360
		;Short (2 byte matches)
		.byte %10110110		;329-1352
		.byte %10110110		;73-328
		.byte %11110110		;9-72
		.byte %11111110		;1-8
_lz_moff_adjust_hi = *-2
		;Long (>2 byte matches)
;		.byte %11111111		;1-16 (unreferenced)
;		.byte %11111111		;17-144 (unreferenced)
		.byte %01111111		;145-1168
		.byte %01111011		;1169-9360
		;Short (2 byte matches)
		.byte %01111110		;329-1352
		.byte %11111110		;73-328
;		.byte %11111111		;9-72 (unreferenced)
;		.byte %11111111		;1-8 (unreferenced)
