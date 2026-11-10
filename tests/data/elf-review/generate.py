from pathlib import Path
import struct
import zlib
out=Path(__file__).resolve().parent
def elf(name, sections=(), names=0, ph=None, shstr=None):
    n=1+len(sections) if sections else 0
    shoff=64 if n else 0
    data=bytearray(64+64*n+(56 if ph else 0))
    ident=b'\x7fELF\x02\x01\x01'+b'\0'*9
    data[:64]=struct.pack('<16sHHIQQQIHHHHHH',ident,1,62,1,0,64+64*n if ph else 0,shoff,0,64,56,1 if ph else 0,64,n,names if shstr is None else shstr)
    for i,section in enumerate(sections,1):
        payload=section.get('data',b'');off=len(data);data.extend(payload)
        fields=(section.get('name',0),section['type'],section.get('flags',0),0,off,len(payload),section.get('link',0),section.get('info',0),section.get('align',1),section.get('entsize',0))
        struct.pack_into('<IIQQQQIIQQ',data,64+64*i,*fields)
    if ph: struct.pack_into('<IIQQQQQQ',data,64+64*n,*ph)
    (out/(name+'.elf')).write_bytes(data)
elf('nonutf8-section',sections=[dict(type=3,data=b'\0\xff\0'),dict(type=1,name=1)],names=1)
elf('bom-section',sections=[dict(type=3,data=b'\0\xef\xbb\xbfA\0'),dict(type=1,name=1)],names=1)
elf('null-segment',ph=(0,0,0xffffffffffffffff,0,0,0xffffffffffffffff,0,3))
elf('note-padding',sections=[dict(type=7,align=4,data=struct.pack('<III',2,1,1)+b'A\0\xff\xff'+b'X\xff\xff\xff')])
elf('shstr-no-sections',shstr=1)
# A valid null symbol followed by one ordinary undefined global symbol.
syms=b'\0'*24+struct.pack('<IBBHQQ',1,0x10,0,0,0,0)
elf('bad-hash-index',sections=[dict(type=3,data=b'\0x\0'),dict(type=2,data=syms,link=1,info=1,entsize=24,align=8),dict(type=5,data=struct.pack('<IIII',1,1,99,99),link=2,entsize=4,align=4)])
elf('bad-group-index',sections=[dict(type=3,data=b'\0x\0'),dict(type=2,data=syms,link=1,info=1,entsize=24,align=8),dict(type=17,data=struct.pack('<II',1,999),link=2,info=1,entsize=4,align=4)])
elf('bad-symbol-info',sections=[dict(type=3,data=b'\0x\0'),dict(type=2,data=syms,link=1,info=99,entsize=24,align=8)])
elf('empty-shstr',sections=[dict(type=3,data=b'')],names=1)
elf('nonfirst-null',sections=[dict(type=0,align=3,data=b'')])
# Change fields of an inactive nonzero section; these fields have no associated bytes.
p=out/'nonfirst-null.elf'; b=bytearray(p.read_bytes());struct.pack_into('<Q',b,128+24,2**64-1);p.write_bytes(b)
# Two companion index sections bypass unused-entry validation in decode-symbols.
elf('duplicate-shndx',sections=[dict(type=3,data=b'\0x\0'),dict(type=2,data=syms,link=1,info=1,entsize=24,align=8),dict(type=18,data=struct.pack('<II',99,99),link=2,entsize=4,align=4),dict(type=18,data=struct.pack('<II',99,99),link=2,entsize=4,align=4)])
elf('nonutf8-symbol',sections=[dict(type=3,data=b'\0\xff\0'),dict(type=2,data=syms,link=1,info=1,entsize=24,align=8)])
elf('reserved-xindex',sections=[dict(type=3,data=b'\0x\0'),dict(type=2,data=b'\0'*24+struct.pack('<IBBHQQ',1,0x10,0,65535,0,0),link=1,info=1,entsize=24,align=8),dict(type=18,data=struct.pack('<II',0,65521),link=2,entsize=4,align=4)])
# Valid UTF-8 continues to decode as Unicode rather than one character per byte.
elf('utf8-section',sections=[dict(type=3,data=b'\0\xc3\xa9\0'),dict(type=1,name=1)],names=1)
# An empty linked string table can still supply the empty name of symbol zero.
elf('empty-symbol-strings',sections=[dict(type=3,data=b''),dict(type=2,data=b'\0'*24,link=1,info=1,entsize=24,align=8)])
# Inactive sections do not have meaningful name indices either.
p=out/'nonfirst-null.elf'; b=bytearray(p.read_bytes());struct.pack_into('<I',b,128,999);p.write_bytes(b)

# Every nonempty string table, including a symbol-name table, has NUL boundaries.
elf('string-table-missing-first-nul', sections=[dict(type=3, data=b'x\0')])
elf('string-table-missing-last-nul', sections=[dict(type=3, data=b'\0x')])
prefix = [dict(type=3, data=b'\0x\0'),
          dict(type=2, data=syms, link=1, info=1, entsize=24, align=8)]
group = dict(type=17, data=struct.pack('<II', 1, 4), link=2, info=1, entsize=4, align=4)
elf('group-missing-member-flag', sections=prefix + [group, dict(type=1)])
elf('group-valid', sections=prefix + [group, dict(type=1, flags=0x200)])
elf('group-in-executable', sections=prefix + [group, dict(type=1, flags=0x200)])
p = out / 'group-in-executable.elf'
b = bytearray(p.read_bytes())
struct.pack_into('<H', b, 16, 2)
p.write_bytes(b)
# Two distinct groups cannot claim the same member section.
group5 = dict(group, data=struct.pack('<II', 1, 5))
elf('group-multiple-membership', sections=prefix + [group5, group5, dict(type=1, flags=0x200)])
# Sections cannot share file-backed bytes. NOBITS describes memory only.
elf('overlapping-sections', sections=[dict(type=1, data=b'abc'), dict(type=1, data=b'def')])
p = out / 'overlapping-sections.elf'
b = bytearray(p.read_bytes())
first_offset = struct.unpack_from('<Q', b, 128 + 24)[0]
struct.pack_into('<Q', b, 192 + 24, first_offset)
p.write_bytes(b)
struct.pack_into('<I', b, 192 + 4, 8)
(out / 'nobits-overlap.elf').write_bytes(b)

# Fourth-pass fixtures use explicit class, byte order, and physical section alignment.
def variant_elf(name, sections, elf64=True, endian='<', filetype=1, names=0):
    header_size, section_size = (64, 64) if elf64 else (52, 40)
    data = bytearray(header_size + section_size * (len(sections) + 1))
    ident = b'\x7fELF' + bytes([2 if elf64 else 1, 1 if endian == '<' else 2, 1]) + b'\0'*9
    header_format = '16sHHIQQQIHHHHHH' if elf64 else '16sHHIIIIIHHHHHH'
    struct.pack_into(endian + header_format, data, 0, ident, filetype, 62 if elf64 else 3,
                     1, 0, 0, header_size, 0, header_size, 56 if elf64 else 32, 0,
                     section_size, len(sections) + 1, names)
    for index, section in enumerate(sections, 1):
        alignment = section.get('align', 1)
        data.extend(b'\0' * (-len(data) % max(alignment, 1)))
        offset = len(data)
        payload = section.get('data', b'')
        data.extend(payload)
        fields = (section.get('name', 0), section['type'], section.get('flags', 0), 0,
                  offset, len(payload), section.get('link', 0), section.get('info', 0),
                  alignment, section.get('entsize', 0))
        struct.pack_into(endian + ('IIQQQQIIQQ' if elf64 else 'IIIIIIIIII'), data,
                         header_size + section_size * index, *fields)
    (out / (name + '.elf')).write_bytes(data)

# Error cases: flagged orphan sections and a group header following its member.
variant_elf('group-orphan', [dict(type=1, flags=0x200)])
variant_elf('group-orphan-executable', [dict(type=1, flags=0x200)], filetype=2)
variant_elf('group-after-member', prefix + [dict(type=1, flags=0x200),
            dict(group, data=struct.pack('<II', 1, 3))])

for elf64 in (False, True):
    for endian in ('<', '>'):
        tag = ('64' if elf64 else '32') + ('le' if endian == '<' else 'be')
        header_format = 'IIQQ' if elf64 else 'III'
        def compressed(payload, alignment=1, method=1, size=None):
            fields = (method, 0, len(payload) if size is None else size, alignment) if elf64 else (
                method, len(payload) if size is None else size, alignment)
            return struct.pack(endian + header_format, *fields) + zlib.compress(payload)
        variant_elf('compressed-strings-' + tag,
                    [dict(type=3, flags=0x800, align=8, data=compressed(b'\0x\0'))],
                    elf64, endian)
        for alignment in ((4, 8) if elf64 else (4,)):
            # A five-byte owner forces padding based on the full twelve-byte note header.
            note = struct.pack(endian + 'III', 5, 1, 1) + b'ABCD\0'
            note += b'\xff' * (-len(note) % alignment)
            note += b'X' + b'\xff' * (-1 % alignment)
            variant_elf('note-align' + str(alignment) + '-' + tag,
                        [dict(type=7, align=alignment, data=note)], elf64, endian)
            if alignment == 8:
                # Error case: omit the final four padding bytes of an eight-aligned note.
                variant_elf('note-short8-' + tag,
                            [dict(type=7, align=8, data=struct.pack(endian + 'III', 2, 1, 1)
                                  + b'A\0\0\0X\0\0\0')], elf64, endian)
        if elf64 and endian == '<':
            strings = dict(type=3, flags=0x800, align=8, data=compressed(b'\0x\0'))
            variant_elf('compressed-symbol-strings', [strings, prefix[1]])
            variant_elf('compressed-section-names',
                        [dict(strings, data=compressed(b'\0.text\0')),
                         dict(type=1, name=1)], names=1)
            variant_elf('compressed-empty', [dict(strings, data=compressed(b''))])
            # The compression header records logical alignment independently of file alignment.
            note = struct.pack('<III', 2, 1, 1) + b'A\0\0\0X\0\0\0'
            variant_elf('compressed-note',
                        [dict(type=7, flags=0x800, align=8, data=compressed(note, 4))])
            for label, payload in [
                ('method', compressed(b'\0', method=2)),
                ('size', compressed(b'\0x\0', size=4)),
                ('alignment', compressed(b'\0', alignment=3)),
                ('truncated', compressed(b'\0')[:-1]),
                ('trailing', compressed(b'\0') + b'x'),
                ('header', b'\0'*12),
                ('huge-size', compressed(b'\0', size=2**63)),
            ]:
                # Error case: each malformed compression header or stream is rejected.
                variant_elf('compressed-bad-' + label, [dict(strings, data=payload)])
            # Compressed allocated sections are allowed in relocatable files.
            variant_elf('compressed-alloc', [dict(strings, flags=0x802)])
            # Error cases: allocated compression is forbidden in executables/shared files.
            variant_elf('compressed-alloc-exec', [dict(strings, flags=0x802)], filetype=2)
            variant_elf('compressed-alloc-shared', [dict(strings, flags=0x802)], filetype=3)
            # Error case: no SHT_NOBITS section can carry SHF_COMPRESSED.
            variant_elf('compressed-nobits', [dict(strings, type=8)])
            # Error case: Elf64_Chdr's reserved field is zero.
            payload = bytearray(compressed(b'\0'))
            struct.pack_into('<I', payload, 4, 1)
            variant_elf('compressed-bad-reserved', [dict(strings, data=payload)])

# Symbol-free REL/RELA sections in executables/shared files need no symbol table.
for elf64 in (False, True):
    for endian in ('<', '>'):
        tag = ('64' if elf64 else '32') + ('le' if endian == '<' else 'be')
        for addends in (False, True):
            entry_format = ('QQ' if elf64 else 'II') + ('q' if elf64 else 'i') * addends
            payload = struct.pack(endian + entry_format, 0x1000, 8, *([-4] if addends else []))
            variant_elf('symbolless-' + ('rela-' if addends else 'rel-') + tag,
                        [dict(type=4 if addends else 9, align=8 if elf64 else 4,
                              entsize=len(payload), data=payload)], elf64, endian, filetype=3)

# MIPS64EL r_info contains a little-endian symbol followed by big-endian type bytes.
for endian in ('<', '>'):
    for addends in (False, True):
        for symbol, relocation_type, label in ((1, 18, 'ordinary'), (1, 0, 'none'),
                                               (1, 0x01020304, 'compound'), (2, 18, 'bad-symbol')):
            info = (struct.pack('<I', symbol) + struct.pack('>I', relocation_type)
                    if endian == '<' else struct.pack('>Q', (symbol << 32) | relocation_type))
            payload = struct.pack(endian + 'Q', 0x1000) + info
            if addends:
                payload += struct.pack(endian + 'q', -4)
            name = ('mips-' + ('rela-' if addends else 'rel-') + label +
                    ('-64le' if endian == '<' else '-64be'))
            variant_elf(name, [dict(type=3, data=b'\0x\0'),
                              dict(type=2, data=b'\0'*24 + struct.pack(endian + 'IBBHQQ',
                                   1, 0x10, 0, 0, 0, 0), link=1, info=1, entsize=24, align=8),
                              dict(type=4 if addends else 9, data=payload, link=2,
                                   entsize=len(payload), align=8)], endian=endian)
            path = out / (name + '.elf')
            data = bytearray(path.read_bytes())
            struct.pack_into(endian + 'H', data, 18, 8)  # EM_MIPS
            path.write_bytes(data)

# Section cardinality and address alignment are independent of class and byte order.
for elf64 in (False, True):
    for endian in ('<', '>'):
        word = 8 if elf64 else 4
        tag = ('64' if elf64 else '32') + ('le' if endian == '<' else 'be')
        symbol_size = 24 if elf64 else 16
        strings = dict(type=3, data=b'\0')
        symbol = dict(type=2, data=b'\0'*symbol_size, link=1, info=1,
                      align=word, entsize=symbol_size)
        dynsym = dict(symbol, type=11)
        dynamic = dict(type=6, data=b'\0'*(word*2), link=1, align=word, entsize=word*2)
        hash_table = dict(type=5, data=struct.pack(endian+'IIII', 1, 1, 0, 0),
                          link=2, align=4, entsize=4)
        # Error cases: gABI permits at most one of each of these section types.
        for label, sections in (
            ('symtab', [strings, symbol, symbol]),
            ('dynsym', [strings, dynsym, dynsym]),
            ('hash', [strings, symbol, hash_table, hash_table]),
            ('dynamic', [strings, dynamic, dynamic]),
        ):
            variant_elf('duplicate-' + label + '-' + tag, sections, elf64, endian)
        variant_elf('single-tables-' + tag,
                    [strings, symbol, dynsym, hash_table, dynamic], elf64, endian)
        variant_elf('multiple-strtab-' + tag, [strings, strings], elf64, endian)
        for label, address, alignment in (('misaligned-address', 1, 8),
                                           ('aligned-address', 8, 8),
                                           ('unconstrained-address', 1, 0),
                                           ('unit-alignment-address', 1, 1)):
            variant_elf(label + '-' + tag,
                        [dict(type=1, data=b'x', flags=2, align=alignment)], elf64, endian)
            # Error case: only the first address violates its declared alignment.
            path = out / (label + '-' + tag + '.elf')
            data = bytearray(path.read_bytes())
            hsize, ssize = (64, 64) if elf64 else (52, 40)
            struct.pack_into(endian + ('Q' if elf64 else 'I'), data,
                             hsize + ssize + (16 if elf64 else 12), address)
            path.write_bytes(data)
