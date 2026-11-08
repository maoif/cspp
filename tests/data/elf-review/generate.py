from pathlib import Path
import struct
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
