# c-compiler-hs
I don't (can't) use C though.

## Build
```bash
stack install
```

## Usage
```bash
~/.local/bin/c-compiler-hs-exe [Source File] >> out.asm
nasm -f elf64 out.asm
ld -o out-exe out.o
./out-exe
```
