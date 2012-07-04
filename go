as -g compiled.s -o compiled.o
as -g til_funcs.s -o til_funcs.o
ld -g compiled.o til_funcs.o -o a.out


