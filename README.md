# Virtual machine translator

This is an implementation of the Jack compiler as described in [Project
10](https://www.nand2tetris.org/project10) and [Project
11](https://www.nand2tetris.org/project11).

Run the compiler:
```sh
  dune exec ./jack_compiler.exe <path_to_directory_without_last_Slash>
```

The input must either be a  a directory holding `.jack` files at
the top level. The output will be `.vm` files in the same directory
