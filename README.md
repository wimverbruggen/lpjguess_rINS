# LPJ-GUESS rINS
- R package to read/write LPJ-GUESS ins files.
- Flexible because you can add any parameters, PFTs or groups, and it will handle them!
- The reader also recursively includes any "imports" of other ins files.

How to use
- Reading: `my_params <- read_ins("my_ins_file.ins")`
- Writing: `write_ins(my_params,"my_new_ins_file.ins")`

To do
- Deal with "param" lines in a better way.

