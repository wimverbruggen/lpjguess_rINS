# LPJ-GUESS rINS

What?
- R package to read/write LPJ-GUESS ins files.
- Flexible because you can add any parameters, PFTs, groups or stands to your ins file, and it will handle them, i.e. no parameter names are hardcoded.
- The reader also recursively includes any "imports" of other ins files, so just read the main one.

Why?
- For tracking changes in model parameters in a more structured and automated way than using the text files.
- Helpful for doing sensitivity tests!

How to use
- Reading: `my_params <- read_ins("my_ins_file.ins")`
- Writing: `write_ins(my_params,"my_new_ins_file.ins")`
- See `readExample.R` for a script that reads the included global\*.ins files!

To do
- Deal with "param" lines in a better, more structured, way.
- Add metadata, which could be included as comments in the INS file header?
