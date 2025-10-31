# Load reader function
source("functions.r")

# Path to ins file
path.ins <- "global_cf.ins"

# Read parameters
params <- read_ins(path.ins)

# Change a few things
params$pft$TeBE$longevity <- 1234

# Write to new file! Note that this will look a bit different and doesn't have any comments, but it should be readable by LPJ-GUESS.
write_ins(params,"new_ins_file.ins")

# Now as a test, reread that ins file and write it again! 
# Then check the difference between the files.
params_new <- read_ins("new_ins_file.ins")
params_new$pft$TeBE$longevity <- 4321
write_ins(params_new,"new_ins_file2.ins")
system("diff new_ins_file.ins new_ins_file2.ins")
